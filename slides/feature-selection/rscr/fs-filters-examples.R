

# check if thelibraries are installed, otherwise install
if (!requireNamespace("mlr3")) {
  install.packages("mlr3")
}
if (!requireNamespace("mlr3data")) {
  install.packages("mlr3data")
}
if (!requireNamespace("mlr3filters")) {
  install.packages("mlr3filters")
}
if (!requireNamespace("FSelectorRcpp")) {
  install.packages("FSelectorRcpp")
}
if (!requireNamespace("mlr3learners")) {
  install.packages("mlr3learners")
}
if (!requireNamespace("stats")) {
  install.packages("stats")
}
if (!requireNamespace("mlr3viz")) {
  install.packages("mlr3viz")
}
if (!requireNamespace("praznik")) {
  install.packages("praznik")
}
if (!requireNamespace("OpenML")) {
  install.packages("OpenML")
}
if (!requireNamespace("farff")) {
  install.packages("farff")
}

# Load the mlr3 library
library(mlr3)
library(mlr3data)
library(mlr3learners)
library(mlr3filters)
library(mlr3viz)
library(OpenML)
library(ggplot2)
library(dplyr)


task_name = 'har'
dataset_id=listOMLDataSets()
dataset_id=dataset_id[dataset_id$name==task_name,"data.id"]
dataset = getOMLDataSet(data.id = dataset_id)
dataset=dataset$data
backend = as_data_backend(dataset)
task=mlr3::TaskClassif$new("har",backend,target="Class")

classifier = "classif.kknn"
measure_name = "classif.mbrier"
max_features= 500
sample_size=0.8 
min_features=1
n_runs=10
# filter
filter_names = c("information_gain","mrmr")

filename = paste0("slides/feature-selection/figure/",
                  "filter_comparison_",task_name,"_",classifier,".png")

features = task$feature_names
# Sample from the dataset without replacement to make it smaller
set.seed(123)
sample = sample(1:nrow(task$data()), size = sample_size*nrow(task$data()), replace = FALSE)
task$filter(sample)

num_features = min_features:max_features
df = expand.grid(filter_names,num_features)
names(df) <- c("filter", "num_features")
df$test_measure = 0 


################ THIS SCRIPT WILL TAKE A LOT OF TIME TO RUN #####################

# For each filter
for (filter_name in filter_names){
  # Create a for loop to get the top i attributes
  for (i in min_features:max_features){
    # create the measure vectores
    train_measures = c()
    test_measures = c()
    # Run the model n times ( basically a random CV)
    for (j in 1:n_runs){
      # Instanciate the task and the splitting technique
      task=mlr3::TaskClassif$new(task_name,backend,target="Class")
      task$filter(sample)
      splits = partition(task, ratio = 3/4)
      task_train = task$clone(deep = TRUE)
      task_train = task_train$filter(splits$train)
      # Select the features based only on training set
      filter = flt(filter_name)
      # We filter based on the training data only
      ranked_features= as.data.table(filter$calculate(task_train))
      filtered_attribues= head(ranked_features,i)$feature
      # filter the task using the attributes obtained by the filter
      task$select(filtered_attribues)
      
      #Train the model
      learner <- lrn(classifier, predict_type = "prob")
      learner$train(task, splits$train)
      # Assess errors
      train_pred = learner$predict(task,splits$train)
      test_pred = learner$predict(task,splits$test)
      measure = msr(measure_name)
      # add the performances to the vectors
      train_measures[j] = train_pred$score(measure)[[1]]
      test_measures[j] = test_pred$score(measure)[[1]]
    }
    # calculate the mean of the vectors and add it to the df
    df[df$filter==filter_name & df$num_features==i, "test_measure"] = mean(test_measures)
    df[df$filter==filter_name & df$num_features==i, "train_measure"] = mean(train_measures)
  }
}
# Write the data to a csv file
write.csv(df, file = paste0("slides/feature-selection/rscr/filter_examples_data.csv"),row.names = TRUE)


################# LOAD DATA FROM DISK AND PLOT ###############################
df = read.csv("slides/feature-selection/rscr/filter_examples_data.csv")

# Melt the data frame from wide to long format for plotting
df_long <- reshape2::melt(df, id.vars = c("filter", "num_features"), 
                          measure.vars = c("test_measure"), 
                          variable.name = "error_type", value.name = "error")

# get the best performance for each filter
best_performance = df_long %>% group_by(filter) %>% 
  filter(error == min(error)) %>% 
  select(filter, num_features, error)

# Create the lineplot of the performance and highlight the best performance for each filter 
graph<- ggplot(df_long, aes(x = num_features, y = error, color = filter, group = filter)) +
  geom_line() +
  geom_point(data = best_performance, aes(x = num_features, y = error, color = filter), size = 6) +
  # add the perforamnce and number of features in text
  geom_text(data = best_performance, aes(x = num_features, y = error, label = paste0(num_features, " features", "\n", round(error, 3))), 
            size = 6, hjust = 0.5, vjust = 1.5) +
  # Set y axis limits and change the tick marks
  scale_y_continuous(limits = c(-0.05, 0.6), breaks = seq(-0.05, 0.6, 0.05)) +
  labs(x = "Number of Features", y = measure_name, color = "Error Type")  +
  ggtitle(paste("Performance of", classifier, "with increasing number of features on dataset",task_name)) +
  theme_grey() +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # change the legend title
  guides(color = guide_legend(title = "Filter")) 
 
# Save the plot
ggsave(filename, graph, width = 10, height = 10, units = "in", dpi = 300)
