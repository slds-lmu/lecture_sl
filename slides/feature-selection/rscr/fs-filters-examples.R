

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
if (!requireNamespace("ranger")) {
  install.packages("ranger")
}
if (!requireNamespace("praznik")) {
  install.packages("praznik")
}

filter_nonrelevant_features_from_task<-function(task){
  features=task$feature_names
  # Remove some features in the case of boston housing or kc housing
  features = features [features!="town"]
  features = features [features!="chas"]
  features = features [features!="cmedv"]
  features = features [features!="date"]
  features = features [features!="waterfront"]
  task$select(features)
  return(task)
}



# Load the mlr3 library
library(mlr3)
library(mlr3data)
library(mlr3filters)
library(mlr3viz)

# Parameters
task_name = "boston_housing" # Sonar or boston housing
if (task_name == "boston_housing"){
  classifier = "regr.rpart"
  measure_name = "regr.rmse"
  max_features= 15 # Max 15
  n_runs=20
  sample_size=0.1 # Make dataset smaller to make  p ~ n
  min_features=1
  # filter
  filter_names = c("correlation","carscore")
} else if (task_name == "sonar") {
  classifier = "classif.ranger"
  measure_name = "classif.acc"
  max_features= 60 # Max 60
  sample_size=0.75
  min_features=5
  n_runs=10
  # filter
  filter_names = c("auc","information_gain","anova")
} else{ 
  stop("Invalid Dataset")
}
filename = paste0("slides/feature-selection/figure/",
                  "filter_comparison_",task_name,"_",classifier,".png")


mlr_learners$get(classifier)
task = tsk(task_name)
features = task$feature_names
# Sample from the dataset without replacement to make it smaller
set.seed(123)
sample = sample(1:nrow(task$data()), size = sample_size*nrow(task$data()), replace = FALSE)
task$filter(sample)

num_features = min_features:max_features
df = expand.grid(filter_names,num_features)
names(df) <- c("filter", "num_features")
df$train_measure = 0
df$test_measure = 0 



# For each filter
for (filter_name in filter_names){
  # Create a for loop to get the top i attributes
  for (i in min_features:max_features){
    # create the measure vectores
    train_measures = c()
    test_measures = c()
    # Run the analysis n times
    for (j in 1:n_runs){
      # Instanciate the task and the splitting technique
      task=tsk(task_name)
      task$filter(sample)
      splits = partition(task, ratio = 3/4)
      task_train = task$clone(deep = TRUE)
      task_train = task_train$filter(splits$train)
      # Remove some non-relevant features
      task_train = filter_nonrelevant_features_from_task(task_train)
      
      # Select the features based only on training set
      filter = flt(filter_name)
      ranked_features= as.data.table(filter$calculate(task_train))
      filtered_attribues= head(ranked_features,i)$feature
      # filter the task using the attributes obtained by the filter
      task$select(filtered_attribues)
      
      #Train the model
      learner <- lrn(classifier)
      learner$train(task, splits$train)
      # Assess errors
      train_pred = learner$predict(task,splits$train)
      test_pred = learner$predict(task,splits$test)
      measure = msr(measure_name)
      # add the performances to the vectors
      train_measures[j] = train_pred$score(measure)[[1]]
      test_measures[j] = test_pred$score(measure)[[1]]
    }
    # calculate the mean of the vectors
    df[df$filter==filter_name & df$num_features==i, "train_measure"] = mean(train_measures)
    df[df$filter==filter_name & df$num_features==i, "test_measure"] = mean(test_measures)
  }
}

# Melt the data frame from wide to long format for plotting
df_long <- reshape2::melt(df, id.vars = c("filter", "num_features"), 
                          measure.vars = c("train_measure", "test_measure"), 
                          variable.name = "error_type", value.name = "error")


# Create the plot
graph<- ggplot(df_long, aes(x = num_features, y = error, color = interaction(filter,error_type), group = interaction(filter, error_type))) +
  geom_line() +
  labs(x = "Number of Features", y = measure_name, color = "Error Type")  +
  ggtitle(paste("Performance of", classifier, "with increasing number of features on dataset",task_name, "with N=",length(sample))) +
  theme_grey() +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # change the legend title
  guides(color = guide_legend(title = "Filter and Error Type")) 

# Save the plot
ggsave(filename, graph, width = 10, height = 6, units = "in", dpi = 300)