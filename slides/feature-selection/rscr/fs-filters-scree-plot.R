if (!requireNamespace("mlr3")) {
  install.packages("mlr3")
}
if (!requireNamespace("mlr3data")) {
  install.packages("mlr3data")
}
if (!requireNamespace("mlr3filters")) {
  install.packages("mlr3filters")
}
if (!requireNamespace("ggplot2")) {
  install.packages("ggplot2")
}
if (!requireNamespace("pROC")) {
  install.packages("pROC")
}

# Load the mlr3 library
library(mlr3)
library(mlr3learners)
library(mlr3data)
library(mlr3filters)
library(mlr3viz)
library(OpenML)
library(ggplot2)
filter_method = "information_gain"


task = tsk("spam")
task_data=data.frame(task$data())
features=task$feature_names
task$select(features)
filter = flt(filter_method)
ranked_features= as.data.table(filter$calculate(task))
ranked_features[, score := score^2]

# Create a line plot with the number of features in the x axis and the score in the y
p <- ggplot(ranked_features, aes(x = 1:nrow(ranked_features), y = score)) +
  geom_line(color = "#619CFF") +
  geom_point(size=2,color="#619CFF") +
  theme_minimal() +
  ylab("Information Gain")

classifier <- "classif.log_reg"
measure_name <- "classif.auc"
n_runs <- 2
min_features <- 1
max_features <- 53
mlr_learners$get(classifier)

num_features <- min_features:max_features
df = expand.grid(filter_method,num_features)
names(df) <- c("filter", "num_features")
df$test_measure = 0

for (i in min_features:max_features){
  train_measures <- c()
  test_measures <- c()
  for (j in 1:n_runs){
    task<- tsk("spam")
    # filter
    filtered_attributes <- head(ranked_features,i)$feature
    task$select(filtered_attributes)
    # split
    splits = partition(task, ratio = 3/4)
    learner <- lrn(classifier, predict_type = "prob")
    learner$train(task, splits$train)
    # Assess errors
    train_pred = learner$predict(task,splits$train)
    test_pred = learner$predict(task,splits$test)
    measure = msr(measure_name)

    train_measures[j] = train_pred$score(measure)[[1]]
    test_measures[j] = test_pred$score(measure)[[1]]
  }
  df[df$num_features==i, "test_measure"] = mean(test_measures)
}
df = read.csv("slides/feature-selection/rscr/scree-plot-data.csv")
# Melt the data frame from wide to long format for plotting
df_long <- reshape2::melt(df, id.vars = c("filter", "num_features"),
                          measure.vars = c("test_measure"),
                          variable.name = "error_type", value.name = "error")

score.diff <- max(ranked_features$score) - min(ranked_features$score)
measure.diff <- max(df$test_measure) - min(df$test_measure)
score.min <- min(ranked_features$score)
measure.min <- min(df$test_measure)


# Add the plot of the performance to the first plot in a second y axis
p <- p + labs(title = "Feature Score and log_reg model performance. Dataset: SPAM", x = "Feature rank/ Features used in the model")

plot <- p +
  geom_line(data = df_long,
            aes(x = num_features,
                y = (error-measure.min)/ measure.diff * score.diff + score.min,
                color = filter,
                group = filter),
            size=1.5) +
  geom_point(data = df_long,
             aes(x = num_features,
                 y = (error-measure.min)/ measure.diff * score.diff + score.min,
                 color = filter,
                 group = filter),
             size=3) +
  scale_y_continuous(sec.axis = sec_axis(
    ~((. -score.min) * measure.diff / score.diff) + measure.min, name = "AUC")) +
  theme_gray() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20),  # increased size for plot title
        axis.title.x = element_text(size = 20),  # increased size for x axis title
        axis.title.y = element_text(size = 20),  # increased size for y axis title
        axis.text.x = element_text(size = 20),   # increased size for x axis text
        axis.text.y = element_text(size = 20),   # increased size for y axis text
        legend.text = element_text(size = 20),   # increased size for legend text
        legend.title = element_text(size = 20))  # increased size for legend title

# save the plot
ggsave("slides/feature-selection/figure/fs-filters-scree-plot.png", plot, width = 20, height = 15, units = "cm")
