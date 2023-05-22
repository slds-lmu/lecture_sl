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
library(mlr3)
library(mlr3data)
library(mlr3filters)
library(ggplot2)
library(pROC)

# define a function that calculates the ROC curve for a given feature
apply_roc_curve <- function(target, feature) {
  roc <- roc(target, feature)
  # If the AUC is less than 0.5, we need to invert the curve
  if (roc$auc < 0.5) {
    roc <- roc(target,-feature)
  }
  return(roc)
}

task = tsk("spam")
task_data=data.frame(task$data())
features=task$feature_names
task$select(features)
filter = flt('auc')
ranked_features= as.data.table(filter$calculate(task))

# Get the best feature, the worst and another one in the middle
first_feature=ranked_features[1,feature]
mid_feature=ranked_features[round(nrow(ranked_features)/2)-20,feature]
last_feature=ranked_features[nrow(ranked_features),feature]

features= c(first_feature, mid_feature, last_feature)


roc_first_feature <- apply_roc_curve(task_data$type,task_data[,first_feature])
roc_mid_feature <- apply_roc_curve(task_data$type,task_data[,mid_feature])
roc_last_feature <- apply_roc_curve(task_data$type,task_data[,last_feature])

#Plot
p <- ggroc(list(charExclamation = roc_first_feature,
                parts = roc_last_feature,
                our= roc_mid_feature))
p <- p + ggtitle("ROC using features (word/character frequency) as thresholds, Dataset: Spam")
p <- p + theme(plot.title = element_text(hjust = 0.5))
p <- p + ylab("Sensitivity")
p <- p + xlab("Specificity")
p <- p + theme(legend.position = "bottom")
p <- p + geom_segment(
  aes(x = 1, xend = 0, y = 0, yend = 1), color = "grey", linetype = "dashed"
)
# Adding AUCs
p <- p + annotate(
  "text", x = 0.9, y = 0.9, colour='#F8766D', label = paste0("AUC = ", round(roc_first_feature$auc, 2))
)
p <- p + annotate(
  "text", x = 0.9, y = 0.8,colour='#619CFF',label = paste0("AUC = ", round(roc_mid_feature$auc, 2))
)
p <- p + annotate(
  "text", x = 0.9, y = 0.7,colour='#00BA38', label = paste0("AUC = ", round(roc_last_feature$auc, 2))
)
#save the plot
ggsave("slides/feature-selection/figure/fs-roc-curve.png", p, width = 20, height = 12, units = "cm")






