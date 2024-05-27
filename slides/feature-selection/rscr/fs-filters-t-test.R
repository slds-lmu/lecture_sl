# check if thelibraries are installed, otherwise install
if (!requireNamespace("GGally")) {
  install.packages("GGally")
}
if (!requireNamespace("ggplot2")) {
  install.packages("ggplot2")
}
if (!requireNamespace("mvtnorm")) {
  install.packages("mvtnorm")
}
if (!requireNamespace("gridExtra")) {
  install.packages("gridExtra")
}
if (!requireNamespace("ggpubr")) {
  install.packages("ggpubr")
}

library(gridExtra)
library(GGally)
library(ggplot2)
library(mvtnorm)

create_classification_task<- function(covariance_matrix,n_data,means){
  y = c(rep(1,n_data/2),rep(0,n_data/2))
  x = rmvnorm(n_data,
            mean = means,
            sigma = covariance_matrix
            )
  return(list(x=x,y=y))
}

#Parameters
variance=1
n_data=1000
covariance_matrix = matrix(c(variance,0,0,variance),ncol= 2)
task_data = create_classification_task(covariance_matrix = covariance_matrix,
                           n_data = n_data,
                           means = c(0,0)
                           )

create_histogram_plot <-function (task_data, translation){
  x1 = task_data$x[, 1]
  x2 = task_data$x[, 2]
  y = task_data$y
  x1[y==1] = x1[y==1]+translation * variance
  data = data.frame(x1=x1,x2=x2,y=y)
  data$y = as.factor(data$y)
  #calculate the t test for x1 and y and extract the p value
  t_test = t.test(data[data$y==1,]$x1,data[data$y==0,]$x1)
  statistic= t_test$statistic
  # Plot histogram and a line representing the gaussian of x1 for each y and add the p value in some text
  plot <- ggplot(data, aes(x = x1, fill = y)) +
  geom_histogram(alpha = 0.6, position = "identity",aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(data[data$y==1,]$x1),
   sd = sd(data[data$y==1,]$x1)), color = "#00BFC4", size = 1) +
  stat_function(fun = dnorm, args = list(mean = mean(data[data$y==0,]$x1),
   sd = sd(data[data$y==0,]$x1)), color = "#F8766D", size = 1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  #labs(title = "Histogram of x1 for each y", x = "x1", y = "Count") +
  geom_text(aes(x = 0.5, y = 0.5,
   label = paste("statistic =", round(statistic, 3))), size = 6)
  
  return(plot)
}

plot1 = create_histogram_plot(task_data, 1) 
plot25 = create_histogram_plot(task_data, 2.5)
grid=grid.arrange(plot1,plot25, ncol=2)
#save the plot
ggsave("slides/feature-selection/figure/fs-t-test.png", grid, width = 20, height = 10, units = "cm")




