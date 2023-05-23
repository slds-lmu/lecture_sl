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

####### INTRA CLASS COVARIANCE EXAMPLE ########
# Create a multi-variate  normal distribution with high covariance
covariance_matrix = matrix(c(variance,0,0,variance),ncol= 2)

results = create_classification_task(covariance_matrix = covariance_matrix,
                           n_data = n_data,
                           means = c(0,0)
                           )
x1 = results$x[, 1]
x2 = results$x[, 2]
y = results$y
# constant in direction of covariance
x1[y==1] = x1[y==1]+0*variance

# Create a dataframe with x and y
data = data.frame(x1=x1,x2=x2,y=y)
data$y = as.factor(data$y)

#calculate the t test for x1 and y and extract the p value
t_test = t.test(data[data$y==1,]$x1,data[data$y==0,]$x1)
statistic0= t_test$statistic
# Plot histogram of x1 for each y and add the p value in some text
p0 <- ggplot(data, aes(x = x1, fill = y)) +
  geom_histogram(alpha = 0.6, position = "identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Histogram of x1 for each y", x = "x1", y = "Count") +
  geom_text(aes(x = 0.5, y = 50, label = paste("statistic =", round(statistic0, 3))), size = 6)

x1 = results$x[, 1]
x2 = results$x[, 2]
y = results$y
# constant in direction of covariance
x1[y==1] = x1[y==1]+0.5*variance

# Create a dataframe with x and y
data = data.frame(x1=x1,x2=x2,y=y)
data$y = as.factor(data$y)

#calculate the t test for x1 and y and extract the p value
t_test = t.test(data[data$y==1,]$x1,data[data$y==0,]$x1)
statistic05 = t_test$statistic

# Plot histogram of x1 for each y and add the p value in some text
p5 <- ggplot(data, aes(x = x1, fill = y)) +
  geom_histogram(alpha = 0.6, position = "identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Histogram of x1 for each y", x = "x1", y = "Count") +
  geom_text(aes(x = 0.5, y = 50, label = paste("statistic =", round(statistic05, 3))), size = 5)


x1 = results$x[, 1]
x2 = results$x[, 2]
y = results$y
# constant in direction of covariance
x1[y==1] = x1[y==1]+1*variance

# Create a dataframe with x and y
data = data.frame(x1=x1,x2=x2,y=y)
data$y = as.factor(data$y)

#calculate the t test for x1 and y and extract the p value
t_test = t.test(data[data$y==1,]$x1,data[data$y==0,]$x1)
statistic10 = t_test$statistic

# Plot histogram of x1 for each y and add the p value in some text
p10 <- ggplot(data, aes(x = x1, fill = y)) +
  geom_histogram(alpha = 0.6, position = "identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Histogram of x1 for each y", x = "x1", y = "Count") +
  geom_text(aes(x = 0.5, y = 50, label = paste("statistic =", round(statistic10, 3))), size = 5)

x1 = results$x[, 1]
x2 = results$x[, 2]
y = results$y
# constant in direction of covariance
x1[y==1] = x1[y==1]+2.5*variance

# Create a dataframe with x and y
data = data.frame(x1=x1,x2=x2,y=y)
data$y = as.factor(data$y)

#calculate the t test for x1 and y and extract the p value
t_test = t.test(data[data$y==1,]$x1,data[data$y==0,]$x1)
statistic25 = t_test$statistic

# Plot histogram of x1 for each y and add the p value in some text
p25 <- ggplot(data, aes(x = x1, fill = y)) +
  geom_histogram(alpha = 0.6, position = "identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Histogram of x1 for each y", x = "x1", y = "Count") +
  geom_text(aes(x = 0.5, y = 50, label = paste("statistic =", round(statistic25, 3))), size = 5)

# Create one plot with the 4 histogram

grid= grid.arrange(p0,p5,p10,p25, ncol=2, nrow=2)

#save the plot
ggsave("slides/feature-selection/figure/fs-t-test.png", grid, width = 20, height = 20, units = "cm")




