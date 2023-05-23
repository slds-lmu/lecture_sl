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

library(gridExtra)
library(GGally)
library(ggplot2)
library(mvtnorm)

custom_hist <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) +
    geom_histogram(data = data[data$y == 0,], fill = "#F8766D", alpha = 0.6) +
    geom_histogram(data = data[data$y == 1,], fill = "#00BFC4", alpha = 0.6)
}

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
n_data=2000

####### Variable useless by itselfb but useful with others ########

# Create a multi-variate  normal distribution
covariance_matrix = matrix(c(variance,
                             0.96*variance,0.96*variance,
                             variance),
                           ncol= 2)
results=create_classification_task(covariance_matrix = covariance_matrix,
                           n_data = n_data,
                           means = c(0,0)
                           )

x1 <- results$x[, 1]
x2 <- results$x[, 2]
y <- results$y
# add a constant if y is 1 to x2
x2[y==1] = x2[y==1]+2*variance
# Create a dataframe with x and y
data = data.frame(x1=x1,x2=x2,y=y)
data$y = as.factor(data$y)
# Use ggpairs only for x and x1 
graph<- ggpairs(data = data,
                columns = c("x1","x2"),
                legend=2,
                aes(group = y,colour=y, alpha = 0.6),
                upper = list(continuous = wrap("points", alpha = 0.6)),
                diag = list(continuous = custom_hist)
                )+
  theme(legend.position = "bottom")
#Save the plot
ggsave(filename = "slides/feature-selection/figure/guyon_example_correlation.png",
       plot = graph,
       width = 10, height = 10, units = "cm")


####### Information gain from presumably redundant variables ########

# Create a multi-variate  normal distribution with no cov
covariance_matrix <- matrix(c(variance, 0, 0, variance), ncol = 2)
results <- create_classification_task(covariance_matrix = covariance_matrix,
                           n_data = n_data,
                           means = c(0, 0)
                           )
x1 <- results$x[, 1]
x2 <- results$x[, 2]
y <- results$y

# add a constant if y is 1 to x2
x1[y==1] = x1[y==1]+1.5*variance
x2[y==1] = x2[y==1]+1.5*variance
# Create a dataframe with x and y
data = data.frame(x1=x1,x2=x2,y=y)
data$y = as.factor(data$y)

# Use ggpairs only for x and x1 
graph<- ggpairs(data = data,
                columns = c("x1","x2"),
                legend=2,
                aes(group = y,colour=y, alpha = 0.6),
                upper = list(continuous = wrap("points", alpha = 0.6)),
                diag = list(continuous = custom_hist)
                )+
  theme(legend.position = "bottom")
ggsave(filename = "slides/feature-selection/figure/guyon_example_presumably_redundant.png",
       plot = graph,
       width = 10, height = 10, units = "cm")

x1 <- results$x[, 1]
x2 <- results$x[, 2]
y <- results$y

# Perform a rotation
x1[y==1] = x1[y==1]+sqrt(2)*1.5*variance
# Create a dataframe with x and y
data = data.frame(x1=x1,x2=x2,y=y)
data$y = as.factor(data$y)



graph <- ggpairs(data = data,
                columns = c("x1","x2"),
                legend=2,
                aes(group = y,colour=y, alpha = 0.6),
                upper = list(continuous = wrap("points", alpha = 0.6)),
                diag = list(continuous = custom_hist)
                )+
  theme(legend.position = "bottom")
ggsave(filename = "slides/feature-selection/figure/guyon_example_presumably_redundant_rotated.png",
       plot = graph,
       width = 10, height = 10, units = "cm")



####### INTRA CLASS COVARIANCE EXAMPLE ########
# Create a multi-variate  normal distribution with high covariance
covariance_matrix = matrix(c(variance,0.95*variance,0.95*variance,variance),ncol= 2)

results = create_classification_task(covariance_matrix = covariance_matrix,
                           n_data = n_data,
                           means = c(0,0)
                           )
x1 = results$x[, 1]
x2 = results$x[, 2]
y = results$y
# constant in direction of covariance
x1[y==1] = x1[y==1]+2.5*variance
x2[y==1] = x2[y==1]+2.5*variance
# Create a dataframe with x and y
data = data.frame(x1=x1,x2=x2,y=y)
data$y = as.factor(data$y)

graph<- ggpairs(data = data,
                columns = c("x1","x2"),
                legend=2,
                aes(group = y,colour=y, alpha = 0.3),
                upper = list(continuous = wrap("points", alpha = 0.3)),
                diag = list(continuous = custom_hist)
                )+
  theme(legend.position = "bottom")
ggsave(filename = "slides/feature-selection/figure/guyon_example_intra_class_covariance.png",
       plot = graph,
       width = 10, height = 10, units = "cm")

x1 = results$x[, 1]
x2 = results$x[, 2]
y = results$y
# constant in direction of covariance
x1[y==1] = x1[y==1]+2.5*variance
x2[y==1] = x2[y==1]-2.5*variance
# Create a dataframe with x and y
data = data.frame(x1=x1,x2=x2,y=y)
data$y = as.factor(data$y)

graph<- ggpairs(data = data,
                columns = c("x1","x2"),
                legend=2,
                aes(group = y,colour=y, alpha = 0.3),
                upper = list(continuous = wrap("points", alpha = 0.3)),
                diag = list(continuous = custom_hist)
                )+
  theme(legend.position = "bottom")
ggsave(filename = "slides/feature-selection/figure/guyon_example_intra_class_covariance_perpendicular.png",
       plot = graph,
       width = 10, height = 10, units = "cm")


#### XOR-example

xor_pos = c(rep(1,n_data/4),rep(0,n_data/4),rep(1,n_data/4),rep(0,n_data/4))
# Create a multi-variate  normal distribution with 0 covariance
covariance_matrix = matrix(c(variance,0,0,variance),ncol= 2)

results = create_classification_task(covariance_matrix = covariance_matrix,
                           n_data = n_data,
                           means = c(0,0)
                           )

x1 = results$x[, 1]
x2 = results$x[, 2]
y = results$y

#Move half of the positive to (1,1)
x1[y==1 & xor_pos==0] = x1[y==1 & xor_pos==0]+ 4*variance
x2[y==1 & xor_pos==0] = x2[y==1 & xor_pos==0]+ 4*variance
#Move the other half of the positives to (-1,-1)
x1[y==1 & xor_pos==1] = x1[y==1 & xor_pos==1]- 4*variance
x2[y==1 & xor_pos==1] = x2[y==1 & xor_pos==1]- 4*variance
#Move half of the negatives to (-1,1)
x1[y==0 & xor_pos==0] = x1[y==0 & xor_pos==0]- 4*variance
x2[y==0 & xor_pos==0] = x2[y==0 & xor_pos==0]+ 4*variance
#Move the other half of the negatives to (1,-1)
x1[y==0 & xor_pos==1] = x1[y==0 & xor_pos==1]+ 4*variance
x2[y==0 & xor_pos==1] = x2[y==0 & xor_pos==1]- 4*variance

# Create a dataframe with x and y
data = data.frame(x1=x1,x2=x2,y=y)
data$y = as.factor(data$y)

graph<- ggpairs(data = data,
                columns = c("x1","x2"),
                legend=2,
                aes(group = y,colour=y, alpha = 0.6),
                upper = list(continuous = wrap("points", alpha = 0.3)),
                diag = list(continuous = custom_hist)
                )+
  theme(legend.position = "bottom")
ggsave(filename = "slides/feature-selection/figure/guyon_example_xor.png",
       plot = graph,
       width = 10, height = 10, units = "cm")
