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

library(GGally)
library(ggplot2)
library(mvtnorm)

custom_hist <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) +
    geom_histogram(data = data[data$y == 0,], fill = "#F8766D", alpha = 0.6) +
    geom_histogram(data = data[data$y == 1,], fill = "#00BFC4", alpha = 0.6)
}

#Parameters
variance=1
n_data=1500


# Create the class vector
y = c(rep(1,n_data/2),rep(0,n_data/2))
# Create a multi-variate  normal distribution
covariance_matrix = matrix(c(variance,
                             0.96*variance,0.96*variance,
                             variance),
                           ncol= 2)
x = rmvnorm(n_data,
            mean = c(0,0),
            sigma = covariance_matrix
            )
x1 = x[,1]
x2 = x[,2]
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



