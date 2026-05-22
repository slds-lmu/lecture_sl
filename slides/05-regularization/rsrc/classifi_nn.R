# ------------------------------------------------------------------------------
# nonlin

# FIG: 
#  (1) classification prediction, weights histogram, weights values
#      with different lambdas (decay parameter) by nn.
#  (2) classification prediction with different sizes of hidden layer by nn.
#  (3) how classification errors change with different lambdas, 
#      and with different sizes of hidden layer by nn.

# DATA: "spirals" from mlr3.
# ------------------------------------------------------------------------------

library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(mlr3misc)

library(ggplot2)

library(gridExtra)
library(grid)

options(digits = 3, 
        width = 65, 
        str = strOptions(strict.width = "cut", vec.len = 3))

set.seed(1234)

# PLOT FUNCTIONS ---------------------------------------------------------------

# value for each weight
plot_weights <- function (weights, zero_lamb=TRUE) {
  weight_data <- data.frame(value = weights, weight_index = seq_along(weights))
  
  ylimit <- c(-30, 30)
  if(zero_lamb){
    ylimit <- c(-200, 200)
  }
  
  ggplot(weight_data, aes(x=weight_index, y = value)) + 
    geom_bar(stat ="identity", color="black", fill="white") +
    ylim(ylimit) +
    ggtitle("Weights")
}

# histogram of weights
plot_histogram <- function (weights, zero_lamb=TRUE) {
  weight_data <- data.frame(value = weights)
  
  xlimit <- c(-30, 30)
  if(zero_lamb){
    xlimit <- c(-200, 200)
  }
  
  ggplot(weight_data, aes(x=weights)) + 
    geom_histogram (bins= 15, color="black", fill="white") +
    ggtitle("Histogram of weights") +
    xlab ("value of weights") +
    xlim(xlimit)
}

# classification model visualization
plot_prediction <- function (learner, task) {
  plot_learner_prediction(learner, task) + 
    scale_fill_viridis_d(end = .9) + 
    theme(legend.position = "none") +
    ggtitle("Prediction")
}

# DATA -------------------------------------------------------------------------

spirals_generator <- tgen("spirals", sd = 0.1)

spirals_task <- spirals_generator$generate(n=100)

# PLOT PREDICTION & WEIGHTS ----------------------------------------------------

### Different decay parameters
decay_list <- list(0, 0.001, 0.005, 0.01, 0.05, 0.1)

size <- 10

for(i in seq_along(decay_list)){
  learner <- lrn("classif.nnet", size = size, decay = decay_list[[i]])
  learner$train(spirals_task)
  weights <- learner$model$wts
  zero_lamb = FALSE
  if(decay_list[[i]]==0){
    zero_lamb = TRUE
  }
  weight_plot <- plot_weights(weights = weights, zero_lamb = zero_lamb) 
  historgram_plot <-plot_histogram(weights = weights, zero_lamb = zero_lamb)
  
  prediction_plot <- plot_prediction(learner, spirals_task)
  
  grid <- grid.arrange(prediction_plot, historgram_plot, weight_plot, ncol = 3, 
                       top = textGrob(bquote(lambda==.(decay_list[[i]])), 
                                      gp = gpar(fontsize = 14))) 
  
  ggsave(filename = paste0("../figure/classifi_nn_w_size_", i ,".png"), 
         plot = grid, width = 8, height = 3) 
  
}

### Different size of hidden layer
size_list <- list(1, 2, 3, 5, 10, 100)
decay <- 0.001

for(i in seq_along(size_list)){
  learner <- lrn("classif.nnet", size = size_list[[i]], decay = decay )
  
  learner$train(spirals_task)
  weights <- learner$model$wts
  weight_plot <- plot_weights(weights = weights) 
  historgram_plot <-plot_histogram(weights = weights)
  
  prediction_plot <- plot_prediction(learner, spirals_task)
  
  grid <- grid.arrange(prediction_plot, ncol = 1, 
                       top = textGrob(bquote(size~of~hidden~layer==.(size_list[[i]])), 
                                      gp = gpar(fontsize = 14))) 
  
  ggsave(filename = paste0("../figure/classifi_nn_size_", i ,".png"), 
         plot = grid, width = 3, height = 3) 
  
}

# PLOT CLASSIFICATION ERROR ----------------------------------------------------

### Different decay parameters
folds <- 10; reps <- 5;
size <- 10
decay_list <- seq(0, 0.02, length.out = 20)
rdesc <- rsmp("repeated_cv", folds = folds, repeats = reps)
lrns <- lapply(decay_list, function(d) lrn("classif.nnet", size = size, decay = d))
gg <- benchmark_grid(tasks = spirals_task, resamplings = rdesc, learners = lrns)
br <- benchmark(gg)
a1 <- br$aggregate(measures = msr("classif.ce"), params = TRUE)
a1 <- unnest(a1, "params")

a1$log_decay <- log(a1$decay + 1) #make U-shape more obivious
p1 <- ggplot(data = a1, aes(x = log_decay, y = classif.ce)) +
  geom_line() + 
  xlab(expression("log("~lambda~"+ 1 )")) + ylab("classif err") +
  xlim(0, 0.01) + ylim(0.1, 0.25)

ggsave(filename = paste0("../figure/classifi_nn_err_decay.png"), 
       plot = p1, width = 6, height = 3)
### Different size of hidden layer
folds <- 10; reps <- 5; by <- 1
decay <- 0.001
size_list <- seq(1, 30, by = by)
rdesc <- rsmp("repeated_cv", folds = folds, repeats = reps)
lrns <- lapply(size_list, function(s) lrn("classif.nnet", size = s, decay = decay))
gg <- benchmark_grid(tasks = spirals_task, resamplings = rdesc, learners = lrns)
br <- benchmark(gg)
a2 <- br$aggregate(measures = msr("classif.ce"), params = TRUE)
a2 <- unnest(a2, "params")
p2 <- ggplot(data = a2, aes(x = size, y = classif.ce)) +
  geom_line() + 
  xlab("size hidden layer") + ylab("classif err")

ggsave(filename = paste0("../figure/classifi_nn_err_size.png"), 
  plot = p2, width = 6, height = 3) 

