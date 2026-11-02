# ------------------------------------------------------------------------------
# l2 nonlin

# FIG: decompose MSE to bias_square and variance for ridge regression.
#      plot lines to show how each part varies 
#      with ln(lambda) (natural logarithm of regularization constant).

# DATA: y = sin(x(100*1 ~Uniform)) + epi (100*1 ~Normal)
#       X = (x^1,...,x^8) (100*8 design matrix)
# ------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(glmnet)

set.seed(0)

# DATA -------------------------------------------------------------------------

true_function <- function(x) sin(x)
n_datasets <- 100
n_samples <- 100
n_test_samples <- 10000
n_order <- 8
lambdas <- exp(seq(-6, 7, length.out = 25))

# Generate polynomial features
poly_features <- function(x, degree) {
  model.matrix(~ poly(x, degree, raw = TRUE) - 1)
}

# Initialize arrays to store the bias, variance, and error
bias_square <- rep(0, length(lambdas))
variance <- rep(0, length(lambdas))
test_error <- rep(0, length(lambdas))

# Generate shared x values for all datasets
x_shared <- runif(n_samples)
x_shared_poly <- poly_features(x_shared, n_order)

# Generate test data
x_test <- runif(n_test_samples)
y_test <- true_function(x_test) + rnorm(n_test_samples)
x_test_poly <- poly_features(x_test, n_order)

for (i in 1:length(lambdas)) {
  predictions <- matrix(0, nrow = n_datasets, ncol = n_samples)
  
  for (j in 1:n_datasets) {
    epsilon <- rnorm(n_samples)
    y <- true_function(x_shared) + epsilon
    
    model <- glmnet(x_shared_poly, y, alpha = 0, lambda = lambdas[i])
    predictions[j, ] <- predict(model, newx = x_shared_poly)
  }
  
  average_prediction <- apply(predictions, 2, mean)
  
  bias_square[i] <- mean((average_prediction - true_function(x_shared))^2)
  variance[i] <- mean(apply(predictions, 2, var))
}


data <- data.frame(log_lambdas = log(lambdas),
                   bias_square = bias_square,
                   variance = variance,
                   MSE = bias_square + variance) %>%
  pivot_longer(cols = c(bias_square, variance, MSE), names_to = "component", values_to = "value")

p <- ggplot(data, aes(x = log_lambdas, y = value, color = component, linetype = component)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("red", "green", "blue")) +
  scale_linetype_manual(values = c("solid", "solid", "solid")) +
  labs(x = expression("ln("~λ~")"), y = "value", title = "Bias-Variance Tradeoff with L2 Regularization") +
  theme_minimal()

ggsave("bias_var_decomp.png", p, width = 12, height = 6)
