# ------------------------------------------------------------------------------
# early stopping

# FIG: 
# LEFT: how coefficients of a linear model change 
#       with regularization constant (lambda) for ridge regression.
# RIGHT: how coefficients of a linear model change with iterations for SGD.

# DATA: linear regression model data generated by
#       y = X(100*10 ~Normal)·true_coef(10*1) + noise(100*1 ~Normal).
# ------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

set.seed(6)

# DATA -------------------------------------------------------------------------

# generate data for design matrix, response variable, 
# and true coefficients for a linear model
# with n samples, p features and no intercept.
generate_data <- function(n, p) {
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  true_coef <- seq(-1, 1, length.out = p)
  noise <- rnorm(n)
  y <- X %*% true_coef + noise
  return(list(X = X, y = y, true_coef = true_coef))
}

# compute the ridge coefficients analytically
compute_ridge_path <- function(X, y, alphas) {
  coefs <- matrix(0, nrow = 1, ncol = ncol(X))
  for (i in 1:length(alphas)) {
    ridge_coefs <- solve(t(X) %*% X + alphas[i] * diag(ncol(X))) %*% t(X) %*% y
    coefs <- rbind(coefs, as.vector(ridge_coefs))
  }
  return(coefs)
}

# compute the optimization trajectory for SGD
compute_sgd_trajectory <- function(X, y, batch_size, learning_rate, n_iter) {
  w <- rep(0, ncol(X))
  coefs <- matrix(0, nrow = 1, ncol = ncol(X))
  for (i in 1:n_iter) {
    indices <- sample(1:nrow(X), replace = FALSE)
    for (j in seq(1, nrow(X), batch_size)) {
      indices_batch <- indices[j:min(j + batch_size - 1, nrow(X))]
      X_batch <- X[indices_batch, ]
      y_batch <- y[indices_batch]
      gradient <- -2 * t(X_batch) %*% (y_batch - X_batch %*% w) / batch_size
      w <- w - learning_rate * gradient
    }
    coefs <- rbind(coefs, as.vector(w))
  }
  return(coefs)
}

n <- 100
p <- 10
batch_size <- 4
learning_rate <- 0.01
n_iter <- 50
t_values <- seq(0.001, n_iter + 1, by = 1)  # Include 0 in t_values for the zero coefficients
alphas <- 1 / (learning_rate * t_values[1:length(t_values)])  # Exclude 0 to avoid division by zero

data <- generate_data(n, p)
X <- data$X
y <- data$y
true_coef <- data$true_coef

ridge_coefs <- compute_ridge_path(X, y, alphas)

sgd_coefs <- compute_sgd_trajectory(X, y, batch_size, learning_rate, n_iter)

# PLOT -------------------------------------------------------------------------

# Ridge path
inv_alphas <- 1/alphas
df_ridge <- data.frame(inv_alphas, ridge_coefs[-1,])

df_ridge_long <- df_ridge %>%
  pivot_longer(cols = starts_with("X"), names_to = "line", values_to = "value")

p1 <- ggplot(df_ridge_long, aes(x = inv_alphas, y = value, color = line)) +
  geom_line(show.legend = FALSE) +
  labs(title = "Ridge Regression Path", x = expression("1 / ( lr *"~lambda~")"), y = "Parameters") +
  theme_minimal()

# SGD path
df_SGD <- data.frame(t_values, sgd_coefs)

df_SGD_long <- df_SGD %>%
  pivot_longer(cols = starts_with("X"), names_to = "line", values_to = "value")

p2 <- ggplot(df_SGD_long, aes(x = t_values, y = value, color = line)) +
  geom_line(show.legend = FALSE) +
  labs(title = "SGD Trajectory", x = "Iterations", y = "Parameters") +
  theme_minimal()

p = grid.arrange(p1, p2, ncol = 2)

ggsave("../figure/ridge_vs_sgd_path.png", plot=p, width=12, height=4.5)
