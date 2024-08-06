# ------------------------------------------------------------------------------
# l1, l2

# FIG: solution path under l1 and l2 regularization.
# DATA:
#   x = seq(0, 1, length.out = 40)
#   noise ~ Unif(0, 1)
#   y = sin(x * 1.5 * pi)
#   y_noise = (y + noise) - mean(y + noise)
# ------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(Matrix)
library(glmnet)
library(pracma)
library(gridExtra)
set.seed(0)

# DATA -------------------------------------------------------------------------

# Cost function definitions
cost_l2 <- function(x, y) {
  return(x^2 + y^2)
}

cost_l1 <- function(x, y) {
  return(abs(x) + abs(y))
}

costfunction <- function(X, y, theta) {
  m <- length(y)
  h <- X %*% theta
  return((1 / (2 * m)) * t(h - y) %*% (h - y))
}

closed_form_reg_solution <- function(X, y, lambda = 10) {
  m <- nrow(X)
  n <- ncol(X)
  I <- diag(n)
  return(solve(t(X) %*% X + lambda * I) %*% t(X) %*% y)
}

# Dataset creation and normalization
x <- seq(0, 1, length.out = 40)
noise <- runif(40, 0, 1)
y <- sin(x * 1.5 * pi)
y_noise <- (y + noise) - mean(y + noise)
X <- cbind(x, x^2)
X <- sweep(X, 2, sqrt(colSums(X^2)), FUN = "/")

# Setup of meshgrid of theta values
theta1 <- seq(-2, 17, length.out = 100)
theta2 <- seq(-17, 3, length.out = 100)
grid <- expand.grid(theta1 = theta1, theta2 = theta2)

# Computing the cost function for each theta combination
grid <- grid %>%
  mutate(Z_l2 = cost_l2(theta1, theta2),
         Z_l1 = cost_l1(theta1, theta2),
         Z_ls = apply(grid, 1, function(row) costfunction(X, y_noise, matrix(c(row[1], row[2]), nrow = 2))))

# Calculating the regularization paths
lambda_range_l2 <- 10^seq(0, 4, length.out = 100) / 1000
lambda_range_l1 <- 10^seq(0, 2, length.out = 100) / 1000

theta_l2 <- sapply(lambda_range_l2, function(l) closed_form_reg_solution(X, y_noise, l))
theta_l1 <- sapply(lambda_range_l1, function(l) coef(glmnet(X, y_noise, alpha=1, lambda=l, standardize=FALSE, intercept=FALSE))[2:3])

theta_l2_df <- data.frame(t(theta_l2))
theta_l1_df <- data.frame(t(theta_l1))


# L2 plot

l2_contour_levels <- c(.5, 1.5, 3, 6, 9, 15, 30, 60, 100, 150, 250)

p2 <- ggplot() +
  geom_contour(data = grid, aes(x = theta1, y = theta2, z = Z_l2), color = 'cyan', breaks = l2_contour_levels) +
  geom_contour(data = grid, aes(x = theta1, y = theta2, z = Z_ls), color = 'red', breaks = c(.01, .06, .09, .11, .15)) +
  geom_point(data = theta_l2_df, aes(x = X1, y = X2), color = 'red', alpha = 0.2) +
  labs(title = 'L2 regularization solution path', x = expression(theta[1]), y = expression(theta[2])) +
  theme_minimal() + 
  coord_fixed()

# L1 & L2 plot

# Plot L2 Regularization
inside_l2 <- theta_l2_df %>%
  filter(cost_l2(X1, X2) < max(l2_contour_levels))

p_l2 <- ggplot() +
  geom_contour(data = grid, aes(x = theta1, y = theta2, z = Z_l2), color = 'cyan', breaks = l2_contour_levels) +
  geom_contour(data = grid, aes(x = theta1, y = theta2, z = Z_ls), color = 'red', breaks = c(.01, .06, .09, .11, .15)) +
  geom_point(data = inside_l2, aes(x = X1, y = X2), color = 'green', alpha = 0.5) +
  labs(title = 'L2 regularization solution path', x = expression(theta[1]), y = expression(theta[2])) +
  theme_minimal() + 
  coord_fixed()

# Plot L1 Regularization
l1_contour_levels = c(.5, 1, 2, 3, 4, 5, 6, 8, 10, 12, 14)

inside_l1 <- theta_l1_df %>%
  filter(cost_l1(X1, X2) < max(l1_contour_levels))

p_l1 <- ggplot() +
  geom_contour(data = grid, aes(x = theta1, y = theta2, z = Z_l1), color = 'cyan', breaks = l1_contour_levels) +
  geom_contour(data = grid, aes(x = theta1, y = theta2, z = Z_ls), color = 'red', breaks = c(.01, .06, .09, .11, .15)) +
  geom_point(data = inside_l1, aes(x = X1, y = X2), color = 'green', alpha = 0.5) +
  labs(title = 'L1 regularization solution path', x = expression(theta[1]), y = expression(theta[2])) +
  theme_minimal() + 
  coord_fixed()

p <- grid.arrange(p_l2, p_l1, ncol = 2) 

ggsave(filename = "../figure/solution_paths_02.png", plot = p2, width = 5, height = 5)
ggsave(filename = "../figure/solution_paths_01.png", plot = p, width = 10, height = 5)
