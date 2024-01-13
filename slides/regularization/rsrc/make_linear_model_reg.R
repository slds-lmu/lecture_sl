# Load necessary libraries
library(ggplot2)
library(MASS)
library(glmnet)

# Example dataset
set.seed(123)
n <- 100
X <- matrix(rnorm(2 * n), n, 2)
beta_true <- c(3, -2)
y <- X %*% beta_true + rnorm(n)

# Train unregularized linear model
lm_unreg <- lm(y ~ X - 1)  # '-1' to remove the intercept

# Train L2 regularized models with different lambdas
lambdas <- c(0.1, 1, 2.5, 5, 10, 20, 100)
models <- lapply(lambdas, function(lambda) {
  return(glmnet::glmnet(X, y, alpha = 0, lambda = lambda, standardize = FALSE, intercept = FALSE))
})

coefs <- sapply(models, function(model, lambda) {
  coef(model, s = lambda)[-1, 1]  # Exclude the intercept
}, lambdas)

# Transpose to make each column represent a model
coefs <- t(coefs)

# Create a data frame from the matrix
coefs_df <- as.data.frame(coefs)
names(coefs_df) <- c("X1", "X2")

# Prepare data for contour plot
grid_range <- seq(-5, 5, length.out = 100)
grid_data <- expand.grid(X1 = grid_range, X2 = grid_range)
grid_data$loss <- apply(grid_data, 1, function(vec) {
  sum((y - X %*% vec)^2) / (2 * n)
})

# Create the plot without the legend
p <- ggplot(grid_data, aes(x = X1, y = X2)) +
  geom_contour_filled(aes(z = loss), breaks = seq(min(grid_data$loss), max(grid_data$loss), length.out = 15)) +
  geom_point(data = coefs_df, aes(x = X1, y = X2), color = "red", size = 4) +
  ggtitle("Effect of L2 Regularization on Linear Model Solutions") +
  xlab(expression(theta[1])) +  # Label for X-axis as theta_1
  ylab(expression(theta[2])) +  # Label for Y-axis as theta_2
  theme(panel.grid = element_blank(),  # Remove grid lines
        panel.background = element_blank())  # Remove grey background

# Omit the legend
p <- p + guides(fill = "none")


ggsave("../figure/lin_reg_l2.png", plot = p, width =8, height = 5)