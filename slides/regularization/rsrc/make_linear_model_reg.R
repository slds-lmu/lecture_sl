# Load necessary libraries
library(ggplot2)
library(MASS)
library(glmnet)
library(gridExtra)

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

# Adjusted lambda values
lambdas_l1 <- c(0.01, 0.5, 1, 1.5, 2, 2.5, 10)

# Train L1 regularized models with the adjusted lambdas
models_l1 <- lapply(lambdas_l1, function(lambda) {
  return(glmnet::glmnet(X, y, alpha = 1, lambda = lambda, standardize = FALSE, intercept = FALSE))
})

# Extract coefficients for L1 regularized models
coefs_l1 <- sapply(models_l1, function(model, lambda) {
  coef(model, s = lambda)[-1, 1]
}, lambdas)

# Transpose to make each column represent a model
coefs_l1 <- t(coefs_l1)

# Create a data frame for L1 coefficients
coefs_df_l1 <- as.data.frame(coefs_l1)
names(coefs_df_l1) <- c("X1", "X2")

# Plot for L1 Regularization
p_l1 <- ggplot(grid_data, aes(x = X1, y = X2)) +
  geom_contour_filled(aes(z = loss), breaks = seq(min(grid_data$loss), max(grid_data$loss), length.out = 15)) +
  geom_point(data = coefs_df_l1, aes(x = X1, y = X2), color = "red", size = 4) +
  ggtitle("Effect of L1 Regularization on Linear Model Solutions") +
  xlab(expression(theta[1])) +
  ylab(expression(theta[2])) +
  geom_segment(aes(x = 0, y = -5, xend = 0, yend = 5), color = "black", linetype = "solid") +
  geom_segment(aes(x = -5, y = 0, xend = 5, yend = 0), color = "black", linetype = "solid") +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank()) +
  guides(fill = "none")


# Plot for L2 Regularization
p_l2 <- ggplot(grid_data, aes(x = X1, y = X2)) +
  geom_contour_filled(aes(z = loss), breaks = seq(min(grid_data$loss), max(grid_data$loss), length.out = 15)) +
  geom_point(data = coefs_df, aes(x = X1, y = X2), color = "red", size = 4) +
  ggtitle("Effect of L2 Regularization on Linear Model Solutions") +
  xlab(expression(theta[1])) +
  ylab(expression(theta[2])) +
  geom_segment(aes(x = 0, y = -5, xend = 0, yend = 5), color = "black", linetype = "solid") +
  geom_segment(aes(x = -5, y = 0, xend = 5, yend = 0), color = "black", linetype = "solid") +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank()) +
  guides(fill = "none")


# Save the L2 plot
ggsave("../figure/lin_reg_l2.png", plot = p_l2, width = 8, height = 5)

# Save the L1 plot
ggsave("../figure/lin_reg_l1.png", plot = p_l1, width = 8, height = 5)


#### CREATE RIDGE CONTOUR PLOTS

lambdas_l2 <- c(1, 2.5, 100)

# Create the plot
p_min <- ggplot(grid_data, aes(x = X1, y = X2)) +
  geom_contour_filled(aes(z = loss), breaks = seq(min(grid_data$loss), max(grid_data$loss), length.out = 15)) +
  geom_point(aes(x = 3, y = -2), color = "green", size = 2) +  # Mark the true minimizer
  ggtitle(expression("Contour Plot with " * theta * "*" ~ "= (3, -2.5)"^T)) +
  xlab(expression(theta[1])) +
  ylab(expression(theta[2])) +
  geom_segment(aes(x = 0, y = -5, xend = 0, yend = 5), color = "black", linetype = "solid") +
  geom_segment(aes(x = -5, y = 0, xend = 5, yend = 0), color = "black", linetype = "solid") +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank()) +
  guides(fill = "none")

regularized_loss <- function(beta, X, y, lambda, alpha) {
  n <- nrow(X)
  ridge_term <- ifelse(alpha == 0, lambda * sum(beta^2), 0)  # L2 Regularization
  lasso_term <- ifelse(alpha == 1, lambda * sum(abs(beta)), 0)  # L1 Regularization
  return(sum((y - X %*% beta)^2) / (2 * n) + ridge_term + lasso_term)
}

models <- lapply(lambdas_l2, function(lambda) {
  return(glmnet::glmnet(X, y, alpha = 0, lambda = lambda, standardize = FALSE, intercept = FALSE))
})

coefs <- sapply(models, function(model, lambda) {
  coef(model, s = lambda)[2:3, 1]  # Only the coefficients, excluding the intercept
}, lambdas_l2)

coefs <- t(coefs)
coefs_df <- as.data.frame(coefs)
names(coefs_df) <- c("X1", "X2")


create_reg_contour_plot <- function(coefs_df, title, lambda, alpha) {
  grid_data$reg_loss <- apply(grid_data[, c("X1", "X2")], 1, function(vec) {
    regularized_loss(as.numeric(vec), X, y, lambda, alpha)
  })
  
  ggplot(grid_data, aes(x = X1, y = X2)) +
    geom_contour_filled(aes(z = reg_loss), breaks = seq(min(grid_data$reg_loss), max(grid_data$reg_loss), length.out = 15)) +
    geom_point(aes(x = 3, y = -2), color = "green", size = 2) +  # True minimizer
    geom_point(data = coefs_df, aes(x = X1, y = X2), color = "red", size = 2) +  # Regularized solution
    ggtitle(title) +
    xlab(expression(theta[1])) +
    ylab(expression(theta[2])) +
    geom_segment(aes(x = 0, y = -5, xend = 0, yend = 5), color = "black", linetype = "solid") +
    geom_segment(aes(x = -5, y = 0, xend = 5, yend = 0), color = "black", linetype = "solid") +
    theme(panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_blank()) +
    guides(fill = "none")
}


# Generate plots for the specified lambdas
plots_l2 <- list()
plots_l2[[1]] <- p_min  # The original plot

for (i in 1:length(lambdas_l2)) {
  lambda_value <- lambdas_l2[i]
  title_expression <- bquote("L2 Regularization:" ~ lambda == .(lambda_value))
  plots_l2[[i + 1]] <- create_reg_contour_plot(coefs_df[i, , drop = FALSE], title_expression, lambdas_l2[i], alpha = 0)
}

# Grid for L2 Regularization
ridge_contours = grid.arrange(grobs = plots_l2, ncol = 2, nrow = 2)

ggsave("../figure/ridge_contours.png", plot = ridge_contours, width =9, height = 6)


### CREATE LASSO CONTOUR PLOTS

lambdas_l1 <- c(0.1, 1, 10)  # Example of smaller lambda values

models_l1 <- lapply(lambdas_l1, function(lambda) {
  return(glmnet::glmnet(X, y, alpha = 1, lambda = lambda, standardize = FALSE, intercept = FALSE))
})

coefs_l1 <- sapply(models_l1, function(model, lambda) {
  coef(model, s = lambda)[2:3, 1]  # Only the coefficients, excluding the intercept
}, lambdas_l1)

coefs_l1 <- t(coefs_l1)
coefs_df_l1 <- as.data.frame(coefs_l1)
names(coefs_df_l1) <- c("X1", "X2")

plots_l1 <- list()
plots_l1[[1]] <- p_min  # The original plot

for (i in 1:length(lambdas_l1)) {
  lambda_value <- lambdas_l1[i]
  title_expression <- bquote("L1 Regularization:" ~ lambda == .(lambda_value))
  plots_l1[[i + 1]] <- create_reg_contour_plot(coefs_df_l1[i, , drop = FALSE], title_expression, lambdas_l1[i], alpha = 1)
}

lasso_contours = grid.arrange(grobs = plots_l1, ncol = 2, nrow = 2)

ggsave("../figure/lasso_contours.png", plot = lasso_contours, width =9, height = 6)
