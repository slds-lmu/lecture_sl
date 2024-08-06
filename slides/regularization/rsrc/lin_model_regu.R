# ------------------------------------------------------------------------------
# l1, l2

# FIG: optimal points with different regularization constants (lambda)
#      on contour plot for linear regression with 
#      l1 and l2 regularization.

# DATA: y = X(100*2 ~Normal)·beta_true(3,-2) + noise(100*1 ~Normal)
# ------------------------------------------------------------------------------

library(ggplot2)
library(MASS)
library(glmnet)
library(gridExtra)

set.seed(123)

# DATA -------------------------------------------------------------------------

n <- 100
X <- matrix(rnorm(2 * n), n, 2)
beta_true <- c(3, -2)
y <- X %*% beta_true + rnorm(n)

lm_unreg <- lm(y ~ X - 1)  # '-1' to remove the intercept

lambdas_l2 <- c(0.1, 1, 2.5, 5, 10, 20, 100)
models <- lapply(lambdas_l2, function(lambda) {
  return(glmnet::glmnet(X, y, alpha = 0, lambda = lambda, standardize = FALSE, intercept = FALSE))
})

coefs <- sapply(models, function(model, lambda) {
  coef(model, s = lambda)[-1, 1]  # Exclude the intercept
}, lambdas_l2)

# Transpose so each column represents a model
coefs_l2 <- t(coefs)

coefs_df_l2 <- as.data.frame(coefs_l2)
names(coefs_df_l2) <- c("X1", "X2")

grid_range <- seq(-5, 5, length.out = 100)
grid_data <- expand.grid(X1 = grid_range, X2 = grid_range)
grid_data$loss <- apply(grid_data, 1, function(vec) {
  sum((y - X %*% vec)^2) / (2 * n)
})

lambdas_l1 <- c(0.01, 0.5, 1, 1.5, 2, 2.5, 10)

models_l1 <- lapply(lambdas_l1, function(lambda) {
  return(glmnet::glmnet(X, y, alpha = 1, lambda = lambda, standardize = FALSE, intercept = FALSE))
})

# Extract coefficients for L1 regularized models
coefs_l1 <- sapply(models_l1, function(model, lambda) {
  coef(model, s = lambda)[-1, 1]
}, lambdas_l1)

# Transpose so each column represents a model
coefs_l1 <- t(coefs_l1)

coefs_df_l1 <- as.data.frame(coefs_l1)
names(coefs_df_l1) <- c("X1", "X2")

coefs_df_l1$lambda <- factor(lambdas_l1)

red_colors <- c("#ffcccc",
                "#ff9999",
                "#ff6666",
                "#ff3333",
                "#ff0000",
                "#cc0000",
                "#800000")

if(length(red_colors) != length(lambdas_l1)) {
  stop("The number of manually defined colors does not match the number of lambda values.")
}

# PLOT -------------------------------------------------------------------------

# L1 Regularization
p_l1 <- ggplot(grid_data, aes(x = X1, y = X2)) +
  geom_contour_filled(aes(z = loss), breaks = seq(min(grid_data$loss), max(grid_data$loss), length.out = 15)) +
  geom_point(data = coefs_df_l1, aes(x = X1, y = X2, color = lambda), size = 4) +
  ggtitle("Effect of L1 Regularization on Linear Model Solutions") +
  xlab(expression(theta[1])) +
  ylab(expression(theta[2])) +
  geom_segment(aes(x = 0, y = -5, xend = 0, yend = 5), color = "black", linetype = "solid") +
  geom_segment(aes(x = -5, y = 0, xend = 5, yend = 0), color = "black", linetype = "solid") +
  scale_color_manual(values = red_colors, name = expression(lambda)) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank()) +
  guides(fill = "none")

coefs_df_l2$lambda <- factor(lambdas_l2)

# L2 Regularization
p_l2 <- ggplot(grid_data, aes(x = X1, y = X2)) +
  geom_contour_filled(aes(z = loss), breaks = seq(min(grid_data$loss), max(grid_data$loss), length.out = 15)) +
  geom_point(data = coefs_df_l2, aes(x = X1, y = X2, color = lambda), size = 4) +
  ggtitle("Effect of L2 Regularization on Linear Model Solutions") +
  xlab(expression(theta[1])) +
  ylab(expression(theta[2])) +
  geom_segment(aes(x = 0, y = -5, xend = 0, yend = 5), color = "black", linetype = "solid") +
  geom_segment(aes(x = -5, y = 0, xend = 5, yend = 0), color = "black", linetype = "solid") +
  scale_color_manual(values = red_colors, name = expression(lambda)) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank()) +
  guides(fill = "none")

ggsave("../figure/lin_model_regu_01.png", plot = p_l1, width = 8, height = 5) #L1
ggsave("../figure/lin_model_regu_02.png", plot = p_l2, width = 8, height = 5) #L2
