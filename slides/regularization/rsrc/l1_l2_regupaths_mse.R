# ------------------------------------------------------------------------------
# l1 vs l2

# FIG: 
#   (1) how each coefficient changes with log(lambda) under l1 and l2 regularization
#   (2) how MSE changes with log(lambda) under l1 and l2 regularization
# DATA: mtcars
# ------------------------------------------------------------------------------
library(glmnet)
library(datasets)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

set.seed(42)

# DATA -------------------------------------------------------------------------

data(mtcars)
x <- as.matrix(mtcars[, -1])  
y <- mtcars$mpg 

# Normalize data
x <- scale(x, center = TRUE, scale = TRUE)
y <- scale(y, center = TRUE, scale = FALSE)

# Lasso with cross-validation
cvfit_lasso <- cv.glmnet(x, y, alpha = 1)

# Ridge with cross-validation
cvfit_ridge <- cv.glmnet(x, y, alpha = 0)

# Extracting data for Lasso
lasso_coefs <- as.data.frame(t(as.matrix(cvfit_lasso$glmnet.fit$beta)))
lasso_coefs$lambda <- cvfit_lasso$glmnet.fit$lambda
lasso_coefs <- lasso_coefs %>% pivot_longer(-lambda, names_to = "Variable", values_to = "Coefficient")

# Extracting data for Ridge
ridge_coefs <- as.data.frame(t(as.matrix(cvfit_ridge$glmnet.fit$beta)))
ridge_coefs$lambda <- cvfit_ridge$glmnet.fit$lambda
ridge_coefs <- ridge_coefs %>% pivot_longer(-lambda, names_to = "Variable", values_to = "Coefficient")

# RMSE data
lasso_rmse <- data.frame(lambda = cvfit_lasso$lambda, MSE = cvfit_lasso$cvm, SE = cvfit_lasso$cvsd)
ridge_rmse <- data.frame(lambda = cvfit_ridge$lambda, MSE = cvfit_ridge$cvm, SE = cvfit_ridge$cvsd)

# PLOT -------------------------------------------------------------------------

# Lasso coefficients path plot
p1 <- ggplot(lasso_coefs, aes(x = log(lambda), y = Coefficient, color = Variable)) +
  geom_line() +
  labs(title = "Lasso coefficients path", x = expression(log~lambda), y = "Coefficients") +
  theme_minimal()

# Ridge coefficients path plot
p2 <- ggplot(ridge_coefs, aes(x = log(lambda), y = Coefficient, color = Variable)) +
  geom_line() +
  labs(title = "Ridge coefficients path", x = expression(log~lambda), y = "Coefficients") +
  theme_minimal()

# Lasso RMSE vs. Lambda plot
p3 <- ggplot(lasso_rmse, aes(x = log(lambda), y = MSE)) +
  geom_errorbar(aes(ymin = MSE - SE, ymax = MSE + SE), width = 0.1, color = "gray") +
  geom_point(color = "red", size=0.3) +
  geom_line(color = "red") +
  geom_vline(xintercept = log(cvfit_lasso$lambda.min), linetype = "dashed", color = "gray", linewidth = 0.6) +
  geom_vline(xintercept = log(cvfit_lasso$lambda.1se), linetype = "dashed", color = "gray", linewidth = 0.6) +
  labs(title = expression(MSE ~ vs. ~ lambda ~ "for lasso"),
       x = expression(Log~lambda),
       y = "MSE") +
  theme_minimal()

# Ridge RMSE vs. Lambda plot
p4 <- ggplot(ridge_rmse, aes(x = log(lambda), y = MSE)) +
  geom_errorbar(aes(ymin = MSE - SE, ymax = MSE + SE), width = 0.1, color = "gray") +
  geom_point(color = "red", size=0.3) +
  geom_line(color = "red") +
  geom_vline(xintercept = log(cvfit_ridge$lambda.min), linetype = "dashed", color = "gray", linewidth = 0.6) +
  geom_vline(xintercept = log(cvfit_ridge$lambda.1se), linetype = "dashed", color = "gray", linewidth = 0.6) +
  labs(title = expression(MSE ~ vs. ~ lambda ~ "for ridge"),
       x = expression(Log~lambda),
       y = "MSE") +
  theme_minimal()

# Arrange plots in a 2x2 grid
p <- grid.arrange(p1, p2, p3, p4, nrow = 2)

ggsave("../figure/l1_l2_regupaths_mse.png", plot = p, width = 7, height = 4)
