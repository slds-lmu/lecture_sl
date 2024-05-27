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

# Prepare data for contour plot
grid_range <- seq(-5, 5, length.out = 100)
grid_data <- expand.grid(X1 = grid_range, X2 = grid_range)
grid_data$loss <- apply(grid_data, 1, function(vec) {
  sum((y - X %*% vec)^2) / (2 * n)
})


lambdas_l2 <- c(0, 10, 100, 500)
# Ridge regression implementation
ridge_regression <- function(X, y, lambda) {
  n <- nrow(X)
  d <- ncol(X)
  
  # Adding a column of ones for the intercept term
  X_ext <- cbind(1, X)  # Ensure X_ext has n rows and d+1 columns
  
  # Ridge regression closed-form solution
  I <- diag(d + 1)
  I[1, 1] <- 0  # No regularization on the intercept
  
  beta <- solve(t(X_ext) %*% X_ext + lambda * I) %*% t(X_ext) %*% y
  return(beta)
}

# OLS regression implementation
ols_regression <- function(X, y) {
  n <- nrow(X)
  d <- ncol(X)
  
  # Adding a column of ones for the intercept term
  X_ext <- cbind(1, X)
  
  # OLS closed-form solution
  beta <- solve(t(X_ext) %*% X_ext) %*% t(X_ext) %*% y
  return(beta)
}

# Calculate coefficients using ridge_regression for each lambda
coefs_manual <- sapply(lambdas_l2, function(lambda) {
  beta <- ridge_regression(X, y, lambda)
  return(beta[2:3, 1])  # Extract coefficients excluding the intercept
})

coefs_manual <- t(coefs_manual)
coefs_df_manual <- as.data.frame(coefs_manual)
names(coefs_df_manual) <- c("X1", "X2")

# Function to create contour plots for regularized loss
create_reg_contour_plot <- function(coefs, title, lambda, alpha, X, y, grid_range, true_minimizer = c(3, -2.5)) {
  n <- nrow(X)
  d <- ncol(X)
  
  # Make sure coefs is a numeric vector
  coefs <- as.numeric(coefs)
  
  # Define the loss function for OLS
  loss_ols <- function(beta, X, y) {
    X_ext <- cbind(1, X) # Include intercept term
    return(sum((y - X_ext %*% beta)^2) / (2 * n))
  }
  
  # Define the regularized loss function for Ridge
  regularized_loss_ridge <- function(beta, X, y, lambda) {
    ridge_term <- ifelse(alpha == 0, lambda * sum(beta[-1]^2), 0)
    X_ext <- cbind(1, X) # Include intercept term
    return(sum((y - X_ext %*% beta)^2) / (2 * n) + ridge_term)
  }

  # Define the regularized loss function for LASSO
  regularized_loss_lasso <- function(beta, X, y, lambda) {
    lasso_term <- ifelse(alpha == 1, lambda * sum(abs(beta[-1])), 0)
    X_ext <- cbind(1, X) # Include intercept term
    return(sum((y - X_ext %*% beta)^2) / (2 * n) + lasso_term)
  }
  
  # Prepare grid data for contour plot
  grid_data <- expand.grid(X1 = grid_range, X2 = grid_range)
  X_ext <- cbind(1, grid_data)
  
  if(lambda == 0 && alpha == 0) {
    # Use OLS loss function
    beta_center_ols <- c(1, coefs)
    grid_data$reg_loss <- apply(X_ext, 1, function(vec) {
      loss_ols(vec, X, y)  # Directly use vec as beta values
    })
  } else if(alpha == 0) {
    # Use Ridge loss function
    beta_center <- c(1, coefs)
    grid_data$reg_loss <- apply(X_ext, 1, function(vec) {
      regularized_loss_ridge(vec - beta_center, X, y, lambda)
    })
  } else {
    # Use LASSO loss function
    beta_center <- c(1, coefs)
    grid_data$reg_loss <- apply(X_ext, 1, function(vec) {
      regularized_loss_lasso(vec, X, y, lambda)
    })
  }
  
  # Create the contour plot
  plot <- ggplot(grid_data, aes(x = X1, y = X2)) +
    geom_contour_filled(aes(z = reg_loss), breaks = pretty(range(grid_data$reg_loss), n = 15)) +
    geom_point(aes(x = coefs[1], y = coefs[2]), color = "red", size = 2) +
    geom_point(aes(x = true_minimizer[1], y = true_minimizer[2]), color = "green", size = 2) +
    geom_segment(aes(x = 0, y = -5, xend = 0, yend = 5), color = "black", linetype = "solid") +
    geom_segment(aes(x = -5, y = 0, xend = 5, yend = 0), color = "black", linetype = "solid") +
    ggtitle(title) +
    xlab(expression(theta[1])) +
    ylab(expression(theta[2])) +
    theme(panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_blank()) +
    guides(fill = "none")
  
  return(plot)
}


# Calculate coefficients using OLS regression for lambda = 0
coefs_ols <- ols_regression(X, y)[2:3, 1]  # Extract coefficients excluding the intercept

# Create plots for each lambda
plots_l2 <- list()
for (i in 1:length(lambdas_l2)) {
  lambda_value <- lambdas_l2[i]
  title_expression <- bquote("L2 Regularization:" ~ lambda == .(lambda_value))
  
  # Use OLS coefficients for lambda = 0
  if (lambda_value == 0) {
    coefs_to_use <- coefs_ols
  } else {
    coefs_to_use <- coefs_df_manual[i, ]
  }
  
  plots_l2[[i]] <- create_reg_contour_plot(coefs_to_use, title_expression, lambda_value, 0, X, y, grid_range)
}

# Display the grid of contour plots
ridge_contours <- grid.arrange(grobs = plots_l2, ncol = 2, nrow = 2)

ggsave("../figure/ridge_contours.png", plot = ridge_contours, width =9, height = 6)


# Assuming lambdas_l1 contains your lambda values for LASSO
lambdas_l1 <- c(0, 1, 2, 10)

# glmnet requires a matrix for X and a vector for y
X_matrix <- as.matrix(X)
y_vector <- as.vector(y)

# Fit LASSO model for each lambda and extract coefficients
lasso_models <- lapply(lambdas_l1, function(lambda) {
  glmnet(X_matrix, y_vector, alpha = 1, lambda = lambda)
})

coefs_l1 <- sapply(lasso_models, function(model) {
  coef(model)[2:3,1]  # Extracting only the relevant coefficients
})

# Transpose and convert to data frame
coefs_l1 <- t(coefs_l1)
coefs_df_l1 <- as.data.frame(coefs_l1)
names(coefs_df_l1) <- c("X1", "X2")


plots_l1 <- list()
for (i in 1:length(lambdas_l1)) {
  lambda_value <- lambdas_l1[i]
  title_expression <- bquote("L1 Regularization:" ~ lambda == .(lambda_value))
  
  # Use OLS coefficients for lambda = 0
  if (lambda_value == 0) {
    coefs_to_use <- coefs_ols
    plots_l1[[i]] <- create_reg_contour_plot(coefs_to_use, title_expression, lambda_value, 0, X, y, grid_range)
  } else {
    coefs_to_use <- coefs_df_l1[i, ]
  }
  
  plots_l1[[i]] <- create_reg_contour_plot(coefs_to_use, title_expression, lambda_value, 1, X, y, grid_range)
}

# Display the grid of LASSO contour plots
lasso_contours <- grid.arrange(grobs = plots_l1, ncol = 2, nrow = 2)


ggsave("../figure/lasso_contours.png", plot = lasso_contours, width =9, height = 6)

                                 