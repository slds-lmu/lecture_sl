# ------------------------------------------------------------------------------
# l1 vs l2

# TABLE: coefficients and MSE of OLS and ridge for X and rescaled X
# DATA: Y  = X(100*5 ~Normal) * beta_true + epsilon(100*1 ~Normal)
# ------------------------------------------------------------------------------

library(MASS)
library(xtable)
library(dplyr)

set.seed(123)

# DATA -------------------------------------------------------------------------

n <- 100
p <- 5
X <- matrix(rnorm(n * p), n, p)
beta_true <- c(1, 2, 3, 4, 5)
epsilon <- rnorm(n)
Y <- X %*% beta_true + epsilon

# OLS Solution
beta_ols <- solve(t(X) %*% X) %*% t(X) %*% Y

# Ridge Solution
lambda <- 10
beta_ridge <- solve(t(X) %*% X + lambda * diag(p)) %*% t(X) %*% Y

# Rescale and repeat
X_rescaled <- X
X_rescaled[,5] <- 100 * X_rescaled[,5]
beta_ols_rescaled <- solve(t(X_rescaled) %*% X_rescaled) %*% t(X_rescaled) %*% Y
beta_ridge_rescaled <- solve(t(X_rescaled) %*% X_rescaled + lambda * diag(p)) %*% t(X_rescaled) %*% Y

# Results
results <- rbind(t(beta_ols), t(beta_ols_rescaled), t(beta_ridge), t(beta_ridge_rescaled))
colnames(results) <- paste("Coefficient", 1:p)

# MSE
loss_ols <- mean((Y - X %*% beta_ols)^2)
loss_ols_rescaled <- mean((Y - X_rescaled %*% beta_ols_rescaled)^2)
loss_ridge <- mean((Y - X %*% beta_ridge)^2) # + lambda * sum(beta_ridge^2)
loss_ridge_rescaled <- mean((Y - X_rescaled %*% beta_ridge_rescaled)^2) #+ lambda * sum(beta_ridge_rescaled^2)

losses <- c(loss_ols, loss_ols_rescaled, loss_ridge, loss_ridge_rescaled)
results <- cbind(results, MSE = losses)
rownames(results) <- c("OLS", "OLS Rescaled", "Ridge", "Ridge Rescaled")
print(results)

# TABLE ------------------------------------------------------------------------
results <- round(results, 3)
# Function to bold specific column values
bold_coefficient5 <- function(x) {
  x[, "Coefficient 5"] <- paste0("\\textbf{", formatC(x[, "Coefficient 5"], format = "f", digits = 3), "}")
  x
}

table_ols <- bold_coefficient5(results[1:2,])
table_ridge <- bold_coefficient5(results[3:4,])

table_ols <- xtable(table_ols)
align(table_ols) <- "|c|cccccc|"
table_ridge <- xtable(table_ridge)
align(table_ridge) <- "|c|cccccc|"

add.to.row <- list(pos = list(-1, nrow(table_ols)),
                   command = c("\\hline\n\\textbf{Method} & \\( \\hat{\\theta}_1 \\) & \\( \\hat{\\theta}_2 \\) & \\( \\hat{\\theta}_3 \\) & \\( \\hat{\\theta}_4 \\) & \\( \\hat{\\theta}_5 \\) & MSE \\\\ \\hline\n",
                               "\\hline\n"))

print(table_ols, file = "table_equivariance_ols.tex", include.rownames = TRUE,
      include.colnames = FALSE,
      sanitize.text.function = identity, 
      tabular.environment = "tabular",
      floating = FALSE,
      add.to.row = add.to.row,
      hline.after = NULL,
      booktabs = FALSE,
      comment = FALSE)

print(table_ridge, file = "table_equivariance_ridge.tex", include.rownames = TRUE,
      include.colnames = FALSE,
      sanitize.text.function = identity, 
      tabular.environment = "tabular",
      floating = FALSE,
      add.to.row = add.to.row,
      hline.after = NULL,
      booktabs = FALSE,
      comment = FALSE)
