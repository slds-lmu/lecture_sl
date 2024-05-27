library(MASS)

# Data
set.seed(123)
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
