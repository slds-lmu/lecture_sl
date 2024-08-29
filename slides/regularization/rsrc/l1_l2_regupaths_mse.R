library(glmnet)
library(datasets)

set.seed(42)

# Load mtcars
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

# Plot layout
par(mfrow = c(2, 2), oma = c(0, 0, 0, 0), mar = c(5, 4, 4, 2) + 0.1)
par(cex.main = 1.7, cex.axis = 1.5, cex.lab = 1.5, lwd=1.2)

# Plot regularization path for Lasso
plot(cvfit_lasso$glmnet.fit, xvar = "lambda", label = TRUE, main = "")
title(main = "lasso coefficients path", line = 2.5)

# Plot regularization path for Ridge
plot(cvfit_ridge$glmnet.fit, xvar = "lambda", label = TRUE, main = "")
title(main = "ridge coefficients path", line = 2.5)

# Plot RMSE vs. Lambda for Lasso
plot(cvfit_lasso, main = "")
title(main = "MSE vs. lambda for lasso", line = 2.5)

# Plot RMSE vs. Lambda for Ridge
plot(cvfit_ridge, main = "")
title(main = "MSE vs. lambda for ridge", line = 2.5)

# Reset layout to default
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(5, 4, 4, 2) + 0.1)
