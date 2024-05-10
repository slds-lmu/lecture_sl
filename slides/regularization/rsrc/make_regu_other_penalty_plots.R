library(ggplot2)
library(ncvreg)
library(gridExtra)
library(grid)
library(MASS)

# Generate data
ndata <- 200
ncoef <- 1000
beta <- c(4, 2, -4, -2, rep(0, ncoef-4))
set.seed(123)
mean_vec <- sample(0:10, size=ncoef, replace=TRUE)
set.seed(110)
X <- mvrnorm(n=ndata, mu=mean_vec, Sigma=diag(ncoef))

epsilon <- rnorm(200)
y <- X %*% beta + epsilon


### lasso
fit_lasso <- ncvreg(cbind(1,X), y, penalty="lasso", lambda.min=0.1)
plot(fit_lasso, main="Lasso", ylab="coef value")
dev.copy(png, "../figure/other-pen-lasso.png")
dev.off()


### MCP
fit_mcp <- ncvreg(X, y, penalty="MCP", gamma=3, lambda.min=0.07)
plot(fit_mcp, main=expression(MCP~gamma~"=3"), ylab="coef value")
dev.copy(png, "../figure/other-pen-MCP.png")
dev.off()


### SCAD
fit_scad <- ncvreg(X, y, penalty="SCAD", gamma=4, lambda.min=0.07)
plot(fit_scad, main=expression(SCAD~gamma~"=4"), ylab="coef value")
dev.copy(png, "../figure/other-pen-SCAD.png")
dev.off()
