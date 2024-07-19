# This script simulates a high-dimensional data set and compares the regu paths of lasso, SCAD, and MCP
# The package ncvreg and its plot method are used to create the three individual plots

library(ggplot2)
library(ncvreg)
library(gridExtra)
library(grid)
library(MASS)

# Generate data
ndata <- 100
ncoef <- 1500
true_coefs <- c(-4,4,-2,2)
beta <- c(true_coefs, rep(0, ncoef-4))
numsignals = sum(beta != 0)
lam_min = 2e-2
set.seed(42)
X =  matrix(rnorm(ndata*ncoef), ncol=ncoef)

epsilon <- rnorm(ndata)
y <- X %*% beta + epsilon

linewidth = 4
fontsize = 2

### Lasso
fit_lasso <- ncvreg(X, y, penalty="lasso", lambda.min=lam_min)
cv_lasso <- ncvreg::cv.ncvreg(X,y, penalty="lasso", lambda.min=lam_min)
plot(fit_lasso, main=expression("Lasso"), ylab=expression(theta), 
     cex.lab=fontsize, cex.main=1.2*fontsize, cex.axis=fontsize, 
     lwd=linewidth, log.l=TRUE, vertical.line=FALSE, selected=FALSE) 
abline(h = true_coefs, col = "grey", lty = 8)
abline(v = log(cv_lasso$lambda.min))
dev.copy(png, "../figure/ncpen-compar-lasso.png")
dev.off()

### MCP
fit_mcp <- ncvreg(X, y, penalty="MCP", gamma=3, lambda.min=2.2*lam_min)
cv_mcp <- ncvreg::cv.ncvreg(X,y, penalty="MCP", lambda.min=2.2*lam_min)
plot(fit_mcp, main=expression("MCP"~~(gamma~"=3")), ylab=expression(theta), 
     cex.lab=fontsize, cex.main=1.2*fontsize, cex.axis=fontsize, 
     lwd=linewidth, log.l=TRUE, vertical.line=FALSE, selected=FALSE) 
abline(h = true_coefs, col = "grey", lty = 8)
abline(v = log(cv_mcp$lambda.min))
dev.copy(png, "../figure/ncpen-compar-MCP.png")
dev.off()

### SCAD
fit_scad <- ncvreg(X, y, penalty="SCAD", gamma=3.7, lambda.min=3*lam_min)
cv_scad <- ncvreg::cv.ncvreg(X,y, penalty="SCAD", lambda.min=3*lam_min)
plot(fit_scad, main=expression("SCAD"~~(gamma~"=3.7")), ylab=expression(theta), 
     cex.lab=fontsize, cex.main=1.2*fontsize, cex.axis=fontsize, 
     lwd=linewidth, log.l=TRUE, vertical.line=FALSE, selected=FALSE) 
abline(h = true_coefs, col = "grey", lty = 8)
abline(v = log(cv_scad$lambda.min))
dev.copy(png, "../figure/ncpen-compar-SCAD.png")
dev.off()