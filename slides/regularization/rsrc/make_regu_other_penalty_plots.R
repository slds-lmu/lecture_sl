library(ggplot2)
library(ncvreg)
library(gridExtra)
library(grid)
library(MASS)

# Generate data
ndata <- 200
ncoef <- 1000
beta <- c(4, 2, -4, -2, rep(0, ncoef-4))
numsignals = sum(beta != 0)
lam_min = 0.1
set.seed(42)
X =  matrix(rnorm(ndata*ncoef), ncol=ncoef)

epsilon <- rnorm(ndata)
y <- X %*% beta + epsilon

linewidth = 4
fontsize = 2

### Lasso
fit_lasso <- ncvreg(X, y, penalty="lasso", lambda.min=lam_min)
plot(fit_lasso, main=expression("Lasso"), ylab=expression(theta), 
     cex.lab=fontsize, cex.main=1.2*fontsize, cex.axis=fontsize, 
     lwd=linewidth, log.l=TRUE) 
abline(h = c(-4, -2, 2, 4), col = "grey", lty = 8)
dev.copy(png, "../figure/other-pen-lasso.png")
dev.off()

### MCP
fit_mcp <- ncvreg(X, y, penalty="MCP", gamma=3.1, lambda.min=lam_min)
plot(fit_mcp, main=expression("MCP"~~(gamma~"=3.1")), ylab=expression(theta), 
     cex.lab=fontsize, cex.main=1.2*fontsize, cex.axis=fontsize, 
     lwd=linewidth, log.l=TRUE) 
abline(h = c(-4, -2, 2, 4), col = "grey", lty = 8)
dev.copy(png, "../figure/other-pen-MCP.png")
dev.off()

### SCAD
fit_scad <- ncvreg(X, y, penalty="SCAD", gamma=3.8, lambda.min=lam_min)
plot(fit_scad, main=expression("SCAD"~~(gamma~"=3.8")), ylab=expression(theta), 
     cex.lab=fontsize, cex.main=1.2*fontsize, cex.axis=fontsize, 
     lwd=linewidth, log.l=TRUE) 
abline(h = c(-4, -2, 2, 4), col = "grey", lty = 8)
dev.copy(png, "../figure/other-pen-SCAD.png")
dev.off()