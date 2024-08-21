### This script produces a plot illustrating the ERM under pinball losses
### Data: simulated univariate heteroskedastic regression y=theta0+theta1*x+eps(x)
### eps(x) is normally distributed with mean 0 and standard deviation 0.5+0.5*x
### x is chosen uniformly in [0,10], theta0=1, theta1=0.2, n=200 samples

rm(list=ls())
library(quantreg)
library(ggplot2)

# Data simulation
set.seed(123)  
n <- 200  
x <- runif(n, 0, 10)  
theta0 <- 1
theta1 <- 0.2
eps <- rnorm(n, mean = 0, sd = 0.5+0.5*x)  
y <- theta0 + theta1 * x + eps 
simu.dat <- data.frame(x = x, y = y)

# Pinball loss for 0.05 quantile
mod.5 <- rq(formula = as.formula("y ~ x"), tau=0.05, data=simu.dat)
res.5 <- mod.5$coefficients
resids.5 <- mod.5$residuals
preds.5 <- predict(mod.5)
# Pinball loss for 0.5 quantile
mod.50 <- rq(formula = as.formula("y ~ x"), tau=0.5, data=simu.dat)
res.50 <- mod.50$coefficients
resids.50 <- mod.50$residuals
preds.50 <- predict(mod.50)
# Pinball loss for 0.95 quantile
mod.95 <- rq(formula = as.formula("y ~ x"), tau=0.95, data=simu.dat)
res.95 <- mod.95$coefficients
resids.95 <- mod.95$residuals
preds.95 <- predict(mod.95)

# Aggregate results
res.dat <- simu.dat
res.dat$resid_5 <- resids.5
res.dat$resid_50 <- resids.50
res.dat$resid_95 <- resids.95
res.dat$pred_5 <- preds.5
res.dat$pred_50 <- preds.50
res.dat$pred_95 <- preds.95

# Calculate true conditional mean
res.dat$true_mean <- theta0 + theta1 * res.dat$x


p <- ggplot(res.dat, aes(x = x)) +
  geom_line(aes(y = pred_5, color = "q=0.05"), size = 1.6, alpha=1) +   
  geom_line(aes(y = pred_50, color = "q=0.5"), size = 1.6, alpha=1) +
  geom_line(aes(y = pred_95, color = "q=0.95"), size = 1.6, alpha=1) +
  geom_line(aes(y = true_mean, linetype = "True mean"), color = "black", size = 1.6) +
  geom_point(aes(y = y), color = "darkgrey", size = 2, alpha=1) +  
  labs(y = "y", x = "x", color = "Quantile", linetype = "") +
  scale_linetype_manual(values = c("True mean" = "dashed")) +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        axis.title = element_text(size = rel(2)),   
        axis.text = element_text(size = rel(2)),  
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.3))) +
  guides(color = guide_legend(order = 1), linetype = guide_legend(order = 2))
# Print plot
print(p)

# Save figure
ggsave(filename = paste0("../figure/quantile-regression.pdf"), plot = p)

