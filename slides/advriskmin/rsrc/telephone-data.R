### This script produces a plot comparing various loss functions for robust regression
### Dataset: telephone data (number of calls per year in Belgium)
### Losses: L2, L1, log-cosh, Huber loss

rm(list=ls())

library(Rfit)
library(hqreg)
library(MASS)
library(quantreg)
library(CVXR)
library(ggplot2)

set.seed(123)

# Data
tel.dat <- Rfit::telephone
tel.dat$year <- as.numeric(tel.dat$year)

# L2 loss (least-squares)
fm0 <- lm(calls ~ year, tel.dat)
X <- model.matrix(fm0)
res.l2 <- fm0$coefficients
resids.l2 <- fm0$residuals
preds.l2 <- predict(fm0)

# Log-cosh regression
f <- function(b) with(tel.dat, sum(log(cosh(calls - X %*% b))))
res.optim <- optim(coef(fm0), f, method = "Nelder-Mead", control=list(alpha=1.0, beta=0.5, gamma=2.0))
res.logcosh <- res.optim$par
resids.logcosh <- with(tel.dat, calls - X %*% res.logcosh)
preds.logcosh <- as.numeric(with(tel.dat, X %*% res.logcosh))

# L1 loss robust regression
#mod.l1 <- rq(formula = as.formula("calls ~ year"), tau=0.5, data=tel.dat)
#res.l1 <- mod.l1$coefficients
#resids.l1 <- mod.l1$residuals
#preds.l1 <- predict(mod.l1)

# L1 loss robust regression (manual optimization)
f.l1 <- function(b) with(tel.dat, sum(abs(calls - X %*% b)))
res.optim.l1 <- optim(coef(fm0), f.l1, method = "Nelder-Mead", control=list(alpha=1.0, beta=0.5, gamma=2.0))
res.l1.manual <- res.optim.l1$par
resids.l1.manual <- with(tel.dat, calls - X %*% res.l1.manual)
preds.l1.manual <- as.numeric(with(tel.dat, X %*% res.l1.manual))

# Huber loss robust regression using CVXR
beta <- Variable(2)
obj <- sum(CVXR::huber(tel.dat$calls - X %*% beta, 1))
prob <- Problem(Minimize(obj))
result <- solve(prob)
res.huber <- result$getValue(beta)
names(res.huber) <- names(res.l2)
resids.huber <- with(tel.dat, calls - X %*% res.huber)
preds.huber <- as.numeric(with(tel.dat, X %*% res.huber))

# aggregate results data
res.tel <- tel.dat
res.tel$resid_l2 <- resids.l2
#res.tel$resid_l1 <- resids.l1
res.tel$resid_l1_manual <- resids.l1.manual
res.tel$resid_huber <- resids.huber
res.tel$resid_logcosh <- resids.logcosh
res.tel$pred_l2 <- preds.l2
#res.tel$pred_l1 <- preds.l1
res.tel$pred_l1_manual <- preds.l1.manual
res.tel$pred_huber <- preds.huber
res.tel$pred_logcosh <- preds.logcosh

#coef.b0 <- c(res.l2[1], res.l1[1], res.huber[1], res.logcosh[1])
#coef.b1 <- c(res.l2[2], res.l1[2], res.huber[2], res.logcosh[2])

# Plot results for telephone data
p <- ggplot(res.tel, aes(x = year)) +
  geom_line(aes(y = pred_l2, color = "L2 (OLS)"), size = 1.6, alpha=1) +   
  #geom_line(aes(y = pred_l1, color = "L1"), size = 1.4) +
  geom_line(aes(y = pred_l1_manual, color = "L1 (man)"), size = 1.6, alpha=1) +
  geom_line(aes(y = pred_huber, color = "Huber"), size = 1.6, alpha=1) +
  geom_line(aes(y = pred_logcosh, color = "Log-Cosh"), size = 1.6, alpha=1) +
  geom_point(aes(y = calls), color = "black", size = 4, alpha=1) +  
  labs(y = "Calls (in mio.)", x = "Year", color = "Loss") +
  theme_minimal() +
  theme(text = element_text(size = 12), 
        axis.title = element_text(size = rel(2)),   
        axis.text = element_text(size = rel(2)),  
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.3)))

# Print plot
print(p)

# Save figure
ggsave(filename = paste0("../figure/telephone-data.pdf"), plot = p)
