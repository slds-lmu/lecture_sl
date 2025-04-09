### This script produces a plot comparing various loss functions for robust regression
### Dataset: telephone data (number of calls per year in Belgium)
### Losses: L2, L1, log-cosh, Huber loss, Cauchy loss

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

# Cauchy loss robust regression
# Cauchy loss: ρ(r) = log(1 + r^2) with γ = 1
f.cauchy <- function(b) with(tel.dat, sum(log(1 + (calls - X %*% b)^2)))
res.optim.cauchy <- optim(coef(fm0), f.cauchy, method = "Nelder-Mead", control=list(alpha=1.0, beta=0.5, gamma=2.0))
res.cauchy <- res.optim.cauchy$par
resids.cauchy <- with(tel.dat, calls - X %*% res.cauchy)
preds.cauchy <- as.numeric(with(tel.dat, X %*% res.cauchy))

# Aggregate results data
res.tel <- tel.dat
res.tel$resid_l2 <- resids.l2
res.tel$resid_l1_manual <- resids.l1.manual
res.tel$resid_huber <- resids.huber
res.tel$resid_logcosh <- resids.logcosh
res.tel$resid_cauchy <- resids.cauchy
res.tel$pred_l2 <- preds.l2
res.tel$pred_l1_manual <- preds.l1.manual
res.tel$pred_huber <- preds.huber
res.tel$pred_logcosh <- preds.logcosh
res.tel$pred_cauchy <- preds.cauchy

p <- ggplot(res.tel, aes(x = year)) +
  geom_line(aes(y = pred_l2, color = "L2 (OLS)", linetype = "L2 (OLS)"), size = 1.6, alpha = 1) +   
  geom_line(aes(y = pred_l1_manual, color = "L1", linetype = "L1"), size = 1.6, alpha = 1) +
  geom_line(aes(y = pred_huber, color = "Huber", linetype = "Huber"), size = 1.6, alpha = 1) +
  geom_line(aes(y = pred_logcosh, color = "Log-Cosh", linetype = "Log-Cosh"), size = 1.6, alpha = 1) +
  geom_line(aes(y = pred_cauchy, color = "Cauchy", linetype = "Cauchy"), size = 1.6, alpha = 1) +
  geom_point(aes(y = calls), color = "black", size = 4, alpha = 1) +  
  labs(y = "Calls (in mio.)", x = "Year", color = "Loss", linetype = "Loss") +
  scale_linetype_manual(values = c(
    "L2 (OLS)" = "solid", 
    "L1" = "solid", 
    "Huber" = "dotted", 
    "Log-Cosh" = "dotdash", 
    "Cauchy" = "twodash"
  )) +
  theme_minimal() +
  ggplot2::guides(
    color = guide_legend(
      ncol = 1,
      keywidth = unit(2, "cm"),  # Increase the length of the legend lines
      override.aes = list(
        linetype = c("twodash", "dotted", "solid", "solid", "dotdash")
      )
    ),
    linetype = "none"
  ) +
  theme(text = element_text(size = 12), 
        axis.title = element_text(size = rel(2)),   
        axis.text = element_text(size = rel(2)),  
        legend.title = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.3)))

print(p)

# Save figure
ggsave(filename = paste0("../figure/telephone-data.pdf"), plot = p)
