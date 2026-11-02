library(ggplot2)
library(gridExtra)
library(extraDistr)

####### PLOT KL DIVERGENCE FOR NORMAL AND LAPLACE DISTRIBUTION
set.seed(123)

kl_calc <- function(x, p, q, first){
  ratio <- p/q
  log_ratio <- log(ratio)
  dens_ratio <- p*log_ratio
  data <- data.frame(x = x, P = p,
                     Q = q,
                     Ratio_Density = ratio,
                     LogRatio = log_ratio,
                     DensityRatio = dens_ratio)
  
  if(first == "normal"){
    integrand <- function(x) {
      p <- dnorm(x, 0, 1)
      q <- dlaplace(x, 0, 1.5)
      log_ratio <- log(p/q)
      p*log_ratio
    }
  } else if (first == "laplace"){
    integrand <- function(x) {
      q <- dnorm(x, 0, 1)
      p <- dlaplace(x, 0, 1.5)
      log_ratio <- log(p/q)
      p*log_ratio
    }
  }
  
  
  result <- integrate(integrand, lower = -20, upper = 20)
  kl <- round(result$value, 2)
  
  plot1 = ggplot(data, aes(x = x)) +
    geom_line(aes(y = P), color = "blue", size = 1, linetype = "solid") +
    geom_line(aes(y = Q), color = "red", size = 1, linetype = "solid") +
    labs(title = "N(0,1) and LP(0,1.5) Densities", x = "x", y = "Density") +
    scale_color_manual(values = c("blue"))
  
  plot2 = ggplot(data, aes(x = x)) +
    geom_line(aes(y = Ratio_Density), color = "darkgreen", size = 1, linetype = "solid") +
    labs(title = "Ratio of Densities", x = "x", y = "p(x)/q(x)") +
    scale_color_manual(values = c("red"))
  
  plot3 = ggplot(data, aes(x = x)) +
    geom_line(aes(y = LogRatio), color = "purple", size = 1, linetype = "solid") +
    labs(title = "Log-Ratio of Densities", x = "x", y = "log(p(x)/q(x))") +
    scale_color_manual(values = c("red"))
  
  plot4 = ggplot(data, aes(x = x)) +
    geom_line(aes(y = DensityRatio), color = "orange", size = 1, linetype = "solid") +
    labs(title = "Integrand", x = "x", y = "p(x)*log(p(x)/q(x))") +
    geom_ribbon(aes(ymax = DensityRatio, ymin = 0), fill = "grey", alpha = 0.5) +
    geom_text(aes(x = 2.5, y = 0.1, label = paste("D_KL =",kl)), color = "black", size = 3) +
    scale_color_manual(values = c("orange"))
  
  plot = grid.arrange(plot1, plot2, plot3, plot4,  ncol = 2)
  
  return(plot)
}

x <- seq(-4, 4, length.out = 1000)
plot1 = kl_calc(x, p = dnorm(x, 0, 1), q = dlaplace(x, 0, 1.5), first = "normal")
plot2 = kl_calc(x, p = dlaplace(x, 0, 1.5), q = dnorm(x, 0, 1), first = "laplace")

ggsave("..figure/kl_calculation_plot_1.png", plot = plot1, width =8, height = 5)
ggsave("..figure/kl_calculation_plot_2.png", plot = plot2, width =8, height = 5)

###### VARY LAPLACE DENSITY AND CALCULATE KL WITH STANDARD NORMAL

kl_norm_lp <- function(x, a, b){
p = dnorm(x, 0, 1)
q = dlaplace(x, a, b)
data <- data.frame(x = x, P = p, Q = q)

integrand <- function(x) {
  p <- dnorm(x, 0, 1)
  q <- dlaplace(x, a, b)
  log_ratio <- log(p/q)
  p*log_ratio
}
result <- integrate(integrand, lower = -20, upper = 20)
kl <- round(result$value, 2)

plot = ggplot(data, aes(x = x)) +
  geom_line(aes(y = P), color = "blue", size = 1, linetype = "solid") +
  geom_line(aes(y = Q), color = "red", size = 1, linetype = "solid") +
  labs(title = sprintf("N(0,1) and LP(%d,%s) Densities", a, b), x = "x", y = "Density") +
  scale_color_manual(values = c("blue")) +
  geom_text(aes(x = 2.5, y = 0.2, label = paste("D_KL =",kl)), color = "black", size = 3)
return(plot)
}

x <- seq(-4, 4, length.out = 1000)
plot1 = kl_norm_lp(x = x, a = 0, b = 0.2)
plot2 = kl_norm_lp(x = x, a = 0, b = 1.5)
plot3 = kl_norm_lp(x = x, a = 0, b = 3)
plot_1 = grid.arrange(plot1, plot2, plot3, ncol = 3)
ggsave("..figure/kl_norm_lp.png", plot = plot_1, width = 12, height = 3)

### CREATE KL PLOT FOR VARYING b of LP(0,b) with N(0,1)

x <- seq(-4, 4, length.out = 1000)
seq <- seq(0.1, 10, length.out = 1000)
kls = list()

for (i in seq_along(seq)){
  
integrand <- function(x) {
  p <- dnorm(x, 0, 1)
  q <- dlaplace(x, 0, seq[i])
  log_ratio <- log(p/q)
  p*log_ratio
}

result <- integrate(integrand, lower = -20, upper = 20)
kl <-result$value
kls[[i]] = kl
}

data <- data.frame(Sigma = seq, KL = unlist(kls))

plot_2 = ggplot(data, aes(x = Sigma)) +
  geom_line(aes(y = KL), color = "orange", size = 1, linetype = "solid") +
  labs(title = "KL Divergence depending on Sigma", x = "Sigma", y = "KL Divergence") +
  scale_color_manual(values = c("orange"))

ggsave("..figure/kl_norm_lp_sigma.png", plot = plot_2, width = 5, height = 3)


### CREATE LOG DIFFERENCE PLOT FOR KL

x <- seq(-4, 4, length.out = 1000)
p <- dnorm(x, 0, 1)
q <- dlaplace(x, 0, 3)
logp <- log(p)
logq <- log(q)
plogp <- p*logp
plogq <- p*logq
diff <- plogp - plogq

data <- data.frame(x = x, P = p,
                   Q = q,
                   LogP = logp,
                   LogQ = logq,
                   PlogP = plogp,
                   PlogQ = plogq,
                   Diff = diff)

integrand <- function(x) {
  p <- dnorm(x, 0, 1)
  q <- dlaplace(x, 0, 3)
  log_ratio <- log(p/q)
  p*log_ratio
}
result <- integrate(integrand, lower = -20, upper = 20)
kl <- round(result$value, 2)

plot1 = ggplot(data, aes(x = x)) +
  geom_line(aes(y = P), color = "blue", size = 1, linetype = "solid") +
  geom_line(aes(y = Q), color = "red", size = 1, linetype = "solid") +
  labs(title = "N(0,1) and LP(0,3) Densities", x = "x", y = "Density") +
  scale_color_manual(values = c("blue"))


plot2 = ggplot(data, aes(x = x)) +
  geom_line(aes(y = LogP), color = "blue", size = 1, linetype = "solid") +
  geom_line(aes(y = LogQ), color = "red", size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = LogQ, ymax = LogP), alpha = 0.2) +
  labs(title = "log(p(x)) - log(q(x))", x = "x", y = "Log-Density") +
  scale_color_manual(values = c("blue"))

plot3 = ggplot(data, aes(x = x)) +
  geom_line(aes(y = PlogP), color = "blue", size = 1, linetype = "solid") +
  geom_line(aes(y = PlogQ), color = "red", size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = PlogQ, ymax = PlogP), alpha = 0.2) +
  labs(title = "E[log(p(x))] - E[log(q(x))]", x = "x", y = "Expectation Difference") +
  geom_text(aes(x = 2.5, y = -0.5, label = paste("D_KL =",kl)), color = "black", size = 3) +
  scale_color_manual(values = c("blue"))

plot4 = ggplot(data, aes(x = x)) +
  geom_line(aes(y = diff), color = "orange", size = 1, linetype = "solid") +
  geom_ribbon(aes(ymin = 0, ymax = diff), alpha = 0.2) +
  labs(title = "E[log(p(x)/q(x))]", x = "x", y = "Expectation Ratio") +
  geom_text(aes(x = 2.5, y = 0.1, label = paste("D_KL =",kl)), color = "black", size = 3) +
  scale_color_manual(values = c("blue"))

plot = grid.arrange(plot1, plot2, plot3, plot4,  ncol = 2)
ggsave("..figure/kl_log_diff.png", plot = plot, width =8, height = 5)

