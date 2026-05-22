library(ggplot2)
library(gridExtra)
library(extraDistr)

set.seed(123)

### CREATE KL PLOT FOR VARYING b of LP(0,b) with N(0,1)

x <- seq(-4, 4, length.out = 1000)
seq <- seq(0.1, 10, length.out = 1000)
kls = list()
ces = list()

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

for (i in seq_along(seq)){
  
  integrand <- function(x) {
    p <- dnorm(x, 0, 1)
    q <- dlaplace(x, 0, seq[i])
    -p*log(q)
  }
  
  result <- integrate(integrand, lower = -20, upper = 20)
  ce <-result$value
  ces[[i]] = ce
}

data <- data.frame(Sigma = seq, KL = unlist(kls), CE = unlist(ces))

min_kl <- round(min(data$KL), 2)
min_ce <- round(min(data$CE), 2)
minimizer_kl <- round(data$Sigma[which.min(data$KL)], 2)
minimizer_ce <- round(data$Sigma[which.min(data$CE)], 2)

plot1 = ggplot(data, aes(x = Sigma)) +
  geom_line(aes(y = KL), color = "orange", size = 1, linetype = "solid") +
  labs(title = "KL Divergence depending on Sigma", x = "Sigma", y = "KL Divergence") +
  scale_y_continuous(limits = c(0, 5)) +
  geom_text(aes(x = 5, y = 3, label = paste("min D_KL(p||q) =",min_kl)), color = "black", size = 3) +
  geom_text(aes(x = 5, y = 2, label = paste("Minimizer =",minimizer_kl)), color = "black", size = 3) +
  scale_color_manual(values = c("orange"))


plot2 = ggplot(data, aes(x = Sigma)) +
  geom_line(aes(y = CE), color = "darkgreen", size = 1, linetype = "solid") +
  labs(title = "Cross-Entropy depending on Sigma", x = "Sigma", y = "Cross-Entropy") +
  geom_text(aes(x = 5, y = 1.5, label = paste("min H(p||q) =",min_ce)), color = "black", size = 3) +
  geom_text(aes(x = 5, y = 0.5, label = paste("Minimizer =",minimizer_ce)), color = "black", size = 3) +
  scale_y_continuous(limits = c(0, 5)) +
  scale_color_manual(values = c("darkgreen"))

plot = grid.arrange(plot1, plot2,  ncol = 2)

ggsave("..figure/kl_ce_comparison.png", plot = plot, width =8, height = 3)
