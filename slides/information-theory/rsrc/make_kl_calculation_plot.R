library(ggplot2)
library(gridExtra)
library(extraDistr)

####### PLOT KL DIVERGENCE FOR NORMAL AND LAPLACE DISTRIBUTION

set.seed(123)

x <- seq(-4, 4, length.out = 1000)

norm_density <- dnorm(x, 0, 1)
lp_density <- dlaplace(x, 0, 1.5)
ratio_norm_lp <- norm_density/lp_density
log_ratio <- log(ratio_norm_lp)
dens_ratio <- norm_density*log_ratio
data <- data.frame(x = x, NormalDensity = norm_density,
                   LaPlaceDensity = lp_density,
                   Ratio_Density = ratio_norm_lp,
                   LogRatio = log_ratio,
                   DensityRatio = dens_ratio)

integrand <- function(x) {
  
  n_density <- dnorm(x, 0, 1)
  l_density <- dlaplace(x, 0, 1.5)
  log_ratio <- log(n_density/l_density)
  n_density*log_ratio
}

result <- integrate(integrand, lower = 0, upper = 1)
kl <- round(result$value, 2)

plot1 = ggplot(data, aes(x = x)) +
  geom_line(aes(y = NormalDensity), color = "blue", size = 1, linetype = "solid") +
  geom_line(aes(y = LaPlaceDensity), color = "red", size = 1, linetype = "solid") +
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

p = grid.arrange(plot1, plot2, plot3, plot4,  ncol = 2)
ggsave("..figure/kl_calculation_plot.png", plot = p, width =8, height = 5)



