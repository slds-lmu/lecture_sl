library(ggplot2)
library(MASS)     
library(gridExtra)

########## CREATE NORMAL DISTRIBUTIONS

  
set.seed(123)

x <- seq(-4, 7, length.out = 1000)
normal_density1 <- dnorm(x, 0, 1)
normal_density2 <- dnorm(x, 3, 1)
data <- data.frame(x = x, NormalDensity1 = normal_density1, NormalDensity2 = normal_density2)
p = ggplot(data, aes(x = x)) +
    geom_line(aes(y = NormalDensity1), color = "blue", size = 1) +
    geom_line(aes(y = NormalDensity2), color = "red", size = 1) +
    labs(x = "x",
         y = "Density") +
    geom_text(aes(x = 1.5, y = 0.34, label = "?"), color = "black", size = 15)

ggsave("../figure/normal_distributions.png", plot = p, width = 7, height = 3)



# Generate samples from both distributions

samples1 <- rnorm(1000, 0, 1)
samples2 <- rnorm(1000, 5, 1)

# Combine samples to form a Gaussian mixture
mixture_samples <- c(samples1, samples2)

data <- data.frame(value = mixture_samples)

# Use density function to estimate the density of the mixture
density_data <- density(mixture_samples, bw = "nrd0")
density_df <- data.frame(value = density_data$x, density = density_data$y)

# Plotting the density using ggplot2
p1 = ggplot(density_df, aes(x = value, y = density)) +
       geom_line(color = "blue") +
       labs(title = "Reverse KL",
       x = "x",
       y = "Density") +
       stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "red")


p2 <- ggplot(density_df, aes(x = value, y = density)) +
  geom_line(aes(color = "p(x)")) + 
  labs(title = "Forward KL",
       x = "x",
       y = "Density") +
  stat_function(fun = dnorm, args = list(mean = 2.5, sd = 3), aes(color = "q(x)")) + 
  scale_color_manual(name = "", values = c("p(x)" = "blue", "q(x)" = "red")) +  
  theme(legend.position = "right") 

plot = grid.arrange(p1, p2, ncol = 2, widths = c(1, 1.25))
ggsave("../figure/kl_fitting_plot.png", plot =plot, width = 8, height = 3)
