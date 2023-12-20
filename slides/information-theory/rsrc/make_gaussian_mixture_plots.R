library(ggplot2)
library(MASS)     
library(gridExtra) 


# Define parameters for the Gaussian distributions
mean1 <- c(0, 0)
mean2 <- c(5, 5)
covariance <- matrix(c(1, 0, 0, 1), nrow = 2)

# Generate samples
set.seed(0)
samples1 <- mvrnorm(n = 1000, mu = mean1, Sigma = covariance)
samples2 <- mvrnorm(n = 1000, mu = mean2, Sigma = covariance)
samples <- rbind(samples1, samples2)

# Create a data frame
df <- data.frame(x = samples[,1], y = samples[,2])

# Create the main contour plot
p_main <- ggplot(df, aes(x = x, y = y)) + 
  geom_density_2d_filled() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  theme(legend.position = "none")

# Create the top marginal plot (X-axis)
p_top <- ggplot(df, aes(x = x)) +
  geom_density(aes(y = after_stat(density)), fill = "blue", alpha = 0.5) +
  theme_void() +
  theme(plot.margin = margin(t = 0, r = 30, b = 0, l = 0),
        axis.title.x = element_text(size = 20)) + 
  labs(x = "x1")

# Create the right marginal plot (Y-axis)
p_right <- ggplot(df, aes(x = y)) +
  geom_density(aes(y = after_stat(density)), fill = "blue", alpha = 0.5) +
  theme_void() +
  theme(plot.margin = margin(t = 0, r = 30, b = 0, l = 0),
        axis.title.y = element_text(size = 20)) + 
  labs(x = "x2") +
  coord_flip() 

# Arrange the plots together
empty <- ggplot() + geom_blank(aes(x = 0, y = 0)) + theme_void()
p1 <- arrangeGrob(p_top, empty, p_main, p_right,
                          ncol = 2, nrow = 2,
                          widths = c(4, 0.5),
                          heights = c(2, 7))


# Define parameters for the Gaussian distributions
mean1 <- c(0, 0)
mean2 <- c(10, 10)
mean3 <- c(10, 0)
mean4 <- c(0, 10)

# Generate samples
set.seed(0)
samples1 <- mvrnorm(n = 1000, mu = mean1, Sigma = covariance)
samples2 <- mvrnorm(n = 1000, mu = mean2, Sigma = covariance)
samples3 <- mvrnorm(n = 1000, mu = mean3, Sigma = covariance)
samples4 <- mvrnorm(n = 1000, mu = mean4, Sigma = covariance)

samples <- rbind(samples1, samples2, samples3, samples4)

# Create a data frame
df <- data.frame(x = samples[,1], y = samples[,2])

# Create the main contour plot
p_main <- ggplot(df, aes(x = x, y = y)) + 
  geom_density_2d_filled() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  theme(legend.position = "none")

# Create the top marginal plot (X-axis)
p_top <- ggplot(df, aes(x = x)) +
  geom_density(aes(y = after_stat(density)), fill = "blue", alpha = 0.5) +
  theme_void() +
  theme(plot.margin = margin(t = 0, r = 30, b = 0, l = 0),
        axis.title.x = element_text(size = 20)) + 
  labs(x = "x1")

# Create the right marginal plot (Y-axis)
p_right <- ggplot(df, aes(x = y)) +
  geom_density(aes(y = after_stat(density)), fill = "blue", alpha = 0.5) +
  theme_void() +
  theme(plot.margin = margin(t = 0, r = 30, b = 0, l = 0),
        axis.title.y = element_text(size = 20)) + 
  labs(x = "x2") +
  coord_flip() 

# Arrange the plots together
empty <- ggplot() + geom_blank(aes(x = 0, y = 0)) + theme_void()
p2 <- arrangeGrob(p_top, empty, p_main, p_right,
                          ncol = 2, nrow = 2,
                          widths = c(4, 0.5),
                          heights = c(2, 7))

# Display the plot
grid.newpage()
grid.draw(p2)

plot = grid.arrange(p1, p2,  ncol = 2)

ggsave(file = "../figure/gaussian_mixture_with_marginals.png", plot = plot, width = 24, height = 8, dpi = 300)

