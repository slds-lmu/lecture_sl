library(ggplot2)
library(MASS)     
library(gridExtra) 


mean1 <- c(6, 0)
mean2 <- c(0, 10)
covariance <- matrix(c(1, 0, 0, 1), nrow = 2)

# Generate samples
set.seed(0)
samples1 <- mvrnorm(n = 1000, mu = mean1, Sigma = covariance)
samples2 <- mvrnorm(n = 1000, mu = mean2, Sigma = covariance)
samples <- rbind(samples1, samples2)

# Additional bivariate gaussian with mean c(0,0)
q1 <- mvrnorm(n = 100, mu = c(0, 0), Sigma = covariance)
q2 <- mvrnorm(n = 100, mu = c(2.5, 5), Sigma = matrix(c(1.5, 0.5, 0.5, 1.5), nrow = 2))
q3 <- mvrnorm(n = 100, mu = c(6, 0), Sigma = covariance)
q4 <- mvrnorm(n = 100, mu = c(0, 10), Sigma = covariance)

# Create a data frame
df <- data.frame(x = samples[,1], y = samples[,2])
q1_df <- data.frame(x = q1[,1], y = q1[,2])
q2_df <- data.frame(x = q2[,1], y = q2[,2])
q3_df <- data.frame(x = q3[,1], y = q3[,2])
q4_df <- data.frame(x = q4[,1], y = q4[,2])

p1 <- ggplot(df, aes(x = x, y = y)) +
  geom_density_2d_filled() +
  geom_point(data = additional_df, aes(x = x, y = y), color = "red", size = 0.5) +
  theme_minimal() +
  labs(x = expression(theta[1]), y = expression(theta[2]), title = "") +
  theme(legend.position = "none",
        plot.title = element_text(color = "red", size = 15, hjust = 0.5, face = "bold"))


p2 <- ggplot(df, aes(x = x, y = y)) +
  geom_density_2d_filled() +
  geom_point(data = q2_df, aes(x = x, y = y), color = "red", size = 0.5) +
  theme_minimal() +
  labs(x = expression(theta[1]), y = expression(theta[2]), title = "Sampling from q(x)") +
  theme(legend.position = "none",
        plot.title = element_text(color = "red", size = 15, hjust = 0.5, face = "bold"))

# Update p3
p3 <- ggplot(df, aes(x = x, y = y)) +
  geom_density_2d_filled() +
  geom_point(data = q3_df, aes(x = x, y = y), color = "red", size = 0.5) +
  geom_point(data = q4_df, aes(x = x, y = y), color = "red", size = 0.5) +
  theme_minimal() +
  labs(x = expression(theta[1]), y = expression(theta[2]), title = "") +
  theme(legend.position = "none",
        plot.title = element_text(color = "red", size = 15, hjust = 0.5, face = "bold"))



plot = grid.arrange(p1, p2, p3, ncol = 3)


ggsave("../figure/gaussian_mixture_scatter.png", plot = plot, width = 11, height = 3)
