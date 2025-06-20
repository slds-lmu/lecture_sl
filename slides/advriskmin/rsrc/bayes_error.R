# ------------------------------------------------------------------------------
# classification 01

# FIG: Simulate data from a bivariate normal 
# ------------------------------------------------------------------------------
 
library(mvtnorm)
library(ggplot2)

# ------------------------------------------------------------------------------

mu1 = c(-1, 1)
mu2 = c(1, -1)

sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2)

df1 = as.data.frame(rmvnorm(n = 1000, mean = mu1, sigma = sigma))
df1$class = 0
df2 = as.data.frame(rmvnorm(n = 1000, mean = mu2, sigma = sigma))
df2$class = 1

data.grid = expand.grid(x1 = seq(-5, 5, length.out=100), x2 = seq(-5, 5, length.out=100))
q.samp = cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = mu1, sigma = sigma), class = 0)
q.samp2 = cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = mu2, sigma = sigma), class = 1)
data.grid = rbind(q.samp, q.samp2)
data.grid$class = as.factor(data.grid$class)

data.grid$overlap = FALSE
data.grid[data.grid$class == 1 & data.grid$x1 <= 0, ]$overlap = TRUE
data.grid[data.grid$class == 0 & data.grid$x1 > 0, ]$overlap = TRUE


df = rbind(df1, df2)
names(df)[1:2] = c("x1", "x2")

df$class = as.factor(df$class)

p <- ggplot() +
  geom_line(
    data = data.grid, 
    aes(x = x1, y = prob, colour = class), 
    alpha = 0.5, 
    size = 2) + 
  scale_color_viridis_d(end = 0.9) +
  geom_line(
    data = data.grid[data.grid$overlap == TRUE, ], 
    aes(x = x1, y = prob), 
    colour = "darkred", 
    alpha = 1, size = 2) +
  xlab("x") +
  ylab("P(x|y)") +
  geom_vline(xintercept = 0, colour = "orange", size = 2) +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = 20L))
p

ggsave("../figure/bayes_error.png", p, width = 9, height = 4)



