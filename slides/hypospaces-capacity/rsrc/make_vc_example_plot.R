# ------------------------------------------------------------------------------
# FIG: VC EXAMPLE
# ------------------------------------------------------------------------------
library(knitr)
library(ggplot2)
library(viridis)

theme_set(theme_minimal())


# DATA -------------------------------------------------------------------------

b <- data.frame(x = c(0.75, 2.25), y = c(0, 0), class = c("1", "0"))

d1 <- data.frame(x = c(-2, 0, 4), y = c(0, 1, 1))
d2 <- data.frame(x = c(-2, 1, 4), y = c(0, 1, 1))
d3 <- data.frame(x = c(-2, 3, 4), y = c(0, 1, 1))


# PLOTS ------------------------------------------------------------------------

p <- ggplot() +
  geom_step(data = d1, mapping = aes(x = x, y = y)) +
  geom_step(data = d1, mapping = aes(x = x, y = y)) +
  geom_step(data = d2, mapping = aes(x = x, y = y)) +
  geom_step(data = d3, mapping = aes(x = x, y = y)) +
  geom_point(data = b, mapping = aes(x = x, y = y, color = factor(class)), size =4) + xlab("x") + ylab(expression(paste("I[x>", theta, "]"))) +
  labs(color="Class") +
  scale_color_viridis(discrete = TRUE, end = 0.9)

ggsave("../figure/vc_example.png", plot=p, width=7, height=2)
