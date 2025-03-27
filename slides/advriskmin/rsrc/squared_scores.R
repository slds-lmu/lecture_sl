# ------------------------------------------------------------------------------
# classification further losses

# FIG: squared loss on scores
# ------------------------------------------------------------------------------

library(ggplot2)

# DATA -------------------------------------------------------------------------

x <- seq(-4, 4, by = 0.01)
y <- (1 - x)^2

# PLOTS ------------------------------------------------------------------------

p <- ggplot(data.frame(x, y), aes(x = x, y = y)) + 
  geom_line(size = 1.2) +
  scale_x_continuous(breaks = seq(-4, 4)) +
  xlab(expression(yf(x))) +
  ylab(expression(L(y, f(x)))) + 
  theme_minimal() +
  theme(text = element_text(size = 30))
p
ggsave("../figure/squared_scores.png", p, height = 4, width = 12)

