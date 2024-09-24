# ------------------------------------------------------------------------------
# classification further losses

# FIG: exponential loss
# ------------------------------------------------------------------------------

library(ggplot2)

# DATA -------------------------------------------------------------------------

x <- seq(-4, 4, by = 0.01)
y <- exp(-x)

# PLOTS ------------------------------------------------------------------------

p <- ggplot(data.frame(x, y), aes(x = x, y = y)) + 
  geom_line(size = 1.2) +
  scale_x_continuous(breaks = seq(-4, 4)) +
  xlab(expression(yf)) +
  ylab(expression(L(y, f))) + 
  theme_minimal() +
  theme(text = element_text(size = 30))

ggsave("../figure/exponential.png", p, height = 4, width = 12)

