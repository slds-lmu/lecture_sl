# ------------------------------------------------------------------------------
# classification 01

# FIG: 0-1 Loss
# ------------------------------------------------------------------------------

library(ggplot2)

# DATA -------------------------------------------------------------------------

x <- seq(-2, 2, by = 0.01)
y <- as.numeric(x < 0)

# PLOTS ------------------------------------------------------------------------

p <- ggplot(data.frame(x, y), aes(x = x, y = y)) + 
  geom_line(size = 1.2) +
  xlab(expression(yf(x))) +
  ylab(expression(L(y, f(x))))

p <- p + theme_minimal()
p <- p + theme(text = element_text(size = 20))
p
ggsave("../figure/zero_one.png", p, height = 4, width = 9)

