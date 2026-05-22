# ------------------------------------------------------------------------------
# FIG: LOG-COSH LOSS
# ------------------------------------------------------------------------------

library(ggplot2)

# DATA -------------------------------------------------------------------------

logcosh <- function(res) log(cosh(res))

x <- seq(-10L, 10L, length.out = 800L)
y <- logcosh(x)

# PLOTS ------------------------------------------------------------------------

p <- ggplot(data.frame(x, y), aes(x = x, y = y)) +
  geom_line() +
  theme_minimal() +
  xlab(expression(y-f(x))) + 
  ylab(expression(L(y, f(x)))) + 
  theme(text = element_text(size = 20L))
p
ggsave("../figure/loss_logcosh.png", p, width = 6L, height = 3L)
