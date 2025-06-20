# ------------------------------------------------------------------------------
# pseudo residuals
# FIG: QUADRATIC LOSS WITH PSEUDO-RESIDUAL
# ------------------------------------------------------------------------------

library(ggplot2)

# DATA -------------------------------------------------------------------------

x <- seq(-2L, 2L, by = 0.01)
y <- x^2

# PLOTS ------------------------------------------------------------------------

p <- qplot(
  x, 
  y, 
  geom = "line",
  xlab = bquote(y - f(x)),
  ylab = bquote(L(y, f(x))))

p <- p + 
  geom_segment(
    mapping = aes(x = 0.5, xend = 2L, y = 0L, yend = 3L),
    size = 1.1,
    col = "blue") +
  geom_segment(
    mapping = aes(x = 1L, xend = 1L, y = 0L, yend = 1L),
    size = 1.1,
    col = "blue") +
  geom_point(
    mapping = aes(x = 1L, y = 1L),
    col = "blue",
    size = 3L) +
  annotate(
    "text", 
    x = 0L, 
    y = 3L, 
    label = bquote(L(y, f(x)) ~ "=" ~ (y - f(x))^2), 
    size = 7L)

p <- p + theme_minimal()
p <- p + theme(text = element_text(size = 20L))
p

ggsave(
  "../figure/quad_pseudores.png", 
  p, 
  height = 4L, 
  width = 10L)

