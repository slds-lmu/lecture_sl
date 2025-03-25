# ------------------------------------------------------------------------------
# classification brier

# FIG: Brier score
# ------------------------------------------------------------------------------

library(ggplot2)

# DATA -------------------------------------------------------------------------

x <- seq(0, 1, by = 0.01)

df <- rbind(
  data.frame(x, y = 1, pi = (1 - x)^2),
  data.frame(x, y = 0, pi = x^2))

df$y <- as.factor(df$y)

# PLOTS ------------------------------------------------------------------------

p <- ggplot(data = df, aes(x = x, y = pi, color = y)) + 
  geom_line(size = 1.2) + 
  xlab(expression(pi(x))) + 
  ylab(expression(L(y, pi(x)))) + 
  theme_minimal() +
  theme(text = element_text(size = 20)) +
  scale_color_viridis_d(end = 0.9)
p
ggsave("../figure/brier.png", p, height = 4, width = 9)
