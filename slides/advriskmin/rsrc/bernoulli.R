# ------------------------------------------------------------------------------
# classification bernoulli

# FIG: Bernoulli loss (1) on margin (2) on probability (3) on margin no quote
# ------------------------------------------------------------------------------

library(ggplot2)
theme_set(theme_minimal())

# DATA -------------------------------------------------------------------------

x_1 <- seq(-4, 4, by = 0.01)
y <- log(1L + exp(-x_1))

bernoulli = function(y, pix){
  -y * log(pix) - (1 - y) * log(1 - pix)
}

x_2 <- seq(0, 1, by = 0.001)
df <- data.frame(x = x_2, y = 1L, pi = bernoulli(1L, x_2))
df <- rbind(df, data.frame(x = x_2, y = 0L, pi = bernoulli(0L, x_2)))
df$y <- as.factor(df$y)

# PLOTS ------------------------------------------------------------------------

p_1 <- ggplot(data.frame(x_1, y), aes(x = x_1, y = y)) + 
  geom_line(size = 1.2) +
  xlab(expression(yf)) +
  ylab(expression(L(y, f)))

p_1 <- p_1 + 
  annotate(
    "text",
    x = 2,
    y = 2,
    label = expression(L(y, f) == ln(1 + exp(-y * f))),
    size = 7)

p_1 <- p_1 + theme(text = element_text(size = 20))

ggsave("../figure/bernoulli_margin.png", p_1, height = 4, width = 9)

p_2 <- ggplot(data = df, aes(x = x, y = pi, color = y)) + 
  geom_line(size = 1.2) + 
  xlab(expression(pi)) + 
  ylab(expression(L(y, pi))) + 
  theme(text = element_text(size = 20)) +
  scale_color_viridis_d(end = 0.9)

ggsave("../figure/bernoulli_prob.png", p_2, height = 4, width = 6)

p_3 <- ggplot(data.frame(x_1, y), aes(x = x_1, y = y)) + 
  geom_line(size = 1.6) +
  xlab(expression(yf)) +
  ylab(expression(L(y, f)))

ggsave("../figure/bernoulli.png", p_3, height = 2.85, width = 5)
