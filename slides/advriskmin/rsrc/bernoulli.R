# ------------------------------------------------------------------------------
# classification bernoulli

# FIG: Bernoulli loss (1) on margin (2) on probability (3) on margin no quote
# and (4) on log loss
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
  xlab(expression(yf(x))) +
  ylab(expression(L(y, f(x))))

p_1 <- p_1 + 
  annotate(
    "text",
    x = 1.5,
    y = 2,
    label = expression(L(y, f(x)) == log(1 + exp(-y * f(x)))),
    size = 7)

p_1 <- p_1 + theme(text = element_text(size = 20))
p_1
ggsave("../figure/bernoulli_margin.png", p_1, height = 4, width = 9)

p_2 <- ggplot(data = df, aes(x = x, y = pi, color = y)) + 
  geom_line(size = 1.2) + 
  xlab(expression(pi(x))) + 
  ylab(expression(L(y, pi(x)))) + 
  theme(text = element_text(size = 20)) +
  scale_color_viridis_d(end = 0.9)
p_2
ggsave("../figure/bernoulli_prob.png", p_2, height = 4, width = 6)

p_3 <- ggplot(data.frame(x_1, y), aes(x = x_1, y = y)) + 
  geom_line(size = 1.6) +
  xlab(expression(yf(x))) +
  ylab(expression(L(y, f(x))))
p_3
ggsave("../figure/bernoulli.png", p_3, height = 2.85, width = 5)

# ------------------------------------------------------------------------------

# Define a sequence of f values over the range [-10, 10]
f_values <- seq(-5, 5, length.out = 1000)

# Compute the loss values for y = 0 and y = 1
loss_y0 <- log(1 + exp(f_values))         # L(0, f) = log(1 + exp(f))
loss_y1 <- -f_values + log(1 + exp(f_values))  # L(1, f) = -f + log(1 + exp(f))

# Create individual data frames for y = 0 and y = 1
df_y0 <- data.frame(f = f_values, Loss = loss_y0, y = factor(0))
df_y1 <- data.frame(f = f_values, Loss = loss_y1, y = factor(1))

# Combine the data frames
df <- rbind(df_y0, df_y1)

# Create the ggplot2 plot with similar styling
p_4 <- ggplot(data = df, aes(x = f, y = Loss, color = y)) + 
  geom_line(size = 1.2) + 
  xlab(expression(f(x))) + 
  ylab(expression(L(y, f(x)))) + 
  theme(text = element_text(size = 20)) +
  scale_color_viridis_d(end = 0.9)

print(p_4)
ggsave("../figure/bernoulli_logloss.png", p_4 height = 4, width = 6)