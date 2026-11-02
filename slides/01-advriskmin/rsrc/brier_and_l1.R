# ------------------------------------------------------------------------------
# classification bernoulli

# This script generates four plots for binary classification loss functions.
# 1. The Brier score on probabilities, where for a given prob pi:
#    - For y = 1, the loss is (1-pi)^2.
#    - For y = 0, the loss is pi^2.
# 2. The Brier score on score function, where for a given score f:
#    - For y = 1, the loss is (1-sigmoid(f))^2.
#    - For y = 0, the loss is sigmoid(f)^2.
# 3. The L1 loss (absolute error) on probabilities.
#    For a predicted probability p in [0,1]:
#    - For y = 1, L1 loss is 1 - p.
#    - For y = 0, L1 loss is p.
# 4. The L1 loss (absolute error) on score functions.
#    For sigmoid(f) in [0,1]:
#    - For y = 1, L1 loss is 1 - sigmoid(f).
#    - For y = 0, L1 loss is sigmoid(f).
# Both plots are generated with ggplot2 and saved as PNG files.
# ------------------------------------------------------------------------------

library(ggplot2)

# DATA FOR BRIER SCORE ---------------------------------------------------------

# on prob
x <- seq(0, 1, by = 0.01)
df <- rbind(
  data.frame(x, y = 1, pi = (1 - x)^2),
  data.frame(x, y = 0, pi = x^2)
)
df$y <- as.factor(df$y)

# on score
x2 <- seq(-6, 6, by = 0.01)
sigmoid <- function(z) 1 / (1 + exp(-z))
df2 <- rbind(
  data.frame(x = x2, y = 1, pi = (sigmoid(x2) - 1)^2),
  data.frame(x = x2, y = 0, pi = (sigmoid(x2) - 0)^2)
)
df2$y <- as.factor(df2$y)

# PLOT: Brier Score ------------------------------------------------------------
p <- ggplot(data = df, aes(x = x, y = pi, color = y)) + 
  geom_line(size = 1.2) + 
  xlab(expression(pi(x))) + 
  ylab(expression(L(y, pi(x)))) + 
  theme_minimal() +
  theme(text = element_text(size = 20)) +
  scale_color_viridis_d(end = 0.9)
p
ggsave("../figure/brier_and_l1_brier.png", p, height = 4, width = 9)

p2 <- ggplot(data = df2, aes(x = x, y = pi, color = y)) + 
  geom_line(size = 1.2) + 
  xlab(expression(f(x))) + 
  ylab(expression(L(y, f(x)))) + 
  theme_minimal() +
  theme(text = element_text(size = 20)) +
  scale_color_viridis_d(end = 0.9)
p2
ggsave("../figure/brier_and_l1_brier_on_score.png", p2, height = 4, width = 9)
# DATA FOR L1 LOSS ON PROBABILITIES --------------------------------------------

# on prob
p_seq <- seq(0, 1, by = 0.01)
df_l1 <- rbind(
  data.frame(p = p_seq, y = 1, loss = 1 - p_seq),
  data.frame(p = p_seq, y = 0, loss = p_seq)
)
df_l1$y <- as.factor(df_l1$y)

# on score
f_seq <- seq(-6, 6, by = 0.01)
sigmoid <- function(f) 1 / (1 + exp(-f))
df_l1_score <- rbind(
  data.frame(f = f_seq, y = 1, loss = abs(sigmoid(f_seq) - 1)),
  data.frame(f = f_seq, y = 0, loss = abs(sigmoid(f_seq) - 0))
)
df_l1_score$y <- as.factor(df_l1_score$y)

# PLOT: L1 Loss ---------------------------------------------------------------
p_l1 <- ggplot(data = df_l1, aes(x = p, y = loss, color = y)) + 
  geom_line(size = 1.2) + 
  xlab(expression(pi(x))) + 
  ylab(expression(L(y, pi(x)))) + 
  theme_minimal() +
  theme(text = element_text(size = 20)) +
  scale_color_viridis_d(end = 0.9)
p_l1
ggsave("../figure/brier_and_l1_l1.png", p_l1, height = 4, width = 9)

p_l1_score <- ggplot(data = df_l1_score, aes(x = f, y = loss, color = y)) + 
  geom_line(size = 1.2) + 
  xlab(expression(f(x))) + 
  ylab(expression(L(y, f(x)))) + 
  theme_minimal() +
  theme(text = element_text(size = 20)) +
  scale_color_viridis_d(end = 0.9)
p_l1_score
ggsave("../figure/brier_and_l1_l1_on_score.png", p_l1_score, height = 4, width = 9)

