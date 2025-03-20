# ------------------------------------------------------------------------------
# This script generates two plots for binary classification loss functions.
# 1. The Brier score (squared loss), where for a given parameter x:
#    - For y = 1, the loss is (1-x)^2.
#    - For y = 0, the loss is x^2.
# 2. The L1 loss (absolute error) on probabilities.
#    For a predicted probability p in [0,1]:
#    - For y = 1, L1 loss is 1 - p.
#    - For y = 0, L1 loss is p.
# Both plots are generated with ggplot2 and saved as PNG files.
# ------------------------------------------------------------------------------

library(ggplot2)

# DATA FOR BRIER SCORE ---------------------------------------------------------

x <- seq(0, 1, by = 0.01)
df <- rbind(
  data.frame(x, y = 1, pi = (1 - x)^2),
  data.frame(x, y = 0, pi = x^2)
)
df$y <- as.factor(df$y)

# PLOT: Brier Score ------------------------------------------------------------
p <- ggplot(data = df, aes(x = x, y = pi, color = y)) + 
  geom_line(size = 1.2) + 
  xlab(expression(pi)) + 
  ylab(expression(L[2](y, pi))) + 
  theme_minimal() +
  theme(text = element_text(size = 20)) +
  scale_color_viridis_d(end = 0.9)

ggsave("../figure/brier.png", p, height = 4, width = 9)

# DATA FOR L1 LOSS ON PROBABILITIES --------------------------------------------
p_seq <- seq(0, 1, by = 0.01)
df_l1 <- rbind(
  data.frame(p = p_seq, y = 1, loss = 1 - p_seq),
  data.frame(p = p_seq, y = 0, loss = p_seq)
)
df_l1$y <- as.factor(df_l1$y)

# PLOT: L1 Loss ---------------------------------------------------------------
p_l1 <- ggplot(data = df_l1, aes(x = p, y = loss, color = y)) + 
  geom_line(size = 1.2) + 
  xlab(expression(pi)) + 
  ylab(expression(L[1](y, pi))) + 
  theme_minimal() +
  theme(text = element_text(size = 20)) +
  scale_color_viridis_d(end = 0.9)

ggsave("../figure/l1_loss.png", p_l1, height = 4, width = 9)
