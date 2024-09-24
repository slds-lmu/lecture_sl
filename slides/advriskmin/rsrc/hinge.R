# ------------------------------------------------------------------------------
# classification further losses

# FIG: 
#   (1) hinge loss & 0-1 loss
#   (2) hinge loss , 0-1 loss & hige-squared loss
# ------------------------------------------------------------------------------

library(ggplot2)
library(tidyr)

# DATA -------------------------------------------------------------------------

x <- seq(-1, 2, by = 0.01)
l_01 <- as.numeric(x < 0)
l_hinge <- ifelse(x < 1, 1 - x, 0)
l_hinge_squared <- ifelse(x < 1, (1 - x)^2, 0)

df <- gather(
  data.frame(x, l_hinge_squared, l_hinge, l_01), 
  "loss", 
  "value", 
  -x)

# PLOTS ------------------------------------------------------------------------

p_1 <- ggplot(
  df[df$loss != "l_hinge_squared", ],
  aes(x = x, y = value, col = loss)) +
  geom_line(size = 1.2) +
  scale_color_viridis_d(
    end = 0.9,
    name = "Loss",
    labels = c("0-1", "hinge"),
    direction = -1L) + 
  xlab(expression(yf)) +
  ylab(expression(L(y, f))) +
  theme_minimal() + 
  theme(text = element_text(size = 30))

ggsave("../figure/hinge.png", p_1, height = 4, width = 12)

p_2 <- ggplot(
  df,
  aes(x = x, y = value, col = loss)) +
  geom_line(size = 1.2) +
  scale_color_viridis_d(
    end = 0.9,
    name = "Loss",
    labels = c("0-1", "hinge", "squared hinge"),
    direction = -1L) + 
  xlab(expression(r = yf)) +
  ylab(expression(L(y, f))) +
  theme_minimal() + 
  theme(text = element_text(size = 30))

ggsave("../figure/hinge_squared.png", p_2, height = 6, width = 12)
