# ------------------------------------------------------------------------------
# classification further losses

# FIG: overview losses (hinge, squared scores, 0-1, squared hinge, exponential)
# ------------------------------------------------------------------------------

library(ggplot2)
library(tidyr)

# DATA -------------------------------------------------------------------------

x <- seq(-4, 4, length.out = 800)

hinge <- ifelse(x < 1, 1 - x, 0)
hinge_sq <- ifelse(x < 1, (1 - x)^2, 0)
sq_scores <- (1 - x)^2
exp <- exp(-x)
zero_one <- as.numeric(x < 0)

df <- gather(
  data.frame(x, hinge, hinge_sq, sq_scores, exp, zero_one),
  "loss",
  "value",
  -x)

df$aux <- as.factor(ifelse(df$loss == "sq_scores", 1, 0))

# PLOTS ------------------------------------------------------------------------

p <- ggplot(df, aes(x = x, y = value, color = loss, linetype = aux)) + 
  geom_line(size = 1.1) +
  scale_color_viridis_d(
    end = 0.9,
    name = "",
    labels = c(
      "Exponential",
      "Hinge",
      "Squared hinge",
      "Squared (scores)",
      "0-1")) +
  guides(color = guide_legend(ncol = 2), linetype = FALSE) +
  ylim(c(0, 4)) +
  scale_x_continuous(breaks = seq(-4, 4)) +
  xlab(expression(yf)) +
  ylab(expression(L(y, f))) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.position = "bottom")

ggsave("../figure/overview_classif.png", p, width = 6, height = 4)
