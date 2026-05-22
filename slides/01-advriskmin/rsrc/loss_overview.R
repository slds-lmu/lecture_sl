# ------------------------------------------------------------------------------
# FIG: LOSSES OVERVIEW
# ------------------------------------------------------------------------------

library(tidyr)
library(ggplot2)
source("helpers/loss_functions.R")

# DATA -------------------------------------------------------------------------

x <- seq(-2L, 2L, length.out = 800L)

huber <- Huber(x, c = 2L)
log_bar <- log_barrier(x, a = 1L)
cau <- cauchy(x, c = 1L)
lcosh <- logcosh(x)

epsilon <- 0.8
eps_ins <- ifelse(
  x < -epsilon,
  abs(x) - epsilon,
  ifelse(
    x < epsilon,
    0L,
    x - epsilon))

alpha <- 0.7
quant <- ifelse(
  x < 0L,
  (1 - alpha) * abs(x),
  alpha * x)

df <- gather(
  data.frame(x, huber, log_bar, cau, eps_ins, quant, lcosh),
  "loss",
  "value",
  -x)

# PLOTS ------------------------------------------------------------------------

p <- ggplot(df, aes(x = x, y = value, color = loss)) + 
  geom_line(size = 1.1) +
  scale_color_viridis_d(
    end = 0.9,
    name = "",
    labels = c(
      "Cauchy",
      "Epsilon-ins (eps = 0.8)",
      "Huber (c = 1)",
      "Log-cosine",
      "Log-barrier (eps = 1)",
      "Quant (alpha = 0.7)")) +
  guides(color = guide_legend(ncol = 2)) +
  ylim(c(0L, 2L)) +
  xlab(bquote(y - f(x))) +
  ylab(bquote(L(y, f(x)))) +
  theme_bw() +
  theme(
    text = element_text(size = 20L),
    legend.position = "bottom")

ggsave("../figure/loss_overview.png", p, width = 6L, height = 4L)
