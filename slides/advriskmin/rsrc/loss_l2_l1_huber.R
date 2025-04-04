library(ggplot2)

source("helpers/loss_functions.R")

df = data.frame(res = seq(-10L, 10L, length.out = 800L))

losses <- list(
  "L1" = function(res) L1(res),
  "L2" = function(res) L2(res),
  "Huber" = function(res) Huber(res, c = 5)
)

pl = plotLoss(df, losses) + 
  scale_color_viridis_d() +
  theme_minimal() +
  theme(text = element_text(size = 20L))


ggplot2::ggsave("../figure/loss_l2_l1_huber.png", pl)

