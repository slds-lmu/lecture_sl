library(ggplot2)
library(reshape2)

source("helpers/loss_functions.R")

df = data.frame(res = seq(-10, 10, length.out = 1000))

losses = list(
  "C2" = function(res) cauchy(res, c = 2),
  "C1" = function(res) cauchy(res, c = 1),
  "C05" = function(res) cauchy(res, c = 0.5)
)

p = plotLoss(df, losses)
p = p + scale_color_discrete(name = expression(c), labels = c(2, 1, 0.5))

ggsave(filename = "loss_cauchy_plot1.png", path = "figure_man", width = 4, height = 3)