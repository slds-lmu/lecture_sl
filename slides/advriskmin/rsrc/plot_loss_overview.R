# ------------------------------------------------------------------------------
# FIG: LOSSES OVERVIEW
# This plot sources loss functions from helpers and creates an overview plot of various loss functions.
# ------------------------------------------------------------------------------

library(ggplot2)

# DATA -------------------------------------------------------------------------
source("helpers/loss_functions.R")

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

df <- tidyr::gather(
  data.frame(x, huber, log_bar, cau, eps_ins, quant, lcosh),
  "loss",
  "value",
  -x)

# PLOTS ------------------------------------------------------------------------
# remove inf values from df
df <- df[!is.infinite(df$value), ]

# remove log_bar from df
df <- df[df$loss != "log_bar", ]

p <- ggplot2::ggplot(df, aes(x = x, y = value, color = loss, linetype = loss)) + 
  ggplot2::geom_line(size = 1.5) +
  ggplot2::scale_color_viridis_d(
    end = 0.9,
    name = "",
    breaks = c("huber", "cau", "eps_ins", "quant", "lcosh"),
    labels = c(
      "Huber (c = 1)",
      "Cauchy",
      "Epsilon-ins (eps = 0.8)",
      "Quantile (alpha = 0.7)",
      "Log-cosh"
    )
  ) +
  ggplot2::scale_linetype_manual(
    name = "",
    breaks = c("huber", "cau", "eps_ins", "quant", "lcosh"),
    labels = c(
      "Huber (c = 1)",
      "Cauchy",
      "Epsilon-ins (eps = 0.8)",
      "Quantile (alpha = 0.7)",
      "Log-cosh"
    ),
    values = c(
      "huber" = "dotted",
      "cau" = "solid",
      "eps_ins" = "twodash",
      "quant" = "solid",
      "lcosh" = "dotdash"
    )
  ) +
  ggplot2::guides(
    color = guide_legend(
      ncol = 2,
      override.aes = list(
        linetype = c("dotted", "solid", "twodash", "solid", "dotdash")
      )
    ),
    linetype = "none"
  ) +
  ggplot2::ylim(c(0L, 2L)) +
  ggplot2::xlab(bquote(y - f(x))) +
  ggplot2::ylab(bquote(L(y, f(x)))) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    text = element_text(size = 16L),
    legend.position = "bottom",
    legend.text = element_text(size = 15L)
  )

print(p)

ggplot2::ggsave("../figure/plot_loss_overview.png", p, width = 6L, height = 4L)
