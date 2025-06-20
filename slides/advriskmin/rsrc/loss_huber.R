# ------------------------------------------------------------------------------
# FIG: HUBER LOSS
# ------------------------------------------------------------------------------

library(ggplot2)
source("helpers/loss_functions.R")
set.seed(123L)

# DATA -------------------------------------------------------------------------

x <- runif(50L, 0L, 2L)
y <- x + rnorm(length(x), sd = 0.5)
df_1 <- data.frame(x = x, y = y, diff = abs(y - x))

df_2 <- data.frame(res = seq(-10L, 10L, length.out = 800L))

losses <- list(
  "C2" = function(res) Huber(res, c = 2L),
  "C1" = function(res) Huber(res, c = 1L),
  "C05" = function(res) Huber(res, c = 0.5))

# PLOTS ------------------------------------------------------------------------

p_1 <- ggplot(df_1, aes(x, y)) + 
  geom_point() + 
  geom_abline(intercept = 0L, slope = 1L) + 
  annotate(
    "text",
    x = 0L,
    y = 2.5,
    label = deparse(bquote(epsilon ~ "=" ~ 0.8)),
    size = 10L,
    parse = TRUE) +
  geom_segment(
    x = df_1[which.max(df_1[, "diff"]), "x"],
    xend = df_1[which.max(df_1[, "diff"]), "x"],
    y = df_1[which.max(df_1[, "diff"]), "x"],
    yend = df_1[which.max(df_1[, "diff"]), "y"],
    color = "blue",
    size = 1.1) + 
  geom_point(
    mapping = aes(
      x = df_1[which.max(df_1[, "diff"]), "x"], 
      y = df_1[which.max(df_1[, "diff"]), "y"]),
    size = 2L,
    color = "blue") +
  annotate(
    "text",
    x = df_1[which.max(df_1[, "diff"]), "x"] + 1.2,
    y = df_1[which.max(df_1[, "diff"]), "y"] + 
      0.5 * (df_1[which.max(df_1[, "diff"]), "diff"]),
    label = deparse(bquote("|" ~ y - f(x) ~ "|" ~ ">" ~ epsilon)),
    size = 7L,
    parse = TRUE,
    color = "blue") +
  annotate(
    "rect", 
    xmin = df_1[21, "x"], 
    xmax = df_1[21, "x"] + df_1[21, "diff"], 
    ymin = df_1[21, "y"], 
    ymax = df_1[21, "y"] + df_1[21, "diff"],
    fill = "blue",
    alpha = 0.4) +
  geom_point(
    mapping = aes(
      x = df_1[21, "x"], 
      y = df_1[21, "y"]),
    size = 2L,
    color = "blue") +
  annotate(
    "text",
    x = df_1[21, "x"] + 0.4, 
    y = df_1[21, "y"] - 0.5,
    label = deparse(bquote("|" ~ y - f(x) ~ "|" ~ "<" ~ epsilon)),
    size = 7L,
    parse = TRUE,
    color = "blue") +
  xlim(c(-1L, 3L)) +
  ylim(c(-1L, 3L)) +
  theme_minimal() +
  theme(text = element_text(size = 20L))

p_2 <- plotLoss(df_2, losses) + scale_color_viridis_d(
  end = 0.9,
  name = expression(epsilon), 
  labels = c(2, 1, 0.5)) +
  theme_minimal() +
  theme(text = element_text(size = 20L))

p <- cowplot::plot_grid(
  p_1, 
  NULL,
  p_2, 
  ncol = 3L, 
  align = "h",
  rel_widths = c(1.1, 0.4, 1.5))

ggsave("../figure/loss_huber.png", p, width = 11L, height = 4L)
