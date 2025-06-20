# ------------------------------------------------------------------------------
# FIG: EPSILON-INSENSITIVE LOSS
# ------------------------------------------------------------------------------

library(ggplot2)
set.seed(123L)

# DATA -------------------------------------------------------------------------

x <- runif(50L, 0L, 2L)
y <- x + rnorm(length(x), sd = 0.5)
df_1 <- data.frame(x = x, y = y, diff = abs(y - x))

epsilon <- 0.8

df_2 <- data.frame(x = seq(-1.5, 1.5, length.out = 800L))
df_2$y <- ifelse(
  df_2$x < -epsilon,
  abs(df_2$x) - epsilon,
  ifelse(
    df_2$x < epsilon,
    0L,
    df_2$x - epsilon))


df_3 = data.frame(x = seq(0, 2, by = 0.01))
df_3$ymin = - epsilon + df_3$x
df_3$ymax = epsilon + df_3$x

# PLOTS ------------------------------------------------------------------------

p_1 <- ggplot(df_1, aes(x, y)) + 
  geom_point() + 
  geom_abline(intercept = - epsilon, slope = 1L, colour = "blue", lty = 4) + 
  geom_abline(intercept = epsilon, slope = 1L, colour = "blue", lty = 4) + 
  geom_ribbon(data = df_3, aes(x = x, ymin = ymin, ymax = ymax), fill = "blue", alpha = .2, inherit.aes = FALSE,) + 
  geom_abline(intercept = 0L, slope = 1L) +
  annotate(
    "text",
    x = 0.5,
    y = 2L,
    label = deparse(bquote(epsilon ~ "=" ~ 0.8)),
    parse = TRUE,
    size = 10L) +
  geom_segment(
    x = df_1[which.max(df_1[, "diff"]), "x"],
    xend = df_1[which.max(df_1[, "diff"]), "x"],
    y = df_1[which.max(df_1[, "diff"]), "x"],
    yend = df_1[which.max(df_1[, "diff"]), "y"],
    color = "blue") +
  geom_point(
    mapping = aes(
      x = df_1[which.max(df_1[, "diff"]), "x"], 
      y = df_1[which.max(df_1[, "diff"]), "y"]),
    size = 2L,
    color = "blue") +
  annotate(
    "text",
    x = df_1[which.max(df_1[, "diff"]), "x"] + 0.75,
    y = df_1[which.max(df_1[, "diff"]), "y"] + 
      0.5 * (df_1[which.max(df_1[, "diff"]), "diff"]),
    label = deparse(bquote("|" ~ y - f(x) ~ "|" ~ ">" ~ epsilon)),
    size = 7L,
    parse = TRUE,
    color = "blue") +
  theme_minimal() +
  theme(text = element_text(size = 20L))

p_2 <- qplot(df_2$x, df_2$y, geom = "line") +
  geom_segment(
    aes(
      x = -df_1[which.max(df_1[, "diff"]), "diff"],
      xend = -df_1[which.max(df_1[, "diff"]), "diff"],
      y = 0L,
      yend = abs(df_1[which.max(df_1[, "diff"]), "diff"]) - epsilon),
    color = "blue") +
  geom_point(
    aes(
      x = -df_1[which.max(df_1[, "diff"]), "diff"],
      y = abs(df_1[which.max(df_1[, "diff"]), "diff"]) - epsilon),
    size = 3L,
    color = "blue") +
  geom_point(
    mapping = aes(
      x = c(-0.6, -0.4, 0.3), y = rep(0L, 3L)),
    size = 3L,
    color = "blue",
    shape = 1L) +
  xlab(bquote(y - f(x))) +
  ylab(bquote(L(y, f(x)))) +
  theme_minimal() +
  theme(text = element_text(size = 20L))

p <- cowplot::plot_grid(p_1, p_2, ncol = 2L, align = "h")

ggsave(
  "../figure/loss_eps_insensitive.png", 
  p, 
  width = 10L, 
  height = 4L)
