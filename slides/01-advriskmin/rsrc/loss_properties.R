# ------------------------------------------------------------------------------
# losses properties

# FIG: DISTANCE-BASED, TRANSLATION-INVARIANT, SYMMETRIC LOSSES
# ------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
set.seed(123L)

# DATA -------------------------------------------------------------------------

x <- sort(runif(50L, 0L, 2L))
y <- x + rnorm(length(x), sd = 0.5)
df_1 <- data.frame(x = x, y = y, diff = y - x)

df_2 <- data.frame(x = seq(-1.5, 1.5, length.out = 800L))
df_2$y <- abs(df_2$x)
df_2$z <- (df_2$x)^2

df_3 <- data.frame(x = seq(-1L, 1L, length.out = 800L))
df_3$y <- (df_3$x)^2

# PLOT 1: DISTANCE-BASED -------------------------------------------------------

add_markers <- function(plot, index, negative = TRUE) {

  plot +
    geom_segment(
      mapping = aes(
        x = df_1[index, "x"],
        xend = df_1[index, "x"],
        y = df_1[index, "x"],
        yend = df_1[index, "y"]),
      color = "blue",
      inherit.aes = FALSE) +
      geom_point(
        mapping = aes(
          x = df_1[index, "x"],
          y = df_1[index, "y"]),
        size = 2L,
        color = "blue")

}

add_markers_2 <- function(plot, index, negative = TRUE) {

  plot +
    geom_segment(
      aes(
        x = df_1[index, "diff"],
        xend = df_1[index, "diff"],
        y = 0L,
        yend = abs(df_1[index, "diff"])),
      color = "blue") +
    geom_point(
      mapping = aes(
        x = df_1[index, "diff"],
        y = abs(df_1[index, "diff"])),
      size = 2L,
      color = "blue")

}

p_1 <- ggplot(df_1, aes(x, y)) +
  theme_minimal() +
  geom_point() +
  geom_abline(intercept = 0L, slope = 1L) +
  theme(axis.title = element_text(size = 15L))
p_1 <- p_1 %>%
  add_markers(4L) %>%
  add_markers(45L)

p_2 <- qplot(df_2$x, df_2$y, geom = "line") +
  theme_minimal() +
  xlab("r = y - f(x)") +
  ylab(bquote(paste(psi(r), "= |r|"))) +
  theme(axis.title = element_text(size = 15L))
p_2 <- p_2 %>%
  add_markers_2(4L) %>%
  add_markers_2(45L)

p_3 <- cowplot::plot_grid(
  p_1,
  p_2,
  ncol = 2L,
  align = "h",
  rel_widths = c(0.4, 0.6))

ggsave(
  "../figure/loss_properties_dist_based.png",
  p_2,
  width = 3.5,
  height = 2.6)

# PLOT 2: TRANSLATION-INVARIANT ------------------------------------------------

p_4 <- qplot(df_2$x, df_2$z, geom = "line") +
  theme_minimal() +
  xlab("(y + a) - (f(x) + a))") +
  ylab(bquote(((y + a) - (f(x) + a))**2)) +
  theme(axis.title = element_text(size = 15L))
p_4

ggsave(
  "../figure/loss_properties_transl_inv.png",
  p_4,
  width = 3.5,
  height = 3.2)

# PLOT 3: SYMMETRIC ------------------------------------------------------------

p_5 <- qplot(df_3$x, df_3$y, geom = "line") +
  theme_minimal() +
  xlab(bquote(pi(x) - y)) +
  ylab(bquote((pi(x) - y)**2)) +
  theme(axis.title = element_text(size = 15L))

p_6 <- qplot(df_3$x, df_3$y, geom = "line") +
  theme_minimal() +
  xlab(bquote(y - pi(x))) +
  ylab(bquote((y - pi(x))**2)) +
  theme(axis.title = element_text(size = 15L))

p_7 <- cowplot::plot_grid(p_5, p_6, ncol = 2L, align = "h")
p_7

ggsave(
  "../figure/loss_properties_symmetric.png",
  p_7,
  width = 5L,
  height = 3L)


# PLOT 4: L1 vs L2

# df_2_plus <- rbind(
#   data.frame(x = df_2$x, L = df_2$y, Loss = "L1"),
#   data.frame(x = df_2$x, L = df_2$z, Loss = "L2")
# )

# p_8 <- ggplot(df_2_plus, aes(x = x, y = L, color = Loss)) +
#   theme_minimal() + geom_line() +
#   xlab("r = y - f(x)") +
#   ylab("L") +
#   theme(axis.title = element_text(size = 15L),
#     legend.title = element_text(size=15), #change legend title font size
#     legend.text = element_text(size=13))
# p_8

# ggsave(
#   "../figure/loss_properties_l1_l2.png",
#   p_8,
#   width = 5L,
#   height = 3L)
