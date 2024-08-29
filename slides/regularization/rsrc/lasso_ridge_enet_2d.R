# ------------------------------------------------------------------------------
# enetlogreg

# FIG: penalty shape in 2D of lasso, ridge and elastic net.
# ------------------------------------------------------------------------------

library(ggplot2)
library(data.table)

# DATA -------------------------------------------------------------------------

x <- seq(-1L, 1L, length.out = 1000L)
dt <- as.data.table(expand.grid(x = x, y = x))

dt[, `:=` (
  lasso = 1L - abs(x) - abs(y),
  ridge = 1L - x^2 - y^2,
  enet = 1L - (0.5 * abs(x) + 0.5 * x^2) - (0.5 * abs(y) + 0.5 * y^2))]

dt_long <- melt(
  dt, 
  id = c("x", "y"),
  measure = c("lasso", "ridge", "enet"))

# PLOT -------------------------------------------------------------------------

p <- ggplot(dt_long, aes(x = x, y = y, z = value, col = variable)) +
  geom_contour(bins = 2L) +
  theme_minimal() +
  scale_color_viridis_d(end = 0.85, name = "penalty", 
                        labels = c("L1 (Lasso)", "L2 (Ridge)", "Elastic Net")) +
  xlab(expr(theta[1])) +
  ylab(expr(theta[2])) +
  theme(text = element_text(size = 20L)) +
  coord_fixed(ratio = 1L)

ggsave("../figure/lasso_ridge_enet_2d.png", p, width = 6, height=4)
