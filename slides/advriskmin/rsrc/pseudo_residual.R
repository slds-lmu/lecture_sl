# ------------------------------------------------------------------------------
# pseudo residuals
# FIG: pseudo residual plots for the linear regression example
# ------------------------------------------------------------------------------

library(ggplot2)
theme_set(theme_minimal())

set.seed(1)

# ------------------------------------------------------------------------------

df <- data.frame(x = runif(10, -2.5, 2.5), type = FALSE)
df$y <- 2 * df$x + rnorm(nrow(df))
df <- rbind(df, data.frame(x = 0, type = TRUE, y = 2.5))

p <- ggplot() + 
  geom_point(data = df[!df$type, ], aes(x = x, y = y), alpha = 0.2, size = 3) +
  geom_abline(intercept = 0, slope = 2, colour = "blue", size = 1.1) +
  geom_point(data = df[df$type, ], aes(x = x, y = y), size = 3)

# p_1: Original pseudo residual arrow plot.
p_1 <- p + 
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 2.5), colour = "orange", 
               arrow = arrow(length = unit(0.03, "npc"))) +
  annotate("text", x = -0.25, y = 1, label = "r", colour = "orange", parse = TRUE)
ggsave("../figure/pseudo_residual_1.png", p_1, width = 3, height = 2)

# p_2: Point-wise update plot.
p_2 <- p + 
  geom_segment(aes(x = 0L, xend = 0L, y = 0L, yend = 2.5), size = 1.1, col = "blue")
ggsave("../figure/pseudo_residual_2.png", p_2, width = 3, height = 2)

# p_3: Smoothing spline additive update plot

# Compute residuals for each data point: r = y - 2*x.
df$res <- df$y - 2 * df$x

# Fit smoothing spline to the residuals.
spline_fit <- smooth.spline(x = df$x, y = df$res)

# Create a grid for x to plot the updated prediction function.
grid <- data.frame(x = seq(min(df$x), max(df$x), length.out = 100))
grid$pred_res <- predict(spline_fit, grid$x)$y
grid$f_new <- 2 * grid$x + 0.4 * grid$pred_res  # f_new(x) = 2x + 0.1 * s(x)

p_3 <- p + 
  geom_abline(aes(slope = 2, intercept = 0, linetype = "linear"), colour = "blue", size = 1.1) +
  geom_line(data = grid, aes(x = x, y = f_new, linetype = "linear + 0.1 * spline"), colour = "red", size = 1.1) +
  scale_linetype_manual(values = c("linear" = "solid", "linear + 0.1 * spline" = "solid")) +
  labs(linetype = "") +
  theme(
    legend.position = c(0.8, 0.15), 
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 7) 
  )
ggsave("../figure/pseudo_residual_3.png", p_3, width = 3, height = 2)
