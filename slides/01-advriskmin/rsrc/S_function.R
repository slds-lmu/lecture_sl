# ------------------------------------------------------------------------------
# classification further losses

# FIG: s function
# ------------------------------------------------------------------------------

library(ggplot2)

# DATA -------------------------------------------------------------------------

# Define the function S(θ)
S <- function(theta, a = 3, b = 1) {
  abs(a - theta) + abs(b - theta)
}

theta_vals <- seq(-3, 7, by = 0.01)
S_vals <- S(theta_vals)

plot_data <- data.frame(theta = theta_vals, S_theta = S_vals)

p <- ggplot(plot_data, aes(x = theta, y = S_theta)) +
  geom_line(color = "#5F9EA0", size = 1.2) +
  scale_x_continuous(name = expression(theta), limits = c(-3, 7)) +
  scale_y_continuous(name = NULL, limits = c(-1, 10)) +
  ggtitle(expression(paste("S(", theta, ") == |a-", theta, "| + |b-", theta, "| for (a,b)=(3,1)"))) +
  theme_minimal(base_size = 14)
p
ggsave("../figure/S_function.png", p, height = 3, width = 4)

