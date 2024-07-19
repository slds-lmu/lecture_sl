# Plot of the penalty functions for lasso, SCAD and MCP
# First manually define penalty functions, then plot using ggplot2

library(ggplot2)

# Set the regularization parameters for demonstration
lambda <- 1
a <- 3.7  # For SCAD, typically > 2
gamma <- 3

# Define penalty functions
lasso_penalty <- function(theta) {
  lambda * abs(theta)
}

scad_penalty <- function(theta) {
  ifelse(abs(theta) <= lambda, 
         lambda * abs(theta), 
         ifelse(abs(theta) <= a * lambda, 
                (-theta^2 + 2 * a * lambda * abs(theta) - lambda^2) / (2 * (a - 1)), 
                (a + 1) * lambda^2 / 2))
}

mcp_penalty <- function(theta) {
  ifelse(abs(theta) <= gamma * lambda, 
         lambda * abs(theta) - theta^2 / (2 * gamma), 
         0.5 * gamma * lambda^2)
}

# Create sequence of theta values
theta_vals <- seq(-4, 4, by = 0.1)

# Create df for plotting
penalties <- data.frame(
  theta = theta_vals,
  Lasso = sapply(theta_vals, lasso_penalty),
  SCAD = sapply(theta_vals, scad_penalty),
  MCP = sapply(theta_vals, mcp_penalty)
)

# Plot using ggplot2
p <- ggplot(penalties, aes(x = theta)) + 
  geom_line(aes(y = Lasso, color = "Lasso"), linewidth=1.2) +
  geom_line(aes(y = SCAD, color = "SCAD"), linewidth=1.2) +
  geom_line(aes(y = MCP, color = "MCP"), linewidth=1.2) +
  labs(title = "Lasso, SCAD, and MCP",
       x = expression(theta),
       y = "Penalty") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_blank(),
    legend.text = element_text(size=18)
  ) +
  scale_color_manual(values = c("Lasso" = "blue", "SCAD" = "red", "MCP" = "green"))

# Save figure
ggsave(filename = paste0("../figure/nc_penalties_comparison.png"), plot = p)