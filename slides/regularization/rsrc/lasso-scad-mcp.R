library(ggplot2)

# Set the regularization parameters for demonstration
lambda <- 1
a <- 3.7  # For SCAD, typically > 2
gamma <- 3  # For MCP

# Lasso Penalty Function
lasso_penalty <- function(theta) {
  lambda * abs(theta)
}

# SCAD Penalty Function
scad_penalty <- function(theta) {
  ifelse(abs(theta) <= lambda, 
         lambda * abs(theta), 
         ifelse(abs(theta) <= a * lambda, 
                (-theta^2 + 2 * a * lambda * abs(theta) - lambda^2) / (2 * (a - 1)), 
                (a + 1) * lambda^2 / 2))
}

# MCP Penalty Function
mcp_penalty <- function(theta) {
  ifelse(abs(theta) <= gamma * lambda, 
         lambda * abs(theta) - theta^2 / (2 * gamma), 
         0.5 * gamma * lambda^2)
}

# Create a sequence of theta values
theta_vals <- seq(-4, 4, by = 0.1)

# Create a data frame for plotting
penalties <- data.frame(
  theta = theta_vals,
  Lasso = sapply(theta_vals, lasso_penalty),
  SCAD = sapply(theta_vals, scad_penalty),
  MCP = sapply(theta_vals, mcp_penalty)
)

# Plot using ggplot2
ggplot(penalties, aes(x = theta)) + 
  geom_line(aes(y = Lasso, color = "Lasso"), linewidth=1.2) +
  geom_line(aes(y = SCAD, color = "SCAD"), linewidth=1.2) +
  geom_line(aes(y = MCP, color = "MCP"), linewidth=1.2) +
  labs(title = "Lasso, SCAD, and MCP",
       x = expression(theta),
       y = "Penalty") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_blank(),
    legend.text = element_text(size=13)
  ) +
  scale_color_manual(values = c("Lasso" = "blue", "SCAD" = "red", "MCP" = "green"))