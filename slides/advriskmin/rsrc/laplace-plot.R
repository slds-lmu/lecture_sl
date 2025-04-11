library(ggplot2)
library(extraDistr)

# Define parameters for Laplace distribution
mu <- 0      # location
b <- 1       # scale

# Create a sequence of x values
x_vals <- seq(-10, 10, length.out = 1000)

# Compute the density
y_vals <- dlaplace(x_vals, mu, b)

# Create a data frame for plotting
df <- data.frame(x = x_vals, density = y_vals)

# Plot using ggplot2
laplace_plot = ggplot(df, aes(x = x, y = density)) +
  geom_line(color = "black", linewidth = 1) +
  labs(
    #title = "Laplace Density Function",
    x = "x",
    y = "density"
  ) +
  theme(
    axis.title = element_text(size = 20)  # Axis title font size
   # axis.text = element_text(size = 14)    # Tick label font size
  )
  #theme_minimal()

laplace_plot

ggsave("../figure/laplace-plot.png", plot = laplace_plot, width = 6, height = 4, dpi = 300)
