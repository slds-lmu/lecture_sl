library(ggplot2)
library(ggpubr)

# Define the range for x
x_range <- seq(-6, 6, by = 0.1)

# Set the line width
line_width <- 1.5

# Define theme adjustments for increased sizes
theme_enlarged <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.1, size = 13),
      axis.title = element_text(size = 16),               
      axis.text = element_text(size = 16),
      legend.text = element_text(size=16)
    )
}

# Create plot sequence (L1 => sign => tanh => log-cosh) with updated line width
p1 <- ggplot(data.frame(x = x_range), aes(x)) +
  geom_line(aes(y = abs(x)), size = line_width, color = "skyblue") +
  labs(subtitle = "Step 0) discont. L1 loss", title = expression(abs(y-f(x))), x = "y-f(x)", y = expression(abs(y-f(x)))) +
  theme_enlarged()

p2 <- ggplot(data.frame(x = x_range), aes(x)) +
  geom_line(aes(y = log(cosh(x))), size = line_width, color = "skyblue") +
  labs(subtitle = "Step 3) integrate tanh", title = expression(log(cosh(abs(y-f(x))))), x = "y-f(x)", y = expression(log(cosh(abs(y-f(x)))))) +
  theme_enlarged()

x_sgn <- c(-40, -1e-6, 1e-6, 40)
y_sgn <- sign(x_sgn)
p3 <- ggplot(data.frame(x = x_sgn, y = y_sgn), aes(x, y)) +
  geom_step(size = line_width, color = "orange") +
  labs(subtitle = "Step 1) take derivative of L1", title = expression(sgn(y-f(x))), x = "y-f(x)", y = expression(sgn(y-f(x)))) +
  theme_enlarged()

x_tanh <- seq(-40, 40, by = 0.1)
p4 <- ggplot(data.frame(x = x_tanh), aes(x)) +
  geom_line(aes(y = tanh(x)), size = line_width, color="orange") +
  labs(subtitle = "Step 2) cont. approximation of sign", title = expression(tanh(y-f(x))), x = "y-f(x)", y = expression(tanh(y-f(x)))) +
  theme_enlarged()

# Combine the plots
p <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
p
ggsave("../figure/logcosh-derivation.png", p, width = 7L, height = 5L)
##### plot gaussian and cosh densities together

# Define the range for x
x_range <- seq(-6, 6, length.out = 1000)

# Define the standard Gaussian density function
gaussian_density <- dnorm(x_range)

# Define the cosh density function
cosh_density <- 1 / (pi * cosh(x_range))

# Create a data frame for plotting
data_to_plot <- data.frame(x = x_range, Gaussian = gaussian_density, Cosh = cosh_density)

# Create the ggplot
p1 <- ggplot(data_to_plot, aes(x)) +
  geom_line(aes(y = Gaussian, color = "N(0,1)"), size=line_width) +
  geom_line(aes(y = Cosh, color = "Cosh"), size=line_width) +
  labs(x = "x", y = "Density") +
  scale_color_manual(values = c("N(0,1)" = "skyblue", "Cosh" = "orange")) +
  theme_enlarged() +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size=16))

p1
ggsave("../figure/cosh-gaussian-densities.png", p1, width = 6L, height = 3L)
