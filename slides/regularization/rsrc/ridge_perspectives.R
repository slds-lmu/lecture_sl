# ------------------------------------------------------------------------------
# l2, nonlin

# FIG: schematic diagrams of ridge regularization
#  (1) increase objective function until the constraints are met 
#  (2) optimize the objective function till optimum under constraints
#  (3) different strength of ridge constraint
#  (4) single schematic diagram
# ------------------------------------------------------------------------------

library(ggplot2)
library(grid)
library(dplyr)
library(gridExtra)
library(pracma)

# DATA -------------------------------------------------------------------------

# Define the grid for plotting
x <- seq(-3.0, 3.0, length.out = 400)
y <- seq(-3.0, 3.0, length.out = 400)
X <- outer(rep(1, length(x)), y)
Y <- outer(x, rep(1, length(y)))

# Define elliptical parameters
a <- 1.5
b <- 0.75
rotation_angle <- -30
constraint_radius <- 1.0
objective_center <- c(1.5, 1.5)

# Rotated elliptical objective function
rotated_elliptical_objective <- function(X, Y, center, a, b, angle_deg) {
  angle_rad <- deg2rad(angle_deg)
  X_rot <- cos(angle_rad) * (X - center[1]) - sin(angle_rad) * (Y - center[2])
  Y_rot <- sin(angle_rad) * (X - center[1]) + cos(angle_rad) * (Y - center[2])
  (X_rot^2 / a^2) + (Y_rot^2 / b^2)
}

Z_rotated_elliptical <- rotated_elliptical_objective(X, Y, objective_center, a, b, rotation_angle)

# Create data frame for ggplot
data <- data.frame(
  x = as.vector(X),
  y = as.vector(Y),
  z = as.vector(Z_rotated_elliptical)
)

# Function to create plots
create_plot <- function(data, levels, objective_center, constraint_radius, type, last_plot = FALSE){
  #type: outside / inside
  p <- ggplot()
  if(length(levels)!=0){
    p <- p +
      geom_contour(data = data, aes(x = x, y = y, z = z), color = 'red', breaks = levels)
    if (last_plot){
      plot_build <- ggplot_build(p)
      plot_data <- plot_build$data[[1]]
      level_value <- ifelse(type=="outside", max(levels), min(levels))
      filtered_data <- plot_data[plot_data$level == level_value, c("x","y")]
      distances <- sqrt(filtered_data$x^2 + filtered_data$y^2)
      min_idx <- which.min(abs(distances - constraint_radius))
      intersection_point <- filtered_data[min_idx,]
      p <- p + 
        geom_point(aes(x = intersection_point[[1]], y = intersection_point[[2]]), color = 'green', size = 2) +
        annotate("label", x = intersection_point[[1]] - 0.5, y = intersection_point[[2]] + 0.6, label = expression(hat(theta)[ridge]), color = "green", size = 3)
    }
  }
  
  # Create data for the circle
  theta <- seq(0, 2 * pi, length.out = 100)
  center <- c(0, 0)
  circle_data <- data.frame(
    x = center[1] + constraint_radius * cos(theta),
    y = center[2] + constraint_radius * sin(theta)
  )
  
  # Plot the circle with dashed lines and blue color
  p <- p +
    geom_path(data = circle_data, aes(x = x, y = y), color = 'blue', linetype = 'dashed', size = 0.5, alpha = 0.3) +
    geom_polygon(data = circle_data, aes(x = x, y = y), fill = 'blue', alpha = 0.3) # Fill the circle with blue color and alpha 0.3
  
  p <- p +
    geom_point(aes(x = objective_center[1], y = objective_center[2]), color = "black", size = 2) +
    annotate("label", x = objective_center[1], y = objective_center[2]+0.8, label = expression(hat(theta)), color = "black", size = 3) + 
    geom_hline(yintercept = 0, color = 'black', size = 0.5) +
    geom_vline(xintercept = 0, color = 'black', size = 0.5) +
    theme_linedraw() +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      plot.title = element_blank()
    ) + 
    coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3), expand = FALSE)
  return(p)
}

# PLOT 1: outside --------------------------------------------------------------
# increase objective function until the constraints are met

contour_levels <- list(
  c(),
  c(0.1),
  c(0.1, 0.3),
  c(0.1, 0.3, 0.6)
)

plots_out <- lapply(1:4, function(i) {
  create_plot(data, contour_levels[[i]], objective_center, constraint_radius, type="outside", last_plot = (i == 4))
})

p_outside <- grid.arrange(grobs = plots_out, nrow = 2, ncol = 2)

# PLOT 2: inside ---------------------------------------------------------------
# optimize the objective function till optimum under constraints

contour_levels <- list(
  c(),
  c(2.4),
  c(1.2, 2.4),
  c(0.6, 1.2, 2.4)
)

# Generate plots
plots_in <- lapply(1:4, function(i) {
  create_plot(data, contour_levels[[i]], objective_center, constraint_radius, type="inside", last_plot = (i == 4))
})

# Arrange plots in a 2x2 grid
p_inside <- grid.arrange(grobs = plots_in, nrow = 2, ncol = 2)

# PLOT 3: constraints ----------------------------------------------------------
# different strength of ridge constraint

contour_levels <- c(0.1, 0.3, 0.6)

# p1
p1 <- ggplot() + 
  geom_contour(data = data, aes(x = x, y = y, z = z), color = 'red', breaks = contour_levels)

plot_build_1 <- ggplot_build(p1)
plot_data_1 <- plot_build_1$data[[1]]
filtered_data_1 <- plot_data_1[plot_data_1$level == 0.6, c("x","y")]
distances_1 <- sqrt(filtered_data_1$x^2 + filtered_data_1$y^2)
min_idx_1 <- which.min(abs(distances_1 - constraint_radius))
intersection_point_1 <- filtered_data_1[min_idx_1,]
  
p1 <- p1 + 
    geom_point(aes(x = intersection_point_1[[1]], y = intersection_point_1[[2]]), color = 'green', size = 2) +
    annotate("label", x = intersection_point_1[[1]] - 0.5, y = intersection_point_1[[2]] + 0.6, label = expression(hat(theta)[ridge]), color = "green", size = 3)

theta <- seq(0, 2 * pi, length.out = 100)
center <- c(0, 0)
circle_data_1 <- data.frame(
  x = center[1] + cos(theta),
  y = center[2] + sin(theta)
)
  
p1 <- p1 +
  geom_path(data = circle_data_1, aes(x = x, y = y), color = 'blue', linetype = 'dashed', size = 0.5, alpha = 0.3) +
  geom_polygon(data = circle_data_1, aes(x = x, y = y), fill = 'blue', alpha = 0.3) +
  geom_point(aes(x = objective_center[1], y = objective_center[2]), color = "black", size = 2) +
  annotate("label", x = objective_center[1], y = objective_center[2]+0.8, label = expression(hat(theta)), color = "black", size = 3) + 
  geom_hline(yintercept = 0, color = 'black', size = 0.5) +
  geom_vline(xintercept = 0, color = 'black', size = 0.5) +
  theme_linedraw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.title = element_blank()
  ) + 
  coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3), expand = FALSE)

# p2
constraint_radius <- 1.33
level_value <- 0.3

p2 <- ggplot() + 
  geom_contour(data = data, aes(x = x, y = y, z = z), color = 'red', breaks = contour_levels)

plot_build <- ggplot_build(p2)
plot_data <- plot_build$data[[1]]
filtered_data <- plot_data[plot_data$level == level_value, c("x","y")]
distances <- sqrt(filtered_data$x^2 + filtered_data$y^2)
min_idx <- which.min(abs(distances - constraint_radius))
intersection_point <- filtered_data[min_idx,]

p2 <- p2 + 
  geom_point(aes(x = intersection_point[[1]], y = intersection_point[[2]]), color = 'green', size = 2) +
  annotate("label", x = intersection_point[[1]] - 0.5, y = intersection_point[[2]] + 0.6, label = expression(hat(theta)[ridge]), color = "green", size = 3)

theta <- seq(0, 2 * pi, length.out = 100)
center <- c(0, 0)
circle_data <- data.frame(
  x = center[1] + constraint_radius * cos(theta),
  y = center[2] + constraint_radius * sin(theta)
)


p2 <- p2 +
  geom_path(data = circle_data, aes(x = x, y = y), color = 'blue', linetype = 'dashed', size = 0.5, alpha = 0.3) +
  geom_polygon(data = circle_data, aes(x = x, y = y), fill = 'blue', alpha = 0.3) +
  geom_point(aes(x = objective_center[1], y = objective_center[2]), color = "black", size = 2) +
  annotate("label", x = objective_center[1], y = objective_center[2]+0.8, label = expression(hat(theta)), color = "black", size = 3) + 
  geom_hline(yintercept = 0, color = 'black', size = 0.5) +
  geom_vline(xintercept = 0, color = 'black', size = 0.5) +
  theme_linedraw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.title = element_blank()
  ) + 
  coord_fixed(xlim = c(-3, 3), ylim = c(-3, 3), expand = FALSE)

# Arrange plots in a 1x2 grid
p_cons <- grid.arrange(p1, p2, nrow = 1, ncol = 2)

# PLOT 4: single schematic plot ------------------------------------------------

constraint_radius <- 1
contour_levels <- c(0.1, 0.3, 0.6)
level_value <- 0.6

p <- ggplot() + 
  geom_contour(data = data, aes(x = x, y = y, z = z), color = 'red', breaks = contour_levels)

plot_build <- ggplot_build(p)
plot_data <- plot_build$data[[1]]
filtered_data <- plot_data[plot_data$level == level_value, c("x","y")]
distances <- sqrt(filtered_data$x^2 + filtered_data$y^2)
min_idx <- which.min(abs(distances - constraint_radius))
intersection_point <- filtered_data[min_idx,]

p <- p + 
  geom_point(aes(x = intersection_point[[1]], y = intersection_point[[2]]), color = 'green', size = 2) +
  annotate("label", x = intersection_point[[1]] - 0.4, y = intersection_point[[2]] + 0.4, label = expression(hat(theta)[ridge]), color = "green", size = 3)
theta <- seq(0, 2 * pi, length.out = 100)
center <- c(0, 0)
circle_datas <- data.frame(
  x1 = center[1] + constraint_radius * cos(theta),
  y1 = center[2] + constraint_radius * sin(theta),
  x2 = center[1] + (constraint_radius / 1.5)* cos(theta),
  y2 = center[2] + (constraint_radius / 1.5) * sin(theta),
  x3 = center[1] + (constraint_radius / 3) * cos(theta),
  y3 = center[2] + (constraint_radius / 3) * sin(theta)
)

p <- p +
  geom_polygon(data = circle_datas, aes(x = x1, y = y1), fill = 'blue', alpha = 0.3) +
  geom_polygon(data = circle_datas, aes(x = x2, y = y2), fill = 'blue', alpha = 0.5) +
  geom_polygon(data = circle_datas, aes(x = x3, y = y3), fill = 'blue', alpha = 0.7) +
  geom_point(aes(x = objective_center[1], y = objective_center[2]), color = "black", size = 2) +
  annotate("label", x = objective_center[1], y = objective_center[2]+0.8, label = expression(hat(theta)), color = "black", size = 3) + 
  geom_hline(yintercept = 0, color = 'black', size = 0.5) +
  geom_vline(xintercept = 0, color = 'black', size = 0.5) +
  geom_segment(aes(x = 0, y = -1.5, xend = 0, yend = 3), color = 'black', 
               arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed")) +  # y-axis with arrow
  annotate("text", x = -0.2, y = 2.8, label = expression(theta[2]), color = "black", size = 3) + 
  geom_segment(aes(x = -1.5, y = 0, xend = 3, yend = 0), color = 'black', 
               arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed")) +  # x-axis with arrow
  annotate("text", x = 2.8, y = -0.2, label = expression(theta[1]), color = "black", size = 3) + 
  theme_void() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.title = element_blank()
  ) + 
  coord_fixed(xlim = c(-1.5, 3), ylim = c(-1.5, 3), expand = FALSE)

ggsave(filename = "../ridge_perspectives_01.png", plot = p_outside, width = 6, height = 6)
ggsave(filename = "../ridge_perspectives_02.png", plot = p_inside, width = 6, height = 6)
ggsave(filename = "../ridge_perspectives_03.png", plot = p_cons, width = 6, height = 3)
ggsave(filename = "../ridge_perspectives_04.png", plot = p, width = 3, height = 3)
