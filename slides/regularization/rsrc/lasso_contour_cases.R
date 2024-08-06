# ------------------------------------------------------------------------------
# l2

# FIG: lasso contour plots under different parameter values.
#  (1) smaller parameter with theta_1 removed
#  (2) small lambda that does not lead to sparsity
#  (3) large lambda that leads to sparsity
# ------------------------------------------------------------------------------

library(ggplot2)
library(gridExtra)

# ------------------------------------------------------------------------------

# Function to create contour plots
create_contour_plot <- function(theta_hat, theta_lasso, l1_edge, outermost_point, annotation_positions, subtitle) {
  theta1 <- seq(-4, 4, length.out = 300)
  theta2 <- seq(-2, 5, length.out = 300)
  grid <- expand.grid(Theta1 = theta1, Theta2 = theta2)
  
  target_direction <- c(1, 4) / sqrt(sum(c(1, 4)^2))
  angle <- atan2(target_direction[2], target_direction[1]) - pi / 18
  rot_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), nrow = 2, byrow = TRUE)
  
  scale <- c(1, 2)
  Z <- as.matrix(grid) - matrix(theta_hat, nrow = nrow(grid), ncol = 2, byrow = TRUE)
  Z <- Z %*% rot_matrix
  Z <- Z %*% diag(scale)
  Z <- Z %*% t(rot_matrix)
  L <- (Z[, 1])^2 + (Z[, 2])^2
  grid$L <- L
  
  outermost_level <- sum((outermost_point - theta_hat)^2)
  
  # Create the ggplot object
  p <- ggplot() +
    geom_contour(data = grid, aes(x = Theta1, y = Theta2, z = L),colour = "red", breaks = seq(min(L), outermost_level, length.out = 5)) +
    geom_polygon(data = data.frame(x = c(l1_edge, 0, -l1_edge, 0), y = c(0, l1_edge, 0, -l1_edge)), aes(x, y), fill = "cyan", alpha = 0.3) +
    labs(x = expression(theta[1]), y = expression(theta[2]), title = subtitle) +
    theme_bw() +
    coord_fixed()
  
  p <- p + 
    geom_point(data=as.data.frame(theta_hat), aes(x=theta_hat[1], y=theta_hat[2]), colour="black") + 
    annotate("label", x=annotation_positions[2, 1], y=annotation_positions[2, 2], label="hat(theta)", parse=TRUE, size=5) + 
    geom_segment(data=cbind(start=as.data.frame(matrix(annotation_positions[2,], nrow = 1, byrow = TRUE)), end=as.data.frame(matrix(theta_hat, nrow = 1, byrow = TRUE))),
                 aes(x=start.V1, y=start.V2, 
                     xend=end.V1, yend=end.V2), colour="black",
                 size=0.9, arrow = arrow(ends="last", type="closed", length=unit(0.04, "npc")),
                 arrow.fill="black")
  
  p <- p + 
    geom_point(data=as.data.frame(theta_lasso), aes(x=theta_lasso[1], y = theta_lasso[2]), colour="black") + 
    annotate("label", x=annotation_positions[1, 1], y=annotation_positions[1, 2], label="hat(theta)[\"Lasso\"]", parse=TRUE, size=5) + 
    geom_segment(data=cbind(start=as.data.frame(matrix(annotation_positions[1,], nrow = 1, byrow = TRUE)), end=as.data.frame(matrix(theta_lasso, nrow = 1, byrow = TRUE))),
                 aes(x=start.V1, y=start.V2, 
                     xend=end.V1, yend=end.V2), colour="black",
                 size=0.9, arrow = arrow(ends="last", type="closed", length=unit(0.04, "npc")),
                 arrow.fill="black") + 
    xlim(-3, 3) +
    ylim(-2, 5)
  return(p)
}

# Create individual plots
plot1 <- create_contour_plot(theta_hat = c(0.5, 3), theta_lasso = c(0, 1), l1_edge = 1, 
                             outermost_point = c(0, 1), annotation_positions = matrix(c(-2, 1.1, 2.5, 2), nrow = 2, byrow = TRUE), 
                             subtitle = expression(paste("smaller param. ", theta[1], " is removed")))

plot2 <- create_contour_plot(theta_hat = c(1, 1), theta_lasso = c(0.5, 0.5), l1_edge = 1, 
                             outermost_point = c(0.5, 0.5), annotation_positions = matrix(c(-0.5, 2.5, 2, 3), nrow = 2, byrow = TRUE), 
                             subtitle = "small λ: no sparsity")

plot3 <- create_contour_plot(theta_hat = c(1, 1), theta_lasso = c(0.5, 0), l1_edge = 0.5, 
                             outermost_point = c(0.5, 0), annotation_positions = matrix(c(-0.5, 2.5, 2, 3), nrow = 2, byrow = TRUE), 
                             subtitle = "larger λ: sparsity")

# Arrange the plots in a grid
p <- grid.arrange(plot1, plot2, plot3, nrow = 1)

ggsave("../figure/lasso_contour_cases.png", plot = p, height = 6, width = 18)