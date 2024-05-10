library(ggplot2)

# Define parameters for the ellipse
center <- c(1.5, 1.5)
axis_len <- c(1.5, 0.75)  # Lengths of the axes for the ellipse
rotation <- pi/3


seq_data <- seq(0, 2*pi, length.out=100) #points for one circle
# Generate data points for plotting ellipses
ellipse_x <- cos(rotation)*cos(seq_data)*axis_len[1]-sin(rotation)*sin(seq_data)*axis_len[2] 
ellipse_y <- sin(rotation)*cos(seq_data)*axis_len[1]+cos(rotation)*sin(seq_data)*axis_len[2]

elli_list <- list()
i <- 1
for(mul in c(0.24, 0.43, 0.62, 0.78)){ #adjust radius
  elli_list[[i]] <- data.frame(x=center[1]+ellipse_x*mul, y=center[2]+ellipse_y*mul)
  i <- i + 1
}

# Generate data points for plotting circles(ridge)
cir_list <- list()
i <- 1
for(mul in c(0.15, 0.4, 0.67, 1)){ #adjust radius
  cir_list[[i]] <- data.frame(x=cos(seq_data)*mul, y=sin(seq_data)*mul)
  i <- i + 1
}

# Create the plot of ellipses
p_elli <- ggplot() + 
  geom_path(data=elli_list[[1]], aes(x, y), color="black") +
  geom_path(data=elli_list[[2]], aes(x, y), color="black") +
  geom_path(data=elli_list[[3]], aes(x, y), color="black") +
  geom_path(data=elli_list[[4]], aes(x, y), color="black") +
  geom_point(aes(x=center[1], y=center[2]), color="black", size=3) + 
  annotate("label", x=1.6, y=1.3, label="hat(theta)",
           parse=TRUE, color='black', size=3)

# Create whole plot
p_ridge_geom <- p_elli + 
  geom_path(data=cir_list[[1]], aes(x, y), color="black", linetype="dashed") +
  geom_path(data=cir_list[[2]], aes(x, y), color="black", linetype="dashed") +
  geom_path(data=cir_list[[3]], aes(x, y), color="black", linetype="dashed") +
  geom_path(data=cir_list[[4]], aes(x, y), color="black", linetype="dashed") +
  geom_point(aes(x=0.83, y=sqrt(1-0.83^2)), color="black", size=3) + #intersection point
  annotate("label", x=1, y=0.2, label="hat(theta)[\"Ridge\"]",
           parse=TRUE, color='black', size=3) +
  xlim(-1.5, 3) +
  ylim(-1.5, 3) +
  coord_equal() +
  theme_light() +
  labs(title = "",
       x = expression(theta_1),
       y = expression(theta_2))

ggsave(filename = paste0("../figure/wd-l2-geom.png"), 
       plot=p_ridge_geom, width=12, height=7.5)

