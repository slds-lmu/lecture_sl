# ------------------------------------------------------------------------------
# geom l2

# FIG: theta_hat (OLS) and theta_ridge (Ridge Regression) points on contour plot
#      to show how l2 penalty influences the optimal value
#      on each principle axis and overall.

# DATA: linear model data from data_func_utils.R
# ------------------------------------------------------------------------------

source("data_func_utils.R")
library(gridExtra)

# DATA -------------------------------------------------------------------------

lambda <- 90
beta_start <- c(0, 0)
step_size <- 0.005
grad <- R_emp_grad
num_steps <- 100

gd_betas <- gradient_descent(beta_start, step_size, grad, num_steps)

theta_min <- gd_betas[num_steps,]
hessian <- R_emp_hessian()

eig_dec <- eigen(hessian)
Q <- eig_dec$vectors
S <- diag(eig_dec$values)

theta_min_rot <-  t(Q) %*% t(as.matrix(theta_min))
theta_min_rot_data <- as.data.frame(t(theta_min_rot))

theta_proj1 <- Q[,1] * (Q[,1] %*% t(as.matrix(theta_min)))[1]
theta_proj2 <- Q[,2] * (Q[,2] %*% t(as.matrix(theta_min)))[1]

theta_proj1_data <- as.data.frame(t(theta_proj1))
theta_proj2_data <- as.data.frame(t(theta_proj2))

theta_min_skew <- solve(S + diag(rep(lambda, length(eig_dec$values)))) %*%
  S %*% theta_min_rot
theta_min_skew_data <- as.data.frame(t(theta_min_skew))

theta_min_ridge_data <- as.data.frame(t(Q %*% theta_min_skew))

x1 <- seq(-2,2,length.out = 100)
x2 <- seq(-1,5,length.out = 100)

# PLOT -------------------------------------------------------------------------

# record contour levels
p_con <- plot_r_emp(R_emp, x1, x2, bins=25)
ct_data <- ggplot_build(p_con)$data[[2]]
ct_levels <- unique(ct_data$level)
# preserve half of them to make plots look better (less contour lines)
ct_levels <- ct_levels[-seq(3, length(ct_levels), by = 2)]

# plot contour lines and theta_hat
init_cond_plot <- plot_r_emp(R_emp, x1, x2, breaks=ct_levels) +
  annotate("label", x = 0.75, y = 3, label = "hat(theta)",
           parse = TRUE, color = 'black', size = 3, fill = "red") +
  theme(legend.position="none") + coord_fixed() +
  geom_hline(yintercept = 0, colour="darkgrey", size=1.2) +
  geom_vline(xintercept = 0, colour="darkgrey", size=1.2) +
  geom_line(data=rbind(rep(0, num_features), theta_min),
            aes(x=V1, y=V2), colour="red", size=1, arrow = arrow(length = unit(0.06, "npc")))

# effect along two principle axes
rot_plot <- plot_r_emp(R_emp, x1, x2, breaks=ct_levels) +
  theme(legend.position="none") + coord_fixed() +
  geom_abline(slope = Q[2,1]/Q[1,1], colour="darkgrey", size=1.2) +
  geom_abline(slope = Q[2,2]/Q[1,2], colour="darkgrey", size=1.2) +
  geom_line(data=rbind(rep(0, num_features), theta_min),
            aes(x=V1, y=V2), colour="red", size=1, arrow = arrow(length = unit(0.06, "npc"))) +
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=theta_proj1_data ), size=0.9,
               arrow=arrow(type="closed", length = unit(0.06, "npc")),
               linetype="dashed", aes(x=start.V1, y=start.V2, xend = end.V1, yend = end.V2),
               colour = "green", arrow.fill = "green") +
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=theta_proj2_data ), size=0.9,
               arrow=arrow(type="closed", length = unit(0.06, "npc")),
               linetype="dashed", aes(x=start.V1, y=start.V2, xend = end.V1, yend = end.V2),
               colour = "green", arrow.fill = "green")

rs <- sapply(1:2, function(i) S[i,i] / (S[i,i] + lambda))
theta_hat <- theta_proj1_data*rs[1] + theta_proj2_data*rs[2] 

# theta_ridge decomposition along principle axes
scale_rot_plot <- rot_plot +
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=
                            theta_proj1_data*rs[1] ), size=0.9,
               arrow=arrow(type="closed", length = unit(0.06, "npc")),
               linetype="dashed", aes(x=start.V1, y=start.V2,
                                      xend = end.V1, yend = end.V2),
               colour = "orange", arrow.fill = "orange") +
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=
                            theta_proj2_data*rs[2] ), size=0.9,
               arrow=arrow(type="closed", length = unit(0.06, "npc")),
               linetype="dashed", aes(x=start.V1, y=start.V2,
                                      xend = end.V1, yend = end.V2),
               colour = "orange", arrow.fill = "orange") +
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=
                            theta_proj1_data*rs[1] +
                            theta_proj2_data*rs[2] ), size=0.9,
               arrow=arrow(length = unit(0.06, "npc")),
               linetype="solid", aes(x=start.V1, y=start.V2,
                                     xend = end.V1, yend = end.V2),
               colour = "yellow")

# theta_hat and theta_ridge
scale_plot <- init_cond_plot +
  annotate("label", x = 0.8, y = 1.5, label = "hat(theta)[Ridge]",
           parse = TRUE, color = 'black', size = 3, fill = "yellow") +
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=
                            theta_proj1_data*rs[1] +
                            theta_proj2_data*rs[2] ), size=0.9,
               arrow=arrow(length = unit(0.06, "npc")),
               linetype="solid", aes(x=start.V1, y=start.V2,
                                     xend = end.V1, yend = end.V2),
               colour = "yellow")

p1 <- grid.arrange(init_cond_plot, rot_plot, ncol=2)

p2 <- grid.arrange(rot_plot, init_cond_plot, ncol=2)

p3 <- grid.arrange(scale_rot_plot, scale_plot, ncol=2)

### contour plot with l2 constraints

# Generate data points for plotting l2 constraints(circles)
radius <- sqrt(theta_hat[1]^2 + theta_hat[2]^2)[[1]] #radius for interception point
cir_list <- list()
seq_data <- seq(0, 2*pi, length.out=100) #points for one circle
i <- 1
for(mul in c(radius/8, radius/3, radius/1.5, radius)){ #adjust radius
  cir_list[[i]] <- data.frame(x=cos(seq_data)*mul, y=sin(seq_data)*mul)
  i <- i + 1
}

eval_grid <- expand.grid(x1,x2)
eval_grid$r_emp <- apply(eval_grid, 1, R_emp)

# ellipse contours
p_elli <- ggplot() +
  geom_raster(data=eval_grid, aes(x=Var1, y=Var2, fill=r_emp)) +
  geom_contour(data=eval_grid, aes(x=Var1, y=Var2, z=r_emp), 
               colour="white", breaks = ct_levels[1:2]) + #use only two contour lines
  theme(legend.position="none") + coord_fixed() +
  xlab(expression(theta[1])) +
  ylab(expression(theta[2])) +
  scale_fill_viridis(end = 0.9)

# ellipse and circle contours
p_ridge <- p_elli + 
  geom_path(data=cir_list[[1]], aes(x, y), color="white", linetype="dashed") +
  geom_path(data=cir_list[[2]], aes(x, y), color="white", linetype="dashed") +
  geom_path(data=cir_list[[3]], aes(x, y), color="white", linetype="dashed") +
  geom_path(data=cir_list[[4]], aes(x, y), color="white", linetype="dashed") +
  ylim(-1, 5)

beta_true <- data.frame(x=beta_true[1], y=beta_true[2])
theta_hat <- data.frame(x=theta_hat[1][[1]], y=theta_hat[2][[1]])

# add points
p_poi <- p_ridge + 
  geom_point(data=beta_true, aes(x=x, y=y), color="red", size=3) +
  geom_point(data=theta_hat, aes(x=x, y=y), color="yellow", size=3) +
  annotate("label", x=0.8, y=1.5, label="hat(theta)[Ridge]",
           parse=TRUE, color='black', size=3, fill="yellow") +
  annotate("label", x = 0.75, y=3, label="hat(theta)",
           parse=TRUE, color='black', size=3, fill="red") +
  geom_hline(yintercept=0, colour="darkgrey", size=1.2) +
  geom_vline(xintercept=0, colour="darkgrey", size=1.2)

# add decomposition arrows
p4 <- p_poi + 
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=beta_true ), size=0.9,
               arrow=arrow(length = unit(0.06, "npc")),
               linetype="solid", aes(x=start.V1, y=start.V2,
                                     xend = end.x, yend = end.y),
               colour = "red") +
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=
                            theta_proj1_data*rs[1] +
                            theta_proj2_data*rs[2] ), size=0.9,
               arrow=arrow(length = unit(0.06, "npc")),
               linetype="solid", aes(x=start.V1, y=start.V2,
                                     xend = end.V1, yend = end.V2),
               colour = "yellow") +
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=theta_proj1_data ), size=0.9,
               arrow=arrow(type="closed", length = unit(0.06, "npc")),
               linetype="dashed", aes(x=start.V1, y=start.V2, xend = end.V1, yend = end.V2),
               colour = "green", arrow.fill = "green") +
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=theta_proj2_data ), size=0.9,
               arrow=arrow(type="closed", length = unit(0.06, "npc")),
               linetype="dashed", aes(x=start.V1, y=start.V2, xend = end.V1, yend = end.V2),
               colour = "green", arrow.fill = "green") +
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=
                            theta_proj1_data*rs[1] ), size=0.9,
               arrow=arrow(type="closed", length = unit(0.06, "npc")),
               linetype="dashed", aes(x=start.V1, y=start.V2,
                                      xend = end.V1, yend = end.V2),
               colour = "orange", arrow.fill = "orange") +
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=
                            theta_proj2_data*rs[2] ), size=0.9,
               arrow=arrow(type="closed", length = unit(0.06, "npc")),
               linetype="dashed", aes(x=start.V1, y=start.V2,
                                      xend = end.V1, yend = end.V2),
               colour = "orange", arrow.fill = "orange")
  


ggsave("../figure/l2_reg_hess_01.png", plot = p1, width = 5.5, height = 3.5, dpi="retina")
ggsave("../figure/l2_reg_hess_02.png", plot = p2, width = 5.5, height = 3.5, dpi="retina")
ggsave("../figure/l2_reg_hess_03.png", plot = p3, width = 5.5, height = 3.5, dpi="retina")
ggsave("../figure/l2_reg_hess_04.png", plot = p4, width = 3, height = 5, dpi="retina")
