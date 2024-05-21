# ------------------------------------------------------------------------------
# FIG: L2 REGULARIZATION HESSIAN PLOTS
# ------------------------------------------------------------------------------

source("utils.R")
library(gridExtra)

lambda <- 50
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

x1 <- seq(-1.5,2,length.out = 100)
x2 <- seq(-1,3.5,length.out = 100)


# R_emp
init_cond_plot <- plot_r_emp(R_emp, x1, x2) +
  annotate("label", x = 0.75, y = 3, label = "hat(theta)",
           parse = TRUE, color = 'black', size = 3, fill = "red") +
  theme(legend.position="none") + coord_fixed() +
  geom_hline(yintercept = 0, colour="darkgrey", size=1.2) +
  geom_vline(xintercept = 0, colour="darkgrey", size=1.2) +
  geom_line(data=rbind(rep(0, num_features), theta_min),
            aes(x=V1, y=V2), colour="red", size=1, arrow = arrow(length = unit(0.09, "npc")))

rot_plot <- plot_r_emp(R_emp, x1, x2) +
  theme(legend.position="none") + coord_fixed() +
  geom_abline(slope = Q[2,1]/Q[1,1], colour="darkgrey", size=1.2) +
  geom_abline(slope = Q[2,2]/Q[1,2], colour="darkgrey", size=1.2) +
  geom_line(data=rbind(rep(0, num_features), theta_min),
            aes(x=V1, y=V2), colour="red", size=1, arrow = arrow(length = unit(0.09, "npc"))) +
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=theta_proj1_data ), size=0.9,
               arrow=arrow(type="closed", length = unit(0.09, "npc")),
               linetype="dashed", aes(x=start.V1, y=start.V2, xend = end.V1, yend = end.V2),
               colour = "green", arrow.fill = "green") +
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=theta_proj2_data ), size=0.9,
               arrow=arrow(type="closed", length = unit(0.09, "npc")),
               linetype="dashed", aes(x=start.V1, y=start.V2, xend = end.V1, yend = end.V2),
               colour = "green", arrow.fill = "green")

rs <- sapply(1:2, function(i) S[i,i] / (S[i,i] + lambda))

theta_hat <- theta_proj1_data*rs[1] + theta_proj2_data*rs[2] 
geom_l2_plot <- plot_r_emp(R_emp, x1, x2) +
  theme(legend.position="none") + coord_fixed() +
  geom_hline(yintercept = 0, colour="darkgrey", size=1.2) +
  geom_vline(xintercept = 0, colour="darkgrey", size=1.2) +
  geom_point(aes(x=beta_true[1], y=beta_true[2], color="red", size=3))  +
  geom_point(aes(x=theta_hat[1], y=theta_hat[2], color="yellow", size=3))

geom_l2_plot <- geom_l2_plot +
  annotate("label", x = 1.3, y = 1.5, label = "hat(theta)[Ridge]",
           parse = TRUE, color = 'black', size = 3, fill = "yellow") + 
  annotate("label", x = 0.75, y = 3, label = "hat(theta)",
           parse = TRUE, color = 'black', size = 3, fill = "red") 

##############shang

scale_rot_plot <- rot_plot +
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=
                            theta_proj1_data*rs[1] ), size=0.9,
               arrow=arrow(type="closed", length = unit(0.09, "npc")),
               linetype="dashed", aes(x=start.V1, y=start.V2,
                                      xend = end.V1, yend = end.V2),
               colour = "orange", arrow.fill = "orange") +
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=
                            theta_proj2_data*rs[2] ), size=0.9,
               arrow=arrow(type="closed", length = unit(0.09, "npc")),
               linetype="dashed", aes(x=start.V1, y=start.V2,
                                      xend = end.V1, yend = end.V2),
               colour = "orange", arrow.fill = "orange") +
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=
                            theta_proj1_data*rs[1] +
                            theta_proj2_data*rs[2] ), size=0.9,
               arrow=arrow(length = unit(0.09, "npc")),
               linetype="solid", aes(x=start.V1, y=start.V2,
                                     xend = end.V1, yend = end.V2),
               colour = "yellow")

scale_plot <- init_cond_plot +
  annotate("label", x = 1.3, y = 1.5, label = "hat(theta)[Ridge]",
           parse = TRUE, color = 'black', size = 3, fill = "yellow") +
  geom_segment(data=cbind(start=as.data.frame(t(c(0,0))), end=
                            theta_proj1_data*rs[1] +
                            theta_proj2_data*rs[2] ), size=0.9,
               arrow=arrow(length = unit(0.09, "npc")),
               linetype="solid", aes(x=start.V1, y=start.V2,
                                     xend = end.V1, yend = end.V2),
               colour = "yellow")

p1 <- grid.arrange(init_cond_plot, rot_plot, ncol=2)

p2 <- grid.arrange(rot_plot, init_cond_plot, ncol=2)

p3 <- grid.arrange(scale_rot_plot, scale_plot, ncol=2)

### contour plot for l2
#set a wider range
x1 <- seq(-2,2,length.out = 100)
x2 <- seq(-1,5,length.out = 100)

#calculate ellipse distance
dis_elli <- function(x, y, theta){
  dr1 <- x - beta_true[1]
  dr2 <- y - beta_true[2]
  data <- cbind(dr1, dr2)
  mat <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow=2)
  dr <- data %*% mat
  dr[,1] <- dr[,1]/3 #axis ~= 3:1
  apply(dr, 1, dist)
}

# Generate data points for plotting circles(ridge)
cir_list <- list()
seq_data <- seq(0, 2*pi, length.out=100) #points for one circle
i <- 1
for(mul in c(0.15, 0.6, 0.9, 1.26)){ #adjust radius
  cir_list[[i]] <- data.frame(x=cos(seq_data)*mul, y=sin(seq_data)*mul)
  i <- i + 1
}

eval_grid <- expand.grid(x1,x2)
eval_grid$r_emp <- apply(eval_grid, 1, R_emp)

#preserve only center part of contour lines
#chose the parameter manually acoording to the plots
distance <- dis_elli(eval_grid[,1], eval_grid[,2], theta=-pi/3-0.014)
eval_grid$dist <- distance
eval_grid_sub <- subset(eval_grid, dist < 1.5)

p_elli <- ggplot() +
  geom_raster(data=eval_grid, aes(x=Var1, y=Var2, fill=r_emp)) +
  geom_contour(data=eval_grid_sub, aes(x=Var1, y=Var2, z=r_emp), 
               colour="white", bins=7) +
  theme(legend.position="none") + coord_fixed() +
  xlab(expression(theta[1])) +
  ylab(expression(theta[2])) +
  #geom_point(aes(x=theta_hat[1], y=theta_hat[2], color="yellow", size=3)) +
  scale_fill_viridis(end = 0.9)

p_ridge <- p_elli + 
  geom_path(data=cir_list[[1]], aes(x, y), color="white", linetype="dashed") +
  geom_path(data=cir_list[[2]], aes(x, y), color="white", linetype="dashed") +
  geom_path(data=cir_list[[3]], aes(x, y), color="white", linetype="dashed") +
  geom_path(data=cir_list[[4]], aes(x, y), color="white", linetype="dashed")


p4 <- p_ridge + 
  geom_point(aes(x=beta_true[1], y=beta_true[2]), color="red", size=3)  +
  geom_point(aes(x=0.73, y=1.03), color="yellow", size=3) +#intersection point
  annotate("label", x = 1.1, y = 0.9, label = "hat(theta)[Ridge]",
           parse = TRUE, color = 'black', size = 3, fill = "yellow") +
  annotate("label", x = 0.75, y = 3, label = "hat(theta)",
           parse = TRUE, color = 'black', size = 3, fill = "red") +
  geom_hline(yintercept = 0, colour="darkgrey", size=1.2) +
  geom_vline(xintercept = 0, colour="darkgrey", size=1.2) + 
  xlim(-1.4, 1.6) +
  ylim(-1, 4.5)

ggsave("../figure/l2_reg_hess_01_plot.png", plot = p1, width = 5.5, height = 3.5, dpi="retina")
ggsave("../figure/l2_reg_hess_02_plot.png", plot = p2, width = 5.5, height = 3.5, dpi="retina")
ggsave("../figure/l2_reg_hess_03_plot.png", plot = p3, width = 5.5, height = 3.5, dpi="retina")
ggsave("../figure/l2_reg_hess_04_plot.png", plot = p4, width = 3, height = 5, dpi="retina")
