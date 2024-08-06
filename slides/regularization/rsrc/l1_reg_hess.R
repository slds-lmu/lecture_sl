# ------------------------------------------------------------------------------
# geom l1

# FIG: theta_hat (OLS) and theta_lasso (Lasso Regression) points on contour plot
#      to show how l1 penalty influences the optimal value by pulling them 
#      towards zero on each axis and overall.

# DATA: principal components of linear model data from data_func_utils.R
# ------------------------------------------------------------------------------

source("data_func_utils.R")
library(gridExtra)

# DATA -------------------------------------------------------------------------

prc <- prcomp(X , scale. = FALSE)
X_dc <- prc$x
X_dc[,1] <- X_dc[, 1]/2

colnames(X_dc) <- NULL

hessian <- R_emp_hessian(X_dc)

x1 <- seq(-4, 2.5,length.out = 100)
x2 <- seq(-4, 5,length.out = 100)

df <- data.frame(X = X_dc, y = y)

res <- lm(y ~ ., df) 
y_new <- y - res$coefficients[1]
theta_hat <- t(res$coefficients[2:3])
colnames(theta_hat) <- NULL

lambda <- 10
theta_l1_reg <- sign(theta_hat) * pmax(abs(theta_hat) - lambda / diag(hessian),0)

# PLOT -------------------------------------------------------------------------

# plot contour lines and theta_hat
init_plot_l1 <- plot_r_emp(function(beta) R_emp(beta, features = X_dc, target = y_new), 
                           x1, x2) +
  theme(legend.position = "none") +
  coord_fixed() +
  annotate("label", x = -1, y = 3.5, label = "hat(theta)",
           parse = TRUE, color = 'black', size = 4, fill = "red") +
  geom_hline(yintercept = 0, colour="lightblue",
             linetype = "dashed", alpha= 0.8, size = 1.1) +
  geom_vline(xintercept = 0, colour="lightblue",
             linetype = "dashed", alpha= 0.8, size = 1.1) +
  geom_point(data=as.data.frame(theta_hat), aes(x=theta_hat[1], y=theta_hat[2]), color="red", size=2) +
  geom_vline(xintercept = -lambda/hessian[1,1], colour="yellow",
             linetype = "dashed", alpha= 0.8, size = 1.1) +
  annotate("label", x = -2, y = -2.5, label =
             "frac(-lambda, H[\"1,1\"])",
           parse = TRUE, color = 'black', size = 4, fill = "yellow") 

# plot with arrows and points
theta_hat_1 <- theta_hat
theta_hat_1[,1] <- 0

# effect along theta1
plot_l1_theta1 <- init_plot_l1 +
  geom_point(data=as.data.frame(theta_hat_1), aes(x=theta_hat_1[1], y=theta_hat_1[2]), color="green", size=2) +
  geom_segment(data=cbind(start=as.data.frame(theta_hat), end=as.data.frame(theta_hat_1)),
               aes(x=start.V1, y=start.V2, 
                   xend=end.V1, yend=end.V2), colour="green",
               size=1.1, arrow=arrow(ends="last", type="closed", length=unit(0.04, "npc")),
               arrow.fill="green")

p1 <- grid.arrange(init_plot_l1, plot_l1_theta1, ncol=2)

###################################################################

# effect along theta2
theta_hat_2 <- theta_hat
theta_hat_2[,2] <- theta_l1_reg[2]


plot_l1_theta2 <- init_plot_l1 +
  geom_hline(yintercept=lambda/hessian[2,2], colour="yellow",
             linetype="dashed", alpha=0.8, size=1.1) +
  geom_point(data=as.data.frame(theta_hat_2), aes(x=theta_hat_2[1], y=theta_hat_2[2]), color="green", size=2) +
  geom_segment(data=cbind(start=as.data.frame(theta_hat), end=as.data.frame(theta_hat_2)),
               aes(x=start.V1, y=start.V2, 
                   xend=end.V1, yend=end.V2), colour="green",
               size=1.1, arrow = arrow(ends="last", type="closed", length=unit(0.04, "npc")),
               arrow.fill="green") +
  annotate("label", x=-3, y=2, label="frac(lambda, H[\"2,2\"])",
           parse=TRUE, color='black', size=4, fill="yellow") 

# effect along both axes
plot_l1_theta2_dash <- init_plot_l1 +
  geom_hline(yintercept=lambda/hessian[2,2], colour="yellow",
             linetype="dashed", alpha=0.8, size=1.1) +
  geom_point(data=as.data.frame(theta_hat_1), aes(x=theta_hat_1[1], y=theta_hat_1[2]), color="green", size=2) +
  geom_segment(data=cbind(start=as.data.frame(theta_hat), end=as.data.frame(theta_hat_1)),
               aes(x=start.V1, y=start.V2, 
                   xend=end.V1, yend=end.V2), colour="green",
               size=1.1, linetype = 'dashed', arrow = arrow(ends="last", type="closed", length=unit(0.04, "npc")),
               arrow.fill="green") +
  geom_point(data=as.data.frame(theta_hat_2), aes(x=theta_hat_2[1], y=theta_hat_2[2]), color="green", size=2) +
  geom_segment(data=cbind(start=as.data.frame(theta_hat), end=as.data.frame(theta_hat_2)),
               aes(x=start.V1, y=start.V2, 
                   xend=end.V1, yend=end.V2), colour="green",
               size=1.1, linetype = 'dashed', arrow = arrow(ends="last", type="closed", length=unit(0.04, "npc")),
               arrow.fill="green") +
  annotate("label", x=-3, y=2, label="frac(lambda, H[\"2,2\"])",
           parse=TRUE, color='black', size=4, fill="yellow") 
  
# sum of two axes leads to theta_lasso
plot_l1_theta_lasso <- plot_l1_theta2_dash +
  geom_point(data=as.data.frame(theta_l1_reg), aes(x=theta_l1_reg[1], y=theta_l1_reg[2]), color="orange", size=2) + 
  geom_segment(data=cbind(start=as.data.frame(theta_hat), end=as.data.frame(theta_l1_reg)),
               aes(x=start.V1, y=start.V2, 
                   xend=end.V1, yend=end.V2), colour="orange",
               size=1.1, arrow = arrow(ends="last", type="closed", length=unit(0.04, "npc")),
               arrow.fill="orange") +
  annotate("label", x=1.1, y=1.5, label="hat(theta)[\"Lasso\"]",
           parse=TRUE, color='black', size=4, fill="orange")

p2 <- grid.arrange(plot_l1_theta2, plot_l1_theta_lasso, ncol=2)

ggsave("../figure/l1_reg_hess_01.png", plot = p1, height = 3.5, width = 5.5)
ggsave("../figure/l1_reg_hess_02.png", plot = p2, height = 3.5, width = 5.5)
