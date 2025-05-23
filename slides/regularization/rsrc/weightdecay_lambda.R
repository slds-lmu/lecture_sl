# ------------------------------------------------------------------------------
# wd vs l2

# FIG: draw the path of optimal point for each iteration using weight decay. 
#      use different decay parameters(lambda) to show how strong the pulling is.

# DATA: linear model data from data_func_utils.R
# ------------------------------------------------------------------------------

source("data_func_utils.R")
library(gridExtra)

# DATA -------------------------------------------------------------------------

x1 <- seq(0,1.5,length.out = 100)
x2 <- seq(0,3.5,length.out = 100)
lambda <- 5
num_steps <- 100
beta_start <- c(0, 0)
step_size <- 0.005
grad <- R_emp_grad
num_steps <- 100
gd_l2_betas <- gradient_descent(beta_start, step_size,
                                function(beta) R_reg_l2_grad(beta, lambda), num_steps)

ret <- weight_decay(beta_start, lambda, step_size, R_emp_grad, num_steps)

# PLOT -------------------------------------------------------------------------

remp_l2_plot_1 <-  plot_r_emp(R_emp, x1, x2) +
  geom_path(data = ret$betas_gd, aes(x=V1, y=V2), colour = "red", size=1.1) +
  geom_path(data = ret$betas_wd, aes(x=V1, y=V2), colour = "yellow", size=1.1) +
  geom_point(data = gd_l2_betas, aes(x=V1, y=V2), colour = "white") +
  theme(legend.position="none")

lambda <- 30
gd_l2_betas <- gradient_descent(beta_start, step_size,
                                function(beta) R_reg_l2_grad(beta, lambda), num_steps)

ret <- weight_decay(beta_start, lambda, step_size, R_emp_grad, num_steps)

remp_l2_plot_2 <-  plot_r_emp(R_emp, x1, x2) +
  geom_path(data = ret$betas_gd, aes(x=V1, y=V2), colour = "red", size=1.1) +
  geom_path(data = ret$betas_wd, aes(x=V1, y=V2), colour = "yellow", size=1.1) +
  geom_point(data = gd_l2_betas, aes(x=V1, y=V2), colour = "white") +
  theme(legend.position="none")

#p <- grid.arrange(remp_l2_plot_1 , remp_l2_plot_2 , ncol=2)

ggsave("../figure/weightdecay_lambda_01.png", plot = remp_l2_plot_1, width = 2.6, height = 3.1, dpi="retina")
ggsave("../figure/weightdecay_lambda_02.png", plot = remp_l2_plot_2, width = 2.6, height = 3.1, dpi="retina")
