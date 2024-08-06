# ------------------------------------------------------------------------------
# l1, l2

# FIG: plot 3D regression surfaces with two coefficients 
#      under different regularization constanta (lambda 0, 1, 10) 
#      using l1 and l2 regularization.

# DATA: y(500*1) = -0.5 * x1(~Unif(-1,1)) + 
#                  3 * x2(~Unif(-1,1)) + epsilon(~Norm(0,0.1)).

# ENV: for use of vistool, need to build a virtual env in miniconda 3
#      conda create -n r-reticulate python=3.12
#      conda activate r-reticulate
#      conda install -c plotly plotly-orca python-kaleido
#      conda deactivate
# ------------------------------------------------------------------------------

library(vistool)
library(plotly)
set.seed(0)

#library(plotly)
#use_condaenv("r-reticulate", required = TRUE)
#py_config()

# DATA -------------------------------------------------------------------------

n <- 500
x1 <- runif(n, -1, 1)
x2 <- runif(n, -1, 1)
epsilon <- rnorm(n, 0, 0.1)
y <- -0.5 * x1 + 3 * x2 + epsilon

# Regularization Norm Functions
l1_norm <- function(beta1, beta2) {
  return(abs(beta1) + abs(beta2))
}

l2_norm_squared <- function(beta1, beta2) {
  return(beta1^2 + beta2^2)
}

# Updated Regularized Least Squares Objective Function with 1/n factor
updated_objective <- function(x, x1, x2, y, lam, regularization) {
  # x: beta, need to use x for building objective in vistool
  residuals <- y - x[1] * x1 - x[2] * x2
  error_term <- sum(residuals^2) / n
  if (regularization == 'l1') {
    penalty <- l1_norm(x[1], x[2])
  } else if (regularization == 'l2') {
    penalty <- l2_norm_squared(x[1], x[2])
  }
  return(error_term + lam * penalty)
}

# PLOT -------------------------------------------------------------------------

regularizations <- c('l1', 'l2')
lambdas <- c(0, 1, 10)

for (reg in regularizations) {
  for (lam in lambdas) {
    obj_lm = Objective$new(id = "reg surfaces", fun = updated_objective, xdim = 2, 
                           x1 = x1, x2 = x2, y = y, 
                           lam = lam, regularization = reg, minimize = TRUE)
    viz_lm = as_visualizer(obj_lm, x1_limits = c(-10, 10), x2_limits = c(-10, 10))
    result <- optim(c(0, 0), updated_objective, x1 = x1, x2 = x2, y = y, lam = lam, 
                    regularization = reg, method = 'L-BFGS-B')
    plot_obj <- viz_lm$plot()
    plot_obj <- plot_obj %>%
      layout(
        title = paste("Regularization:",reg,"λ:", as.character(lam)),
        scene = list(
          xaxis = list(title = "β1"),
          yaxis = list(title = "β2"),
          zaxis = list(title = "Objective")
        )
      ) %>%
      add_trace(
        type = "scatter3d",
        mode = "markers",
        x = result$par[1],  # beta1
        y = result$par[2],  # beta2
        z = result$value,  # objective value
        marker = list(color = 'red', size = 3),
        name = "Minimum Point"
      )
    savename = paste0("../figure/reg_surfaces_", reg, "_lam", as.character(lam),".png")
    save_image(plot_obj, savename, engine = "kaleido", width = 600, height = 500)
  }
}

