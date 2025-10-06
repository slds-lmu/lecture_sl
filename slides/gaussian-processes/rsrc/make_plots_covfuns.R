# ------------------------------------------------------------------------------
# PLOT FUNCTION SAMPLES FOR DIFFERENT COVARIANCE FUNCTIONS
# ------------------------------------------------------------------------------

# Purpose: create plots (mainly for covariance chapter) to visualize samples
# from GP priors characterized by different kernels

# PREREQ -----------------------------------------------------------------------

library(checkmate)
library(data.table)
library(ggplot2)
library(patchwork)

source("covariance_functions.R")
source("plot_functions.R")

# FUNCTIONS --------------------------------------------------------------------

save_topdf = function(p, kernel_type, width = 8) {
    ggsave(
        sprintf("../figure/cov_funs/cov_%s.pdf", kernel_type),
        p,
        width = width,
        height = 2.5
    )
}

# PLOTS ------------------------------------------------------------------------

set.seed(123)
x = seq(-2, 2, length.out = 25)

save_topdf(plot_priors(kernel_type = "constant", intercept = 2), "constant")

save_topdf(
  plot_priors(kernel_type = "linear", intercept = 2) +
    plot_cov(get_kmat(x, x, "linear")) + plot_layout(widths = c(0.75, 0.25)), 
  "linear"
)

p_polynomial = lapply(
    c(1, 2, 5),
    function(i) {
        plot_priors(kernel_type = "polynomial", intercept = 2, degree = i) +
            ggtitle(sprintf("degree %i", i))
    }
)
save_topdf(
  Reduce("+", p_polynomial) + 
    plot_cov(get_kmat(x, x, "polynomial", degree = 2)) +
    ggtitle("degree 2") +
    plot_layout(widths = rep(0.25, 4)), 
  "polynomial"
)

p_periodic = lapply(
    c(1, 2, 5),
    function(i) {
        plot_priors(kernel_type = "periodic", period = i, lengthscale = 1) +
            ggtitle(sprintf("period %i", i))
    }
)
save_topdf(
  Reduce("+", p_periodic) +
    plot_cov(get_kmat(x, x, "periodic", period = 3, lengthscale = 1)) +
    ggtitle("period 3") +
    plot_layout(widths = rep(0.25, 4)), 
  "periodic"
)

p_matern = lapply(
    c(0.5, 2, 10),
    function(i) {
        plot_priors(kernel_type = "matern", nu = i, lengthscale = 1) +
            ggtitle(bquote(nu == .(i)))
    }
)
save_topdf(
  Reduce("+", p_matern) +
    plot_cov(get_kmat(x, x, "matern", nu = 2, lengthscale = 1)) +
    ggtitle(bquote(nu == 2)) +
    plot_layout(widths = rep(0.25, 4)), 
  "matern"
)

p_exponential = lapply(
    c(0.1, 1, 10),
    function(i) {
        plot_priors(kernel_type = "exponential", lengthscale = i) +
            ggtitle(sprintf("length scale %.1f", i))
    }
)
save_topdf(
  Reduce("+", p_exponential) +
    plot_cov(get_kmat(x, x, "exponential", lengthscale = 1)) +
    ggtitle("length scale 1.0") +
    plot_layout(widths = rep(0.25, 4)), 
  "exponential"
)

p_squaredexp = lapply(
    c(0.1, 1, 10),
    function(i) {
        plot_priors(kernel_type = "squaredexp", lengthscale = i) +
            ggtitle(sprintf("length scale %.1f", i))
    }
)
save_topdf(
  Reduce("+", p_squaredexp) +
    plot_cov(get_kmat(x, x, "squaredexp", lengthscale = 1)) +
    ggtitle("length scale 1.0") +
    plot_layout(widths = rep(0.25, 4)), 
  "squaredexp"
)

