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

save_topdf = function(p, kernel_type) {
    ggsave(
        sprintf("../figure/cov_funs/cov_%s.pdf", kernel_type),
        p,
        width = 6,
        height = 2.5
    )
}

# PLOTS ------------------------------------------------------------------------

set.seed(123)

save_topdf(plot_priors(kernel_type = "constant", intercept = 2), "constant")
save_topdf(plot_priors(kernel_type = "linear", intercept = 2), "linear")

p_polynomial = lapply(
    c(1, 2, 5),
    function(i) {
        plot_priors(kernel_type = "polynomial", intercept = 2, degree = i) +
            ggtitle(sprintf("degree %i", i))
    }
)
save_topdf(Reduce("+", p_polynomial), "polynomial")

p_periodic = lapply(
    c(1, 2, 5),
    function(i) {
        plot_priors(kernel_type = "periodic", period = i, lengthscale = 1) +
            ggtitle(sprintf("period %i", i))
    }
)
save_topdf(Reduce("+", p_periodic), "periodic")

p_matern = lapply(
    c(0.5, 2, 10),
    function(i) {
        plot_priors(kernel_type = "matern", nu = i, lengthscale = 1) +
            ggtitle(bquote(nu == .(i)))
    }
)
save_topdf(Reduce("+", p_matern), "matern")

p_exponential = lapply(
    c(0.1, 1, 10),
    function(i) {
        plot_priors(kernel_type = "exponential", lengthscale = i) +
            ggtitle(sprintf("length scale %.1f", i))
    }
)
save_topdf(Reduce("+", p_exponential), "exponential")

p_squaredexp = lapply(
    c(0.1, 1, 10),
    function(i) {
        plot_priors(kernel_type = "squaredexp", lengthscale = i) +
            ggtitle(sprintf("length scale %.1f", i))
    }
)
save_topdf(Reduce("+", p_squaredexp), "squaredexp")
