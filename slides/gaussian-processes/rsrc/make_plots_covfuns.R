# ------------------------------------------------------------------------------
# PLOT FUNCTION SAMPLES FOR DIFFERENT COVARIANCE FUNCTIONS
# ------------------------------------------------------------------------------

# Purpose: create plots (mainly for covariance chapter) to visualize samples
# from GP priors characterized by different kernels

# PREREQ -----------------------------------------------------------------------

if (FALSE) install.packages("pak")

library(checkmate)
library(data.table)
library(ggplot2)
library(patchwork)
library(mvtnorm)
pak::pak("slds-lmu/vistool")
library(vistool)

source("covariance_functions.R")

# FUNCTIONS --------------------------------------------------------------------

sample_zeromean_gp_prior = function(
        kernel_type, n_samples, n_points, x_range, eps = 1e-6, ...
    ) {
    x = seq(x_range[1], x_range[2], length.out = n_points)
    kmat = get_kmat(x, x, kernel_type, ...)
    kmat = kmat + diag(eps, n_points)
    samples = rmvnorm(n_samples, mean = rep(0, n_points), sigma = kmat)
    dt = as.data.table(t(samples))
    names(dt) = sprintf("sample_%i", seq_len(n_samples))
    dt[, x := x]
}

plot_priors = function(
        kernel_type, 
        n_samples = 5, 
        n_points = 500, 
        x_range = c(-2, 2), 
        ...) {
    plot_data = sample_zeromean_gp_prior(
        kernel_type, n_samples, n_points, x_range, ...
    )
    plot_data_long = melt(plot_data, id.vars = "x")
    ggplot(plot_data_long, aes(x = x, y = value, color = variable)) +
        geom_line() +
        scale_color_viridis_d() +
        theme_bw() +
        labs(x = "x", y = "f(x)") +
        theme(
            legend.position = "none",
            axis.text.y = element_blank()
        )
    
}

# PLOTS ------------------------------------------------------------------------

set.seed(123)

plot_priors(kernel_type = "constant", intercept = 2)

plot_priors(kernel_type = "linear", intercept = 2)

p_polynomial = lapply(
    c(1, 2, 5),
    function(i) {
        plot_priors(kernel_type = "polynomial", intercept = 2, degree = i) +
            ggtitle(sprintf("degree %i", i))
    }
)
Reduce("+", p_polynomial)

p_periodic = lapply(
    c(1, 2, 5),
    function(i) {
        plot_priors(kernel_type = "periodic", period = i, lengthscale = 1) +
            ggtitle(sprintf("period %i", i))
    }
)
Reduce("+", p_periodic) # + plot_annotation(title = "length scale 1")

p_matern = lapply(
    c(0.5, 2, 10),
    function(i) {
        plot_priors(kernel_type = "matern", nu = i, lengthscale = 1) +
            ggtitle(bquote(nu == .(i)))
    }
)
Reduce("+", p_matern)

p_exponential = lapply(
    c(0.1, 1, 10),
    function(i) {
        plot_priors(kernel_type = "exponential", lengthscale = i) +
            ggtitle(sprintf("length scale %.1f", i))
    }
)
Reduce("+", p_exponential)

p_squaredexp = lapply(
    c(0.1, 1, 10),
    function(i) {
        plot_priors(kernel_type = "squaredexp", lengthscale = i) +
            ggtitle(sprintf("length scale %.1f", i))
    }
)
Reduce("+", p_squaredexp)
