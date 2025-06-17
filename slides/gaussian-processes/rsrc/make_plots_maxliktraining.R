# ------------------------------------------------------------------------------
# PLOT MARGINAL LOGLIKELIHOOD COMPONENTS
# ------------------------------------------------------------------------------

# Purpose: plot trade-off in loglik components for vayring kernel smoothness

# PREREQ -----------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(mvtnorm)

source("covariance_functions.R")

# FUNCTIONS --------------------------------------------------------------------

compute_mll_components = function(x, y, length_scale) {
    kmat = kernel_sqexp(x, x, length_scale) + diag(noise, length(x))
    kmat_inv = solve(kmat)
    fit = -0.5 * t(y) %*% kmat_inv %*% y
    complexity = - 0.5 * log(det(kmat))
    norm_const = - length(x) / 2 * log(2 * pi)
    nll = -(fit + complexity + norm_const)
    data.table(
        ls = length_scale,
        fit = -fit,
        complexity = -complexity,
        norm_const = norm_const,
        nll = nll
    )
}

# PLOTS ------------------------------------------------------------------------

set.seed(123)
num_obs = 15
noise = 0.5
x = seq(-2, 2, length.out = num_obs)
num_obs = length(x)
y = c(rmvnorm(1, sigma = kernel_sqexp(x, x, 0.5)))
dt = lapply(
    seq(0.01, 2, by = 0.01),
    function(l) compute_mll_components(x, y, length_scale = l)
)
dt = do.call(rbind, dt)
dt_long = melt(
    dt,
    id.vars = c("ls")
)
ggplot(dt_long[variable != "norm_const"], aes(x = ls, y = value, color = variable)) +
    geom_line() + 
    theme_bw() + 
    scale_color_manual(
        "",
        values = c("blue", "darkorange", "black"),
        labels = c("residual variance", "complexity", "NLL")
    )
