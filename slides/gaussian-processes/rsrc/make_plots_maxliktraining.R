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

compute_nll_components = function(x, y, length_scale) {
    kmat = get_kmat(x, x, "squaredexp", lengthscale = length_scale) + 
        diag(noise, length(x))
    kmat_inv = solve(kmat)
    fit = -0.5 * t(y) %*% kmat_inv %*% y
    complexity = - 0.5 * log(det(kmat))
    norm_const = - length(x) / 2 * log(2 * pi)
    nll = -(fit + complexity) #  + norm_const)
    data.table(
        ls = length_scale,
        fit = -fit,
        complexity = -complexity,
        norm_const = norm_const,
        nll = nll
    )
}

compute_posterior_pred = function(x, x_new, y, length_scale, noise) {
    num_obs = length(y)
    kmat = kernel_sqexp(x, x_new, length_scale)
    k = kmat[seq_len(num_obs), seq_len(num_obs)]
    kx = kmat[seq_len(num_obs), (num_obs + 1):nrow(kmat)]
    kxx = kmat[(num_obs + 1):nrow(kmat), (num_obs + 1):nrow(kmat)]
    ky = k + diag(rep(noise, length(x)))
    ky_inv = solve(ky)
    m_post = crossprod(kx, ky_inv) %*% y
    k_post = kxx - crossprod(kx, ky_inv) %*% kx
    data.table(
        ls = length_scale, 
        x = x_new, 
        m_post = m_post, 
        sd_post = diag(k_post)
    )
}

# DATA -------------------------------------------------------------------------

set.seed(123)
num_obs = 15
noise = 0.5
x = seq(-2, 2, length.out = num_obs)
num_obs = length(x)
y = c(rmvnorm(1, sigma = kernel_sqexp(x, x, 0.3)[seq_along(x), seq_along(x)]))
x_new = seq(-2, 2, length.out = 100L)
ls_plot = c(0.2, 2, 0.5)

dt_nll = lapply(
    seq(0.01, 2, by = 0.01),
    function(l) compute_nll_components(x, y, length_scale = l)
)
dt_nll = do.call(rbind, dt_nll)
dt_nll_long = melt(dt_nll, id.vars = c("ls"))

dt_post_pred = lapply(
    ls_plot,
    function(l) compute_posterior_pred(x, x_new, y, l, noise)
)
dt_post_pred = do.call(rbind, dt_post_pred)

# PLOTS ------------------------------------------------------------------------

p_points = ggplot(data.table(x = x, y = y), aes(x, y)) +
    geom_point() +
    theme_bw()
ggsave(
    "../figure/gp_training/datapoints.pdf",
    p_points,
    height = 3,
    width = 4
)

p_nll = ggplot(
    dt_nll_long[variable != "norm_const"], 
    aes(x = ls, y = value, color = variable)
) +
    geom_line() + 
    theme_bw() + 
    scale_color_manual(
        "",
        values = c("blue", "darkorange", "black"),
        labels = c("fit penalty", "complexity", "NLL (unnorm.)")
    ) +
    theme(legend.position = "top")
ggsave(
    "../figure/gp_training/nll_components.pdf",
    p_nll,
    height = 3,
    width = 4
)

for (idx in seq_along(ls_plot)) {
    l = ls_plot[[idx]]
    ggsave(
        sprintf("../figure/gp_training/nll_components_ls_%i.pdf", idx),
        p_nll +
            geom_vline(xintercept = l, color = "darkgray"),
        height = 3,
        width = 4
    )
    ggsave(
        sprintf("../figure/gp_training/datapoints_pred_%i.pdf", idx),
        p_points +
            geom_line(
                data = dt_post_pred[ls == l], 
                aes(x, m_post.V1), 
                color = "darkgray"
            ),
        height = 3,
        width = 4
    )
}
