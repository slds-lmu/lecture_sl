# ------------------------------------------------------------------------------
# PLOT GP PRIORS
# ------------------------------------------------------------------------------

# Purpose: plot samples drawn from GP priors + posterior after observing data

# PREREQ -----------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(mvtnorm)

source("covariance_functions.R")
source("plot_functions.R")




# FUNCTIONS --------------------------------------------------------------------

compute_posterior_pred = function(x, x_new, y, length_scale, noise) {
    num_obs = length(y)
    kmat = kernel_sqexp(x, x_new, length_scale)
    k = kmat[seq_len(num_obs), seq_len(num_obs)]
    kx = kmat[seq_len(num_obs), (num_obs + 1):nrow(kmat)]
    kxx = kmat[(num_obs + 1):nrow(kmat), (num_obs + 1):nrow(kmat)]
    ky = k + diag(rep(noise, length(x_new)))
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
num_obs = 100
x = seq(-2, 2, length.out = num_obs)
y =  c(0, 1, 2, 1.5)
x_new =  c(-1.5, 1/3, 4/3, -0.5)

dt_post_pred = compute_posterior_pred(x, x_new, y, 0.1, noise)

p_points = ggplot(data.table(x = xnew, y = y), aes(x, y)) +
  geom_point() +
  theme_bw()
p_points +
  geom_line(
    data = dt_post_pred, 
    aes(x, m_post.V1), 
    color = "darkgray"
  )



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

ggsave(
    "../figure/gp_sample/zeromean_prior_50n.pdf",
    plot_priors(
        kernel_type = "squaredexp", 
        lengthscale = 1, 
        x_range = c(-2, 2),
        n_samples = 50
    ),
    height = 4,
    width = 8
)

ggsave(
    "../figure/gp_sample/zeromean_prior_10n.pdf",
    plot_priors(
        kernel_type = "squaredexp", 
        lengthscale = 1, 
        x_range = c(-10, 10),
        n_samples = 10
    ) + geom_hline(yintercept = 0, linetype = "dashed", color = "black"),
    height = 2,
    width = 8
)
