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

# Update prior samples with observed data
plot_prior_updates = function(x, x_obs, y, kmat, n_samples = 20) {
  
  num_obs = length(x)
  kss = kmat[seq_len(num_obs), seq_len(num_obs)]
  ks = kmat[seq_len(num_obs), (num_obs + 1):nrow(kmat)]
  k = kmat[(num_obs + 1):nrow(kmat), (num_obs + 1):nrow(kmat)]
  
  lapply(
    seq_along(x_obs),
    function(i) {
      k_inv = solve(k[seq_len(i), seq_len(i)])
      ks_i = as.matrix(ks[, seq_len(i)])
      ks_kinv = ks_i %*% k_inv
      m_post = ks_kinv %*% y[seq_len(i)]
      k_post = kss - tcrossprod(ks_i %*% k_inv, ks_i)
      dt = lapply(
        seq_len(n_samples),
        function(j) {
          set.seed(j)
          data.table(
            iteration = j, x = x, pred = c(rmvnorm(1, m_post, sigma = k_post))
          )
        }
      )
      dt = do.call(rbind, dt)
      ggplot() +
        geom_line(data = dt, aes(x = x, y = pred, col = as.factor(iteration))) +
        geom_point(
          data = data.table(x = x_obs[seq_len(i)], y = y[seq_len(i)]),
          mapping = aes(x = x, y = y), 
          size = 2
        ) +
        scale_color_viridis_d() +
        theme_bw() +
        labs(x = "x", y = "f(x)") +
        theme(
          legend.position = "none",
          axis.text.y = element_blank()
        )
    }
  )
}

# DATA -------------------------------------------------------------------------

set.seed(123)
num_obs = 100
x = seq(-2, 2, length.out = num_obs)
y =  c(0, 1, 2, 1.5)
x_new =  c(-1.5, 1/3, 4/3, -0.5)

p_points = ggplot(data.table(x = x_new, y = y), aes(x, y)) +
  geom_point() +
  theme_bw()

plot_prior_updates(x, x_new, y, kernel_sqexp(x, x_new, 1))

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
