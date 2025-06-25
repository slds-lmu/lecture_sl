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

plot_prior_updates = function(
    x, x_obs, y, kmat, mean_fun = NULL, n_samples = 20
) {
  
  num_obs = length(x_obs)
  k = kmat[seq_len(num_obs), seq_len(num_obs)]
  ks = kmat[(num_obs + 1):nrow(kmat), seq_len(num_obs)]
  kss = kmat[(num_obs + 1):nrow(kmat), (num_obs + 1):nrow(kmat)]
  # num_obs = length(x_obs)
  # k = kmat[seq_len(num_obs), seq_len(num_obs)]
  # ks = kmat[seq_len(num_obs), (num_obs + 1):nrow(kmat)]
  # kss = kmat[(num_obs + 1):nrow(kmat), (num_obs + 1):nrow(kmat)]
  
  lapply(
    seq_along(x_obs),
    function(i) {
      k_inv = solve(k[seq_len(i), seq_len(i)])
      ks_i = as.matrix(ks[, seq_len(i)])
      ks_kinv = ks_i %*% k_inv
      if (is.null(mean_fun)) {
        m_post = ks_kinv %*% y[seq_len(i)]
      } else {
        res = y[seq_len(i)] - mean_fun(x_obs[seq_len(i)])
        m_post = mean_fun(x) + ks_kinv %*% res
      }
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
          legend.position = "none"
          # axis.text.y = element_blank()
        )
    }
  )
}


# Update prior samples with observed data
plot_prior_updates = function(
    x, x_obs, y, kmat, mean_fun = NULL, n_samples = 20
) {
  
  num_obs = length(x)
  kss = kmat[seq_len(num_obs), seq_len(num_obs)]
  ks = kmat[seq_len(num_obs), (num_obs + 1):nrow(kmat)]
  k = kmat[(num_obs + 1):nrow(kmat), (num_obs + 1):nrow(kmat)]
  # num_obs = length(x_new)
  # k = kmat[seq_len(num_obs), seq_len(num_obs)]
  # ks = kmat[seq_len(num_obs), (num_obs + 1):nrow(kmat)]
  # kss = kmat[(num_obs + 1):nrow(kmat), (num_obs + 1):nrow(kmat)]
  
  lapply(
    seq_along(x_obs),
    function(i) {
      k_inv = solve(k[seq_len(i), seq_len(i)])
      ks_i = as.matrix(ks[, seq_len(i)])
      ks_kinv = ks_i %*% k_inv
      if (is.null(mean_fun)) {
        m_post = ks_kinv %*% y[seq_len(i)]
      } else {
        res = y[seq_len(i)] - mean_fun(x[seq_len(i)])
        m_post = mean_fun(x_obs) + ks_kinv %*% res
      }
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
          data = data.table(x = x_obs[seq_len(i)], y = y[seq_len(i)]), # 1.5 * x_obs[seq_len(i)] + (y[seq_len(i)] - 1.5 * x_obs[seq_len(i)])),
          mapping = aes(x = x, y = y + 3), 
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
x_obs =  c(-1.5, 1/3, 4/3, -0.5)

# PLOTS ------------------------------------------------------------------------

# ggsave(
#     "../figure/gp_sample/zeromean_prior_50n.pdf",
#     plot_priors(
#         kernel_type = "squaredexp", 
#         lengthscale = 1, 
#         x_range = c(-2, 2),
#         n_samples = 50
#     ),
#     height = 4,
#     width = 8
# )
# 
# ggsave(
#     "../figure/gp_sample/zeromean_prior_10n.pdf",
#     plot_priors(
#         kernel_type = "squaredexp", 
#         lengthscale = 1, 
#         x_range = c(-10, 10),
#         n_samples = 10
#     ) + geom_hline(yintercept = 0, linetype = "dashed", color = "black"),
#     height = 2,
#     width = 8
# )
# 
# ggsave(
#   "../figure/gp_sample/sinmean_prior_50n.pdf",
#   plot_priors(
#     kernel_type = "squaredexp", 
#     lengthscale = 1, 
#     x_range = c(-2, 2),
#     n_samples = 50,
#     n_points = 50,
#     mu = 1.5 * seq(-2, 2, length.out = 50)
#   ),
#   height = 4,
#   width = 8
# )
# 
# zeromean_updates = plot_prior_updates(x, x_new, y, kernel_sqexp(x_obs, x, 1))
# invisible(lapply(
#     seq_along(zeromean_updates),
#     function(i) {
#         ggsave(
#             sprintf("../figure/gp_sample/zeromean_prior_updates_%i.pdf", i),
#             zeromean_updates[[i]],
#             width = 6,
#             height = 4
#         
#       )
#     }
# ))
# 
# x = seq(-2, 2, length.out = 50)
# plot_priors(kernel_type = "squaredexp", 
#             lengthscale = 1, 
#             x_range = c(-2, 2),
#             n_samples = 10,
#             n_points = 500,
#             mu = 1.5 * seq(-2, 2, length.out = 500))
plot_prior_updates(
  x, x_obs, y, kernel_sqexp(x_obs, x, 1), mean_fun = function(z) 1.5 * z
)

