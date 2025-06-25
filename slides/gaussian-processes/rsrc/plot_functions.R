# ------------------------------------------------------------------------------
# PLOT FUNCTIONS
# ------------------------------------------------------------------------------

# Purpose: implement plotting helpers used throughout GP slides

# PREREQ -----------------------------------------------------------------------

library(checkmate)
library(data.table)
library(mvtnorm)

# FUNCTIONS --------------------------------------------------------------------

# Sample from GP prior
sample_gp_prior = function(
    kernel_type, n_samples, n_points, x_range, eps = 1e-6, mu = NULL, ...
) {
  if (is.null(mu)) mu = rep(0, n_points)
  x = seq(x_range[1], x_range[2], length.out = n_points)
  kmat = get_kmat(x, x, kernel_type, ...)
  kmat = kmat + diag(eps, n_points)
  samples = rmvnorm(n_samples, mean = mu, sigma = kmat)
  dt = as.data.table(t(samples))
  names(dt) = sprintf("sample_%i", seq_len(n_samples))
  dt[, x := x]
}

# Plot random samples from GP prior
plot_priors = function(
    kernel_type, 
    n_samples = 5, 
    n_points = 500, 
    x_range = c(-2, 2), 
    ...
) {
  plot_data = sample_gp_prior(
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

# Compute posterior process parameters for squaredexp kernel
compute_posterior_pred_sqexp = function(x, x_new, y, length_scale, noise) {
  num_obs = length(x)
  kmat = kernel_sqexp(x, x_new, length_scale)
  k = kmat[seq_len(num_obs), seq_len(num_obs)]
  ks = kmat[seq_len(num_obs), (num_obs + 1):nrow(kmat)]
  kss = kmat[(num_obs + 1):nrow(kmat), (num_obs + 1):nrow(kmat)]
  ky = k + diag(rep(noise, length(x)))
  ky_inv = solve(ky)
  m_post = crossprod(ks, ky_inv) %*% y
  k_post = kss - crossprod(ks, ky_inv) %*% ks
  data.table(
    ls = length_scale, 
    x = x_new, 
    m_post = m_post, 
    sd_post = diag(k_post)
  )
}
