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

# Plot covariance as bivariate density or multivariate heatmap
plot_cov = function(cov_mat, mu = NULL) {
  
  assert_matrix(
    cov_mat, 
    mode = "numeric", 
    any.missing = FALSE, 
    min.rows = 2, 
    min.cols = 2
  )
  n = nrow(cov_mat)
  if (is.null(mu)) mu = rep(0, n)
  
  if (n == 2) {
    h1_min = mu[1] - 2 * cov_mat[1, 1]
    h1_max = mu[1] + 2 * cov_mat[1, 1]
    h2_min = mu[2] - 2 * cov_mat[2, 2]
    h2_max = mu[2] + 2 * cov_mat[2, 2]
    grid = expand.grid(
      h1 = seq(h1_min, h1_max, length.out = 100), 
      h2 = seq(h2_min, h2_max, length.out = 100)
    )
    probs = cbind(grid, density = dmvnorm(grid, mean = mu, sigma = cov_mat))
    p = ggplot() +
      labs(x = expression(f[1]), y = expression(f[2])) +
      geom_raster(data = probs, aes(x = h1, y = h2, fill = density)) +
      geom_contour(
        data = probs,
        aes(x = h1, y = h2, z = density),
        colour = "white",
        bins = 5
      ) +
      guides(fill = "none") +
      scale_fill_gradientn(
        colours = c(low = "black", high = "white")
      ) +
      theme_bw() +
      theme(panel.grid = element_blank())
  } else {
    p = ggplot() +
      geom_tile(
        data = reshape2::melt(cov_mat), 
        aes(x = Var1, y = Var2, fill = value)
      ) +
      scale_x_reverse() +
      theme_bw() +
      scale_fill_gradientn(
        "covariance", colours = c(low = "white", high = "black")
      ) +
      theme(
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none"
      )
  }
  p
  
}

