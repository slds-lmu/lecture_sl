# ----------------------------------------------------------------------
# Extrapolation demo with Gaussian Processes
# ----------------------------------------------------------------------
# Purpose: Compare extrapolation behavior of a stationary kernel (Matérn)
#          and a linear kernel when the true data-generating process (DGP)
#          has a linear trend. Training data are sampled from [-2, 2],
#          while predictions are made up to x = 4. The script demonstrates
#          that stationary kernels revert to the prior mean outside the
#          training domain, whereas the linear kernel continues the linear
#          trend. Additional noisy DGP samples are generated beyond the
#          training domain (x in [2, 3.5]) and shown as red points.
# ----------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(viridisLite)

source("covariance_functions.R")  # provides get_kmat and kernels

# ----------------------------------------------------------------------
# Global plotting defaults
# ----------------------------------------------------------------------
AXIS_TITLE_SIZE <- 24
AXIS_TEXT_SIZE  <- 24
TITLE_SIZE      <- 16
POINT_SIZE      <- 2.5
LWIDTH          <- 1.2
PRED_COLOR      <- viridis(5, option = "D")[4] 

# ----------------------------------------------------------------------
# GP posterior with optional centering
# ----------------------------------------------------------------------
gp_posterior <- function(x_train, y_train, x_test,
                         kernel_type, noise_var = 0.1^2,
                         center = TRUE, ...) {
  y_mean <- if (center) mean(y_train) else 0
  y_centered <- y_train - y_mean
  
  K   <- get_kmat(x_train, x_train, kernel_type, ...)
  Ks  <- get_kmat(x_train, x_test,  kernel_type, ...)
  Kss <- get_kmat(x_test,  x_test,  kernel_type, ...)
  Ky  <- K + diag(noise_var, length(x_train))
  
  alpha <- solve(Ky, y_centered)
  m <- crossprod(Ks, alpha) + y_mean
  v <- Kss - crossprod(Ks, solve(Ky, Ks))
  
  data.table(x = x_test, m = as.numeric(m), s = sqrt(pmax(0, diag(v))))
}

# ----------------------------------------------------------------------
# Data
# ----------------------------------------------------------------------
set.seed(1)
n_train <- 40
x_train <- sort(runif(n_train, -2, 2))
a <- 1.2; b <- 0.0; sigma <- 0.15
y_train <- a * x_train + b + rnorm(n_train, 0, sigma)

x_test <- seq(-2, 4, length.out = 400)
f_true <- a * x_test + b

# Noisy DGP points to the right of training domain
x_right <- seq(2, 3.5, length.out = 10)
y_right <- a * x_right + b + rnorm(length(x_right), 0, sigma)
dgp_right <- data.table(x = x_right, y = y_right)

# ----------------------------------------------------------------------
# GP posteriors
# ----------------------------------------------------------------------
post_matern <- gp_posterior(
  x_train, y_train, x_test,
  kernel_type = "matern",
  lengthscale = 2.0, noise_var = 0.15^2
)

post_linear <- gp_posterior(
  x_train, y_train, x_test,
  kernel_type = "linear",
  intercept = 0, noise_var = 0.15^2
)

vline_x <- max(x_train)

# ----------------------------------------------------------------------
# Plot helper
# ----------------------------------------------------------------------
plot_gp <- function(dt_post, title_str) {
  ggplot() +
    geom_ribbon(data = dt_post,
                aes(x = x, ymin = m - 2*s, ymax = m + 2*s),
                alpha = 0.15) +
    geom_line(data = data.table(x = x_test, y = f_true),
              aes(x = x, y = y), linewidth = 1, color = "black") +
    geom_line(data = dt_post,
              aes(x = x, y = m),
              linewidth = 1.2, color = PRED_COLOR) +
    geom_point(data = data.table(x = x_train, y = y_train),
               aes(x = x, y = y), size = POINT_SIZE) +
    geom_point(data = dgp_right,
               aes(x = x, y = y), color = "red", size = POINT_SIZE) +
    geom_vline(xintercept = vline_x,
               linetype = "dashed", linewidth = LWIDTH) +
    annotate("text", x = 5, y = 0.03, label = "outside training domain",
             hjust = 0.9, vjust = -0.0, size = 5) +
    labs(x = "x", y = "f(x)", title = title_str) +
    theme_bw() +
    theme(
      axis.title = element_text(size = AXIS_TITLE_SIZE),
      axis.text  = element_text(size = AXIS_TEXT_SIZE),
      plot.title = element_text(size = TITLE_SIZE, hjust = 0.5)
    )
}

# ----------------------------------------------------------------------
# Create plots
# ----------------------------------------------------------------------
p_se  <- plot_gp(post_matern, "Stationary kernel (Matérn): reverts to 0 outside train domain")
p_lin <- plot_gp(post_linear, "Linear kernel: reverts to linear trend")

print(p_se)
print(p_lin)

ggsave("../figure/plot_extrapol_matern.png", p_se, width = 7, height = 3.5)
ggsave("../figure/plot_extrapol_linear.png",  p_lin, width = 7, height = 3.5)
