#remotes::install_github("schalkdaniel/cpsp")
library(cpsp)
library(ggplot2)
library(patchwork)
library(compboost)

# Custom function to estimate the least squares estimator
myEstimator = function(X, y, penmat = 0, xtx = NULL, xty = NULL) {
  if (! (missing(X) || missing(y))) {
    xtx = crossprod(X)
    xty = crossprod(X, y)
  }
  L = chol(xtx + penmat)
  z = backsolve(L, xty, transpose = TRUE)
  return(as.vector(backsolve(L, z)))
}

# SETUP
set.seed(31415)
nsim = 100

# Sample data:
x = sort(runif(nsim, 0, 10))
y = 2 * sin(x) + rnorm(nsim, 0, 0.5)

# Calculate knots of given x values:
knots = createKnots(values = x, n_knots = 20, degree = 3)

# Create basis using these knots:
basis = createSplineBasis(values = x, degree = 3, knots = knots)
K = penaltyMat(ncol(basis), differences = 2)
penalty = 4

y_lt = 2 + x + y

df_lt = data.frame(x = x, y = y_lt)

ggplot() +
  geom_point(data = df_lt, aes(x = x, y = y)) +
  theme_minimal()


X_linear = cbind(1, x)



### CWB without centering:
dat = data.frame(y = y_lt, x = x)
cboost = Compboost$new(data = dat, target = "y")

cboost$addBaselearner("x", "linear", BaselearnerPolynomial)
cboost$addBaselearner("x", "spline", BaselearnerPSpline, df = 4)

cboost$train(500)

#table(cboost$getSelectedBaselearner())

ggtl = plotBaselearnerTraces(cboost) +
  ggtitle("Without centered nonlinear base learner") +
  labs(fill = "Selected base learner", y = "#Sel / M") +
  ggsci::scale_color_aaas() +
  ggsci::scale_fill_aaas(labels = "Spline")

dl = plotPEUni(cboost, "x")$layers[[1]]$data
dl$y = dl$y + cboost$getCoef()$offset
ggl = ggplot() +
  geom_point(data = dat, aes(x = x, y = y), alpha = 0.5) +
  geom_line(data = dl, aes(x = x, y = y), linewidth = 1.2) +
  ylim(-2.5, 12.5)

gg1 = (ggtl / ggl) + plot_layout(heights = c(1, 4)) & theme_minimal()

### CWB with centering:
cboost_c = Compboost$new(data = dat, target = "y")
cboost_c$addComponents("x", df = 4)
cboost_c$train(500)

table(cboost_c$getSelectedBaselearner())
dc = plotPEUni(cboost_c, "x")$data

cpal = ggsci::pal_aaas()(3)
ggtl_c = plotBaselearnerTraces(cboost_c) +
  ggtitle("With centered nonlinear base learner") +
  labs(fill = "Selected base learner", y = "#Sel / M") +
  scale_color_manual(values = cpal[c(2,1)]) +
  scale_fill_manual(labels = c("Linear", "Centered spline"), values = cpal[c(2,1)])

dlc = plotPEUni(cboost_c, "x")$layers[[1]]$data
dlc$bl = vapply(dlc$bl, function(bn) tail(strsplit(bn, "_")[[1]], 1), character(1), USE.NAMES = FALSE)

lidx = dlc$bl == "linear"
dlc$y[lidx] = dlc$y[lidx] + cboost_c$getCoef()$offset
dlc = rbind(dlc, data.frame(x = dlc$x[lidx], y = dlc$y[lidx] + dlc$y[! lidx], bl = "aggregated"))
dlc$bl = as.factor(dlc$bl)
ggl_c = ggplot() +
  geom_point(data = dat, aes(x = x, y = y), alpha = 0.5) +
  geom_line(data = dlc, aes(x = x, y = y, color = bl), linewidth = 1.2) +
  labs(color = "Base learner") +
  scale_color_manual(values = cpal[c(3,1,2)]) +
  ylim(-2.5, 12.5)

gg2 = (ggtl_c / ggl_c) + plot_layout(heights = c(1, 4)) & theme_minimal()


### Save figures:
ggsave(gg1, filename = "figure/fig-decomp1.png", width = 5, height = 3.5)
ggsave(gg2, filename = "figure/fig-decomp2.png", width = 5, height = 3.5)


if (FALSE) {
# ROTATION:
## src = "
## arma::mat armaQR (arma::mat X) {
##   arma::mat Q;
##   arma::mat R;
##   arma::qr(Q, R ,X);
##
##   return Q;
## }
## "
## Rcpp::cppFunction(code = src, depends = "RcppArmadillo")
## Q = armaQR(t(basis) %*% X_linear)
## rotation_m = Q[, 3:ncol(Q)]
rotation = getSubtractionRotation(basis, X_linear)

K_rotation = t(rotation) %*% K %*% rotation
X_nonlinear = basis %*% rotation

pred_intercept = rep(mean(y_lt), length(x))
pred_full = basis %*% myEstimator(basis, y_lt, penalty * K)

pred_nonlinear = X_nonlinear %*% myEstimator(X_nonlinear, y_lt, penalty * K_rotation)
pred_linear = pred_full - pred_nonlinear - pred_intercept

df_plot = data.frame(
  x = rep(x, 4),
  y = c(pred_intercept, pred_linear, pred_nonlinear, pred_full),
  type = rep(c("Intercept", "Linear effect", "Non linear effect", "Full effect"), each = length(x)))

ggplot() +
  geom_point(data = data.frame(x = x, y = y_lt), aes(x = x, y = y)) +
  geom_line(data = df_plot, aes(x = x, y = y, color = type)) +
  theme_minimal() +
  ggsci::scale_color_aaas()

}
