# ------------------------------------------------------------------------------
# l2

# FIG: 
#  (1) true and fitted polynomials by OLS regression (degree = 10, overfitted).
#  (2) true and fitted polynomials with different regularization 
#      constant (lambda) by ridge regression (large lambda helps with overfit).

# DATA: y = 5 + 2x + 10x^2 - 2*x^3 (x 40*1 ~Unif) + noise (40*1 ~Normal)
# ------------------------------------------------------------------------------

library(ggplot2)
library(viridis)

set.seed(314259)

theme_set(theme_minimal())

# DATA -------------------------------------------------------------------------

f <- function (x) {
  return (5 + 2 * x + 10 * x^2 - 2 * x^3)
}

x <- runif(40, -2, 5)
y <- f(x) + rnorm(length(x), 0, 10)

x.true <- seq(-2, 5, length.out = 400)
y.true <- f(x.true)
df <- data.frame(x = x.true, y = y.true)

lambda <- 0

lambda.vec <- c(0, 10, 100)

# FUNC -------------------------------------------------------------------------

# calculate ridge coefficients
betaRidge <- function (X, y, lambda)
{
  return (solve(t(X) %*% X + lambda * diag(ncol(X))) %*% (t(X) %*% y))
}

# generate polynomials
baseTrafo <- function (x, degree)
{
  out <- cbind(1, x)
  for (i in seq_len(degree)[-1]) {
    out <- cbind(out, x^i)
  }
  return (out)
}

# generate df with polynomial features, true polynomial values, coefficients
getPolyData <- function(x, y, lambda.vec, base.trafo, ...)
{
  X <- base.trafo(x, ...)
  
  x.pred <- seq(min(x), max(x), length.out = 500)
  X.pred <- base.trafo(x.pred, ...)
  
  df.truth <- data.frame(feature = x, truth = y)
  
  # browser()
  
  df.betas <- matrix(NA, nrow=length(lambda.vec), ncol=ncol(X))
  row.names(df.betas) <- lambda.vec
  
  for(i in 1:length(lambda.vec)){
    df.betas[i,] <- betaRidge(X, y, lambda.vec[i])
  }
  
  df.polys <- lapply(1:length(lambda.vec), function (i) {
    return (data.frame(
      feature = x.pred,
      pred = X.pred %*% df.betas[i,],
      lambda = row.names(df.betas)[i]
    ))
  })
  return (list(polys = df.polys,
               truth = df.truth,
               betas = df.betas))
}

# plot true and fitted polynomials
plotRidge <- function (x, y, lambda.vec, base.trafo, ...)
{
  requireNamespace("ggplot2")
  
  # browser()
  
  res <- getPolyData(x, y, lambda.vec, base.trafo, ...)
  df.polys <- res$polys
  df.truth <- res$truth
  
  plot.df <- df.polys[[1]]
  for (i in seq_along(df.polys)[-1]) {
    plot.df <- rbind(plot.df, df.polys[[i]])
  }
  plot.df$lambda <- as.factor(plot.df$lambda)
  
  gg <- ggplot()
  if (length(lambda.vec) == 1) {
    gg <- gg + geom_line(data = plot.df, aes(x = feature, y = pred, color = lambda), show.legend = FALSE)
  } else {
    gg <- gg + geom_line(data = plot.df, aes(x = feature, y = pred, color = lambda))
  }
  
  return (
    gg +
      geom_point(data = df.truth, mapping = aes(x = feature, y = truth))
  )
}

# PLOTS ------------------------------------------------------------------------

p1 <- plotRidge(x, y, lambda, baseTrafo, degree = 10) +
  geom_line(data = df, aes(x = x, y = y), color = "red", size = 1) +
  xlab("x") + ylab("f(x)") +
  theme(plot.title = element_text(size = 15)) +
  scale_color_viridis(end = 0.9, discrete = TRUE)

# multiple lines
p2 <- plotRidge(x, y, lambda.vec, baseTrafo, degree = 10) +
  geom_line(data = df, aes(x = x, y = y), color = "red", size = 1) +
  xlab("x") + ylab("f(x)") +
  labs(color=expression(lambda)) +
  theme(plot.title = element_text(size = 15)) +
  scale_color_viridis(end = 0.9, discrete = TRUE)

ggsave("../figure/poly_ridge_01.png", plot = p1, width = 6, height = 2)
ggsave("../figure/poly_ridge_02.png", plot = p2, width = 7.5, height = 3)
