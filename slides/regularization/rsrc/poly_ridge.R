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
source("func_ridge_polynomial_reg.R")

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
