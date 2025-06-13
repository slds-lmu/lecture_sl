# ------------------------------------------------------------------------------
# COVARIANCE FUNCTIONS
# ------------------------------------------------------------------------------

# Purpose: implement covariance functions used throughout GP slides

# PREREQ -----------------------------------------------------------------------

library(checkmate)
library(data.table)
supported_kernels = c(
    "constant",
    "linear",
    "polynomial",
    "periodic",
    "matern",
    "exponential",
    "squaredexp"
)

# FUNCTIONS --------------------------------------------------------------------

# Different kernels

kernel_constant = function(x1, x2, intercept = 0) {
    matrix(intercept, length(x1), length(x2))
}

kernel_linear = function(x1, x2, intercept = 0) intercept + tcrossprod(x1, x2)

kernel_polynomial = function(x1, x2, intercept = 0, degree, scale = 1) {
    (intercept + scale * tcrossprod(x1, x2))**degree
}

kernel_periodic = function(x1, x2, period, lengthscale) {
    dist_mat = outer(x1, x2, function(i, j) abs(i - j))
    kmat = exp((-2 * sin(pi * dist_mat / period)**2) / lengthscale**2)
    kmat[seq_along(x1), seq_along(x2)]
}

kernel_matern = function(x1, x2, nu = 1.5, lengthscale = 1, sigma2 = 1) {
    dist_mat = outer(x1, x2, function(i, j) abs(i - j))
    dist_mat[dist_mat == 0] <- .Machine$double.eps
    scale = sqrt(2 * nu) * dist_mat / lengthscale
    kmat = sigma2 * (2**(1 - nu) / gamma(nu)) * scale**nu * besselK(scale, nu)
    # Fill diagonal manually for zero distances
    diag_idx <- which(outer(x1, x2, `==`), arr.ind = TRUE)
    for (i in seq_len(nrow(diag_idx))) {
        kmat[diag_idx[i, 1], diag_idx[i, 2]] = sigma2
    }
    kmat
}

kernel_exp = function(x1, x2, lengthscale = 0.1) {
    dist_mat = outer(x1, x2, function(i, j) abs(i - j))
    kmat = exp(- dist_mat / lengthscale)
    kmat[seq_along(x1), seq_along(x2)]
}

kernel_sqexp = function(x1, x2, lengthscale = 0.1) {
    dist_mat = as.matrix(dist(c(x1, x2), method = "euclidean"))
    kmat = exp(-0.5 * dist_mat**2 / lengthscale**2)
    kmat[seq_along(x1), seq_along(x2)]
}

# Kernel factory

get_kmat = function(x1, x2, kernel_type, ...) {
    assert_choice(kernel_type, supported_kernels)
    switch(
        kernel_type,
        constant = kernel_constant(x1, x2, ...),
        linear = kernel_linear(x1, x2, ...),
        polynomial = kernel_polynomial(x1, x2, ...),
        periodic = kernel_periodic(x1, x2, ...),
        matern = kernel_matern(x1, x2, ...),
        exponential = kernel_exp(x1, x2, ...),
        squaredexp = kernel_sqexp(x1, x2, ...)
    )
}