# Load required library
library(MASS) # For mvrnorm

# Define the polynomial kernel function
polynomial_kernel <- function(x1, x2, degree = 2, coef0 = 1, scale = 1) {
  return((scale * (x1 %*% t(x2)) + coef0)^degree)
}

# Function to sample from a Gaussian process
sample_gaussian_process <- function(n_samples, x_range, n_points, degree = 2, coef0 = 1, scale = 1, noise = 1e-6) {
  # Generate feature points
  x <- seq(x_range[1], x_range[2], length.out = n_points)
  
  # Compute the covariance matrix using the polynomial kernel
  K <- polynomial_kernel(as.matrix(x), as.matrix(x), degree, coef0, scale)
  
  # Add a small noise term to the diagonal for numerical stability
  K <- K + diag(noise, n_points)
  
  # Sample from the Gaussian process
  samples <- mvrnorm(n_samples, mu = rep(0, n_points), Sigma = K)
  
  # Return the samples and x values
  list(x = x, samples = samples)
}

# Example usage
set.seed(42)
n_samples <- 5
x_range <- c(-2, 2)
n_points <- 100
degree <- 1
coef0 <- 0
scale <- 1

result <- sample_gaussian_process(n_samples, x_range, n_points, degree, coef0, scale)

# Plot the sampled functions
plot(result$x, result$samples[1, ], type = "l", ylim = range(result$samples),
     xlab = "x", ylab = "f(x)", main = "Gaussian Process Samples", col = 1, lty = 1)
for (i in 2:n_samples) {
  lines(result$x, result$samples[i, ], col = i, lty = i)
}
