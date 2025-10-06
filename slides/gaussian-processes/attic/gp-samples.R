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


# Define the cosine kernel function
cosine_kernel <- function(x1, x2, scale = 1) {
  # Compute pairwise differences
  diff_matrix <- outer(x1, x2, "-")
  # Apply cosine kernel: K(x,x') = scale * cos(x - x')
  return(scale * cos(diff_matrix))
}

# Function to sample from a Gaussian process with cosine kernel
sample_cosine_gaussian_process <- function(n_samples, x_range, n_points, scale = 1, noise = 1e-6) {
  # Generate feature points
  x <- seq(x_range[1], x_range[2], length.out = n_points)
  
  # Compute the covariance matrix using the cosine kernel
  K <- cosine_kernel(x, x, scale)
  
  # Show correlation matrix (first few entries)
  R <- cov2cor(K)
  print("Correlation matrix (first 6x6):")
  print(R[1:6, 1:6])
  
  # Add a small noise term to the diagonal for numerical stability
  K <- K + diag(noise, n_points)
  
  # Sample from the Gaussian process
  samples <- mvrnorm(n_samples, mu = rep(0, n_points), Sigma = K)
  
  # Return the samples and x values
  list(x = x, samples = samples)
}

# Alternative: Direct sampling using the feature space decomposition
# This is more efficient and numerically stable
sample_cosine_gp_direct <- function(n_samples, x_range, n_points, scale = 1) {
  # Generate feature points
  x <- seq(x_range[1], x_range[2], length.out = n_points)
  
  # Feature vectors: phi(x) = [cos(x), sin(x)]
  cos_x <- cos(x)
  sin_x <- sin(x)
  
  # Sample coefficients: alpha1, alpha2 ~ N(0, scale)
  alpha1 <- rnorm(n_samples, 0, sqrt(scale))
  alpha2 <- rnorm(n_samples, 0, sqrt(scale))
  
  # Construct samples: f(x) = alpha1 * cos(x) + alpha2 * sin(x)
  samples <- matrix(nrow = n_samples, ncol = n_points)
  for (i in 1:n_samples) {
    samples[i, ] <- alpha1[i] * cos_x + alpha2[i] * sin_x
  }
  
  list(x = x, samples = samples, coefficients = data.frame(alpha1 = alpha1, alpha2 = alpha2))
}

# Example usage
library(MASS)  # for mvrnorm
set.seed(42)
n_samples <- 5
x_range <- c(0, 2*pi)
n_points <- 100
scale <- 1

# Method 1: Full covariance matrix sampling
result1 <- sample_cosine_gaussian_process(n_samples, x_range, n_points, scale)

# Method 2: Direct feature space sampling
result2 <- sample_cosine_gp_direct(n_samples, x_range, n_points, scale)

# Plot comparison
par(mfrow = c(1, 2))

# Plot Method 1
plot(result1$x, result1$samples[1, ], type = "l", ylim = range(result1$samples),
     xlab = "x", ylab = "f(x)", main = "Cosine GP (Full Covariance)", col = 1, lty = 1)
for (i in 2:n_samples) {
  lines(result1$x, result1$samples[i, ], col = i, lty = i)
}

# Plot Method 2
plot(result2$x, result2$samples[1, ], type = "l", ylim = range(result2$samples),
     xlab = "x", ylab = "f(x)", main = "Cosine GP (Direct Sampling)", col = 1, lty = 1)
for (i in 2:n_samples) {
  lines(result2$x, result2$samples[i, ], col = i, lty = i)
}

# Show the coefficients for Method 2
print("Coefficients for direct sampling:")
print(result2$coefficients)

# Verify they give similar results by checking covariance
cat("Sample covariance from Method 1:", cov(result1$samples[1, ], result1$samples[2, ]), "\n")
cat("Sample covariance from Method 2:", cov(result2$samples[1, ], result2$samples[2, ]), "\n")