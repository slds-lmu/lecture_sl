# Define the quadratic function
quadratic_function <- function(x, a, theta_hat, lambda, c) {
  return(a * x^2 - theta_hat * x + lambda *abs(x)+ c)
}

# Set up the coefficients (customizable)
theta_hat <- 1   # Coefficient for x
c <- 0    # Constant term
lambda <- 3

plot_fun <- function(theta_hat, lambda, c) {
  # Define the range of x values
  x_values <- seq(-10, 10, by = 0.1)
  
  # Compute the y values using the quadratic function
  y_values <- quadratic_function(x_values, 0.5, theta_hat, lambda, c)
  
  # Plot the function
  plot(x_values, y_values, type = "l", col = "blue", lwd = 2,
       main = c(paste("theta_hat =", theta_hat,", lambda =", lambda)),
       xlab = "x", ylab = "y",
       xlim = c(-10, 10), ylim = range(y_values))
  # Add a grid for better visualization
  grid()
}

plot_fun(theta_hat, lambda, c)
abline(v = 0)
  
# Set up the coefficients (customizable)
theta_hat <- 8
c <- 0    
lambda <- 3
plot_fun(theta_hat, lambda, c)
abline(v = theta_hat - lambda)