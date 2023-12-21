import numpy as np
import matplotlib.pyplot as plt
from sklearn.utils import shuffle

# Set the random seed for reproducibility
np.random.seed(6)

# Function to generate data
def generate_data(n, p):
    X = np.random.normal(0, 1, (n, p))
    true_coef = np.linspace(-1, 1, p)
    noise = np.random.normal(0, 1, n)
    y = X.dot(true_coef) + noise
    return X, y, true_coef

# Function to compute the ridge coefficients analytically
def compute_ridge_path(X, y, alphas):
    coefs = [np.zeros(X.shape[1])]  # Start with a row of zeros
    n, p = X.shape
    for alpha in alphas:
        ridge_coefs = np.linalg.inv(X.T @ X + alpha * np.identity(p)) @ X.T @ y
        coefs.append(ridge_coefs)
    return np.array(coefs)

# Function to compute the optimization trajectory for SGD
def compute_sgd_trajectory(X, y, batch_size, learning_rate, n_iter):
    w = np.zeros(X.shape[1])
    coefs = [w.copy()]  # Start with a row of zeros
    for i in range(n_iter):
        X_shuffled, y_shuffled = shuffle(X, y)
        for j in range(0, n, batch_size):
            X_batch = X_shuffled[j:j+batch_size]
            y_batch = y_shuffled[j:j+batch_size]
            gradient = -2 * X_batch.T @ (y_batch - X_batch @ w) / batch_size
            w -= learning_rate * gradient
        coefs.append(w.copy())
    return np.array(coefs)

# Parameters
n = 100
p = 10
batch_size = 4
learning_rate = 0.01
n_iter = 50
t_values = np.arange(0.001, n_iter + 1)  # Include 0 in t_values for the zero coefficients
alphas = 1/(learning_rate * t_values[0:])  # Exclude 0 to avoid division by zero

# Generate data
X, y, true_coef = generate_data(n, p)

# Compute the regularization path for ridge regression
ridge_coefs = compute_ridge_path(X, y, alphas)

# Compute the optimization trajectory for SGD
sgd_coefs = compute_sgd_trajectory(X, y, batch_size, learning_rate, n_iter)

# Plotting
fig, axs = plt.subplots(1, 2, figsize=(14, 5))
# Regularization path for ridge regression
# Skip the first element (0) in t_values for plotting to match dimensions with ridge_coefs
axs[0].plot(1/alphas, ridge_coefs[1:])
axs[0].set_xlabel('1/(lr * lambda)', fontsize=18)
axs[0].set_ylabel('Parameters', fontsize=18)
axs[0].set_title('Ridge Regression Path', fontsize=22)

# Optimization trajectory for SGD
# Use t_values for x-axis to include the initial zero coefficients
axs[1].plot(t_values, sgd_coefs)
axs[1].set_xlabel('iteration', fontsize=18)
axs[1].set_ylabel('Parameters', fontsize=18)
axs[1].set_title('SGD Trajectory', fontsize=22)

plt.tight_layout()
plt.show()

