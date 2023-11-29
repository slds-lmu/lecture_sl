import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from scipy.optimize import minimize

# Data Generation
n = 500
np.random.seed(0)
x1 = np.random.uniform(-1, 1, n)
x2 = np.random.uniform(-1, 1, n)
epsilon = np.random.normal(0, 0.1, n)
y = -0.5 * x1 + 3 * x2 + epsilon

# Regularization Norm Functions
def l1_norm(beta1, beta2):
    return np.abs(beta1) + np.abs(beta2)

def l2_norm_squared(beta1, beta2):
    return beta1**2 + beta2**2

# Updated Regularized Least Squares Objective Function with 1/n factor
def updated_objective(beta, x1, x2, y, lam, regularization):
    beta1, beta2 = beta
    residuals = y - beta1 * x1 - beta2 * x2
    error_term = np.sum(residuals**2) / n
    if regularization == 'l1':
        penalty = l1_norm(beta1, beta2)
    elif regularization == 'l2':
        penalty = l2_norm_squared(beta1, beta2)
    return error_term + lam * penalty

# Compute the Minima for each plot
minima = {}
regularizations = ['l1', 'l2']
lambdas = [0, 1, 10]
for reg in regularizations:
    for lam in lambdas:
        result = minimize(updated_objective, [0, 0], args=(x1, x2, y, lam, reg), method='L-BFGS-B')
        minima[(reg, lam)] = result.x

# Parameter Space for Beta1 and Beta2
beta1_range = np.linspace(-10, 10, 100)
beta2_range = np.linspace(-10, 10, 100)
beta1_grid, beta2_grid = np.meshgrid(beta1_range, beta2_range)

# Plotting
fig, axes = plt.subplots(2, 3, subplot_kw={"projection": "3d"}, figsize=(18, 12))
for i, reg in enumerate(regularizations):
    for j, lam in enumerate(lambdas):
        objective_values = np.array([updated_objective([b1, b2], x1, x2, y, lam, reg) 
                                     for b1, b2 in zip(np.ravel(beta1_grid), np.ravel(beta2_grid))])
        objective_values = objective_values.reshape(beta1_grid.shape)

        ax = axes[i, j]
        ax.plot_surface(beta1_grid, beta2_grid, objective_values, cmap='viridis')
        ax.set_title(f'Regularization: {reg.upper()}, Lambda: {lam}', fontsize=20)  # Increased font size
        ax.set_xlabel('Theta 1', fontsize=14)  # Increased font size
        ax.set_ylabel('Theta 2', fontsize=14)  # Increased font size
        ax.set_zlabel('Emp. risk', fontsize=14)  # Increased font size

        # Add the minima as a red dot
        min_beta1, min_beta2 = minima[(reg, lam)]
        min_val = updated_objective([min_beta1, min_beta2], x1, x2, y, lam, reg)
        ax.scatter(min_beta1, min_beta2, min_val, color='red', s=50)

plt.tight_layout()
plt.subplots_adjust(wspace=0.1, hspace=0.1)  # Adjust spacing between the plots if needed
plt.savefig('..figure/reg_surfaces.png', bbox_inches='tight', pad_inches=0, facecolor='white')