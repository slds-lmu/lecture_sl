import numpy as np
import matplotlib.pyplot as plt
from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import Ridge
from sklearn.metrics import mean_squared_error


# Set the random seed for reproducibility
np.random.seed(0)

# Define the true function and the number of datasets
true_function = lambda x: np.sin(x)
n_datasets = 100  # Number of datasets for training
n_samples = 100
n_test_samples = 10000
n_order = 8
lambdas = np.exp(np.linspace(-6, 7, 25))  

# Generate polynomial features
poly = PolynomialFeatures(degree=n_order, include_bias=False)

# Initialize arrays to store the bias, variance, and error
bias_squared = np.zeros_like(lambdas)
variance = np.zeros_like(lambdas)
test_error = np.zeros_like(lambdas)

# Generate shared x values for all datasets
x_shared = np.random.uniform(0, 1, n_samples).reshape(-1, 1)
x_shared_poly = poly.fit_transform(x_shared)

# Generate test data
x_test = np.random.uniform(0, 1, n_test_samples).reshape(-1, 1)
y_test = true_function(x_test).reshape(-1, 1) + np.random.randn(n_test_samples,1)
x_test_poly = poly.transform(x_test)

# Loop over the lambda values
for i, lambda_val in enumerate(lambdas):
    # Initialize arrays to store predictions for each model
    predictions = np.zeros((n_datasets, n_samples))

    # Train and predict with n_datasets models
    for j in range(n_datasets):
        # Generate new y values for each dataset
        epsilon = np.random.randn(n_samples, 1)
        y = true_function(x_shared) + epsilon

        # Fit Ridge regression model
        model = Ridge(alpha=lambda_val, fit_intercept=True)
        model.fit(x_shared_poly, y)
        predictions[j, :] = model.predict(x_shared_poly).flatten()

    # Calculate the average prediction for each x
    average_prediction = np.mean(predictions, axis=0)

    # Compute itegrated bias^2 and variance using MC
    bias_squared[i] = np.mean((average_prediction - true_function(x_shared).flatten()) ** 2)
    variance[i] = np.mean(np.var(predictions, axis=0))

# Train a final model on a new dataset and compute test error for each lambda
for i, lambda_val in enumerate(lambdas):
    # Generate new data for the final model
    x_train_final = np.random.uniform(0, 1, n_samples).reshape(-1, 1)
    y_train_final = true_function(x_train_final) + np.random.randn(n_samples, 1)
    x_train_final_poly = poly.transform(x_train_final)

    # Fit the final model
    model_final = Ridge(alpha=lambda_val, fit_intercept=True)
    model_final.fit(x_train_final_poly, y_train_final)

    # Predict on the test set and compute the error
    y_test_pred_final = model_final.predict(x_test_poly).flatten()
    # The test error
    test_error[i] = mean_squared_error(y_test, y_test_pred_final)

# Plotting the results with two y-axes
fig, ax1 = plt.subplots(figsize=(12, 6))

# Plot bias^2 and variance on the primary y-axis
ax1.plot(np.log(lambdas), bias_squared, label='(bias)^2', color='red')
ax1.plot(np.log(lambdas), variance, label='variance', color='blue')
ax1.plot(np.log(lambdas), bias_squared + variance, label='(bias)^2 + variance', color='green')

ax1.set_xlabel('ln(λ)', fontsize=16)
ax1.set_ylabel('(bias)^2, variance', fontsize=16)
ax1.legend(loc='upper left')

# Create secondary y-axis for test error
ax2 = ax1.twinx()
ax2.plot(np.log(lambdas), test_error, label='test error', color='magenta', linestyle='--', alpha=.6)
ax2.set_ylabel('Test error on single dataset', fontsize=16)
ax2.legend(loc='upper right')

plt.title('Bias-Variance Tradeoff with L2 Regularization', fontsize=20)
plt.show()
