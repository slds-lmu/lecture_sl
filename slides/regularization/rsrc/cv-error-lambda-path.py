from sklearn.datasets import load_wine
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import RidgeCV
import numpy as np
import matplotlib.pyplot as plt

# Load wine dataset
X, y = load_wine(return_X_y=True)

# Generating standard normal noise features
np.random.seed(42)
noise_features = np.random.normal(size=(X.shape[0], 75))

# Adding these noise features to the original dataset
X_extended = np.hstack((X, noise_features))

# Splitting the augmented dataset into training and test sets
X_train_ext, X_test_ext, y_train_ext, y_test_ext = train_test_split(
    X_extended, y, test_size=0.2, random_state=42
)

# Standardizing the augmented dataset
scaler_ext = StandardScaler()
X_train_ext_scaled = scaler_ext.fit_transform(X_train_ext)
X_test_ext_scaled = scaler_ext.transform(X_test_ext)

# Define a range of lambda (alpha) values
lambda_values = np.logspace(-4, 4, 50)

# Performing Ridge Regression with Cross-Validation on the extended dataset
ridge_cv_ext = RidgeCV(alphas=lambda_values, store_cv_values=True)
ridge_cv_ext.fit(X_train_ext_scaled, y_train_ext)

# Plotting the CV Curve for the extended dataset
mean_cv_scores_ext = np.mean(ridge_cv_ext.cv_values_, axis=0)

# Finding the lambda value with the minimum CV score
min_lambda_index = np.argmin(mean_cv_scores_ext)
min_lambda_value = lambda_values[min_lambda_index]

# Re-plotting with a vertical blue bar at the minimum CV score
plt.figure(figsize=(8, 6))
plt.plot(lambda_values, mean_cv_scores_ext, marker='o', color='red')
plt.axvline(x=min_lambda_value, color='blue', linestyle='--', label=f'Min CV Score at λ={min_lambda_value:.4f}')
plt.xscale('log')
plt.xlabel('Lambda (Regularization strength)', fontsize = 14)
plt.ylabel('Generalization error', fontsize = 14)
#plt.title('Wine dataset with add. noise features', fontsize=12)
plt.title('Effect of L2 Regularization', fontsize = 16)
plt.legend()
plt.show()
