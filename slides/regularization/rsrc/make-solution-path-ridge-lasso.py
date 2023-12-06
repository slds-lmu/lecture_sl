# -*- coding: utf-8 -*-
"""
Created on Fri Dec  6 12:36:47 2023

@author: chris
"""

import numpy as np
from matplotlib import pyplot as plt
from sklearn import linear_model

# Cost function definitions
def cost_l2(x, y):
    return x**2 + y**2

def cost_l1(x, y):
    return np.abs(x) + np.abs(y)

def costfunction(X, y, theta):
    m = np.size(y)
    h = X @ theta
    return float((1./(2*m)) * (h - y).T @ (h - y))

def closed_form_reg_solution(X, y, lamda=10): 
    m, n = X.shape
    I = np.eye((n))
    return (np.linalg.inv(X.T @ X + lamda * I) @ X.T @ y)[:, 0]

# Dataset creation and normalization
x = np.linspace(0, 1, 40)
noise = 1 * np.random.uniform(size=40)
y = np.sin(x * 1.5 * np.pi) 
y_noise = (y + noise).reshape(-1, 1) - np.mean(y + noise)
X = np.vstack((x, x**2)).T
X = X / np.linalg.norm(X, axis=0)

# Setup of meshgrid of theta values
xx, yy = np.meshgrid(np.linspace(-2, 17, 100), np.linspace(-17, 3, 100))

# Computing the cost function for each theta combination
zz_l2 = np.array([cost_l2(xi, yi) for xi, yi in zip(np.ravel(xx), np.ravel(yy))]) # L2 function
zz_l1 = np.array([cost_l1(xi, yi) for xi, yi in zip(np.ravel(xx), np.ravel(yy))]) # L1 function
zz_ls = np.array([costfunction(X, y_noise, np.array([t0, t1]).reshape(-1, 1))
                  for t0, t1 in zip(np.ravel(xx), np.ravel(yy))]) # Least square cost function

# Reshaping the cost values    
Z_l2 = zz_l2.reshape(xx.shape)
Z_l1 = zz_l1.reshape(xx.shape)
Z_ls = zz_ls.reshape(xx.shape)

# Calculating the regularization paths
lambda_range_l2 = np.logspace(0, 4, num=100) / 1000
theta_0_list_reg_l2, theta_1_list_reg_l2 = zip(*[closed_form_reg_solution(X, y_noise, l) for l in lambda_range_l2])

lambda_range_l1 = np.logspace(0, 2, num=100) / 1000
theta_0_list_reg_l1, theta_1_list_reg_l1 = zip(*[linear_model.Lasso(alpha=l, fit_intercept=False).fit(X, y_noise).coef_
                                                  for l in lambda_range_l1])

# Plotting the contours and paths with updated aesthetics
fig = plt.figure(figsize=(16, 7))

# L2 regularization plot
ax = fig.add_subplot(1, 2, 1)
ax.contour(xx, yy, Z_l2, levels=[.5, 1.5, 3, 6, 9, 15, 30, 60, 100, 150, 250], colors='cyan')
ax.contour(xx, yy, Z_ls, levels=[.01, .06, .09, .11, .15], cmap='coolwarm')
ax.set_xlabel(r'$\theta_1$', fontsize=18)
ax.set_ylabel(r'$\theta_2$', fontsize=18)
ax.set_title('L2 regularization solution path', fontsize=20)
ax.plot(theta_0_list_reg_l2, theta_1_list_reg_l2, linestyle='none', marker='o', color='red', alpha=.2)

# L1 regularization plot
ax = fig.add_subplot(1, 2, 2)
ax.contour(xx, yy, Z_l1, levels=[.5, 1, 2, 3, 4, 5, 6, 8, 10, 12, 14], colors='cyan')
ax.contour(xx, yy, Z_ls, levels=[.01, .06, .09, .11, .15], cmap='coolwarm')
ax.set_xlabel(r'$\theta_1$', fontsize=18)
ax.set_ylabel(r'$\theta_2$', fontsize=18)
ax.set_title('L1 regularization solution path', fontsize=20)
ax.plot(theta_0_list_reg_l1, theta_1_list_reg_l1, linestyle='none', marker='o', color='red', alpha=.2)

plt.show()

# L2 regularization plot only
fig_l2 = plt.figure(figsize=(8, 7))
ax_l2 = fig_l2.add_subplot(1, 1, 1)

ax_l2.contour(xx, yy, Z_l2, levels=[.5, 1.5, 3, 6, 9, 15, 30, 60, 100, 150, 250], colors='cyan')
ax_l2.contour(xx, yy, Z_ls, levels=[.01, .06, .09, .11, .15], cmap='coolwarm')
ax_l2.set_xlabel(r'$\theta_1$', fontsize=16)
ax_l2.set_ylabel(r'$\theta_2$', fontsize=16)
ax_l2.set_title('L2 regularization solution path', fontsize=17)
ax_l2.plot(theta_0_list_reg_l2, theta_1_list_reg_l2, linestyle='none', marker='o', color='red', alpha=.2)

plt.show()