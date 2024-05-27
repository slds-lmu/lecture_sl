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

# Define the L2 regularization contour levels
l2_contour_levels = [.5, 1.5, 3, 6, 9, 15, 30, 60, 100, 150, 250]

# Determine which points are inside or outside the L2 regularization contours
inside_points = []
outside_points = []

for theta_0, theta_1 in zip(theta_0_list_reg_l2, theta_1_list_reg_l2):
    cost = cost_l2(theta_0, theta_1)
    if any(cost < level for level in l2_contour_levels):
        inside_points.append((theta_0, theta_1))
    else:
        outside_points.append((theta_0, theta_1))

# Separate the points into x and y coordinates for plotting
inside_x, inside_y = zip(*inside_points)
outside_x, outside_y = zip(*outside_points)

# Plot 1: Points inside the L2 regularization contours
fig_inside, ax_inside = plt.subplots(figsize=(8, 7))
ax_inside.contour(xx, yy, Z_l2, levels=l2_contour_levels, colors='cyan')
ax_inside.contour(xx, yy, Z_ls, levels=[.01, .06, .09, .11, .15], cmap='coolwarm')
ax_inside.scatter(inside_x, inside_y, color='green', marker='o', alpha=.5)  # Points inside
ax_inside.set_xlabel(r'$\theta_1$', fontsize=16)
ax_inside.set_ylabel(r'$\theta_2$', fontsize=16)
ax_inside.set_title('L2 regularization solution path', fontsize=17)

# Plot 2: Points outside the L2 regularization contours
fig_outside, ax_outside = plt.subplots(figsize=(8, 7))
ax_outside.contour(xx, yy, Z_l2, levels=l2_contour_levels, colors='cyan')
ax_outside.contour(xx, yy, Z_ls, levels=[.01, .06, .09, .11, .15], cmap='coolwarm')
ax_outside.scatter(outside_x, outside_y, color='blue', marker='o', alpha=.5)  # Points outside
ax_outside.set_xlabel(r'$\theta_1$', fontsize=16)
ax_outside.set_ylabel(r'$\theta_2$', fontsize=16)
ax_outside.set_title('Solutions outside of L2 regularization', fontsize=17)

plt.show()

# L2 regularization contour levels
l2_levels = [.5, 1.5, 3, 6, 9, 15, 30, 60, 100, 150, 250]

# L1 regularization contour levels
l1_levels = [.5, 1, 2, 3, 4, 5, 6, 8, 10, 12, 14]

# Determine points inside the contours for L2
inside_l2 = [(t0, t1) for t0, t1 in zip(theta_0_list_reg_l2, theta_1_list_reg_l2) if cost_l2(t0, t1) < max(l2_levels)]

# Determine points inside the contours for L1
inside_l1 = [(t0, t1) for t0, t1 in zip(theta_0_list_reg_l1, theta_1_list_reg_l1) if cost_l1(t0, t1) < max(l1_levels)]

fig = plt.figure(figsize=(16, 7))

# L2 Regularization Plot
ax1 = fig.add_subplot(1, 2, 1)
ax1.contour(xx, yy, Z_l2, levels=l2_levels, colors='cyan')
ax1.contour(xx, yy, Z_ls, levels=[.01, .06, .09, .11, .15], cmap='coolwarm')
ax1.scatter(*zip(*inside_l2), color='green', marker='o', alpha=.5)  # Points inside L2
ax1.set_xlabel(r'$\theta_1$', fontsize=18)
ax1.set_ylabel(r'$\theta_2$', fontsize=18)
ax1.set_title('L2 regularization solution path', fontsize=20)

# L1 Regularization Plot
ax2 = fig.add_subplot(1, 2, 2)
ax2.contour(xx, yy, Z_l1, levels=l1_levels, colors='cyan')
ax2.contour(xx, yy, Z_ls, levels=[.01, .06, .09, .11, .15], cmap='coolwarm')
ax2.scatter(*zip(*inside_l1), color='green', marker='o', alpha=.5)  # Points inside L1
ax2.set_xlabel(r'$\theta_1$', fontsize=18)
ax2.set_ylabel(r'$\theta_2$', fontsize=18)
ax2.set_title('L1 regularization solution path', fontsize=20)

plt.show()