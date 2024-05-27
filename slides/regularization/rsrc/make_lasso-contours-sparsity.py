# -*- coding: utf-8 -*-
"""
Created on Fri Dec  1 03:40:27 2023

@author: chris
"""

import matplotlib.pyplot as plt
import numpy as np

# Function to create contour plots
def create_contour_plot(ax, theta_hat, theta_lasso, l1_edge, outermost_point, annotation_positions, subtitle):
    theta1 = np.linspace(-4, 4, 300)
    theta2 = np.linspace(-2, 5, 300)
    Theta1, Theta2 = np.meshgrid(theta1, theta2)

    target_direction = np.array([1, 4]) / np.linalg.norm([1, 4])
    angle = np.arctan2(target_direction[1], target_direction[0]) - np.pi / 18
    rot_matrix = np.array([[np.cos(angle), -np.sin(angle)], 
                           [np.sin(angle), np.cos(angle)]])
    
    scale = np.array([1, 2])
    Z = np.vstack((Theta1.ravel() - theta_hat[0], Theta2.ravel() - theta_hat[1])).T @ rot_matrix
    Z = Z * scale
    Z = Z @ rot_matrix.T
    L = (Z[:, 0])**2 + (Z[:, 1])**2
    L = L.reshape(Theta1.shape)

    outermost_level = (outermost_point[0] - theta_hat[0])**2 + (outermost_point[1] - theta_hat[1])**2

    # Plot the contours
    ax.contour(Theta1, Theta2, L, levels=np.linspace(np.min(L), outermost_level, 5), colors='red')

    # L1 regularization path with adjusted darker blue color
    diamond = plt.Polygon([[l1_edge,0], [0,l1_edge], [-l1_edge,0], [0,-l1_edge]], closed=True, color='cyan', alpha=0.3)  # Medium Blue
    ax.add_patch(diamond)

    # Plot theta_hat and theta_lasso
    ax.plot(*theta_hat, 'ko')
    ax.plot(*theta_lasso, 'ko')

    # Annotations with adjusted sizes
    ax.annotate(r'$\hat{\theta}_{Lasso}$', xy=theta_lasso, xytext=annotation_positions[0],
                 arrowprops=dict(facecolor='black', shrink=0.05, width=0.5, headwidth=3), ha='right', va='bottom', fontsize=35)
    ax.annotate(r'$\hat{\theta}$', xy=theta_hat, xytext=annotation_positions[1],
                 arrowprops=dict(facecolor='black', shrink=0.05, width=0.5, headwidth=3), ha='left', va='bottom', fontsize=35)

    # Axes settings
    ax.set_xlabel(r'$\theta_1$', fontsize=30)
    ax.set_ylabel(r'$\theta_2$', fontsize=30)
    ax.tick_params(axis='both', which='major', labelsize=25)
    ax.axis('equal')
    ax.set_xlim([-4, 4])
    ax.set_ylim([-2, 5])

    # Add subtitle
    ax.set_title(subtitle, fontsize=30)

# Initialize a figure with three subplots
fig, axs = plt.subplots(1, 3, figsize=(24, 8))

# First plot
create_contour_plot(axs[0], theta_hat=[0.5, 3], theta_lasso=[0, 1], l1_edge=1, 
                    outermost_point=[0, 1], annotation_positions=[(-2, 1.1), (2.5, 2)], subtitle=r'$\text{smaller param. }\theta_{1}\text{ is removed}$')

# Second plot with subtitle "small λ"
create_contour_plot(axs[1], theta_hat=[1, 1], theta_lasso=[0.5, 0.5], l1_edge=1, 
                    outermost_point=[0.5, 0.5], annotation_positions=[(-0.5, 2.5), (2, 3)], subtitle='small λ: no sparsity')

# Third plot with subtitle "large λ"
create_contour_plot(axs[2], theta_hat=[1, 1], theta_lasso=[0.5, 0], l1_edge=0.5, 
                    outermost_point=[0.5, 0], annotation_positions=[(-0.5, 2.5), (2, 3)], subtitle='larger λ: sparsity')

plt.tight_layout()
plt.show()
