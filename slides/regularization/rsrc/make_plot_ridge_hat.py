import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Circle

# Define the grid for plotting
x = np.linspace(-3.0, 3.0, 400)
y = np.linspace(-3.0, 3.0, 400)
X, Y = np.meshgrid(x, y)

# Define the center of the objective function
objective_center = np.array([1.5, 1.5])  # Adjust as needed

# Elliptical objective function with rotation
def rotated_elliptical_objective(X, Y, center, a, b, angle_deg):
    """ Rotated elliptical objective function. """
    angle_rad = np.radians(angle_deg)
    X_rot = np.cos(angle_rad) * (X - center[0]) - np.sin(angle_rad) * (Y - center[1])
    Y_rot = np.sin(angle_rad) * (X - center[0]) + np.cos(angle_rad) * (Y - center[1])
    return (X_rot**2 / a**2) + (Y_rot**2 / b**2)

# Define elliptical parameters
a, b = 1.5, 0.75  # Semi-major and semi-minor axes lengths
rotation_angle = -30  # Rotation angle in degrees

# Calculate rotated elliptical objective function values
Z_rotated_elliptical = rotated_elliptical_objective(X, Y, objective_center, a, b, rotation_angle)

# Define the constraint circle for ridge regression (L2)
constraint_radius = 1.0  # Example radius

# Create contour levels
contour_levels = [0.1, 0.3, 0.6]  # Example contour levels

# Create a 2x1 grid of plots
fig, axs = plt.subplots(figsize=(8, 8), dpi=100)

def draw_plot(ax, constraint_radius, contour_levels):
    # Plot contour lines around the objective center
    CS = ax.contour(X, Y, Z_rotated_elliptical, levels=contour_levels, colors='red', linewidths=0.5)
    
    # Plot the constraint circle
    colors = ['cornflowerblue', 'blue', 'navy']
    rads = [1, 1.5, 3]
    for i in range(3):
        circle = Circle((0, 0), constraint_radius/rads[i], color=colors[i], alpha=0.3, linestyle='--')
        ax.add_artist(circle)
    
    # Plot the minimum point
    ax.plot(objective_center[0], objective_center[1], 'o', color='red', markersize=6)
    ax.text(objective_center[0]+0.05, objective_center[1]+0.05, r'$\hat{\theta}$', fontsize=12, color='black')

    # Set the same scale for both axes and set limits
    ax.set_aspect('equal', 'box')
    ax.set_xlim(-1.2, 2.7)
    ax.set_ylim(-1.2, 2.5)
    ax.axis('off')

    # Define the legend elements
    #legend_elements = [
    #    plt.Line2D([0], [0], marker='o', color='black', markersize=6, label=r'$\hat{\theta}$', linestyle='None')
    #]
    
    last_contour = CS.allsegs[2][0]  # Use the second contour for intersection
    distances = np.sqrt((last_contour[:, 0])**2 + (last_contour[:, 1])**2)
    min_idx = np.argmin(np.abs(distances - constraint_radius))
    intersection_point = last_contour[min_idx]
    ax.plot(intersection_point[0], intersection_point[1], 'o', color='green', markersize=6)
    ax.text(intersection_point[0]+0.05, intersection_point[1]+0.05, r'$\hat{\theta}_{ridge}$', fontsize=12, color='black')
    #legend_elements.append(plt.Line2D([0], [0], color='green', marker='o', linestyle='None', markersize=6, label=r'$\hat{\theta}_{ridge}$')) 

    # Add the legend
    #ax.legend(handles=legend_elements, loc='upper left', fontsize='large', frameon=True, handletextpad=0.4, borderpad=0.1, labelspacing=0.1)

    # Add arrows indicating the axes
    ax.arrow(-1.2, 0, 3.6, 0, head_width=0.1, head_length=0.2, fc='black', ec='black')
    ax.text(2.3, -0.1, r'$\theta_1$', fontsize=12, color='black')
    ax.arrow(0, -1.2, 0, 3.4, head_width=0.1, head_length=0.2, fc='black', ec='black')
    ax.text(-0.13, 2.1, r'$\theta_2$', fontsize=12, color='black')

# Draw plots
draw_plot(axs, constraint_radius, contour_levels)

plt.tight_layout()
plt.show()