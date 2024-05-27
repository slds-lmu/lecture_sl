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

def draw_plot(ax, contour_levels, last_plot=False):
    # Plot contour lines around the objective center if any contour levels are provided
    if contour_levels:
        CS = ax.contour(X, Y, Z_rotated_elliptical, levels=contour_levels, colors='red', linewidths=0.5)
    
    # Plot the constraint circle
    circle = Circle((0, 0), constraint_radius, color='blue', alpha=0.3, linestyle='--')
    ax.add_artist(circle)
    
    # Plot the minimum point in all plots
    ax.plot(objective_center[0], objective_center[1], 'o', color='black', markersize=4)

    # Set the same scale for both axes and set limits
    ax.set_aspect('equal', 'box')
    ax.set_xlim(-3, 3)
    ax.set_ylim(-3, 3)
    ax.axhline(0, color='black', linewidth=0.5)
    ax.axvline(0, color='black', linewidth=0.5)

    # Define the legend elements
    legend_elements = [plt.Line2D([0], [0], color='black', marker='o', linestyle='None', markersize=4, label=r'$\hat{\theta}$')]
    
    # Add the intersection point for the last plot
    if last_plot:
        # Calculate the intersection point
        last_contour = CS.allsegs[-1][0]
        distances = np.sqrt((last_contour[:, 0])**2 + (last_contour[:, 1])**2)
        min_idx = np.argmin(np.abs(distances - constraint_radius))
        intersection_point = last_contour[min_idx]
        ax.plot(intersection_point[0], intersection_point[1], 'o', color='green', markersize=4)
        legend_elements.append(plt.Line2D([0], [0], color='green', marker='o', linestyle='None', markersize=4, label=r'$\hat{\theta}_{ridge}$'))

    # Add the legend to the top-left of the plot
    ax.legend(handles=legend_elements, loc='upper left', fontsize='small', frameon=True, handletextpad=0.2, borderpad=0.1, labelspacing=0.1)

# Rest of your plotting code remains the same


# Create contour levels
first_contour_level = 0.1  # Start with a small contour level
max_contour_level = (constraint_radius**2) * 0.6  # Largest contour level touching the circle

# For each subsequent plot, we add one more contour level, increasing the value
contour_levels_for_plots = [
    [],  # No contour for the first plot
    [first_contour_level],  # One small contour for the second plot
    [first_contour_level, first_contour_level * 3],  # Two contours for the third plot
    [first_contour_level, first_contour_level * 3, max_contour_level]  # Three contours for the last plot
]

# Create a 2x2 grid of plots
fig, axs = plt.subplots(2, 2, figsize=(6, 6), dpi=120)

# Plot for each subplot in the 2x2 grid
for i, ax in enumerate(axs.flatten()):
    last_plot = i == len(axs.flatten()) - 1  # Check if it's the last plot
    draw_plot(ax, contour_levels_for_plots[i], last_plot)

# Adjust layout to prevent overlapping
plt.tight_layout()
plt.show()


def create_diamond(ax, constraint_radius):
    """Create and add a diamond shape for L1 regularization."""
    diamond = plt.Polygon([[-constraint_radius, 0], [0, constraint_radius], [constraint_radius, 0], [0, -constraint_radius]], 
                          closed=True, color='blue', alpha=0.3, linestyle='--')
    ax.add_patch(diamond)


# Elliptical objective function with rotation
def rotated_elliptical_objective(X, Y, center, a, b, angle_deg):
    """ Rotated elliptical objective function. """
    angle_rad = np.radians(angle_deg)
    X_rot = np.cos(angle_rad) * (X - center[0]) - np.sin(angle_rad) * (Y - center[1])
    Y_rot = np.sin(angle_rad) * (X - center[0]) + np.cos(angle_rad) * (Y - center[1])
    return (X_rot**2 / a**2) + (Y_rot**2 / b**2)

# Define elliptical parameters
a, b = 1.5, 0.75  # Semi-major and semi-minor axes lengths
rotation_angle = -10  # Rotation angle in degrees

# Calculate rotated elliptical objective function values
Z_rotated_elliptical = rotated_elliptical_objective(X, Y, objective_center, a, b, rotation_angle)

# Define the constraint circle for ridge regression (L2)
constraint_radius = 1.0  # Example radius

def draw_plot(ax, contour_levels, last_plot=False):
    # Plot contour lines around the objective center if any contour levels are provided
    if contour_levels:
        CS = ax.contour(X, Y, Z_rotated_elliptical, levels=contour_levels, colors='red', linewidths=0.5)
    
    # Plot the diamond shape for L1 regularization
    create_diamond(ax, constraint_radius)
    
    # Plot the minimum point in all plots
    ax.plot(objective_center[0], objective_center[1], 'o', color='black', markersize=4)

    # Set the same scale for both axes and set limits
    ax.set_aspect('equal', 'box')
    ax.set_xlim(-3, 3)
    ax.set_ylim(-3, 3)
    ax.axhline(0, color='black', linewidth=0.5)
    ax.axvline(0, color='black', linewidth=0.5)

    # Define the legend elements
    legend_elements = [plt.Line2D([0], [0], color='black', marker='o', linestyle='None', markersize=4, label=r'$\hat{\theta}$')]
    
    # Add the intersection point for the last plot
   # Add the intersection point for the last plot
    if last_plot:
        # Calculate the intersection point
        last_contour = CS.allsegs[-1][0]
        distances = np.sqrt((last_contour[:, 0])**2 + (last_contour[:, 1])**2)
        min_idx = np.argmin(np.abs(distances - constraint_radius))
        intersection_point = last_contour[min_idx]
        ax.plot(intersection_point[0], intersection_point[1], 'o', color='green', markersize=4)
        legend_elements.append(plt.Line2D([0], [0], color='green', marker='o', linestyle='None', markersize=4, label=r'$\hat{\theta}_{lasso}$'))

    # Add the legend to the top-left of the plot
    ax.legend(handles=legend_elements, loc='upper left', fontsize='small', frameon=True, handletextpad=0.2, borderpad=0.1, labelspacing=0.1)

# Create contour levels
first_contour_level = 0.1  # Start with a small contour level
max_contour_level = (constraint_radius**2) * 1.17  # Largest contour level touching the circle

# For each subsequent plot, we add one more contour level, increasing the value
contour_levels_for_plots = [
    [],  # No contour for the first plot
    [first_contour_level],  # One small contour for the second plot
    [first_contour_level, first_contour_level * 4],  # Two contours for the third plot
    [first_contour_level, first_contour_level * 4, max_contour_level]  # Three contours for the last plot
]

# Create a 2x2 grid of plots
fig, axs = plt.subplots(2, 2, figsize=(6, 6), dpi=120)

# Plot for each subplot in the 2x2 grid
for i, ax in enumerate(axs.flatten()):
    last_plot = i == len(axs.flatten()) - 1  # Check if it's the last plot
    draw_plot(ax, contour_levels_for_plots[i], last_plot)

# Adjust layout to prevent overlapping
plt.tight_layout()
plt.show()


 # Define the center of the objective function and elliptical parameters
objective_center = np.array([1.5, 1.5])
a, b = 1.5, 0.75
rotation_angle = -30

# Elliptical objective function
def rotated_elliptical_objective(X, Y, center, a, b, angle_deg):
    angle_rad = np.radians(angle_deg)
    X_rot = np.cos(angle_rad) * (X - center[0]) - np.sin(angle_rad) * (Y - center[1])
    Y_rot = np.sin(angle_rad) * (X - center[0]) + np.cos(angle_rad) * (Y - center[1])
    return (X_rot**2 / a**2) + (Y_rot**2 / b**2)

Z_rotated_elliptical = rotated_elliptical_objective(X, Y, objective_center, a, b, rotation_angle)

# Define the constraint circle for ridge regression (L2)
constraint_radius = 1.0

# Define contour levels in increasing order
contour_levels = [(constraint_radius**2) * 0.6, (constraint_radius**2) * 1.2, (constraint_radius**2) * 2.4]

def draw_plot(ax, plot_index, last_plot=False):
    # Plot all contours
    CS = ax.contour(X, Y, Z_rotated_elliptical, levels=contour_levels, colors='red', linewidths=0.5)

    # Control visibility of contours based on plot index
    for i, contour in enumerate(CS.collections):
        contour.set_visible(i >= plot_index)

    # Plot the constraint circle
    circle = Circle((0, 0), constraint_radius, color='blue', alpha=0.3, linestyle='--')
    ax.add_artist(circle)

    # Plot the minimum point and set limits
    min_point_handle, = ax.plot(objective_center[0], objective_center[1], 'o', color='black', markersize=4)
    ax.set_aspect('equal', 'box')
    ax.set_xlim(-3, 3)
    ax.set_ylim(-3, 3)

    # Draw coordinate axes
    ax.axhline(0, color='black', linewidth=0.5)
    ax.axvline(0, color='black', linewidth=0.5)

    # Add legend for the minimum point
    if last_plot:
        # Calculate intersection point for the last plot using the last contour segment (smallest contour)
        last_contour = CS.allsegs[-3][0]  # Use the last contour segment
        distances = np.sqrt((last_contour[:, 0])**2 + (last_contour[:, 1])**2)
        min_idx = np.argmin(np.abs(distances - constraint_radius))
        intersection_point = last_contour[min_idx]
        ridge_point_handle, = ax.plot(intersection_point[0], intersection_point[1], 'o', color='green', markersize=4)
        ax.legend([min_point_handle, ridge_point_handle], [r'$\hat{\theta}$', r'$\hat{\theta}_{ridge}$'], loc='upper left', fontsize='small', frameon=True)
    else:
        ax.legend([min_point_handle], [r'$\hat{\theta}$'], loc='upper left', fontsize='small', frameon=True)


# Create a 2x2 grid of plots
fig, axs = plt.subplots(2, 2, figsize=(6, 6), dpi=120)

for i, ax in enumerate(axs.flatten()):
    last_plot = i == len(axs.flatten()) - 1
    draw_plot(ax, 3 - i, last_plot)  # Reverse the order of plots

plt.tight_layout()
plt.show()


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
constraint_radius_large = 1.33  # Larger radius for comparison

# Create contour levels
contour_levels = [0.1, 0.3, 0.6]  # Example contour levels

# Create a 2x1 grid of plots
fig, axs = plt.subplots(1, 2, figsize=(12, 6), dpi=100)

def draw_plot(ax, constraint_radius, contour_levels):
    # Plot contour lines around the objective center
    CS = ax.contour(X, Y, Z_rotated_elliptical, levels=contour_levels, colors='red', linewidths=0.5)
    
    # Plot the constraint circle
    circle = Circle((0, 0), constraint_radius, color='blue', alpha=0.3, linestyle='--')
    ax.add_artist(circle)
    
    # Plot the minimum point
    ax.plot(objective_center[0], objective_center[1], 'o', color='black', markersize=6)

    # Set the same scale for both axes and set limits
    ax.set_aspect('equal', 'box')
    ax.set_xlim(-3, 3)
    ax.set_ylim(-3, 3)
    ax.axhline(0, color='black', linewidth=0.5)
    ax.axvline(0, color='black', linewidth=0.5)

    # Define the legend elements
    legend_elements = [
        plt.Line2D([0], [0], marker='o', color='black', markersize=6, label=r'$\hat{\theta}$', linestyle='None')
    ]
    
    # Calculate and plot the intersection point for the second contour and larger circle if needed
    if constraint_radius == constraint_radius_large:
        last_contour = CS.allsegs[1][0]  # Use the second contour for intersection
        distances = np.sqrt((last_contour[:, 0])**2 + (last_contour[:, 1])**2)
        min_idx = np.argmin(np.abs(distances - constraint_radius))
        intersection_point = last_contour[min_idx]
        ax.plot(intersection_point[0], intersection_point[1], 'o', color='green', markersize=6)
        legend_elements.append(plt.Line2D([0], [0], color='green', marker='o', linestyle='None', markersize=6, label=r'$\hat{\theta}_{ridge}$'))
    else:
        last_contour = CS.allsegs[2][0]  # Use the second contour for intersection
        distances = np.sqrt((last_contour[:, 0])**2 + (last_contour[:, 1])**2)
        min_idx = np.argmin(np.abs(distances - constraint_radius))
        intersection_point = last_contour[min_idx]
        ax.plot(intersection_point[0], intersection_point[1], 'o', color='green', markersize=6)
        legend_elements.append(plt.Line2D([0], [0], color='green', marker='o', linestyle='None', markersize=6, label=r'$\hat{\theta}_{ridge}$')) 

    # Add the legend
    ax.legend(handles=legend_elements, loc='upper left', fontsize='large', frameon=True, handletextpad=0.4, borderpad=0.1, labelspacing=0.1)

# Draw plots
draw_plot(axs[0], constraint_radius, contour_levels)
draw_plot(axs[1], constraint_radius_large, contour_levels)

plt.tight_layout()
plt.show()