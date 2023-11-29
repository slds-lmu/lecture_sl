import matplotlib.pyplot as plt
import numpy as np

# Binary Cross-Entropy Loss function for true value y and predicted probability p
def binary_cross_entropy(y, p):
    return -(y * np.log(p) + (1 - y) * np.log(1 - p))

# Predicted probabilities
p = np.linspace(0.01, 0.99, 100)  # Avoiding the extreme values 0 and 1 for numerical stability

# Calculate the loss for true values 0 and 1
loss_for_1 = binary_cross_entropy(1, p)
loss_for_0 = binary_cross_entropy(0, p)

# Plotting
plt.figure(figsize=(10, 6))
plt.plot(p, loss_for_1, label='True value: 1')
plt.plot(p, loss_for_0, label='True value: 0', color='orange')
plt.title('Binary Cross-Entropy Loss')
plt.xlabel('p')
plt.ylabel('Binary Cross-Entropy Loss')
plt.legend()
plt.grid(True)
plt.show()