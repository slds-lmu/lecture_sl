# -*- coding: utf-8 -*-
"""
Created on Tue Jan 16 22:32:46 2024

@author: chris
"""

import numpy as np
import matplotlib.pyplot as plt

# Define the funs
def f1(x): return x**2
def f2(x): return np.abs(x)
def f3(x): return np.abs(x)**(2/3)
def f4(x): return np.abs(x)**(1/2)
def f0(x): return np.where(np.abs(x)>=0.007, 1, 0)

x_values = np.linspace(-2, 2, 400)

fig, axes = plt.subplots(1, 5, figsize=(15, 4))

# Plot each function
for i, (func, q) in enumerate(zip([f1, f2, f3, f4, f0], ['q=2', 'q=1', 'q=2/3', 'q=1/2', 'q=0'])):
    y_values = func(x_values)
    axes[i].plot(x_values, y_values, color=f'C{i}')
    axes[i].set_title(q, size=16)
    axes[i].set_xlim(-1.2, 1.2)
    axes[i].set_ylim(0, 1.1)
    axes[i].label_outer()
    axes[i].set_xlabel(r'$\theta$', size=20)
    axes[i].set_ylabel('Penalty', size=16)
    axes[i].tick_params(axis='both', which='major', labelsize=10)
    axes[i].grid(False)
    axes[i].spines['top'].set_visible(False)
    axes[i].spines['right'].set_visible(False)

plt.tight_layout()

plt.show()
