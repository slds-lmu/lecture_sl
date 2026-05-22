# -*- coding: utf-8 -*-
"""
Created on Tue Dec 12 17:55:57 2023

@author: chris
"""

from keras.datasets import cifar10
import matplotlib.pyplot as plt
import numpy as np
from tensorflow.keras.preprocessing.image import ImageDataGenerator

# Load CIFAR-10 dataset
(x_train, y_train), (_, _) = cifar10.load_data()

# Selecting a random dog image
# In CIFAR-10, the label for dogs is 5
dog_indices = np.where(y_train == 3)[0] # cat is 3
random_index = np.random.choice(dog_indices)
dog_image = x_train[random_index]

# Data augmentation techniques
datagen = ImageDataGenerator(
    rotation_range=20,
    width_shift_range=0.2,
    height_shift_range=0.2,
    shear_range=0.2,
    zoom_range=0.2,
    horizontal_flip=True,
    fill_mode='nearest'
)

# Preparing the image for augmentation
dog_image = dog_image.reshape((1,) + dog_image.shape)

# Applying the augmentation and plotting
fig, axs = plt.subplots(1, 5, figsize=(15, 3))
axs[0].imshow(dog_image[0])
axs[0].axis('off')
axs[0].set_title("Original", fontsize=20)

# Generate 4 augmented images
i = 1
for batch in datagen.flow(dog_image, batch_size=1):
    axs[i].imshow(batch[0].astype('uint8'))
    axs[i].axis('off')
    axs[i].set_title(f"Augmented {i}", fontsize=20)
    i += 1
    if i > 4:
        break

plt.tight_layout()
plt.show()
