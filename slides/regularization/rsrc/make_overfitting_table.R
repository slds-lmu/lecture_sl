# Load necessary libraries
library(MASS)
library(nnet)
library(e1071)
library(caret)
library(xtable)

# Set seed for reproducibility
set.seed(123)

# Load Boston housing dataset
data(Boston)

# Split data into training and testing sets
index <- createDataPartition(Boston$medv, p = 0.7, list = FALSE)
train_set <- Boston[index, ]
test_set <- Boston[-index, ]

# Preprocessing: Center and scale the data
preproc <- preProcess(train_set[, -14], method = c("center", "scale"))
train_set_preprocessed <- predict(preproc, train_set[, -14])
test_set_preprocessed <- predict(preproc, test_set[, -14])

# Add the medv (median value) column back
train_set_preprocessed$medv <- train_set$medv
test_set_preprocessed$medv <- test_set$medv


# Define and train the overparameterized neural network
nn_model <- nnet(medv ~ ., data = train_set_preprocessed, size = 100, linout = TRUE, maxit = 20000, MaxNWts = 10000, decay = 0)

# Define and train the SVM with a radial basis kernel
svm_model <- svm(medv ~ ., data = train_set_preprocessed, kernel = "radial", cost = 1e6, gamma = 10)

# Predictions
nn_pred_train <- predict(nn_model, train_set_preprocessed)
nn_pred_test <- predict(nn_model, test_set_preprocessed)

svm_pred_train <- predict(svm_model, train_set_preprocessed)
svm_pred_test <- predict(svm_model, test_set_preprocessed)

# Calculate Mean Squared Errors
nn_train_error <- mean((nn_pred_train - train_set_preprocessed$medv)^2)
nn_test_error <- mean((nn_pred_test - test_set_preprocessed$medv)^2)

svm_train_error <- mean((svm_pred_train - train_set_preprocessed$medv)^2)
svm_test_error <- mean((svm_pred_test - test_set_preprocessed$medv)^2)

# Create a 2x2 comparison table with rounded results
results <- matrix(round(c(nn_train_error, nn_test_error, svm_train_error, svm_test_error), 2), nrow = 2, byrow = TRUE)
colnames(results) <- c("Neural Network", "SVM")
rownames(results) <- c("Training Error", "Test Error")


# Convert matrix to LaTeX table
latex_table <- xtable(results)

# Print the LaTeX table
print(latex_table, include.rownames = TRUE, include.colnames = TRUE, comment = FALSE)

