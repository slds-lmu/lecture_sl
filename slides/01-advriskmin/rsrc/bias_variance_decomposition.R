# ------------------------------------------------------------------------------
# bias-var-decomposition simulation plots (generates range of figures)
# computes the components of the bias variance decomposition for models with
# different complexities (linear regression and polynomial regression)
# ------------------------------------------------------------------------------

set.seed(1)

calculate_polynomial <- function(coef,poly_grade,x){
    # Calculate the polynomial using the coeffficients
    y <- coef[1]
    for (i in 2:(poly_grade+1)){
        y <- y + coef[i] * x^(i-1)
    }
    return(y)
}

plot_polynomial <- function(coef,poly_grade,color="blue",lwd=2,lty=2){
    # Plot the polynomial using the coeffficients
    x <- seq(-3, 3, length.out = 100)
    y <- calculate_polynomial(coef,poly_grade,x)
    # 
    lines(x, y, col = color, lwd=lwd,lty=lty)
}

train_model <- function(training_lenght,X_train,Y_train,poly_grade=1,plot_points = FALSE){
    # Bootstrap the training data
    sample_index <- sample(1:training_length, training_length, replace = TRUE)
    X_train_sample <- X_train[sample_index]
    Y_train_sample <- Y_train[sample_index]
    # Make a dashed line with the model if the flag is true
    if (plot_points){
        plot(X_train_sample, Y_train_sample, col = "blue", pch = 19, cex = 1.2, xlim = c(-3, 3), ylim = c(-2, 10), xlab = "x",ylab = "y")
    }
    model = lm(Y_train_sample ~ poly(X_train_sample, poly_grade,raw=TRUE))
    if (poly_grade == 1){
        abline(model,col=rgb(0, 0, 1, 0.6), lwd = 2, lty=2)
    }
    if (poly_grade > 1){
        plot_polynomial(coef(model), poly_grade, color=rgb(0, 0, 1, 0.6))
        }
    return(coef(model))
}

save_plot <- function(path){
png(path, width = 960, height = 960, res=150)
}

data_length=40
training_fraction <- 0.80
training_length <- round(training_fraction * data_length)
number_of_models=10

X <- seq(-3, 3, length.out = data_length)
# Apply a polynomial transformation to X and add noise
error_std <- 1
Y_noiseless <-  X + 0.5 * X^2
Y <- Y_noiseless+ rnorm(data_length, mean = 0, sd = error_std)

# Divide the data into training and test
train_index <- sample(1:data_length, training_length )
X_train <- X[train_index]
Y_train <- Y[train_index]
X_test <- X[-train_index]
Y_test <- Y[-train_index]
Y_true_test <- Y_noiseless[-train_index]

### 1st plot: Plot of train and test data
save_plot("../figure/bias_variance_decomposition-train_test.png")
plot(X_train, Y_train, col = "blue", pch = 19, cex = 1.2, xlim = c(-3, 3), ylim = c(-2, 10),xlab = "x",ylab = "y")
points(X[-train_index], Y[-train_index], col = "orange", pch = 17, cex = 1.5)
# add a legend
legend("topright", legend = c("Train", "Test"), col = c("blue", "orange"), pch = c(19,17), pt.cex = c(1.2, 1.5), cex=1.2)
lines(X, Y_noiseless, col = "black", lwd = 2)
dev.off()

### 2nd plot: Explain the sampling with replacement ( bootstrap), show it two times
save_plot("../figure/bias_variance_decomposition-bootstrap_2.png")
train_model(training_length,X_train,Y_train,1,TRUE)
dev.off()
save_plot("../figure/bias_variance_decomposition-bootstrap_1.png")
train_model(training_length,X_train,Y_train,1,TRUE)
dev.off()


### 3th plot: Train N models using different samples from the training data and show them, also show the average model
coef_list <- list()
save_plot("../figure/bias_variance_decomposition-linear_model.png")
plot(X_train, Y_train, col = "blue", pch = 19, cex = 1.2, xlim = c(-3, 3), ylim = c(-2, 10),xlab = "x",ylab = "y")
for (i in 1:number_of_models) {
  coef_list[[i]] <- train_model(training_length,X_train,Y_train,1,FALSE)

}
coef_df <- do.call(rbind, coef_list)
coef_df_mean <- apply(coef_df, 2, mean)
abline(coef_df_mean, col = "black", lwd = 4)
dev.off()


### 4th plot: Plot test points, real model, average model and show the bias
save_plot("../figure/bias_variance_decomposition-linear_model_bias.png")
bias = mean(((coef_df_mean[1] + coef_df_mean[2] * X_test) - ( X_test + 0.5 * X_test^2))^2)
plot(X_test, Y_test, col = "orange", pch = 17, cex = 1.5, xlim = c(-3, 3), ylim = c(-2, 10),xlab = "x",ylab = "y")
lines(X, Y_noiseless, col = "black", lwd = 2)
abline(coef_df_mean, col=rgb(0, 0, 1, 0.6), lwd = 2, lty=2)
segments(X_test,Y_true_test, X_test, calculate_polynomial(coef_df_mean,1,X_test), col = "blue", lwd = 3, lty = 'solid')
# calculate the mean bias with respect to the real model in the test data
text(0, 9.5, paste("Bias: ",round(bias,3)), cex = 3)
dev.off()

### 5th plot: Plot test points, real model, and the predictions of each trained model
save_plot("../figure/bias_variance_decomposition-linear_model_variance.png")
Y_test_predicted <- matrix(0, nrow = number_of_models, ncol = length(X_test))
for (i in 1:number_of_models) {
  Y_test_predicted[i,] <-  coef_list[[i]][1] + coef_list[[i]][2] * X_test
}
prediction_means <- apply(Y_test_predicted, 2, mean)
prediction_vars <- apply(Y_test_predicted, 2, var)
prediction_min <- apply(Y_test_predicted, 2, min)
prediction_max <- apply(Y_test_predicted, 2, max)
plot(X_test, Y_test, col = "orange", pch = 17, cex = 1.5,
     xlim = c(-3, 3), ylim = c(-2, 10), xlab = "x", ylab = "y")
lines(X, Y_noiseless, col = "black", lwd = 2)
for (i in 1:number_of_models) {
  plot_polynomial(coef_list[[i]], 1, col = rgb(0, 0, 1, 0.6), lwd = 1, lty = 3)
}
for (j in 1:length(X_test)) {
  segments(X_test[j], prediction_min[j], X_test[j], prediction_max[j],
           col = "blue", lty = 'solid', lwd = 1.5)
}
arrows(X_test,
       prediction_means - prediction_vars,
       X_test,
       prediction_means + prediction_vars,
       angle = 90, code = 3, length = 0.08,
       col = "black", lwd = 2)
points(X_test, prediction_means, col = "darkgreen", pch = 19, cex = 1)
text(0, 9.5, paste("Variance: ", round(mean(prediction_vars), 3)), cex = 3)
dev.off()


### 6th plot: MSE
save_plot("../figure/bias_variance_decomposition-linear_model_mse.png")

Y_pred_mean <- calculate_polynomial(coef_df_mean, 1, X_test)
bias_squared <- (Y_pred_mean - Y_true_test)^2
variance <- prediction_vars
noise <- rep(error_std^2, length(X_test))

# MSE = Bias² + Variance + Noise
mse <- bias_squared + variance + noise
mse_total <- mean(mse)

plot(X_test, Y_test, col = "orange", pch = 17, cex = 1.5,
     xlim = c(-3, 3), ylim = c(-2, 10),
     xlab = "x", ylab = "y")
lines(X, Y_noiseless, col = "black", lwd = 2)
abline(coef_df_mean, col = rgb(0, 0, 1, 0.6), lwd = 2, lty=2)
segments(X_test, Y_true_test, X_test, Y_pred_mean,
         col = "blue", lwd = 2) # bias
prediction_min <- apply(Y_test_predicted, 2, min)
prediction_max <- apply(Y_test_predicted, 2, max)

for (j in 1:length(X_test)) {
  segments(X_test[j], prediction_min[j], X_test[j], prediction_max[j],
           col = rgb(0,0,1,0.4), lty = 'solid', lwd = 6)
} # var
points(X_test, Y_pred_mean, col = "darkgreen", pch = 19, cex = 1)
text(0, 9.5, paste("MSE: ", round(mse_total, 3)), cex = 3)

dev.off()


###### Now, a much more complex model ######
grade=7
### 7th plot: Train N models using different samples from the training data and show them, also show the average model
coef_list <- list()
save_plot("../figure/bias_variance_decomposition-complex_model.png")
plot(X_train, Y_train, col = "blue", pch = 19, cex = 1.2, xlim = c(-3, 3), ylim = c(-2, 10),xlab = "x",ylab = "y")
for (i in 1:number_of_models) {
  coef_list[[i]] <- train_model(training_length,X_train,Y_train,grade,FALSE)
}
coef_df <- do.call(rbind, coef_list)
coef_df_mean <- apply(coef_df, 2, mean)
plot_polynomial(coef_df_mean,grade,"black",4,1)
dev.off()

### 8th plot: Plot test points, real model, average model and show the bias
save_plot("../figure/bias_variance_decomposition-complex_model_bias.png")
bias=mean(((calculate_polynomial(coef_df_mean,grade,X_test)) - (X_test + 0.5 * X_test^2))^2)
plot(X_test, Y_test, col = "orange", pch = 17, cex = 1.5,xlim = c(-3, 3), ylim = c(-2, 10),xlab = "x",ylab = "y")
lines(X, Y_noiseless, col = "black", lwd = 2)
plot_polynomial(coef_df_mean,grade,rgb(0, 0, 1, 0.6),2,2)
segments(X_test,Y_true_test, X_test, calculate_polynomial(coef_df_mean,grade,X_test), col = "blue", lwd = 3, lty = 'solid')
text(0, 9.5, paste("Bias: ",round(bias,3)), cex = 3)
dev.off()


### 9th plot: Plot test points, real model, and the predictions of each trained model
save_plot("../figure/bias_variance_decomposition-complex_model_variance.png")
Y_test_predicted <- matrix(0, nrow = number_of_models, ncol = length(X_test))
for (i in 1:number_of_models) {
  Y_test_predicted[i,] <-  calculate_polynomial(coef_list[[i]],grade,X_test)
}
prediction_means <- apply(Y_test_predicted, 2, mean)
prediction_vars <- apply(Y_test_predicted, 2, var)
prediction_min <- apply(Y_test_predicted, 2, min)
prediction_max <- apply(Y_test_predicted, 2, max)
plot(X_test, Y_test, col = "orange", pch = 17, cex = 1.5,
     xlim = c(-3, 3), ylim = c(-2, 10), xlab = "x", ylab = "y")
lines(X, Y_noiseless, col = "black", lwd = 2)
for (i in 1:number_of_models) {
  plot_polynomial(coef_list[[i]], grade, col = rgb(0, 0, 1, 0.6), lwd = 1, lty = 3)
}
for (j in 1:length(X_test)) {
  segments(X_test[j], prediction_min[j], X_test[j], prediction_max[j],
           col = "blue", lty = 'solid', lwd = 1.5)
}
arrows(X_test,
       prediction_means - prediction_vars,
       X_test,
       prediction_means + prediction_vars,
       angle = 90, code = 3, length = 0.08,
       col = "black", lwd = 2)
points(X_test, prediction_means, col = "darkgreen", pch = 19, cex = 1)
text(0, 9.5, paste("Variance: ", round(mean(prediction_vars), 3)), cex = 3)
dev.off()


### 10th plot: MSE
save_plot("../figure/bias_variance_decomposition-complex_model_mse.png")

Y_pred_mean <- calculate_polynomial(coef_df_mean, grade, X_test)
bias_squared <- (Y_pred_mean - Y_true_test)^2
variance <- prediction_vars
noise <- rep(error_std^2, length(X_test))

# MSE = Bias² + Variance + Noise
mse <- bias_squared + variance + noise
mse_total <- mean(mse)

plot(X_test, Y_test, col = "orange", pch = 17, cex = 1.5,
     xlim = c(-3, 3), ylim = c(-2, 10),
     xlab = "x", ylab = "y")
lines(X, Y_noiseless, col = "black", lwd = 2)
plot_polynomial(coef_df_mean, grade, color = rgb(0, 0, 1, 0.6), lwd = 2, lty = 2)
segments(X_test, Y_true_test, X_test, Y_pred_mean,
         col = "blue", lwd = 2) # bias
prediction_min <- apply(Y_test_predicted, 2, min)
prediction_max <- apply(Y_test_predicted, 2, max)

for (j in 1:length(X_test)) {
  segments(X_test[j], prediction_min[j], X_test[j], prediction_max[j],
           col = rgb(0,0,1,0.4), lty = 'solid', lwd = 6)
} # var
points(X_test, Y_pred_mean, col = "darkgreen", pch = 19, cex = 1)
text(0, 9.5, paste("MSE: ", round(mse_total, 3)), cex = 3)

dev.off()

###### Now, the correct model ######
grade=2

### 11th plot: Train N models using different samples from the training data and show them, also show the average model
coef_list <- list()
save_plot("../figure/bias_variance_decomposition-correct_model.png")
plot(X_train, Y_train, col = "blue", pch = 19, cex = 1.2, xlim = c(-3, 3), ylim = c(-2, 10),xlab = "x",ylab = "y")
for (i in 1:number_of_models) {
  coef_list[[i]] <- train_model(training_length,X_train,Y_train,grade,FALSE)
}
coef_df <- do.call(rbind, coef_list)
coef_df_mean <- apply(coef_df, 2, mean)
plot_polynomial(coef_df_mean,grade,"black",4,1)
dev.off()

### 12th plot: Plot test points, real model, average model and show the bias
save_plot("../figure/bias_variance_decomposition-correct_model_bias.png")
bias=mean(((calculate_polynomial(coef_df_mean,grade,X_test)) - (X_test + 0.5 * X_test^2))^2)
plot(X_test, Y_test, col = "orange", pch = 17, cex = 1.5,xlim = c(-3, 3), ylim = c(-2, 10),xlab = "x",ylab = "y")
lines(X, Y_noiseless, col = "black", lwd = 2)
plot_polynomial(coef_df_mean,grade,rgb(0,0,1,0.6),2,2)
segments(X_test,Y_true_test, X_test, calculate_polynomial(coef_df_mean,grade,X_test), col = "blue", lwd = 3, lty = 'solid')
text(0, 9.5, paste("Bias: ",round(bias,3)), cex = 3)
dev.off()

### 13th plot: Plot test points, real model, and the predictions of each trained model
save_plot("../figure/bias_variance_decomposition-correct_model_variance.png")
Y_test_predicted <- matrix(0, nrow = number_of_models, ncol = length(X_test))
for (i in 1:number_of_models) {
  Y_test_predicted[i,] <-  calculate_polynomial(coef_list[[i]],grade,X_test)
}
prediction_means <- apply(Y_test_predicted, 2, mean)
prediction_vars <- apply(Y_test_predicted, 2, var)
prediction_min <- apply(Y_test_predicted, 2, min)
prediction_max <- apply(Y_test_predicted, 2, max)
plot(X_test, Y_test, col = "orange", pch = 17, cex = 1.5,
     xlim = c(-3, 3), ylim = c(-2, 10), xlab = "x", ylab = "y")
lines(X, Y_noiseless, col = "black", lwd = 2)
for (i in 1:number_of_models) {
  plot_polynomial(coef_list[[i]], grade, col = rgb(0, 0, 1, 0.6), lwd = 1, lty = 3)
}
for (j in 1:length(X_test)) {
  segments(X_test[j], prediction_min[j], X_test[j], prediction_max[j],
           col = "blue", lty = 'solid', lwd = 1.5)
}
arrows(X_test,
       prediction_means - prediction_vars,
       X_test,
       prediction_means + prediction_vars,
       angle = 90, code = 3, length = 0.08,
       col = "black", lwd = 2)
points(X_test, prediction_means, col = "darkgreen", pch = 19, cex = 1)
text(0, 9.5, paste("Variance: ", round(mean(prediction_vars), 3)), cex = 3)
dev.off()



### 14th plot: MSE visualization for correct model (grade = 2)

save_plot("../figure/bias_variance_decomposition-correct_model_mse.png")

Y_pred_mean <- calculate_polynomial(coef_df_mean, grade, X_test)

bias_squared <- (Y_pred_mean - Y_true_test)^2
variance <- prediction_vars
noise <- rep(error_std^2, length(X_test))

mse <- bias_squared + variance + noise
mse_total <- mean(mse)

plot(X_test, Y_test, col = "orange", pch = 17, cex = 1.5,
     xlim = c(-3, 3), ylim = c(-2, 10),
     xlab = "x", ylab = "y")

lines(X, Y_noiseless, col = "black", lwd = 2)

plot_polynomial(coef_df_mean, grade, color = rgb(0, 0, 1, 0.6), lwd = 2, lty = 2)
segments(X_test, Y_true_test, X_test, Y_pred_mean,
         col = "blue", lwd = 2) #bias
prediction_min <- apply(Y_test_predicted, 2, min)
prediction_max <- apply(Y_test_predicted, 2, max)
for (j in 1:length(X_test)) {
  segments(X_test[j], prediction_min[j], X_test[j], prediction_max[j],
           col = rgb(0, 0, 1, 0.4), lty = 'solid', lwd = 6)
} #var

points(X_test, Y_pred_mean, col = "darkgreen", pch = 19, cex = 1)
text(0, 9.5, paste("MSE: ", round(mse_total, 3)), cex = 3)
dev.off()

