# This script produces plots for the bias-variance decomposition simulation chunk
# It simulates independent draws from y(x) = x + 0.5 * x^2 + N(0,1), x ~ U(-3,3)
# And fits polynomial regression models of degree 1,2, and 7 and computes bias^2, variance, and MSE


library(ggplot2)
library(dplyr)

# Core definitions
generate_dataset <- function(n, error_std = 1) {
  x <- runif(n, -3, 3)
  y_true <- x + 0.5 * x^2
  y <- y_true + rnorm(n, sd = error_std)
  list(x = x, y = y, y_true = y_true)
}

calculate_polynomial <- function(coef, poly_grade, x) {
  y <- coef[1]
  for (i in 2:(poly_grade + 1)) {
    y <- y + coef[i] * x^(i - 1)
  }
  return(y)
}

train_model <- function(x, y, poly_grade) {
  coef(lm(y ~ poly(x, poly_grade, raw = TRUE)))
}

# Main simulation and plotting function
simulate_and_plot <- function(poly_degree, test_data, n_models, train_size, error_std, prefix_path) {
  X_test <- test_data$x
  Y_true_test <- test_data$y_true
  Y_test <- test_data$y
  x_plot <- seq(-3, 3, length.out = 100)
  
  # Train models
  models <- replicate(n_models, {
    d <- generate_dataset(train_size, error_std)
    list(coef = train_model(d$x, d$y, poly_degree))
  }, simplify = FALSE)
  
  coef_mat <- do.call(rbind, lapply(models, `[[`, "coef"))
  coef_mean <- colMeans(coef_mat)
  
  # Compute predictions on test points
  preds <- t(apply(coef_mat, 1, function(cf) calculate_polynomial(cf, poly_degree, X_test)))
  pred_mean <- calculate_polynomial(coef_mean, poly_degree, X_test)
  pred_var <- apply(preds, 2, var)
  pred_min <- apply(preds, 2, min)
  pred_max <- apply(preds, 2, max)
  
  # Stats
  bias_sq <- (pred_mean - Y_true_test)^2
  mse <- bias_sq + pred_var + error_std^2
  mse_total <- mean(mse)
  bias_total <- mean(bias_sq)
  var_total <- mean(pred_var)
  
  # Dataframes for plotting
  df_test <- data.frame(
    x = X_test,
    y_true = Y_true_test,
    y_pred = pred_mean,
    y_obs = Y_test,
    var = pred_var,
    y_min = pred_min,
    y_max = pred_max
  )
  
  df_all_models <- data.frame(
    x = rep(x_plot, n_models),
    model = factor(rep(1:n_models, each = length(x_plot))),
    y = unlist(lapply(models, function(m) calculate_polynomial(m$coef, poly_degree, x_plot)))
  )
  
  df_mean_model <- data.frame(
    x = x_plot,
    y = calculate_polynomial(coef_mean, poly_degree, x_plot)
  )
  
  
  # 1: Fits 
  g <- ggplot(df_test, aes(x = x)) +
    #geom_point(aes(y = y_obs), shape = 17, color = "orange", size = 2) +
    geom_line(data = df_all_models, aes(x = x, y = y, group = model), color = "darkgrey", alpha = alpha*1.25, inherit.aes = FALSE) +
    geom_line(data = df_mean_model, aes(x = x, y = y), color = "blue", linetype="dashed", linewidth = lwidth, inherit.aes = FALSE) +
    labs(title = paste0("Fits (degree = ", poly_degree, ")"), x = "x", y = "y") +
    coord_cartesian(ylim = c(-2, 10)) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      plot.title = element_text(size=20, hjust = 0.5),
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    )
  ggsave(paste0(prefix_path, "_fits.png"), g, width = 6, height = 4)
  
  # 2: Bias
  g <- ggplot(df_test, aes(x = x)) +
    #geom_point(aes(y = y_obs), shape = 17, color = "orange", size = 2) +
    geom_line(aes(y = y_true), color = "black", linewidth = lwidth) +
    geom_line(aes(y = y_pred), color = "blue", linetype = "dashed", linewidth = lwidth) +
    geom_segment(aes(xend = x, y = y_true, yend = y_pred), color = "blue", size = 1) +
    annotate("text", x = 0, y = 9.5, label = paste("Bias² =", round(bias_total, 3)), size = 5) +
    labs(title = paste("Degree = ", poly_degree), x = "x", y = "y") +
    coord_cartesian(ylim = c(-2, 10)) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      plot.title = element_text(size=20, hjust = 0.5),
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    )
  ggsave(paste0(prefix_path, "_bias.png"), g, width = 6, height = 4)
  
  # 3: Variance
  g <- ggplot(df_test, aes(x = x)) +
    geom_line(data = df_all_models, aes(x = x, y = y, group = model),
              color = "grey", alpha = alpha, inherit.aes = FALSE) +
    geom_line(data = df_mean_model, aes(x = x, y = y),
              color = "blue", linetype = "dashed", linewidth = lwidth, inherit.aes = FALSE) +
    geom_errorbar(aes(ymin = y_pred - sqrt(var), ymax = y_pred + sqrt(var)),
                  width = error_width, color = "black", linewidth = 0.4*lwidth) +
    annotate("text", x = 0, y = 9.5,
             label = paste("Variance =", round(var_total, 3)), size = 5) +
    labs(title = paste0("Degree = ", poly_degree), x = "x", y = "y") +
    coord_cartesian(ylim = c(-2, 10)) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      plot.title = element_text(size=20, hjust = 0.5),
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    )
  
  ggsave(paste0(prefix_path, "_variance.png"), g, width = 6, height = 4)
  
  # 4: MSE 
  g <- ggplot(df_test, aes(x = x)) +
    #geom_point(aes(y = y_obs), shape = 17, color = "orange", size = 1.5) +
    geom_line(aes(y = y_true), color = "black", linewidth = lwidth) +
    geom_line(aes(y = y_pred), color = "blue", linetype = "dashed", linewidth = lwidth) +
    
    # Bias²: true function to mean prediction
    geom_segment(aes(xend = x, y = y_true, yend = y_pred), color = "blue", size = 1) +
    
    # Variance: error bars around mean prediction
    geom_errorbar(aes(ymin = y_pred - sqrt(var), ymax = y_pred + sqrt(var)), width = error_width, color = "black", linewidth=0.4*lwidth) +
    
    # irreducible noise band (visualization)
    geom_ribbon(aes(ymin = y_true - error_std, ymax = y_true + error_std),
                fill = "gray", alpha = 0.5*alpha, inherit.aes = TRUE) +
    
    
    annotate("text", x = 0, y = 9.3, label = paste("MSE =", round(mse_total, 3)), size = 7) +
    annotate("text", x = -1.5, y = 8.1, label = paste("Bias² =", round(bias_total, 3)), size = 5) +
    annotate("text", x = 0, y = 8.1, label = paste("Var =", round(var_total, 3)), size = 5) +
    annotate("text", x = 1.4, y = 8.1, label = paste("Noise =", round(error_std^2, 3)), size = 5) +
    
    labs(title = paste0("Degree = ", poly_degree),
         x = "x", y = "y") +
    coord_cartesian(ylim = c(-2, 10)) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      plot.title = element_text(size=20, hjust = 0.5),
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    )
  
  ggsave(paste0(prefix_path, "_mse.png"), g, width = 6, height = 4)
}

### RUN FUNCTIONS

# Global params
set.seed(1)
error_std <- 1
data_length <- 40
training_fraction <- 0.8
error_width = 0.2
alpha = 0.4
lwidth = 1.2
training_length <- round(training_fraction * data_length)
number_of_models <- 10

# --- Generate test set from same DGP (not fixed grid) ---
test_data <- generate_dataset(data_length, error_std)

# --- Run plots for different model complexities ---
simulate_and_plot(1, test_data, number_of_models, training_length, error_std,
                  "../figure/plots_bias_var_deg1")
simulate_and_plot(2, test_data, number_of_models, training_length, error_std,
                  "../figure/plots_bias_var_deg2")
simulate_and_plot(7, test_data, number_of_models, training_length, error_std,
                  "../figure/plots_bias_var_deg7")

