# This script produces plots for the bias-variance decomposition simulation chunk
# It simulates independent draws from y(x) = x + 0.5 * x^2 + N(0,1), x ~ U(-3,3)
# And fits polynomial regression models of degree 1,2, and 7 and computes bias^2, variance, and MSE


# --- packages ---
library(ggplot2)
library(dplyr)
library(tidyr)
library(mlr3)
library(mlr3extralearners)

prefix_path = "../figure/"

# --- core definitions ---
generate_dataset <- function(n, error_std = 1) {
  x <- runif(n, -3, 3)
  y_true <- x + 0.5 * x^2
  y <- y_true + rnorm(n, sd = error_std)
  list(x = x, y = y, y_true = y_true)
}

# --- main simulation and plotting using ml3 ---
simulate_and_plot <- function(poly_degree, test_data, n_models, train_size, error_std, prefix_path) {
  X_test <- test_data$x
  Y_true_test <- test_data$y_true
  Y_test <- test_data$y
  x_plot <- seq(-3, 3, length.out = 100)
  
  mylrn <- lrn("regr.polyFit")
  mylrn$configure(deg = poly_degree)
  
  preds <- matrix(NA_real_, nrow = length(X_test), ncol = n_models)
  preds_plot <- matrix(NA_real_, nrow = length(x_plot), ncol = n_models)
  
  for (m in seq_len(n_models)) {
    d <- generate_dataset(train_size, error_std)
    task <- as_task_regr(data.frame(x = d$x, y = d$y), target = "y")
    lrn_tr <- mylrn$clone()$train(task)
    
    preds[, m] <- lrn_tr$predict_newdata(data.frame(x = X_test))$response
    preds_plot[, m] <- lrn_tr$predict_newdata(data.frame(x = x_plot))$response
  }
  
  pred_mean <- rowMeans(preds)
  pred_var  <- apply(preds, 1, var)
  pred_min  <- apply(preds, 1, min)
  pred_max  <- apply(preds, 1, max)
  
  bias_sq    <- (pred_mean - Y_true_test)^2
  mse_total  <- mean(bias_sq + pred_var + error_std^2)
  bias_total <- mean(bias_sq)
  var_total  <- mean(pred_var)
  
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
    model = factor(rep(seq_len(n_models), each = length(x_plot))),
    y = as.vector(preds_plot)
  )
  
  df_mean_model <- data.frame(
    x = x_plot,
    y = rowMeans(preds_plot)
  )
  
  # 1: Fits
  g <- ggplot(df_test, aes(x = x)) +
    geom_line(data = df_all_models, aes(x = x, y = y, group = model),
              color = "darkgrey", alpha = alpha * 1.5, inherit.aes = FALSE) +
    geom_line(data = df_mean_model, aes(x = x, y = y),
              color = "blue", linetype = "dashed", linewidth = lwidth, inherit.aes = FALSE) +
    labs(title = paste0("Model fits (degree = ", poly_degree, ")"), x = "x", y = "y") +
    coord_cartesian(ylim = c(-2, 10)) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 25),
      axis.title.y = element_text(size = 25),
      axis.text.x  = element_text(size = 20),
      axis.text.y  = element_text(size = 20),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 25, hjust = 0.5),
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text  = element_text(size = 20)
    )
  ggsave(paste0(prefix_path, "_fits.png"), g, width = 6, height = 4)
  
  # 2: Bias
  g <- ggplot(df_test, aes(x = x)) +
    geom_line(aes(y = y_true), color = "black", linewidth = lwidth) +
    geom_line(aes(y = y_pred), color = "blue", linetype = "dashed", linewidth = lwidth) +
    geom_segment(aes(xend = x, y = y_true, yend = y_pred), color = "blue", size = 1) +
    annotate("text", x = 0.1, y = 8.6, label = paste("Bias² =", round(bias_total, 3)), size = 9) +
    labs(title = paste("Degree = ", poly_degree), x = "x", y = "y") +
    coord_cartesian(ylim = c(-2, 10)) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 25),
      axis.title.y = element_text(size = 25),
      axis.text.x  = element_text(size = 20),
      axis.text.y  = element_text(size = 20),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 25, hjust = 0.5),
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text  = element_text(size = 20)
    )
  ggsave(paste0(prefix_path, "_bias.png"), g, width = 6, height = 4)
  
  # 3: Variance
  g <- ggplot(df_test, aes(x = x)) +
    geom_line(data = df_all_models, aes(x = x, y = y, group = model),
              color = "grey", alpha = 1.5 * alpha, inherit.aes = FALSE) +
    geom_line(data = df_mean_model, aes(x = x, y = y),
              color = "blue", linetype = "dashed", linewidth = lwidth, inherit.aes = FALSE) +
    geom_errorbar(aes(ymin = y_pred - sqrt(var), ymax = y_pred + sqrt(var)),
                  width = error_width, color = "black", linewidth = 0.6 * lwidth) +
    annotate("text", x = 0.1, y = 8.6, label = paste("Variance =", round(var_total, 3)), size = 9) +
    labs(title = paste0("Degree = ", poly_degree), x = "x", y = "y") +
    coord_cartesian(ylim = c(-2, 10)) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 25),
      axis.title.y = element_text(size = 25),
      axis.text.x  = element_text(size = 20),
      axis.text.y  = element_text(size = 20),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 25, hjust = 0.5),
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text  = element_text(size = 20)
    )
  ggsave(paste0(prefix_path, "_variance.png"), g, width = 6, height = 4)
  
  # 4: MSE
  g <- ggplot(df_test, aes(x = x)) +
    geom_line(aes(y = y_true), color = "black", linewidth = lwidth) +
    geom_line(aes(y = y_pred), color = "blue", linetype = "dashed", linewidth = lwidth) +
    geom_segment(aes(xend = x, y = y_true, yend = y_pred), color = "blue", size = 1) +
    geom_errorbar(aes(ymin = y_pred - sqrt(var), ymax = y_pred + sqrt(var)),
                  width = error_width, color = "black", linewidth = 0.6 * lwidth) +
    geom_ribbon(aes(ymin = y_true - error_std, ymax = y_true + error_std),
                fill = "gray", alpha = 0.5 * alpha, inherit.aes = TRUE) +
    annotate("text", x = 0,   y = 9.5, label = paste("mean(MSE) =", round(mse_total, 3)), size = 8) +
    annotate("text", x = -1.6, y = 8.1, label = paste("Bias² =", round(bias_total, 3)), size = 6) +
    annotate("text", x = 0.2,  y = 8.1, label = paste(" Var =", round(var_total, 3)), size = 6) +
    annotate("text", x = 1.8,  y = 8.1, label = paste(" Noise =", round(error_std^2, 3)), size = 6) +
    labs(title = paste0("Degree = ", poly_degree), x = "x", y = "y") +
    coord_cartesian(ylim = c(-2, 10)) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 25),
      axis.title.y = element_text(size = 25),
      axis.text.x  = element_text(size = 20),
      axis.text.y  = element_text(size = 20),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 25, hjust = 0.5),
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text  = element_text(size = 20)
    )
  ggsave(paste0(prefix_path, "_mse.png"), g, width = 6, height = 4)
  
  # 5: Pointwise MSE decomposition (stacked area)
  df_decomp <- df_test %>%
    mutate(
      noise_cum = error_std^2,
      var_cum   = error_std^2 + var,
      bias_cum  = error_std^2 + var + (y_pred - y_true)^2
    ) %>%
    arrange(x)
  
  g <- ggplot(df_decomp, aes(x = x)) +
    geom_ribbon(aes(ymin = 0,          ymax = noise_cum, fill = "Noise")) +
    geom_ribbon(aes(ymin = noise_cum,  ymax = var_cum,   fill = "Variance")) +
    geom_ribbon(aes(ymin = var_cum,    ymax = bias_cum,  fill = "Bias²")) +
    scale_fill_manual(values = c("Noise" = "grey",
                                 "Variance" = "black",
                                 "Bias²" = "blue")) +
    labs(title = paste0("Decomposition (degree = ", poly_degree, ")"),
         x = "x", y = "Generalization Error", fill = "Component") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 25),
      axis.title.y = element_text(size = 22),
      axis.text.x  = element_text(size = 20),
      axis.text.y  = element_text(size = 20),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 25, hjust = 0.5),
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text  = element_text(size = 20)
    )
  
  if (poly_degree == 7) {
    g <- g + coord_cartesian(ylim = c(0, 10))
  } else if (poly_degree == 2) {
    g <- g + coord_cartesian(ylim = c(0.75, 1.3))
  } else {
    g <- g + coord_cartesian(ylim = c(0, 5))
  }
  ggsave(paste0(prefix_path, "_decomp.png"), g, width = 6, height = 4)
}

# --- RUN FUNCTIONS ---
set.seed(1)
error_std <- 1
data_length <- 40
training_fraction <- 0.8
error_width <- 0.2
alpha <- 0.4
lwidth <- 1.2
training_length <- round(training_fraction * data_length)
number_of_models <- 10

test_data <- generate_dataset(data_length, error_std)

simulate_and_plot(1, test_data, number_of_models, training_length, error_std,
                  "../figure/plots_bias_var_deg1")
simulate_and_plot(2, test_data, number_of_models, training_length, error_std,
                  "../figure/plots_bias_var_deg2")
simulate_and_plot(7, test_data, number_of_models, training_length, error_std,
                  "../figure/plots_bias_var_deg7")
