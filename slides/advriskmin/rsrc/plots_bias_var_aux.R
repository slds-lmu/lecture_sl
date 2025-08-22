### This script generates the auxiliary plots for the bias-variance-decomposition simulation chunk
# It creates one plot showing the true function and two random train/test splits with linear fit
# and one plot showing the true function and a train/test split without a fit line


# --- packages ---
library(ggplot2)
library(mlr3)
library(mlr3extralearners)

# Core definitions
generate_dataset <- function(n, error_std = 1) {
  x <- runif(n, -3, 3)
  y_true <- x + 0.5 * x^2
  y <- y_true + rnorm(n, sd = error_std)
  list(x = x, y = y, y_true = y_true)
}

# Global params
set.seed(1)
error_std <- 1
lwidth <- 1.4

# Auxiliary plot using mlr3 for the fit
plot_sample_fit <- function(idx, train_size, test_size, poly_degree, error_std, prefix_path, show_fit = TRUE, show_test = TRUE) {
  train_data <- generate_dataset(train_size, error_std)
  test_data  <- generate_dataset(test_size,  error_std)
  
  x_plot <- seq(-3, 3, length.out = 100)
  y_true_plot <- x_plot + 0.5 * x_plot^2
  
  if (show_fit) {
    lrn <- lrn("regr.polyFit")
    lrn$configure(deg = poly_degree)
    task <- as_task_regr(data.frame(x = train_data$x, y = train_data$y), target = "y")
    fit  <- lrn$clone()$train(task)
    y_fit_plot <- fit$predict_newdata(data.frame(x = x_plot))$response
    df_fit <- data.frame(x = x_plot, y = y_fit_plot)
  }
  
  df_train  <- data.frame(x = train_data$x, y = train_data$y, set = "Train")
  df_test   <- data.frame(x = test_data$x,  y = test_data$y,  set = "Test")
  df_points <- rbind(df_train, df_test)
  
  df_true <- data.frame(x = x_plot, y = y_true_plot)
  
  title_str <- if (show_fit) paste("Individual Sample Fit (degree =", poly_degree, ")") else "Individual Sample Draw"
  
  g <- ggplot() +
    geom_point(data = df_points, aes(x = x, y = y, shape = set, color = set), size = 2.5) +
    scale_shape_manual(values = c("Train" = 16, "Test" = 17)) +
    scale_color_manual(values = c("Train" = "blue", "Test" = "orange")) +
    geom_line(data = df_true, aes(x = x, y = y), color = "black", linewidth = 0.9 * lwidth) +
    labs(title = title_str, x = "x", y = "y", color = "Data", shape = "Data") +
    coord_cartesian(ylim = c(-2, 10)) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 25),
      axis.title.y = element_text(size = 25),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 25, hjust = 0.5),
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text  = element_text(size = 20)
    )
  
  if (show_fit) {
    g <- g + geom_line(data = df_fit, aes(x = x, y = y), color = "grey", linetype = "solid", linewidth = 0.6 * lwidth)
  }
  
  ggsave(paste0(prefix_path, "plots_bias_var_aux_", idx, ".png"), g, width = 6, height = 4)
}

# Calls
plot_sample_fit(1, train_size = 32, test_size = 40, poly_degree = 1, error_std = error_std,
                prefix_path = "../figure/", show_test = FALSE)
plot_sample_fit(2, train_size = 32, test_size = 40, poly_degree = 1, error_std = error_std,
                prefix_path = "../figure/", show_test = FALSE)
plot_sample_fit(3, train_size = 32, test_size = 40, poly_degree = 1, error_std = error_std,
                prefix_path = "../figure/", show_fit = FALSE, show_test = TRUE)
