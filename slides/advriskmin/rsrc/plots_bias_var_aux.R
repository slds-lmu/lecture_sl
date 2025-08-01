### This script generates the auxiliary plots for the bias-variance-decomposition simulation chunk
# It creates one plot showing the true function and two random train/test splits with linear fit
# and one plot showing the true function and a train/test split without a fit line

library(ggplot2)

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

# Global params
set.seed(1)
error_std <- 1
lwidth = 1.4

#### Additional plots showing two draws from the DGP for didactic purposes
plot_sample_fit <- function(idx, train_size, test_size, poly_degree, error_std, prefix_path, show_fit = TRUE, show_test=TRUE) {
  train_data <- generate_dataset(train_size, error_std)
  test_data <- generate_dataset(test_size, error_std)
  
  x_plot <- seq(-3, 3, length.out = 100)
  y_true_plot <- x_plot + 0.5 * x_plot^2
  
  if (show_fit) {
    coef_fit <- train_model(train_data$x, train_data$y, poly_degree)
    y_fit_plot <- calculate_polynomial(coef_fit, poly_degree, x_plot)
    df_fit <- data.frame(x = x_plot, y = y_fit_plot)
  }
  
  df_train <- data.frame(x = train_data$x, y = train_data$y, set = "Train")
  df_test  <- data.frame(x = test_data$x, y = test_data$y, set = "Test")
  df_points <- rbind(df_train, df_test)
  
  df_true <- data.frame(x = x_plot, y = y_true_plot)
  
  # Determine dynamic title
  title_str <- if (show_fit) {
    paste("Individual Sample Fit (degree =", poly_degree, ")")
  } else {
    "Individual Sample Draw"
  }
  
  # Construct plot
  g <- ggplot() +
    #geom_point(data = df_train, aes(x = x, y = y, shape = set, color = set), size = 2.5) +
    #geom_point(data = df_test, aes(x = x, y = y), shape = 17, color = "orange", size = 2.5) + 
    geom_point(data = df_points,
               aes(x = x, y = y, shape = set, color = set), size = 2.5) +
    scale_shape_manual(values = c("Train" = 16, "Test" = 17)) +
    scale_color_manual(values = c("Train" = "blue", "Test" = "orange")) +
    geom_line(data = df_true, aes(x = x, y = y), color = "black", linewidth = 0.9 * lwidth) +
    labs(title = title_str,
         x = "x", y = "y", color = "Data", shape = "Data") +
    coord_cartesian(ylim = c(-2, 10)) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    )
  
  if (show_fit) {
    g <- g + geom_line(data = df_fit, aes(x = x, y = y), color = "grey", linetype = "solid", linewidth = 0.6 * lwidth)
  }
  
  ggsave(paste0(prefix_path, "plots_bias_var_aux_", idx, ".png"), g, width = 6, height = 4)
}


# Plot two random sample fits with linear models 
plot_sample_fit(1, train_size = 32, test_size = 40, poly_degree = 1, error_std = error_std, 
                prefix_path = "../figure/", show_test=FALSE)
plot_sample_fit(2, train_size = 32, test_size = 40, poly_degree = 1, error_std = error_std, 
                prefix_path = "../figure/", show_test=FALSE)
plot_sample_fit(3, train_size = 32, test_size = 40, poly_degree = 1, error_std = error_std, 
                prefix_path = "../figure/", show_fit = FALSE, show_test = TRUE)
