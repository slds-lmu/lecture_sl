# ------------------------------------------------------------------------------
# l1

# FIG: draw lasso and ridge solution paths in terms of OLS.
# ------------------------------------------------------------------------------

library(ggplot2)

# DATA -------------------------------------------------------------------------

soft_threshold <- function(rho, lamda) {
  if (rho < -lamda) {
    return (rho + lamda)
  } else if (rho > lamda) {
    return (rho - lamda)
  } else {
    return (0)
  }
}

lamda <- 3

x1 <- seq(-10, 10, by = 0.1)

y_st <- sapply(x1, function(rho) soft_threshold(rho, lamda))

# ridge estimate
y_ridge <- x1 / (1 + lamda)

# PLOT -------------------------------------------------------------------------

data <- data.frame(rho = x1, theta = y_st, OLS = x1, Ridge = y_ridge)

p <- ggplot(data, aes(x = rho)) +
  geom_line(aes(y = theta), color = 'blue', linetype = "solid", size=1.2) +
  geom_line(aes(y = OLS), color = 'grey', linetype = "dashed", size=1.2) +
  geom_line(aes(y = Ridge), color = 'red', linetype = "solid", size=1.2) + 
  labs(x = expression(theta[OLS]), y = expression(theta[pen]), title = 'Lasso vs Ridge solution in terms of OLS (orthonormal design, lambda=3)') +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),  
    axis.title = element_text(size = 18),              
    axis.text = element_text(size = 18),             
    axis.ticks = element_line(size = 1)
  ) +
  scale_color_manual(values = c('blue', 'grey', 'red')) +
  geom_hline(yintercept = 0, linetype="solid", color = "black") +
  geom_vline(xintercept = 0, linetype="solid", color = "black") +
  guides(color = guide_legend(title = NULL)) +
  theme(legend.position = "bottom") +
  annotate("text", x = -9, y = -4, label = "Lasso", parse = TRUE, size=8, color="blue") +
  annotate("text", x = 7, y = 9, label = "OLS", parse = TRUE, size=8, color ="grey") +
  annotate("text", x = 7, y = 0.5, label = "Ridge", color = "red", parse = TRUE, size=8)

ggsave("../figure/soft_thresholding.png", plot = p, width = 10, height = 5)
