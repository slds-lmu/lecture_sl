library(ggplot2)

set.seed(123)
n = 5
x <- -n:n
y <- x+rnorm(length(x), sd = 1)
beta_0 <- mean(y)

bias_only_model <- function(x) {
  return(rep(beta_0, length(x)))
}

TSS <- round(sum((y - bias_only_model(x))^2), 1)

DATA_COLOR <- 'blue'
MODEL_COLOR <- 'black'
LINEAR_MODEL_ERROR_COLOR <- 'darkgreen'

df <- data.frame(x=x, y=y)
df$prediction <- bias_only_model(df$x)
df$error <- df$y - df$prediction
df$bottom_left_x <- ifelse(df$error > 0, df$x - df$error, df$x)
df$bottom_left_y <- ifelse(df$error > 0, df$prediction, df$y)
df$error_abs <- abs(df$error)


p <- ggplot(df, aes(x=x, y=y)) + 
  geom_point(aes(color=DATA_COLOR), size = 4) + 
  geom_line(aes(y=prediction), color=MODEL_COLOR, size = 1.4) + 
  geom_rect(aes(
    xmin = bottom_left_x,
    xmax = bottom_left_x + error_abs,
    ymin = bottom_left_y,
    ymax = bottom_left_y + error_abs,
    fill = LINEAR_MODEL_ERROR_COLOR
  ), alpha = 0.4) +
  labs(title="Optimal constant model gives ''unscaled'' variance") +
  xlab("") +
  scale_color_identity() +
  scale_fill_identity() +
  theme_minimal() +
  xlim(-5,5) + ylim(-5,5) +
  theme(
    axis.text = element_text(size = rel(3)), 
    title = element_text(size = rel(2.1))     
    #,panel.grid.major = element_blank()       
    ,panel.grid.minor = element_blank()         
  ) + 
  scale_x_continuous(breaks = seq(-6, 6, by = 1)) + # x-axis increments of 1
  scale_y_continuous(breaks = seq(-6, 6, by = 1))  # y-axis increments of 1

ggplot2::ggsave("../figure_man/plot_const_var.png", p, height = 10L, width = 10L)

p