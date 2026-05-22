# ------------------------------------------------------------------------------
# l1 vs l2

# FIG:
#  (1): how coefficient values and MSE changes with regularization constant
#       (lambda) for linear regression with l1 and l2 regularization.
#  (2): histogram of coefficient values with two regularization constants
#       (lambda 0.01, 100) to show how they affect shrinkage
#       for linear regression with l1 and l2 regularization.
# DATA:
#  (1): data from data_regu_example_1.RData
#  (2): data from data_regu_example_2.RData
# ------------------------------------------------------------------------------

library(ggplot2)
library(gridExtra)
library(ggrepel)
library(data.table)
library(viridis)

# DATA -------------------------------------------------------------------------

load("data_regu_example_1.RData")
load("data_regu_example_2.RData")

d_l1 <- rbind(
  data.frame(lam = paste(lams[1]), coefval = cc_l1_1),
  data.frame(lam = paste(lams[2]), coefval = cc_l1_2)
)
d_l1$lam <- as.factor(d_l1$lam)
d_l2 <- rbind(
  data.frame(lam = paste(lams[1]), coefval = cc_l2_1),
  data.frame(lam = paste(lams[2]), coefval = cc_l2_2)
)
d_l2$lam <- as.factor(d_l2$lam)

# PLOTS -------------------------------------------------------------------------

### (1)
plot_coef_paths <- function(path, featnames, title, xlab) {
  ggd <- melt(path, id.vars = "lambda", measure = featnames, variable.name = "featname", value.name = "coefval")
  ggd$label <- ifelse(ggd$lambda == min(lambda_seq), as.character(ggd$featname), NA)
  pl <- ggplot(data = ggd, aes(x = lambda, y = coefval, group = featname, col = featname)) +
    guides(color = "none") +
    geom_line() +
    geom_label_repel(aes(label = label), na.rm = TRUE, max.overlaps = Inf) +
    scale_color_discrete(guide = FALSE) +
    scale_x_log10() +
    ggtitle(title) +
    xlab(xlab) +
    theme_bw() +
    scale_color_viridis(end = 0.9, discrete = TRUE)
}

plot_cv_path <- function(cv_lam, title, xlab, ylab) {
  pl <- ggplot(data = cv_lam, aes(x = lambda, y = mse)) +
    geom_line() +
    scale_x_log10() +
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab)
}

p1l1 <- plot_coef_paths(path_l1$path, featnames, "Lasso", expression(lambda))
p1l2 <- plot_coef_paths(path_l2$path, featnames, "Ridge", expression(lambda))
p1l3 <- plot_cv_path(path_l1$cv_lam, "Lasso", expression(lambda), 'MSE') + 
  theme_minimal() + ylim(25, 90)
p1l4 <- plot_cv_path(path_l2$cv_lam, "Ridge", expression(lambda), 'MSE') + 
  theme_minimal() + ylim(20, 90)

p1 <- grid.arrange(p1l1, p1l2, p1l3, p1l4, nrow = 2)
ggsave("../figure/shrinkage_01.png", plot = p1, width = 8, height = 4)



### (2)
# histogram of coefficients value of data d
plot_coef_hist <- function(d, title) {
  pl <- ggplot(d, aes(x = coefval, fill = lam)) +
    scale_fill_viridis(end = 0.9, discrete = TRUE) +
    geom_histogram(alpha = 0.9, position = "dodge") +
    theme_gray(base_size = 14) +
    ggtitle(title)
  return(pl)
}

# MSE with different lambda for data cv_lam

p2l1 <- plot_coef_hist(d_l1, "Lasso") + guides(fill=guide_legend(title=expression(lambda)))
p2l2 <- plot_coef_hist(d_l2, "Ridge")+ guides(fill=guide_legend(title=expression(lambda))) + 
  ylim(0, 50)
p2l3 <- plot_cv_path(cv_l1, "Lasso", expression(lambda), 'MSE')  + 
  theme_gray(base_size = 14) + ylim(1, 10)
p2l4 <- plot_cv_path(cv_l2, "Ridge", expression(lambda), 'MSE')  + 
  theme_gray(base_size = 14) + ylim(1, 10)

p2 <- grid.arrange(p2l1, p2l2, p2l3, p2l4, nrow = 2)
ggsave("../figure/shrinkage_02.png", plot = p2, width = 8, height = 5)
