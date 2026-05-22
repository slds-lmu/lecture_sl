# ------------------------------------------------------------------------------
# intro

# FIG: show how early stopping influences training and test results.
# LEFT: how MSE changes with iterations.
# RIGHT: two fitted curves (early stopping & overfit).

# DATA: Ozone from package-mlbench
# ------------------------------------------------------------------------------

library(mlbench)
library(reshape2)
library(ggplot2)
library(gridExtra)

theme_set(theme_minimal())

set.seed(6)

# DATA -------------------------------------------------------------------------

data(Ozone)

# gradient of empirical risk
R_emp_grad <- function(beta,
                       features = X,
                       target = y) {
  return(2 * t(features) %*% (features %*% beta - target))
}

gradient_descent <-
  function(beta_start,
           step_size,
           grad_fun,
           num_steps,
           features,
           target) {
    betas <- matrix(0, ncol = length(beta_start), nrow = num_steps)
    betas[1,] <- beta_start
    for (i in seq(2, num_steps)) {
      betas[i,] <-
        betas[i - 1,] - step_size * grad_fun(betas[i - 1, ], features,
                                             target)
    }
    
    betas <- as.data.frame(betas)
    return(betas)
  }

# generate polynomials
poly <- function(x, degree) {
  sapply(0:degree, function(i)
    x ^ i)
}

o_data <- Ozone[, c(4, 8)]
o_data$V8 <- o_data$V8 / 100
o_data <- o_data[complete.cases(o_data), ]

id_train <- sample(1:nrow(o_data), 20)
o_data$type <- "test"
o_data[id_train,]$type <- "train"
o_data$type <- as.factor(o_data$type)

train_data <-  as.matrix(o_data[id_train, 1:2])
test_data  <-  as.matrix(o_data[-id_train, 1:2])

degree <- 15

x_train <- poly(train_data[, 2], degree)
y_train <- o_data[id_train , 1]
x_test <- poly(test_data[, 2], degree)
y_test <- o_data[-id_train , 1]

num_steps <- 1000000
res <- gradient_descent(rep(0, ncol(x_train)),  0.02, #0.003,
                        R_emp_grad, num_steps, x_train, y_train)

errs <- matrix(0, nrow = 2000, ncol = 2)
it1 <- 1:1000
for (i in it1) {
  errs[i, 1] <-
    sum((x_train %*% t(res[i, ]) - y_train) ^ 2) / nrow(x_train)
  errs[i, 2]  <-
    sum((x_test %*% t(res[i, ]) - y_test) ^ 2) / nrow(x_test)
}
it2 <- seq(1000, num_steps, length.out = 1000)
for (i in it2) {
  errs[1000 + which(it2 == i), 1] <-
    sum((x_train %*% t(res[i, ]) - y_train) ^ 2) / nrow(x_train)
  errs[1000 + which(it2 == i), 2]  <-
    sum((x_test %*% t(res[i, ]) - y_test) ^ 2) / nrow(x_test)
}

df <- as.data.frame(errs)
colnames(df) <- c("train", "test")
df$id <- c(it1, it2)

min_te <- which.min(errs[, 2])

learning_df <- melt(df, id.vars = "id")

# PLOT -------------------------------------------------------------------------

# MSE
p1 <- ggplot(learning_df, aes(x = id, y = value)) +
  geom_line(aes(colour = variable), size=1.2) +
  geom_vline(xintercept = min_te, colour="gray", 
             alpha= 0.8, size = 1.5) +
  geom_vline(xintercept = num_steps, colour="gray",
             linetype = "dashed", alpha= 0.8, size = 1.5) +
  scale_x_log10() +
  ylab("MSE") +
  xlab("Iterations") +
  scale_fill_brewer(palette="Dark2") + 
  annotate("text", x=min_te-70, y=175, label="stopped early",
           color='black', size=3) + 
  annotate("text", x=num_steps-4*1e5, y=175, label="overfitted",
         color='black', size=3) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = NULL))

# ozone level
pl_data <- seq(min(o_data[, 2]), max(o_data[, 2]), length.out = 100)
pl_data <- poly(pl_data, degree)

y_overfit <- (pl_data) %*% t(res[num_steps, ])[,1]
y_best <- (pl_data) %*% t(res[min_te, ])[,1]

fitting_df <- data.frame(overfit = y_overfit, best = y_best, x = pl_data[, 2] * 100)
fitting_df <- melt(fitting_df, id.vars = "x")

p2 <- ggplot(o_data, aes(x=V8*100, y=V4)) + 
  geom_point(aes(colour=type)) +
  geom_line(data=fitting_df, aes(linetype=rev(variable), x=x, y=value), alpha = 0.7, 
            show.legend=FALSE, color="gray", size=1.5) +
  ylab("Ozone level") + 
  xlab("Temperature (degrees F)") +
  scale_fill_brewer(palette="Dark2") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(title = NULL))

p = grid.arrange(p1, p2, ncol = 2)

ggsave("../figure/early_stopping.png", plot=p, width=9, height=6)

