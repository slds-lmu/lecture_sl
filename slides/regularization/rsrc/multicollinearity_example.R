# ------------------------------------------------------------------------------
# l1 vs l2

# FIG: draw how coefficient values and MSE of linear regression change with 
#      different regularization constants (lambda).
#  left: Under L1 regularization
#  right: Under L2 regularization
# DATA: 
#     xi ~ Normal(0, 2) i=1,2,3,4 uncorrelated, x5 = x4 + Normal(0, 0.3)
#     y = 0.2*x1 + 0.2*x2 + 0.2*x3 + 0.2*x4 + 0.2*x5 + eps(100*1 ~Normal)
# ------------------------------------------------------------------------------

library(dplyr)
library(ggrepel)
library(BBmisc)
library(MASS)
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(ggplot2)
library(data.table)
library(gridExtra)
options(scipen = 10000)

set.seed(20200611)
# FICTIONAL DATA ---------------------------------------------------------------

# Create 4 normally distributed, uncorrelated RV
Sigma <- diag(rep(2, 4))
design_matrix <- data.frame(mvrnorm(100, mu = rep(0, 4), Sigma = Sigma,
                                    empirical = TRUE))

# Add X5 - almost perfectly correlated to X4
colnames(design_matrix) <- c("X1", "X2", "X3", "X4")
design_matrix <- design_matrix %>%
  mutate(X5 = X4 + rnorm(nrow(design_matrix), 0, 0.3))

# Create target variable
design_matrix <- design_matrix %>% mutate(y = 0.2 * X1 + 0.2 * X2 + 0.2 * X3
                                             + 0.2 * X4 + 0.2 * X5 + 
                                           rnorm(nrow(design_matrix), 0, 1))

# REGRESSION TASK --------------------------------------------------------------

task_mc <- TaskRegr$new(id = "fictional", backend = design_matrix, target = "y")
featnames_mc <- task_mc$feature_names

# COEFFICENT PATHS -------------------------------------------------------------

compute_coef_paths <- function(task, lambda_name, lambda_seq) {
  alpha = ifelse(lambda_name=='lambda1', 1, 0)
  path <- list()
  # Compute coefficients for each model (on entire data)
  for (i in seq_along(lambda_seq)) {
    lamval <- lambda_seq[i]
    learner = lrn("regr.glmnet", alpha = alpha, lambda=lamval)
    learner$train(task)
    cc <- t(as.matrix(coef(learner$model)))
    names <- colnames(cc)
    cc <- as.numeric(cc)
    names(cc) <- names
    cc <- as.list(cc)
    cc$lambda <- lamval
    path[[i]] <- cc
  }
  
  path <- rbindlist(path, fill = TRUE)
  path[is.na(path)] <- 0
  
  # Perform cross validation
  learner = lrn("regr.glmnet", alpha = alpha, lambda=to_tune(lambda_seq))

  # Construct tuning instance
  instance = ti(
    task = task,
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("regr.mse"),
    terminator = trm("evals", n_evals = length(lambda_seq))
  )
  
  tuner <- tnr("grid_search", resolution = length(lambda_seq))
  tuner$optimize(instance)
  cv_lam <- as.data.frame(instance$archive$data)[,1:2]
  colnames(cv_lam) <- c("lambda", "mse")
  cv_lam$lambda <- as.numeric(as.character(cv_lam$lambda))
  cv_lam <- cv_lam %>% arrange(lambda)
  
  list(path = path, cv_lam = cv_lam)
}

# PLOT PATHS -------------------------------------------------------------------

plot_coef_paths_mc <- function(obj, featnames, title, xlab) {
  ggd <- melt(obj$path, id.var = "lambda", measure = featnames,
              variable.name = "featname", value.name = "coefval")
  ggd$label <- ifelse(ggd$lambda == min(lambda_seq_mc),
                      as.character(ggd$featname), NA)
  ggd$mse <- rep(obj$cv_lam[, "mse"], 5)
  pl <- ggplot(data = ggd, mapping = aes(x = lambda, y = coefval,
                                         group = featname, col = featname)) +
    geom_line() +
    geom_label_repel(aes(label = label), na.rm = TRUE) + 
    scale_x_log10() + 
    ggtitle(title) + 
    xlab(xlab) + 
    theme_bw() + 
    scale_color_manual(values = c(rep("black", 3), "#7FFF32", "#067B7F"), 
                       guide = FALSE) + 
    geom_line(mapping = aes(x = ggd$lambda, y = ggd$mse * 0.5), 
              col = "black", linetype = "longdash") + 
    geom_text(x = max(log(ggd$lambda, 10)), 
              y = 0.5 * (max(ggd$mse)) - 0.01, vjust = 1, hjust = 1, 
              label = "MSE", col = "black") +
    scale_y_continuous(sec.axis = sec_axis(~. * 2, name = "MSE")) +
    geom_hline(aes(yintercept = 0), col = "black", linetype = "dotted")

}

#Visualize shrinkage in presence of multicollinearity
lambda_seq_mc <- 2^seq(-10, 20, length.out = 50)

path_l1_mc <- compute_coef_paths(task_mc, "lambda1", lambda_seq_mc)
path_l2_mc <- compute_coef_paths(task_mc, "lambda2", lambda_seq_mc)

p_l1 <- plot_coef_paths_mc(path_l1_mc, featnames_mc, "Lasso", expression(lambda))
p_l2 <- plot_coef_paths_mc(path_l2_mc, featnames_mc, "Ridge", expression(lambda))

p <- grid.arrange(p_l1, p_l2, nrow = 1)
ggsave("../figure/multicollinearity_example.png", plot=p, width= 8, height =3)
