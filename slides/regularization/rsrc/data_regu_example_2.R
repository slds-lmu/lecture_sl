# ------------------------------------------------------------------------------
# l1 vs l2

# RDATA: 
#  (1): generate coefficients for regression with two regularization constants
#       (lambda 0.01, 100) under l1 and l2 regularization.
#  (2): generate coefficients and MSE for cross-validation with 50
#       regularization constants (9.536743e-07 to 2) 
#       under l1 and l2 regularization.
# DATA: 
#     Xi ~ Normal(0, 1), Cov(xi, xj) = 0.7^|i-j|
#     y = 10*x1 + 10*x2 + 5*x3 + 5*x4 + x5 + ... + x14 + eps(100*1 ~Normal)
# ------------------------------------------------------------------------------

library(mlr3)
library(dplyr)
library(mlr3learners)
library(mlr3tuning)
library(BBmisc)
library(ggplot2)
library(gridExtra)
library(MASS)

set.seed(19873)

# DATA -------------------------------------------------------------------------

n <- 100    # Number of observations
p <- 50     # Number of predictors included in model
CovMatrix <- outer(1:p, 1:p, function(x,y) {.7^abs(x-y)})
x <- mvrnorm(n, rep(0,p), CovMatrix)
y <- 10 * apply(x[, 1:2], 1, sum) +
  5 * apply(x[, 3:4], 1, sum) +
  apply(x[, 5:14], 1, sum) +
  rnorm(n)

dd = as.data.frame(x)
dd$y = y
task <- TaskRegr$new(id = "mytask", backend = dd, target = "y")

# order coefficients
extract_numeric <- function(x) {
  as.numeric(gsub("[^0-9]", "", x))
}

get_pen_coefs = function(task, alpha, lam) {
  learner = lrn("regr.glmnet", alpha = alpha, lambda=lam)
  learner$train(task)
  cc <- as.matrix(coef(learner$model))[,1]
  names <- names(cc)
  cc <- as.numeric(cc)
  cc_nonin <- cc[2:length(cc)] # reorder non-intercept cc
  names(cc) <- names
  names_nonin <- extract_numeric(names[2:length(names)])
  names <- c(names[1], paste0("V", as.character(sort(names_nonin))))
  cc <- cc[names]
  names(cc) <- names
  return(abs(cc))
}

compute_cv = function(task, alpha, lambda_seq) {
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
  
  return(cv_lam)
}

lams = c(0.01, 100)
cc_l2_1 = get_pen_coefs(task, alpha = 0, lam = lams[1])
cc_l2_2 = get_pen_coefs(task, alpha = 0, lam = lams[2])
cc_l1_1 = get_pen_coefs(task, alpha = 1, lam = lams[1])
cc_l1_2 = get_pen_coefs(task, alpha = 1, lam = lams[2])


lambda_seq = 2^seq(-20, 1, length.out = 50)
cv_l1 = compute_cv(task, alpha = 1, lambda_seq)
cv_l2 = compute_cv(task, alpha = 0, lambda_seq)

save2("data_regu_example_2.RData", lams, lambda_seq,
      cc_l2_1, cc_l2_2, cc_l1_1, cc_l1_2,
      cv_l1, cv_l2)