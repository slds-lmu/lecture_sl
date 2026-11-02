# ------------------------------------------------------------------------------
# l1 vs l2

# RDATA: 
#  (1): generate coefficients path for regression with 
#       different regularization constants under l1 and l2 regularization.
#  (2): generate coefficients and MSE for cross-validation with 50
#       regularization constants (9.536743e-07 to 2) 
#       under l1 and l2 regularization.
# DATA: boston_housing
# ------------------------------------------------------------------------------

library(BBmisc)
library(data.table)
library(mlr3)
library(dplyr)
library(mlr3learners)
library(mlr3tuning)

set.seed(123)

# DATA -------------------------------------------------------------------------

task = tsk("boston_housing")
feat_drop = c("chas", "nox", "rm", "lat", "lon", "town", "tract")
task$select(setdiff(task$feature_names, feat_drop))
featnames = task$feature_names

compute_coef_paths = function(task, lambda_name, lambda_seq) {
  alpha = ifelse(lambda_name=='lambda1', 1, 0)
  path = list()
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

lambda_seq = 2^seq(-10, 20, length.out = 50)
path_l1 = compute_coef_paths(task, "lambda1", lambda_seq)
path_l2 = compute_coef_paths(task, "lambda2", lambda_seq)

save2("data_regu_example_1.RData", path_l1 = path_l1, path_l2 = path_l2, featnames = featnames, lambda_seq = lambda_seq)

