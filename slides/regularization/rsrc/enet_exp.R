library(mlr3)
library(glmnet)
library(mlr3learners)
library(mlr3tuning)
library(mlr3misc)
library(pracma)
library(mvtnorm)

n_train = 100
n_test = 10000
n = n_train + n_test
n_reps = 20
n_folds = 5
gs1_grid = 30
gs2_grid = c(10, 20)
p = 500
q_seq = c(5, 500)
x_corr = 0.8

tuner1 = tnr("grid_search", resolution = gs1_grid)
tuner2 = tnr("grid_search", 
  param_resolutions = c(alpha = gs2_grid[1], lambda = gs2_grid[2]))
inner = rsmp ("cv", folds = n_folds)
mm = msr("regr.mse")

l1 = lrn("regr.glmnet", alpha = 0, id = "ridge")
l2 = lrn("regr.glmnet", alpha = 1, id = "lasso")
l3 = lrn("regr.glmnet", id = "enet")
ss1 = ps(
  lambda = p_dbl(1e-3, 1e2, logscale = TRUE)        
)        
l1 = auto_tuner(tuner1, l1, inner, mm, search_space = ss1)
l2 = auto_tuner(tuner1, l2, inner, mm, search_space = ss1)
ss2 = ps(
  alpha = p_dbl(0, 1), 
  lambda = p_dbl(1e-3, 1e2, logscale = TRUE)        
)        
l3 = auto_tuner(tuner2, l3, inner, mm, search_space = ss2)

mylearners = list(l1, l2, l3)
myrsmp = rsmp("holdout", ratio = n_train / n)
# lrn_order = c("LM", "ridge", "lasso") 

make_simul_data = function(rep_i, q) {
  sigma = x_corr^(0:(p-1))
  sigma = Toeplitz(sigma)
  X = rmvnorm(n = n, sigma = sigma)
  eps = rnorm(n = n, sd = 0.1)
  theta = c(rep(1, q), rep(0, p-q))
  y = X %*% theta + eps
  d = as.data.frame(X)
  colnames(d) = sprintf("x%03i", 1:p)
  d$y = y
  tt = as_task_regr(d, target = "y", id = sprintf("q:%i", q))
  return(tt)
}

run_bm = function(n_reps) {
  simul_grid = expand.grid(q = q_seq, rep_i = 1:n_reps)
  mytasks = lapply(1:nrow(simul_grid), function(i) {
    row = simul_grid[i,]                 
    make_simul_data(rep_i = row$rep_i, q = row$q)
  })
  bg = benchmark_grid(mytasks, mylearners, myrsmp)
  bmr = benchmark(bg, store_models = TRUE)
  ba = bmr$aggregate(msr("regr.rsq"))
  list(bmr = bmr, ba = ba)
}

future::plan("multicore")
z = run_bm(n_reps)
ba = z$ba
bmr = z$bmr

nn = length(bmr$uhashes)
betas = lapply(1:nn, function(i) {
  at = bmr$resample_results$resample_result[[i]]$learners[[1]]
  gmod = at$learner$model
  as.numeric(gmod$beta)
})
ba$betas = betas
ba$resample_result = NULL
save(file = "enet_exp.RData", bmr_aggr = ba)
