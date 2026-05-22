library(mlr3)
# library(mlr3extralearners)
 load_all("~/cos/mlr3extralearners")
 library(ggplot2)

set.seed(1)

x_min = -3; x_max = 3
n_models = 20
n_train = 20; n_test = 1000
n_data = n_train + n_test
noise_std = 1
mylrn = lrn("regr.poly")
mymsr = msr("regr.mse")

ftrue = function(x) x + 0.5 * x^2

# creates simul task of n_train size
make_task = function(n) {
  x = runif(n, x_min, x_max) 
  dd = data.frame(x = x, y = ftrue(x) + rnorm(n, 0, noise_std))
  as_task_regr(dd, target = "y")
}

pred_task = make_task(n_test)
pred_ftrue = ftrue(pred_task$data()$x)

run_exp_for_degree = function(poly_degree) {
  mylrn$configure(deg = poly_degree)
  preds = matrix(NA, nrow = n_test, ncol = n_models)
  pred_mses = numeric(n_models)
  
  for (i in 1:n_models) {
    mytask = make_task(n_train)
    mylrn$train(mytask)
    p = mylrn$predict(test_task)
    preds[, i] = p$response
    mse = p$score(mymsr)
    pred_mses[i] = mse
    catf("iter = %i, mse = %g", i, mse)
  }
  
  # avg model and var at each test point
  pred_mean = rowMeans(preds)
  pred_var = apply(preds, 1, var)
  
  # compute learner bias and var and mse as averages over test points
  lrn_var = mean(pred_var)
  lrn_mse = mean(pred_mses)
  lrn_bias = mean((pred_ftrue - pred_mean)^2)
  diff = abs(lrn_mse - lrn_bias - lrn_var - noise_std)
  
  catf("mse = %g; lrn_bias = %g; lrn_var = %g, diff = %g", 
    lrn_mse, lrn_bias, lrn_var, diff)
  list(learners = learners)
}


#run_exp_for_degree(poly_degree = 1) 
#run_exp_for_degree(poly_degree = 2) 

#FIXME: plot ftrue
# FIXME: plot avg
plot_poly = function(learners) {
  pl = ggplot(mapping = aes(x = x)) 
  # plot test points  
  ggd_task = pred_task$data()
  pl = pl + geom_point(data = ggd_task, mapping = aes(y = y), alpha = 0.5) 
  # plot all polynomials 
  for (i in seq_along(learners)) {
    ggd_lrn = data.frame(x = seq(x_min, x_max, length.out = 200))
    ggd_lrn$yhat = learners[[i]]$predict_newdata(newdata = ggd_lrn)$response
    pl = pl + geom_line(data = ggd_lrn, mapping = aes(y = yhat), color = "blue", size = 1) 
  }
  pl = pl + theme_minimal() 
  pl = pl + labs(title = "Polynomial Regression (degree 3)", y = "y", x = "x")
  print(pl)
  return(pl)
}


pred_task = make_task(5)
mylrn$configure(deg = 5)
plot_learners = lapply(1:4, function(i) mylrn$train(make_task(n_train))$clone())
plot_poly(plot_learners)
