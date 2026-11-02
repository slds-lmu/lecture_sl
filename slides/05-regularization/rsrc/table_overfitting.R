# ------------------------------------------------------------------------------
# intro
# TABLE: 
#   train and test MSE table using Neural Network and CART (overfitting).
# DATA: mtcars
# ------------------------------------------------------------------------------

library(nnet)
library(xtable)
library(mlr3)
library(mlr3learners)
set.seed(123)

# DATA -------------------------------------------------------------------------

lgr::get_logger("mlr3")$set_threshold("info")

task = tsk("mtcars")

lrn1 = lrn("regr.nnet", size = 100, maxit = 20000, MaxNWts = 10000, decay = 0, abstol = 1e-7)
lrn1$encapsulate = c(train = "evaluate", predict = "evaluate")
lrn2 = lrn("regr.rpart", minsplit = 2, cp = 0)

my_learners = list(lrn1, lrn2)
for (x in my_learners){
  x$predict_sets = c("train", "test")
}

bg = benchmark_grid(task, my_learners, rsmp("cv", folds = 10))

bmr = benchmark(bg)

m1 = msr("regr.mse", predict_sets = c("test"), id = "mse-test")
m2 = msr("regr.mse", predict_sets = c("train"), id = "mse-train")

a = bmr$aggregate(measures = list(m1, m2))
print(a)

# TABLE ------------------------------------------------------------------------

# Create a 2x2 comparison table with rounded results
res = as.data.frame(a)
res = res[, c("mse-train", "mse-test")]
 
rownames(res) = c("Neural Network", "CART")
colnames(res) = c("Train MSE", "Test MSE")

latex_tab = xtable(res)
 
print(latex_tab, file = "table_overfitting.tex", include.rownames = TRUE, include.colnames = TRUE, comment = FALSE)


