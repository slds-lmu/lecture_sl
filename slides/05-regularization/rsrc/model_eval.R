# ------------------------------------------------------------------------------
# intro

# FIG: binary classification visualization under
#      appropriate, overfitted and underfitted models.

# DATA: 100000 2-feature samples from Normal distribution into two classes.
# ------------------------------------------------------------------------------

library(mlr3misc)
library(mvtnorm)
library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(ggplot2)
library(gridExtra)
library(e1071)

set.seed(600000)

# DATA -------------------------------------------------------------------------

n = 100000

mu1 = c(0, 3)
mu2 = c(3, 0)
s1 = matrix(c(1, 0.1, 0.1, 2), 2, 2, byrow = TRUE)
s2 = matrix(c(30, 0.3, 0.3, 1), 2, 2, byrow = TRUE)
d1 = as.data.table(rmvnorm(n = n/2, mean = mu1, sigma = s1))
d1$class = 1
d2 = as.data.table(rmvnorm(n = n/2, mean = mu2, sigma = s2))
d2$class = 2
dd = rbind(d1, d2)
dd$class = as.factor(dd$class)
oo = sample(n)
dd = dd[oo,]
task = TaskClassif$new("2dgauss", dd, target = "class")

trainsize = 200
trainset = 1:trainsize
testset = (trainsize+1):n

l1 = lrn("classif.qda", predict_type = "prob") # appropriate
l2 = lrn("classif.svm", type = "C-classification", predict_type = "prob", 
         kernel = "radial", gamma = 99, cost = 1) # overfit
l3 = lrn("classif.log_reg", predict_type = "prob") # underfit

l1$train(task)
r1 = range(dd[trainset,]$V1)
r2 = range(dd[trainset,]$V2)
r1seq = seq(r1[1], r1[2], length.out = 200)
r2seq = seq(r2[1], r2[2], length.out = 200)
d_grid = expand.grid(V1 = r1seq, V2 = r2seq)
pred_true = as.data.table(l1$predict_newdata(d_grid))
d_grid$prob = pred_true$prob.1 
true_decb = d_grid[d_grid$prob > 0.47 & d_grid$prob < 0.53,]

# PLOT -------------------------------------------------------------------------

make_plot = function(ll) {
  ll$train(task, row_ids = trainset)
  pred_train = ll$predict(task, row_ids = trainset)
  trainerr = pred_train$score(msr("classif.ce"))
  pred_test = ll$predict(task, row_ids = testset)
  testerr = pred_test$score(msr("classif.ce"))
  task_train = task$filter(rows = trainset)
  pl = plot_learner_prediction(ll, task) + guides(shape = FALSE, alpha = FALSE)
  pl = pl + ggtitle(sprintf("TrainErr=%.2f;    TestErr=%.2f", trainerr, testerr))
  pl = pl + geom_point(data = true_decb, alpha=0.5, size=0.2)
  return(pl)
}

p1 = make_plot(l1) # appropriate
p2 = make_plot(l2) # overfit
p3 = make_plot(l3) # underfit

ggsave("../figure/model_eval_01.png", plot = p1, width = 8, height = 6)
ggsave("../figure/model_eval_02.png", plot = p2, width = 8, height = 6)
ggsave("../figure/model_eval_03.png", plot = p3, width = 8, height = 6)
