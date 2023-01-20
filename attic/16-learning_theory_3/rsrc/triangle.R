# triangle example

n = 1000
l.min = 1
l.max = 30

isTrianglePossible = function(edges) {
  edge.max = max(edges)
  edges.remain = sum(edges) - edge.max
  return(as.integer(edges.remain > edge.max))
}

a = runif(n, l.min, l.max)
b = runif(n, l.min, l.max)
c = runif(n, l.min, l.max)
X = data.frame(a, b, c)

y = apply(X, MARGIN = 1, FUN = isTrianglePossible)
mean(y)

df = data.frame(X, y)

# fitting original features
library(mlr)

lrn.nn = makeLearner("classif.neuralnet", predict.type = "response", hidden = c(2), stepmax = 1e+06)

# neural net unsorted
task = makeClassifTask(data = df, target = "y", positive = 1)
mod.nn = train(lrn.nn, task)
summary(mod.nn$learner.model)
plot(mod.nn$learner.model)

pred = predict(mod.nn, task)
performance(pred, measures = mmce)


# sorted data
X.sorted = t(apply(X, MARGIN = 1, FUN = sort, decreasing = TRUE))
df.sorted = data.frame(X.sorted, y)
dimnames(df.sorted)[[2]] = c("a", "b", "c", "y")

task.sorted = makeClassifTask(data = df.sorted, target = "y", positive = 1)
mod.nn.sorted = train(lrn.nn, task.sorted)
plot(mod.nn.sorted$learner.model)

pred.sorted = predict(mod.nn.sorted, task)
performance(pred.sorted, measures = mmce)
