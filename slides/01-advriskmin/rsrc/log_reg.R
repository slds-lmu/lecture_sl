# ------------------------------------------------------------------------------
# max likelihood

# FIG: logreg er and ml
# ------------------------------------------------------------------------------

library(vistool)
library(plotly)
library(bbotk)

# ------------------------------------------------------------------------------

logreg_ER = function(x, Xmat, y) {
  return (sum(log(1+exp(Xmat %*% x)) - y * (Xmat %*% x)) + 0.5*crossprod(x)) 
}

logreg_ML = function(x, Xmat, y) {
  return(exp(-sum(log(1+exp(Xmat %*% x)) - y * (Xmat %*% x)))*prod(dnorm(x)))
}

data = iris[iris$Species == 'setosa' | iris$Species == 'versicolor',]
data$Species = as.integer(data$Species == "setosa")

Xmat = model.matrix(~ Petal.Width + Petal.Length + 0, data = data, )
y = data$Species

obj_erm = Objective$new(id = "iris logreg", fun = logreg_ER, xdim = 2,  
                        Xmat = Xmat, y = y, minimize = TRUE)

viz_erm = as_visualizer(obj_erm, x1_limits =  c(-6.5, -2), x2_limits = c(0, 1.5))
fig1 = viz_erm$plot() %>% layout(
  title = "",
  scene = list(
  xaxis = list(title="\u03B81"),
  yaxis = list(title="\u03B82"),
  zaxis = list(title="Negative log likelihood")
  )
  )
save_image(fig1, file="log_reg_erm.pdf")

obj_ml = Objective$new(id = "iris logreg ml", fun = logreg_ML, xdim = 2,  
                        Xmat = Xmat, y = y, minimize = TRUE)

viz_ml = as_visualizer(obj_ml, x1_limits = c(-6.5, -2), x2_limits = c(0, 1.5))
fig2 = viz_ml$plot() %>% layout(
  title = "",
  scene = list(
  xaxis = list(title="\u03B81"),
  yaxis = list(title="\u03B82"),
  zaxis = list(title="Likelihood")
  )
  )
save_image(fig2, file="log_reg_ml.pdf")




