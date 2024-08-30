# ------------------------------------------------------------------------------
# enetlogreg

# FIG: binary classification task visualization with different values of lambda
# ------------------------------------------------------------------------------

library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(ggplot2)
library(gridExtra)
library(viridis)
library(LiblineaR)

theme_set(theme_minimal())

set.seed(314159)

# DATA -------------------------------------------------------------------------

# Create feature map by taking polynomial feature map for x1 and x2:
polyDf <- function (mydf, y = NULL, degree = 7)
{
  
  if (! is.null(y[1])) {
    out <- data.frame(
      y = mydf$y,
      poly(mydf$x1, degree = degree, raw = TRUE),
      poly(mydf$x2, degree = degree, raw = TRUE)
    )
    
    names(out) <- c("y", paste0("X1.", 1:degree), paste0("X2.", 1:degree))
  } else {
    out <- data.frame(
      poly(mydf$x1, degree = degree, raw = TRUE),
      poly(mydf$x2, degree = degree, raw = TRUE)
    )
    
    names(out) <- c(paste0("X1.", 1:degree), paste0("X2.", 1:degree))
  }
  return (out)
}


n <- 100

# Simulate data frame, y is choosed by grouping after the euklidean norm. 
# This leads to a structure which isn't seperateable by linear hyperplanes:
mydf <- data.frame(
  x1 = runif(n, -1, 1),
  x2 = runif(n, -1, 1)
)

y <- ifelse(
  apply(mydf, 1, function (x) { return (sqrt(sum(x^2))) }) + rnorm(n, 0, 0.2) < 0.6, 0, 1
)

mydf$y <- as.factor(ifelse(y == 0, "Group1", "Group2"))

# Create the new data frame with feature map:
mydf.poly <- polyDf(mydf, mydf$y)

# PLOT -------------------------------------------------------------------------

# visualize result of classification task
plotRegLogReg <- function(lambda) {
  task <- TaskClassif$new(id = "poly_task", backend = mydf.poly, target = "y")
  if (lambda != 0){
    lrn <- lrn("classif.glmnet", alpha = 0, lambda = lambda)
  }else{
    lrn <- lrn("classif.log_reg")
  }
  model <- lrn$train(task)
  
  test <- expand.grid(x1 = seq(-1, 1, 0.05), x2 = seq(-1, 1, 0.03))
  poly.test <- polyDf(test)

  test$Group <- model$predict_newdata(newdata = poly.test)$response
  
  gg <- ggplot(test, aes(x = x1, y = x2, color = Group)) +
    geom_point(alpha = 0.3, stroke = 0, shape = 15) + 
    geom_point(data = mydf, aes(x = x1, y = x2, color = y)) +
    ggtitle(bquote(lambda == .(as.character(lambda)))) +
    scale_color_viridis(end = 0.9, discrete = TRUE)
  
  return (gg)
}

gg1 <- plotRegLogReg(0) + theme(legend.position="none")
gg2 <- plotRegLogReg(0.0001) + theme(legend.position="none")
gg3 <- plotRegLogReg(1)

g_legend <- function (a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

mylegend <- g_legend(gg3)

p <- grid.arrange(arrangeGrob(gg1, gg2, gg3 + theme(legend.position = "none"), ncol = 3),
  mylegend, layout_matrix = matrix(c(1,1,1,2), nrow = 1))

ggsave("../figure/reg_logreg.png", plot = p, width = 8, height = 2.5)
