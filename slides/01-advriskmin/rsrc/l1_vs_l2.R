# ------------------------------------------------------------------------------
# FIG: l1 vs l2
# ------------------------------------------------------------------------------

library(ggplot2)
library(quantreg)
library(data.table)
theme_set(theme_bw())

# ------------------------------------------------------------------------------

x = runif(50, -2, 2)
df = data.frame(x = x, y = rnorm(50, mean = 0, sd = 0.5))

computeOptimalConstant = function(df, loss, a = 2) {
  switch(loss, 
         L1 = rq(y ~ 1, data = df, .5)$fitted.values, 
         L2 = lm(y ~ 1, data = df)$fitted.values,
         quant25 = rq(y ~ 1, data = df, .25)$fitted.values,
         quant75 = rq(y ~ 1, data = df, .75)$fitted.values,
         Huber1 = rlm(y ~ 1, data = df, scale.est = "Huber", k2 = 2, maxit = 100)$fitted.values,
         eps2 = {
           lm_eps_2 = nrbm(epsilonInsensitiveRegressionLoss(x = matrix(rep(1, times = nrow(df))),y = df$y, epsilon = 2))
           predict(lm_eps_2, matrix(rep(1, times = nrow(df))))
         }, 
         log_barrier = {
           f = function(c) {
             mean(log_barrier(df$y - c, a = a))
           }
           
           c = optimize(f, interval = c(-2, 2))
           
           if (!is.finite(c$objective)) {
             c = NA 
           } else {
             c = c$minimum        
           } 
           return(c)
         })
}


plotConstantModel = function(df, loss_type = c("L1", "L2", "quant25", "quant75", "Huber1", "eps2", "log_barrier"),
                             labels = NULL, a = 2) {
  
  optimal_constants = lapply(loss_type, function(loss) computeOptimalConstant(df, loss, a = a))
  
  dfp = do.call(cbind, optimal_constants)
  colnames(dfp) = loss_type
  dfp = cbind(data.frame(x = df$x), dfp)
  
  dfm = reshape2::melt(dfp, id.vars = c("x"))
  colnames(dfm) = c("x", "loss", "y")
  
  dfm = setDT(dfm)
  
  p = ggplot() + geom_point(data = df, aes(x = x, y = y))
  p = p + geom_line(data = dfm[loss %in% loss_type, ],
                    aes(x = x, y = y, colour = loss))
  if (!is.null(labels)) {
    p = p + scale_color_discrete(
      name = "", 
      labels = labels[1:length(loss_type)])
  }
  p = p + theme(legend.position = 'top', legend.direction = 'horizontal')
  
  return(p)
}

#L1-loss vs. L2-loss
p = plotConstantModel(df, loss_type = c("L1", "L2"))
p

ggsave("../figure/l1_vs_l2.png", width = 4, height = 3)
