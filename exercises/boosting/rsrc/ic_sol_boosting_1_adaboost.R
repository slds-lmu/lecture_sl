library(rpart)
library(rpart.plot)
# Define x
x <- c(1, 3, 5)
# Define y
y = as.factor(c('negative', 'positive', 'negative'))
df = data.frame(x, y)
# train and plot tree
tree = rpart(y ~ x,
             data = df,
             control = rpart.control(
               minsplit = 0,
               maxdepth = 1,
               cp = -1
             ))
prp(tree)

w <- rep(1 / 3, 3)

M <- 100
err <- rep(0,M)
beta <- rep(1, M)
b_list <- list()
f <- rep(0,3)
m <- 1
while (m < M) {
  cat("m =", m, ":\n")
  # 3: Fit classifier with weights
  tree = rpart(y ~ x,
               data = df,
               weights = w,
               control = rpart.control(
                 minsplit = 0,
                 maxdepth = 1,
                 cp = -1
               ))
  cat("split point:", tree$splits[,"index"], "\n")
  # 4: Calculate error
  b <- predict(tree, df, type="class")
  indicator <- b!=y
  err[m] <- sum(w*indicator)
  cat("error unweighted:", sum(indicator)/3, "\n")
  cat("error weighted:", err[m], "\n")
  
  # 5: Calculate beta
  beta[m] <- 0.5 * log((1-err[m])/err[m])
  
  # Update weigths
  y_mal_b <- -(indicator*2-1)
  w_unnormalized <- w * exp (-beta[m]*y_mal_b)
  w_normalized <- w_unnormalized/sum(w_unnormalized)
  w <- w_normalized
  cat("new weights after this iteration:", w, "\n")
  b_list[[m]] <- b
  f <- f + beta[m]*(as.numeric(b)*2-3)
  cat("f:", f, "\n")
  h <- f>0
  err_ens <- sum(h!=(y=="positive"))/3
  cat("error of ensemble:", err_ens, "\n")
  if(err_ens==0){
    m=M
  }else{
    m <- m+1
  }
}
