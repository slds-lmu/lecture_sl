library(svmpath)
set.seed(123456)

# Funktion zur besseren Darstellung
grid.fill <- function(svm, length=100){
  x <- svm$x
  y <- svm$y
  step <- max(svm$Step)
  x1grid<-seq(min(x[,1])-1,max(x[,1])+1, length=length)
  x2grid<-seq(min(x[,2])-1,max(x[,2])+1, length=length)
  grid1<-expand.grid(x1grid, x2grid)
  pred <- predict(svm, newx=as.matrix(grid1),
                  lambda = svm$lambda[step], type = "class")
  z<-matrix(pred, nrow=length, ncol=length)
  image(x1grid,x2grid,z, add=TRUE, 
        col=c(rgb(1, 0, 0, 0.25), rgb(0, 0, 1, 0.25)))
}

# Simuliere Spiraldaten
n <- 100
tetha <- seq(length=n, from=0, to=3)
X1p <- exp(0.5*tetha)*cos(tetha*pi)
X2p <- exp(0.5*tetha)*sin(tetha*pi)
X1n <- 1 - (1 + X1p)
X2n <- 0 - X2p
X1 <- c(X1p, X1n)
X2 <- c(X2p, X2n)
class <- c(rep(1, n), rep(-1,n))
e <- rnorm(2*n, mean = 0, sd = 0.10)
Var1 <- X1 + e
Var2 <- X2 + e
dat <- data.frame(Var1=Var1, Var2=Var2, Class=class)

# Aufteilung in Trainingsdaten und Testdaten
IND <- sample(1:nrow(dat), round(0.8*nrow(dat)))
test <- dat[-IND,]
train <- dat[IND,]
coltest <- test$Class
coltest[coltest==1] <- "darkblue"
coltest[coltest==-1] <- "darkred"
plot(train$Var1, train$Var2, col=train$Class+3, 
     xlab="X1", ylab="X2", pch=8, cex=1)
legend("topright", c("positive Klasse", "negative Klasse"), 
       pch=8, col=c(4,2), bg="lightgray")

# Trainiere SVM mit gaussscher radial Basisfunktion
svm1 <- svmpath(x=as.matrix(train[,c("Var1","Var2")]), 
                y=train$Class, kernel=radial.kernel)
plot(svm1, main="", elbow.show=FALSE, support.show=FALSE)
grid.fill(svm1)
points(test$Var1, test$Var2, col=coltest, pch=17, cex=1.5)
legend("topright", c("Trainingsdaten", "Testdaten", "Hyperebene"), col=c(1,1,3),
       pch=c(8,17, NA), lwd=c(NA,NA,3), bg="lightgray", pt.cex=c(1,1.5,NA))
# Hier wurde als Kernfunktion eine gaußsche radiale Basisfunktion der Form 
# exp(-g||x-y||^2) mit g=1 verwendet, siehe auch
?poly.kernel

# Verwende nun eine polynomiale Kernfunktion der Form (xy'+1)^2
svm2 <- svmpath(x=as.matrix(train[,c("Var1","Var2")]), 
                y=train$Class, kernel=poly.kernel, param.kernel=2)
plot(svm2, main="", elbow.show=FALSE, support.show=FALSE)
grid.fill(svm2)
points(test$Var1, test$Var2, col=coltest, pch=17, cex=1.5)
legend("topright", c("Trainingsdaten", "Testdaten", "Hyperebene"), col=c(1,1,3),
       pch=c(8,17, NA), lwd=c(NA,NA,3), bg="lightgray", pt.cex=c(1,1.5,NA))

# Confusionmatrix und Missklassifikationsfehler der Testdaten
pred1 <- predict(svm1, type="class", lambda=svm1$lambda[length(svm1$lambda)], 
                new=as.matrix(test[,c("Var1","Var2")]))
table(pred1, test$Class)

pred2 <- predict(svm2, type="class", lambda=svm2$lambda[length(svm2$lambda)], 
                 new=as.matrix(test[,c("Var1","Var2")]))
table(pred2, test$Class)

sum(pred1!=test$Class)/nrow(test)
sum(pred2!=test$Class)/nrow(test)

# Confusionmatrix und Missklassifikationsfehler der Trainingsdaten
pred3 <- predict(svm1, type="class", lambda=svm1$lambda[length(svm1$lambda)])
table(pred3, train$Class)

pred4 <- predict(svm2, type="class", lambda=svm2$lambda[length(svm2$lambda)])
table(pred4, train$Class)

sum(pred3!=train$Class)/nrow(train)
sum(pred4!=train$Class)/nrow(train)