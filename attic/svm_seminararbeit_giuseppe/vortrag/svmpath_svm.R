set.seed(1234)
clust1 <- cbind(mvrnorm(20, mu=c(1,10), Sigma=matrix(c(1,1,1,10), ncol=2)), Class=1)
clust2 <- cbind(mvrnorm(15, mu=c(5,1), Sigma=matrix(c(0.5,1,1,20), ncol=2)), Class=-1)

# linear separable data
dat <- rbind(clust1,clust2)
colnames(dat) <- c("Var1","Var2","Class")
dat<-as.data.frame(dat)
dat$Class <- as.factor(dat$Class)

# non linear separable data
dat2<-dat
dat2$Class[1] <- "-1"

# works only for degree=1 (linear kernel) and for non linear separable data
degree = 1
svm2 <- svm(Class~Var2+Var1, dat2, kernel="polynomial", type="C-classification", degree=degree, coef0=1, gamma=1, cost=10000)
plot(svm2, data=dat2, Var2~Var1, grid=200, fill=F, svSymbol = "X", dataSymbol = "o")
grid1<-expand.grid(seq(min(dat2[,1]), max(dat2[,1]), length=100),
                   seq(min(dat2[,2]), max(dat2[,2]), length=100))
pred1 <- predict(svm2, grid1)
points(grid1, col=pred1, pch=3)

svmpath2 <- svmpath(as.matrix(dat2[,1:2]),as.numeric(as.character(dat2[,"Class"])),  param.kernel=degree)
plot(svmpath2)

#
dat2 <- dat # use linear separable data gives different results
# works only for degree=1 (linear kernel) and for non linear separable data
degree = 1
svm2 <- svm(Class~Var2+Var1, dat2, kernel="polynomial", type="C-classification", degree=degree, coef0=1, gamma=1, cost=10000)
plot(svm2, data=dat2, Var2~Var1, grid=200, fill=F, svSymbol = "X", dataSymbol = "o")
grid1<-expand.grid(seq(min(dat2[,1]), max(dat2[,1]), length=100),
                   seq(min(dat2[,2]), max(dat2[,2]), length=100))
pred1 <- predict(svm2, grid1)
points(grid1, col=pred1, pch=3)

svmpath2 <- svmpath(as.matrix(dat2[,1:2]),as.numeric(as.character(dat2[,"Class"])),  param.kernel=degree)
plot(svmpath2)