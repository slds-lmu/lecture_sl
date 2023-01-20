#dat2$Class<-as.factor(c(as.character(dat2$Class[2:35]),as.character(dat2$Class[1])))

set.seed(1234)
clust1 <- cbind(mvrnorm(20, mu=c(1,10), Sigma=matrix(c(0.1,0,0,0.1), ncol=2)), Class=1)
clust2 <- cbind(mvrnorm(15, mu=c(5,1), Sigma=matrix(c(0.5,0,0,0.5), ncol=2)), Class=-1)

dat <- rbind(clust1,clust2)
colnames(dat) <- c("Var1","Var2","Class")
dat<-as.data.frame(dat)
dat$Class <- as.factor(dat$Class)

plot(dat[,1],dat[,2], col=dat[,3])
text(dat[,1],dat[,2], rownames(dat))

dat2<-dat
#dat2$Class[4] <- "-1"
dat2$Class[26] <- "1"
#dat2$Class[32] <- "1"
#dat2$Class[25] <- "1"
#dat2$Class[24] <- "1"
#dat2$Class[27] <- "1"
#dat2$Class[31] <- "1"
#dat2<-rbind(dat2,data.frame(Var1=7,Var2=-2,Class=as.factor(-1)))

svmpath1 <- svmpath(as.matrix(dat2[,1:2]),as.numeric(as.character(dat2[,"Class"])),  param.kernel = 2)

plot(svmpath1, xlim=c(1,8), ylim=c(-2,11))
grid1<-expand.grid(seq(1,8, length=20),
                   seq(-2,11, length=20))
pred1 <- predict(svmpath1, newx=as.matrix(grid1[,1:2]), type="class", lambda=svmpath1$lambda[length(svmpath1$lambda)])
col=pred1
col[col=="1"] <- rgb(0, 0, 1, 0.2)
col[col=="-1"] <- rgb(1, 0, 0, 0.2)
points(grid1,col=col, pch=15, cex=3)

svmpath2 <- svmpath(as.matrix(dat2[,1:2]),as.numeric(as.character(dat2[,"Class"])))

plot(svmpath2)
grid1<-expand.grid(seq(1,8, length=20),
                   seq(-2,11, length=20))
pred1 <- predict(svmpath2, newx=as.matrix(grid1[,1:2]), type="class", lambda=svmpath1$lambda[length(svmpath1$lambda)])
col=pred1
col[col=="1"] <- rgb(0, 0, 1, 0.2)
col[col=="-1"] <- rgb(1, 0, 0, 0.2)
points(grid1,col=col, pch=15, cex=3)





########
set.seed(1234)
clust1 <- cbind(mvrnorm(20, mu=c(1,10), Sigma=matrix(c(1,1,1,10), ncol=2)), Class=1)
clust2 <- cbind(mvrnorm(15, mu=c(5,1), Sigma=matrix(c(0.5,1,1,20), ncol=2)), Class=-1)

dat <- rbind(clust1,clust2)
colnames(dat) <- c("Var1","Var2","Class")
dat<-as.data.frame(dat)
dat$Class <- as.factor(dat$Class)
plot(dat[,1],dat[,2], col=dat2$Class)

plot(dat[,1],dat[,2], col=dat[,3])
text(dat[,1],dat[,2], rownames(dat))

dat2<-dat
#dat2$Class[24] <- "1"
dat2$Class[20] <- "-1"
dat2$Class[4] <- "-1"

svmpath1 <- svmpath(as.matrix(dat2[,1:2]),as.numeric(as.character(dat2[,"Class"])), param.kernel=2)#, kernel=radial.kernel)
svmpath2 <- svmpath(as.matrix(dat2[,1:2]),as.numeric(as.character(dat2[,"Class"])))

par(mfrow=c(1,2), mar=c(2,0.5,2,0.5))
plot(svmpath1, axes=F, main="")
box()
grid1<-expand.grid(seq(min(dat2[,1]),max(dat2[,1]), length=30),
                   seq(min(dat2[,2]),max(dat2[,2]), length=30))
pred1 <- predict(svmpath1, newx=as.matrix(grid1[,1:2]), type="class", lambda=svmpath1$lambda[length(svmpath1$lambda)])
col=pred1
col[col=="1"] <- rgb(0, 0, 1, 0.2)
col[col=="-1"] <- rgb(1, 0, 0, 0.2)
points(grid1,col=col, pch=15, cex=2)


plot(svmpath2, axes=F, main="")
box()
grid1<-expand.grid(seq(min(dat2[,1]),max(dat2[,1]), length=30),
                   seq(min(dat2[,2]),max(dat2[,2]), length=30))
pred1 <- predict(svmpath2, newx=as.matrix(grid1[,1:2]), type="class", lambda=svmpath2$lambda[length(svmpath2$lambda)])
col=pred1
col[col=="1"] <- rgb(0, 0, 1, 0.2)
col[col=="-1"] <- rgb(1, 0, 0, 0.2)
points(grid1,col=col, pch=15, cex=2)


orthogonal <- function(svmpath, ...){
  x = svmpath$x
  y = svmpath$y
  step = svmpath$Step[length(svmpath$Step)]
  alpha = svmpath$alpha[, step]
  alpha0 = svmpath$alpha0[step]
  lambda = svmpath$lambda[step]
  beta <- (alpha * y) %*% x
  scale <- diff(range(x[,1]))/diff(range(x[,2]))
  plotit<-plot(svmpath, ...)
  pointIndex <- plotit$support[!plotit$support%in%plotit$Elbow]
  pointClass <- y[pointIndex]
  
  # senkrechte
  m <- -1/((-beta[1]/beta[2])*scale*scale)
  pointCoord <- (x[pointIndex,1:2]) #x[pointIndex,1:2]
  ortho <- t(apply(pointCoord,1,function(X){c(X[2]-m*X[1],m)})) #c(pointCoord[2]-m*pointCoord[1],m)
  xiCoord <- cbind(pointCoord[,1]+0.3,(pointCoord[,1])*ortho[,2]+ortho[,1])
  
  upper <- c(lambda/beta[2] - alpha0/beta[2], -beta[1]/beta[2])
  lower <- c(-lambda/beta[2] - alpha0/beta[2], -beta[1]/beta[2])
  
  # Schnittpunkt upper und orthogonal
  cutUpX <- apply(ortho[pointClass==1,], 1,function(X){(X[1]-upper[1])/(upper[2]-X[2])}) #(ortho[1]-upper[1])/(upper[2]-ortho[2])
  cutUpY <- upper[1] + upper[2]*cutUpX
  
  cutLoX <- apply(ortho[pointClass==-1,], 1,function(X){(X[1]-lower[1])/(lower[2]-X[2])}) #(ortho[1]-lower[1])/(lower[2]-ortho[2])
  cutLoY <- lower[1] + lower[2]*cutLoX
  
  listeUp <- list(pointCoord[pointClass==1,],cbind(cutUpX,cutUpY))
  listeLo <- list(pointCoord[pointClass==-1,],cbind(cutLoX,cutLoY))
  for(i in 1:sum(pointClass==1)){
    lines(rbind(listeUp[[1]][i,],listeUp[[2]][i,]), lwd=3)
  }
  for(j in 1:sum(pointClass==-1)){
    lines(rbind(listeLo[[1]][j,],listeLo[[2]][j,]), lwd=3)
  }
  #text(xiCoord,labels=expression(xi[i]^"*"), cex=2)
  for(i in 1:5) text(xiCoord[i,1], xiCoord[i,2], substitute(list(xi)[list(x)], list(x=i)), cex=2)
}

orthogonal(svmpath2)
orthogonal(svmpath2, 21,"lower")
orthogonal(svmpath2, 20,"lower")
orthogonal(svmpath2, 18,"upper")
orthogonal(svmpath2, 17,"upper")