# ------------------------------------------------------------------------------
# nonlin

# FIG: plot schematic diagrams of one-hidden-layer neural network 
#      with different sizes (1,2,3,5,10,100) (input size: 2, output size: 1).
# ------------------------------------------------------------------------------

library(RSNNS)
library(nnet)
library(clusterGeneration)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

set.seed(2)

# DATA -------------------------------------------------------------------------

num.vars<-2
num.obs<-1000
 
#input variables
cov.mat<-genPositiveDefMat(num.vars,covMethod=c("unifcorrmat"))$Sigma
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)
 
#output variables
parms<-runif(num.vars,-10,10)
y1<-rand.vars %*% matrix(parms) + rnorm(num.obs,sd=20)
 
#final datasets
rand.vars<-data.frame(rand.vars)
resp<-data.frame(y1)
names(resp)<-c('Y1')
dat.in<-data.frame(resp,rand.vars)

# plot -------------------------------------------------------------------------

nn_plot <- function(size) {
  mod1 <- nnet(rand.vars, resp, data=dat.in, size=size, linout=T)
  save_dir <- "../figure"
  filename <- file.path(save_dir, sprintf("nn_size_%d.png", size))
  png(filename, width = 3000, height = 2800, res = 500)
  par(mar = c(1, 1, 1, 1))
  
  plot.nnet(mod1, 
            nid=FALSE,
            rel.rsc=3, 
            circle.cex=2.5,
            node.labs=FALSE,
            var.labs=TRUE,
            bias=FALSE, 
            cex.val=0.7,
            circle.col="grey",
            pos.col='black', 
            neg.col='black',
            max.sp=TRUE)
  
  dev.off()
}

vec <- c(1, 2, 3, 5, 10, 100)
for (i in vec) {
  nn_plot(i)
}


