# Creates an animation demonstrating how greedy forward search in varsel works

library(mlr)

# Specify sampling strategy (10fold CV)
rdesc = makeResampleDesc("CV", iters = 10)

# Specify search strategy (sequential forward search)
ctrl = makeFeatSelControlSequential(method = "sfs", alpha = -20)

# Select features
sFeats = selectFeatures(learner = "classif.rpart", task = iris.task, resampling = rdesc,
  control = ctrl)
sFeats
path = as.data.frame(sFeats$opt.path)


# Picture
library(plotrix)
# Unfortunately, all plots have to be saved separately
#png("wrapperanim1.png", type = "cairo")
#png("wrapperanim2.png", type = "cairo")
#png("wrapperanim3.png", type = "cairo")
#png("wrapperanim4.png", type = "cairo")
#png("wrapperanim5.png", type = "cairo")
#png("wrapperanim6.png", type = "cairo")

# Layout
par(mar = c(0, 0, 0, 0))
# Create base picture
plot(1, type = "n", xlim = c(0, 24), ylim = c(-1, 11), axes = FALSE)

# 1. slide
for (j in c(0, 20)) {
  rect(j, 6, j + 4, 5, border = "black")
    for (i in seq(j + 0.5, j + 3.5, 1)) {
    draw.circle(i, 5.5, 0.3)
    }
}

xleft_init = 5
ybottom = 3
xright_init = 9
ytop = 2

for (k in seq(0,10,5)) {

  xleft = xleft_init + k
  xright = xright_init + k

  m = 6
  n = 0
  if (k == 5) {
    m = 8
    n = -2
  }

  for (j in seq(n,m,2)) {
      rect(xleft, ybottom + j, xright, ytop + j, border = "black")
    for (i in seq(xleft + 0.5, xleft + 3.5, 1)) {
      draw.circle(i, ybottom + j - 0.5, 0.3)
    }
  }
}
#dev.off()

# Animation
# 2. Slide
text(1.6, 4.7, format(round(path$mmce.test.mean[1], 4), nsmall = 3), cex = 1.5)
#dev.off()

# 3. Slide
for (a in seq(8.5, 2.5, -2)) {
  arrows(4, 5.5, 5, a, length = 0.1, lwd = 2)
}
text(c(6.6, 6.6, 6.6, 6.6), rev(c(1.7, 3.7, 5.7, 7.7)),
  format(round(path$mmce.test.mean[2:5], 4), nsmall = 3), cex = 1.5)
i = 0
for (j in seq(6, 0, -2)) {
  draw.circle(xleft_init  + 0.5 + i, ybottom  + j - 0.5, 0.3, col = "black")
  i = i + 1
}
#dev.off()

# 4. Slide
for (a in c(6.5, 2.5, 0.5)) {
  arrows(9, 2.5, 10, a, length = 0.1, lwd = 2)
}
text(c(11.6, 11.6, 11.6), rev(c(-0.3, 1.7, 5.7)),
  format(round( path$mmce.test.mean[6:8], 4), nsmall = 3), cex = 1.5)
i = 0
for (j in c(-2, 0, 4)) {

  draw.circle(xleft_init + 5 + 3 + 0.5, ybottom + j - 0.5, 0.3, col = "black")
  draw.circle(xleft_init + 5 + i + 0.5, ybottom + j - 0.5, 0.3, col = "black")
  i = i + 1
}
#dev.off()

# 5. Slide
for (a in c(8.5, 4.5)) {
  arrows(14, 2.5, 15, a, length = 0.1, lwd = 2)
}
text(c(16.6, 16.6), c(7.7, 3.7),
  format(round(path$mmce.test.mean[9:10], 4), nsmall = 3), cex = 1.5)

l = 0
for (j in c(6, 2)) {
  for (i in c(1, 3)) {
  draw.circle(xleft_init + 10 + i + 0.5, ybottom + j - 0.5, 0.3, col = "black")
  }
  draw.circle(xleft_init + 10 + l + 0.5, ybottom + j - 0.5, 0.3, col = "black")
  l = l + 2
}
#dev.off()

# 6. Slide
arrows(19, 8.5, 20, 5.5, length = 0.1, lwd = 2)
text(21.6, 4.7, format(round(path$mmce.test.mean[11], 4), nsmall = 3), cex = 1.5)
for (j in seq(0, 3, 1)) {
  draw.circle(xleft_init + 15 + j + 0.5, ybottom + 3 - 0.5, 0.3, col = "black")
}
draw.ellipse(2, 5.3, 2.7, 1.2, border = "red", lwd = 2)
draw.ellipse(7, 2.3, 2.7, 1.2, border = "red", lwd = 2)
#dev.off()



