if (!requireNamespace("ggplot2")) {
  install.packages("ggplot2")
}
#if (!requireNamespace("penalizedSVM")) {
#  install.packages("penalizedSVM")
#}

# Load libraries
library(ggplot2)
#library(penalizedSVM)

set.seed(12345)
#dat.orig <- sim.data(n = 200, ng = 100, nsg = 50)
#dat <- as.data.frame(cbind(data.frame(dat.orig$x), dat.orig$y))
#dat.red <- data.frame(cbind(dat$X1, dat$X2, dat$`dat.orig$y`))
x1 = rnorm(200, 0, 1)
x2 = rnorm(200, 0, 1)
y = rbinom(200, 1, plogis(1.5*(x1+x2)))
y[y == 0] = -1
dat.red = data.frame(x1, x2, y)
names(dat.red) <- c("x1", "x2", "y")
dat.red$y <- as.factor(dat.red$y)

p <- ggplot(data = dat.red, aes(x = x1, y = x2, color = y)) +
  geom_point() +
  labs(x = "Feature 1", y = "Feature 2", color = "Class") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        panel.grid.major = element_blank()) +
  scale_x_continuous(breaks = -3:3) +
  scale_y_continuous(breaks = -3:3) +
  ggtitle("")
p




# save the plot
ggsave("slides/feature-selection/figure/fs-micro-array.png", p, width = 12, height = 10, units = "cm")
