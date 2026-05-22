
library(ggplot2)
library(ggpubr)

set.seed(123)

df = data.frame(x = runif(50, -2.5, 2.5), type = FALSE)
df = rbind(df, data.frame(x = rep(0, 10), type = TRUE))

df$y = 2 * df$x + rnorm(60)

lm_fit = lm(data = df, y ~ x)

k <- 5
sigma <- sigma(lm_fit)
ab <- coef(lm_fit); a <- ab[1]; b <- ab[2]

x <- seq(-k*sigma, k*sigma, length.out = 50)
y <- dnorm(x, 0, sigma)/dnorm(0, 0, sigma) * 1

x0 <- 0
y0 <- a+b*x0
path1 <- data.frame(x = y + x0, y = x + y0)
segment1 <- data.frame(x = x0, y = y0 - k*sigma, xend = x0, yend = y0 + k*sigma)
df[df$type, ]$y <- df[df$type, ]$y


p = ggplot() + geom_point(data = df[!df$type, ], aes(x = x, y = y), alpha = 0.2, size = 3)
p = p + geom_point(data = df[df$type, ], aes(x = x, y = y), size = 3)
p = p + geom_path(data = path1, aes(x = x, y = y), color = "orange") 
p = p + geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data = segment1, lty = 2) 
p = p + geom_point(data = df[df$type, ], aes(x = x + 2, y = y + 4), size = 3)
p = p + geom_path(data = path1, aes(x = x + 2, y = y + 4), color = "orange") 
p = p + geom_segment(aes(x=x + 2,y=y + 4,xend=xend + 2,yend=yend + 4), data = segment1, lty = 2)
p = p + geom_point(data = df[df$type, ], aes(x = x - 2, y = y - 4), size = 3)
p = p + geom_path(data = path1, aes(x = x - 2, y = y - 4), color = "orange") 
p = p + geom_segment(aes(x=x - 2,y=y - 4,xend=xend - 2,yend=yend - 4), data = segment1, lty = 2) 
p = p + geom_abline(slope = b, intercept = a)

p

ggsave("../figure/ftrue.pdf", width = 5, height = 3)