# ------------------------------------------------------------------------------
# FIG: pointwise optimization
# ------------------------------------------------------------------------------

library(ggplot2)
library(ggpubr)

set.seed(123)

# ------------------------------------------------------------------------------

df = data.frame(x = runif(50, -2.5, 2.5), type = FALSE)
df = rbind(df, data.frame(x = rep(0, 10), type = TRUE))

df$y = 2 * df$x + rnorm(60)

lm_fit = lm(data = df, y ~ x)

k <- 5
sigma <- sigma(lm_fit)
ab <- coef(lm_fit); a <- ab[1]; b <- ab[2]

x <- seq(-k*sigma, k*sigma, length.out = 50)
y <- dlnorm(x, 0, sigma)/dnorm(0, 0, sigma) * 1

x0 <- 0
y0 <- a+b*x0
path1 <- data.frame(x = y + x0, y = x + y0)
segment1 <- data.frame(x = x0, y = y0 - k*sigma, xend = x0, yend = y0 + k*sigma)
df[df$type, ]$y <- df[df$type, ]$y + 1.5
mean <- exp(sigma/2)
med <- exp(0)
seq_l2 <- seq(mean - 2, mean + 2 , by = 0.02)

parabola <- function(x)
{y = 0.25*(x-mean)^2
return(y)}

values = parabola(seq_l2)

data_l2 <- cbind(as.data.frame(seq_l2), as.data.frame(values))
dat1 <- data_l2[1:(length(seq_l2)/2 + 0.5),]
dat2 <- data_l2[(length(seq_l2)/2 + 0.5):length(seq_l2),]

p_mean = ggplot() + geom_point(data = df[!df$type, ], aes(x = x, y = y), alpha = 0.2, size = 3)
p_mean = p_mean + geom_point(data = df[df$type, ], aes(x = x, y = y), size = 3) + ggtitle("L2 Loss: Fix one x")
p_mean = p_mean + geom_path(data = path1, aes(x = x, y = y), color = "orange") 
p_mean = p_mean + geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data = segment1, lty = 2) 
p_mean = p_mean + geom_point(aes(x = 0, y = mean), colour = "orange", size = 4, shape = 17)
p_mean = p_mean + geom_line(data = dat1, aes(x = values, y = seq_l2))
p_mean = p_mean + geom_line(data = dat2, aes(x = values, y = seq_l2))


p_med = ggplot() + geom_point(data = df[!df$type, ], aes(x = x, y = y), alpha = 0.2, size = 3)
p_med = p_med + geom_point(data = df[df$type, ], aes(x = x, y = y), size = 3) + ggtitle("L1 Loss: Fix one x")
p_med = p_med + geom_path(data = path1, aes(x = x, y = y), color = "orange") 
p_med = p_med + geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data = segment1, lty = 2) 
p_med = p_med + geom_point(aes(x = 0, y = med), colour = "orange", size = 4, shape = 17)
p_med = p_med + geom_segment(aes(x = 0, y = 1, xend = 1, yend = 3))
p_med = p_med + geom_segment(aes(x = 0, y = 1, xend = 1, yend = -1))

p <- ggarrange(p_mean, p_med, ncol = 2, nrow = 1)
p

ggsave("../figure/optimal_pointwise.png", p, width = 8, height = 3)

