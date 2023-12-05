library(ggplot2)

lambda = 2

fun1 <- function(x){
  return(x^2 + 3*abs(x)+ 1)
}

fun2 <- function(x){
  return(0.5*(x-4)^2 + lambda*abs(x)+ 1)
}

fun3 <- function(x){
  return(0.5*(x+4)^2 + lambda*abs(x)+ 1)
}


p1 <- ggplot() +
  xlim(-7, 7) +
  geom_function(fun = fun1) +
  xlab(expression(theta)) +
  ylab(expression(R[reg])) +
  geom_vline(xintercept = 0,
             linetype="dashed") +
  theme_bw(base_size = 20)

pdf("../figure/th_l1_zero.pdf")
print(p1)
dev.off()

p2 <- ggplot() +
  xlim(-7, 7) +
  geom_function(fun = fun2) +
  xlab(expression(theta)) +
  ylab(expression(R[reg])) +
  geom_vline(xintercept = 4 - lambda,
             linetype="dashed") +
  theme_bw(base_size = 20)

pdf("../figure/th_l1_pos.pdf")
print(p2)
dev.off()

p3 <- ggplot() +
  xlim(-7, 7) +
  geom_function(fun = fun3) +
  xlab(expression(theta)) +
  ylab(expression(R[reg])) +
  geom_vline(xintercept = -4 + lambda,
             linetype="dashed") +
  theme_bw(base_size = 20)


pdf("../figure/th_l1_neg.pdf")
print(p3)
dev.off()



