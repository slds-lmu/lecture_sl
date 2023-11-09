library(ggplot2)
library(gridExtra)
library(entropy)

########## CREATE BETA DISTRIBUTIONS 

set.seed(123)

alpha <- 2
beta <- 5

x <- seq(0, 1, length.out = 1000)

beta_density <- dbeta(x, shape1 = alpha, shape2 = beta)
log_beta_density <- -log(dbeta(x, shape1 = alpha, shape2 = beta))
beta_times_log_beta <- beta_density*log_beta_density
data <- data.frame(x = x, BetaDensity = beta_density,
                   LogBetaDensity = log_beta_density,
                   BetaTimesLogBeta = beta_times_log_beta)

integrand <- function(x) {
          density <- dbeta(x, 2, 5)
            density * -log(density)
}

result <- integrate(integrand, lower = 0, upper = 1)
entropy_beta <- round(result$value, 2)

plot1 = ggplot(data, aes(x = x)) +
  geom_line(aes(y = BetaDensity), color = "blue", size = 1, linetype = "solid") +
  labs(title = "Beta(2,5)", x = "x", y = "Density") +
  scale_color_manual(values = c("blue"))

plot2 = ggplot(data, aes(x = x)) +
  geom_line(aes(y = LogBetaDensity), color = "red", size = 1, linetype = "solid") +
  labs(title = "Surprisal", x = "x", y = "-log(f(x)") +
  scale_color_manual(values = c("red"))

plot3 = ggplot(data, aes(x = x)) +
  geom_line(aes(y = BetaTimesLogBeta), color = "orange", size = 1, linetype = "solid") +
  labs(title = "Integrand", x = "x", y = "-f(x)log(f(x))") +
  geom_ribbon(aes(ymax = BetaTimesLogBeta, ymin = 0), fill = "grey", alpha = 0.5) +
  geom_text(aes(x = 0.75, y = -1, label = paste("Integral =",entropy_beta)), color = "black", size = 3) +
  scale_color_manual(values = c("orange"))

p = grid.arrange(plot1, plot2, plot3, ncol = 3)
ggsave("..figure/beta_entropy.png", plot = p, width = 9, height = 3)


########## CREATE CONTINUOUS UNIFORM DISTRIBUTION 


uni_plot = function(a, b){

  set.seed(123)
  x <- seq(a, b, length.out = 1000)
  uniform_density <- dunif(x, min = a, max = b)
  data <- data.frame(x = x, UniformDensity = uniform_density)
  integrand <- function(x) {
    density <- dunif(x, a, b)
    density * -log(density)
  }
  result <- integrate(integrand, lower = a, upper = b)
  entropy_uni <- round(result$value, 2)
  p = ggplot(data, aes(x = x)) +
      geom_line(aes(y = UniformDensity), color = "blue", size = 1) +
      labs(title =  paste("Differential entropy:", entropy_uni), x = "x", 
           y = sprintf("U(%d,%s)", a, b)) +
      geom_segment(aes(x = b, y = 0,
                     xend = b, yend = uniform_density), color = "blue", size = 1) +
      geom_segment(aes(x = a, y = 0,
                     xend = a, yend = uniform_density), color = "blue", size = 1) 
  return(p)
}

plot1 = uni_plot(0, 1)
plot2 = uni_plot(2,8)

p = grid.arrange(plot1, plot2, ncol = 2)
ggsave("..figure/uni_entropy.png", plot = p, width = 8, height = 3)


########## CREATE NORMAL DISTRIBUTIONS

normal_plot = function(mu,sigma){
  
  set.seed(123)
  x <- seq(mu-4*sigma, mu+4*sigma, length.out = 1000)
  normal_density <- dnorm(x, mu, sigma)
  data <- data.frame(x = x, NormalDensity = normal_density)
  integrand <- function(x) {
    density <- dnorm(x, mu, sigma)
    density * -log(density)
  }
  result <- integrate(integrand, lower = mu-4*sigma, upper = mu+4*sigma)
  entropy_normal <- round(result$value, 2)
  p = ggplot(data, aes(x = x)) +
    geom_line(aes(y = NormalDensity), color = "blue", size = 1) +
    labs(title =  paste("Differential entropy:", entropy_normal), x = "x", 
         y = sprintf("N(%d,%s)", mu, sigma))
  return(p)
}

plot1 = normal_plot(0, 1)
plot2 = normal_plot(2,8)

p = grid.arrange(plot1, plot2, ncol = 2)
ggsave("..figure/normal_entropy.png", plot = p, width = 8, height = 3)


############## CREATE ENTROPY PLOT FOR NORMAL DISTRIBUTION DEPENDEND ON SIGMA

sigma <- seq(0, 2, length.out = 1000)
entropy = 0.5*log(2*pi*sigma) + 0.5
data <- data.frame(Sigma = sigma, Entropy = entropy)
zero = round(1/(2*pi*exp(1)), 3)

p = ggplot(data, aes(x = Sigma)) +
  geom_line(aes(y = Entropy), color = "red", size = 1) +
  geom_vline(xintercept = zero, size = 1) +
  geom_text(aes(x = 0.5, y = 0, label = paste("sign switch at", zero)),
            color = "black", size = 3) +
  labs(title =  "Differential Entropy of Normal Density", x = "Sigma^2", 
       y = "h(Sigma^2)")

ggsave("..figure/normal_entropy_sigma.png", plot = p, width = 5, height = 3)
