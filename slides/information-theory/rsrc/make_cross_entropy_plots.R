library(ggplot2)
library(gridExtra)
library(extraDistr)

### CREATE LOG DIFFERENCE PLOT FOR KL

set.seed(123)

cross_ent <- function(x, p, q, first) {
  ent_p <- -p * log(p)
  kl <- p * log(p / q)
  cross_ent1 <- ent_p + kl
  cross_ent2 <- -p * log(q)
  
  data <- data.frame(
    x = x,
    P = p,
    Q = q,
    EntP = ent_p,
    KL = kl,
    Cross_Ent1 = cross_ent1,
    Cross_Ent2 = cross_ent2
  )
  
  if (first == "normal") {
    kl_int <- function(x) {
      p <- dnorm(x, 0, 1)
      q <- dlaplace(x, 0, 3)
      log_ratio <- log(p / q)
      p * log_ratio
    }
    result <- integrate(kl_int, lower = -20, upper = 20)
    kl <- round(result$value, 2)
    
    ent_int <- function(x) {
      p <- dnorm(x, 0, 1)
      q <- dlaplace(x, 0, 3)
      - p * log(p)
    }
    result <- integrate(ent_int, lower = -20, upper = 20)
    entropy <- round(result$value, 2)
    
    cross_entropy1 = entropy + kl
    
    cross_int <- function(x) {
      p <- dnorm(x, 0, 1)
      q <- dlaplace(x, 0, 3)
      - p * log(q)
    }
    result <- integrate(cross_int, lower = -20, upper = 20)
    cross_entropy2 <- round(result$value, 2)
    
  } else if (first == "laplace") {
    kl_int <- function(x) {
      q <- dnorm(x, 0, 1)
      p <- dlaplace(x, 0, 3)
      log_ratio <- log(p / q)
      p * log_ratio
    }
    result <- integrate(kl_int, lower = -20, upper = 20)
    kl <- round(result$value, 2)
    
    ent_int <- function(x) {
      q <- dnorm(x, 0, 1)
      p <- dlaplace(x, 0, 3)
      - p * log(p)
    }
    result <- integrate(ent_int, lower = -20, upper = 20)
    entropy <- round(result$value, 2)
    
    cross_entropy1 = entropy + kl
    
    cross_int <- function(x) {
      q <- dnorm(x, 0, 1)
      p <- dlaplace(x, 0, 3)
      - p * log(q)
    }
    result <- integrate(cross_int, lower = -20, upper = 20)
    cross_entropy2 <- round(result$value, 2)
  }
  
  plot1 = ggplot(data, aes(x = x)) +
    geom_line(aes(y = P),
              color = "blue",
              size = 1,
              linetype = "solid") +
    geom_line(aes(y = Q),
              color = "red",
              size = 1,
              linetype = "solid") +
    labs(title = "N(0,1) and LP(0,3) Densities", x = "x", y = "Density") +
    scale_color_manual(values = c("blue"))
  
  plot2 = ggplot(data, aes(x = x)) +
    geom_line(
      aes(y = EntP),
      color = "blue",
      size = 1,
      linetype = "solid"
    ) +
    geom_line(
      aes(y = KL),
      color = "orange",
      size = 1,
      linetype = "solid"
    ) +
    geom_ribbon(aes(ymin = KL, ymax = EntP), alpha = 0.2) +
    geom_ribbon(aes(ymin = 0, ymax = KL), alpha = 0.2) +
    labs(
      title = sprintf("H(p) = %g, D_KL(p||q) =  %g", entropy, kl),
      x = "x",
      y = "Integrals"
    ) +
    scale_color_manual(values = c("blue"))
  
  plot3 = ggplot(data, aes(x = x)) +
    geom_line(
      aes(y = Cross_Ent1),
      color = "darkgreen",
      size = 1,
      linetype = "solid"
    ) +
    geom_ribbon(aes(ymin = 0, ymax = Cross_Ent1), alpha = 0.2) +
    labs(
      title = sprintf("H(p||q) = %g + %g = %g", entropy, kl, cross_entropy1),
      x = "x",
      y = "Cross-Entropy"
    ) +
    scale_color_manual(values = c("blue"))
  
  plot4 = ggplot(data, aes(x = x)) +
    geom_line(
      aes(y = Cross_Ent2),
      color = "darkgreen",
      size = 1,
      linetype = "solid"
    ) +
    geom_ribbon(aes(ymin = 0, ymax = Cross_Ent2), alpha = 0.2) +
    labs(
      title = sprintf("H(p||q) = -Int[p(x)*log(q(x))dx] = %g", cross_entropy2),
      x = "x",
      y = "Cross-Entropy"
    ) +
    scale_color_manual(values = c("blue"))
  
  plot = grid.arrange(plot1, plot2, plot3, plot4,  ncol = 2)
  
  return(plot)
  
}

x <- seq(-4, 4, length.out = 1000)
plot1 = cross_ent(x, p = dnorm(x, 0, 1), q = dlaplace(x, 0, 3), first = "normal")
plot2 = cross_ent(x, p = dlaplace(x, 0, 3), q = dnorm(x, 0, 1), first = "laplace")

ggsave("..figure/cross_entropy_plot_1.png", plot = plot1, width =8, height = 5)
ggsave("..figure/cross_entropy_plot_2.png", plot = plot2, width =8, height = 5)

