# ------------------------------------------------------------------------------
# FIG: ENTROPY
# ------------------------------------------------------------------------------

library(ggplot2)
library(gridExtra)

# ENTROPY CALCULATION

p1 <- c(0.1, 0.2, 0.4, 0.2, 0.1)

n <- length(p1)
logp <- -log(p1)
lp <- ifelse(p1 == 0, 0, lp)
r <- p1 * lp
H <- sum(r)
dd <- data.frame(x = 1:n, p = p1, logp = logp, r = r)

pl1 <- ggplot(data = dd, aes(x = x)) +
      geom_bar(aes(y = p), stat = "identity") +
      ggtitle("PMF")

pl2 <- ggplot(data = dd, aes(x = x)) +
  geom_bar(aes(y = logp), stat = "identity") +
  ggtitle("Surprise") + ylab("-log(p)")

pl3 <- ggplot(data = dd, aes(x = x)) +
  geom_bar(aes(y = r), stat = "identity") +
  ggtitle("p*Surprise") + ylab("-p*log(p)")

p <- grid.arrange(grobs = list(pl1, pl2, pl3), nrow = 1, ncol = 3)
ggsave(filename = "...figure/entropy_calc.png", plot = p, width = 6, height = 2)



# DATA -------------------------------------------------------------------------

p1 <- c(0.0, 0.0, 1, 0, 0)
p2 <- c(0, 0.1, 0.8, 0.1, 0)
p3 <- c(0.15, 0.15, 0.4, 0.15, 0.15)
p4 <- c(0.2, 0.2, 0.2, 0.2, 0.2)

# ENTROPY MAXIMAL FOR UNIFORM DISTRIBUTION PLOTS ------------------------------------------------------------------------

plot_ent <- function(p) {
  n <- length(p)
  lp <- -log(p)
  lp <- ifelse(p == 0, 0, lp)
  r <- p * lp
  H <- sum(r)
  dd <- data.frame(x = 1:n, p = p, lp = lp, r = r)
  pl <- ggplot(data = dd, aes(x = x)) +
    geom_bar(aes(y = p), stat = "identity") +
    ggtitle(sprintf("Entropy H(p) = %.1f", H)
    )
  return(pl)
}

p <- grid.arrange(grobs = lapply(list(p1, p2, p3, p4), plot_ent()), nrow = 2, ncol = 2)
ggsave(filename = ".../figure/max_entropy.png", plot = p, width = 5, height = 3)


# DATA -------------------------------------------------------------------------

p1 <- c(0.4, 0.2, 0.2, 0.1, 0.1)
p2 <- c(0.2, 0.4, 0.2, 0.1, 0.1)
p3 <- c(0.1, 0.2, 0.4, 0.2, 0.1)
p4 <- c(0.1, 0.1, 0.2, 0.4, 0.2)

# ENTROPY INVARIANT TO REORDERING PLOTS ------------------------------------------------------------------------

p <- grid.arrange(grobs = lapply(list(p1, p2, p3, p4), plot_ent), nrow = 2, ncol = 2)
ggsave(filename = ".../figure/entropy_plot_reordering.png", plot = p, width = 5, height = 3)

# ENTROPY OF BERNOULLI -------------------------------------------

p1 <- c(0.7, 0.3)

plot_ber <- function(p) {
  n <- length(p)
  lp <- -log(p)
  lp <- ifelse(p == 0, 0, lp)
  r <- p * lp
  H <- sum(r)
  dd <- data.frame(x = 0:1, p = p, lp = lp, r = r)
  pl <- ggplot(data = dd, aes(x = x)) +
    geom_bar(aes(y = p), stat = "identity") +
    ggtitle(sprintf("Entropy H(p) = %.1f", H)
    )
  return(pl)
}

pl1 = plot_ber(p1)

x <- seq(0, 1, length.out = 100)
y <- -(x-0.5)^2 +0.7
parabola_data <- data.frame(x = x, y = y)

parabola_plot <- ggplot(data = parabola_data, aes(x, y)) +
  geom_line() +
  geom_vline(xintercept = 0.3, color = "red") +
  labs(x = "s", y = "H(X)") +
  ggtitle("s = 0.3")

  # Display the parabola plot
p <- grid.arrange(grobs = list(parabola_plot, pl1), nrow = 1, ncol = 2)
ggsave(filename = "...figure/entropy_bernoulli.png", plot = p, width =6, height = 3)
