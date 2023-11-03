# ------------------------------------------------------------------------------
# FIG: ENTROPY
# ------------------------------------------------------------------------------

library(ggplot2)
library(gridExtra)


# DATA -------------------------------------------------------------------------

p1 <- c(0.0, 0.0, 1, 0, 0)
p2 <- c(0, 0.1, 0.8, 0.1, 0)
p3 <- c(0.1, 0.1, 0.6, 0.1, 0.1)
p4 <- c(0.15, 0.15, 0.4, 0.15, 0.15)
p5 <- c(0.175, 0.175, 0.3, 0.175, 0.175)
p6 <- c(0.2, 0.2, 0.2, 0.2, 0.2)

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


plot_list = lapply(list(p1, p2, p3, p4, p5, p6), plot_ent)

for (i in 1:length(plot_list)){
  ggsave(paste0(".../figure/max_entropy_", i, ".pdf"), plot_list[[i]], width = 6.5, height = 5)
}


# DATA -------------------------------------------------------------------------

p1 <- c(0.4, 0.2, 0.2, 0.1, 0.1)
p2 <- c(0.2, 0.4, 0.2, 0.1, 0.1)
p3 <- c(0.1, 0.2, 0.4, 0.2, 0.1)
p4 <- c(0.1, 0.1, 0.2, 0.4, 0.2)

# ENTROPY INVARIANT TO REORDERING PLOTS ------------------------------------------------------------------------

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

p <- grid.arrange(grobs = lapply(list(p1, p2, p3, p4), plot_ent), nrow = 2, ncol = 2)
ggsave(filename = ".../figure/entropy_plot_reordering.png", plot = p, width = 5, height = 3)