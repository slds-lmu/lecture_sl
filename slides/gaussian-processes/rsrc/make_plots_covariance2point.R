# ----------------------------------------------------------------------- #
# Covariance 2D example                                                   #
# ----------------------------------------------------------------------- #

# Initialization -------------------------------------------------------- #
library(mvtnorm)
library(ggplot2)
source("covariance_functions.R")

# ----------------------------------------------------------------------- #

# Example for a discrete function with input space containing two points
x <- c(3, 2.5, 5)
df <- data.frame(x = x)

m <- c(0, 0, 0)
K <- kernel_sqexp(x, x, lengthscale = 1)  # sqexp.vec(x)

# sample 
set.seed(1234)
y <- t(rmvnorm(1, mean = m, sigma = K))
df$y <- y


p1 <- ggplot(data.frame(x = c(- 4, 4)), aes(x)) +
    stat_function(fun = kernel_sqexp_distance, geom = "line") +
    theme_bw() +
    geom_point(
        data = data.frame(x = 0.5, y = kernel_sqexp_distance(0.5)),
        aes(x = x, y = y),
        color = "orange",
        size = 2
        ) +
    geom_segment(
        data = data.frame(x = 0.5, y = kernel_sqexp_distance(0.5)), 
        aes(x = x, xend = x, y = 0, yend = y), 
        color = "orange", 
        lty = 2
    ) +
    geom_text(
        data = data.frame(x = 1.8, y = kernel_sqexp_distance(0.5)),
        aes(x = x, y = y, label = "high \n correlation \n of y values"), 
        color = "orange", 
        size = 3
    ) +
  xlab("d") +
  ylab("k(d)") +
  ggtitle("Covariance Function")

p2 <- ggplot() +
    ylim(c(-2, 2)) +
    xlim(c(0, 6)) +
    theme_bw() +
    geom_vline(data = df, aes(xintercept = x), color = "grey", lty = 2) +
    geom_text(
        data = data.frame(
            x = df$x + c(0.3, -0.3, -0.3), label = sprintf("x[%i]", 1:3)
        ),
        aes(x = x, y = -2, label = label),
        size = 3,
        parse = TRUE,
        colour = "darkgrey"
    ) +
    geom_text(
        aes(x = x[1] + 0.5 * (x[2] - x[1]), y = y[1] - 0.4, label = "d = 0.5"), 
        color = "orange", 
        size = 3
    ) +
    geom_text(
        aes(x = x[1] + 0.3, y = y[1], label = "y[1]"), size = 3, parse = TRUE
    ) +
    geom_segment(
        aes(x = x[1], xend = x[2], y = y[1], yend = y[1]), color = "orange"
    ) +
    geom_point(aes(x = x[1], y = y[1]), size = 2) +
    xlab("x") +
    ylab("f(x)")

ggsave(
    filename = "../figure/cov_funs/example_covariance_1.pdf", 
    plot = p1,, 
    height = 3, 
    width = 3
)
ggsave(
    filename = "../figure/cov_funs/example_function_1_1.pdf", 
    plot = p2, 
    height = 2.5, 
    width = 3
)

p2 <- p2 + 
    geom_segment(
        aes(x = x[2], xend = x[2], y = y[1], yend = y[2]), color = "orange"
    ) + 
    geom_point(aes(x = x[2], y = y[2]), size = 2) + 
    geom_text(
        aes(x = x[2] - 0.3, y = y[2], label = "y[2]"), size = 3, parse = TRUE
    )
ggsave(
    filename = "../figure/cov_funs/example_function_1_2.pdf", 
    plot = p2, 
    height = 2.5, 
    width = 3
)

p1 <- p1 + 
    geom_point(
        data = data.frame(x = -2.5, y = kernel_sqexp_distance(-2.5)),
        aes(x = x, y = y), 
        color = "blue", 
        size = 2
    ) + 
    geom_segment(
        data = data.frame(
            x = -2.5, xend = -2.5, y = 0, yend = kernel_sqexp_distance(2.5)
        ),
        aes(x = x, xend = xend, y = y, yend = yend), 
        color = "blue", 
        lty = 2
    )  + 
    geom_text(
        data = data.frame(
            x = - 3, 
            y = kernel_sqexp_distance(- 2.5) + 0.2, 
            label = "low \n correlation \n of y values"
        ),
        aes(x = x, y = y, label = label), 
        color = "blue", 
        size = 3
    ) + 
    labs(x = "d", y = "k(d)")
ggsave(
    filename = "../figure/cov_funs/example_covariance_2.pdf", 
    plot = p1,, 
    height = 3, 
    width = 3
)

p2 <- p2 + 
    geom_segment(
        data = data.frame(
            x = c(x[1], x[3]), xend = x[3], y = y[1], yend = c(y[1], y[3])
        ),
        aes(x = x, xend = xend, y = y, yend = yend), 
        color = "blue"
    ) +  
    geom_point(
        data = data.frame(x = x[3], y = y[3]), aes(x, y), size = 2
    ) + 
    geom_text(
        data = data.frame(x = x[3] - 0.3, y = y[3], label = "y[3]"), 
        aes(x, y , label = label),
        size = 3, 
        parse = TRUE
    )

ggsave(
    filename = "../figure/cov_funs/example_function_2_1.pdf", 
    plot = p2, 
    height = 2.5, 
    width = 3
)

