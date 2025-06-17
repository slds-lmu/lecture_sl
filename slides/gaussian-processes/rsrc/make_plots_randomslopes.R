# ------------------------------------------------------------------------------
# PLOT BAYESIAN LM
# ------------------------------------------------------------------------------

# Purpose: create plots for Bayesian LM chapter

# PREREQ -----------------------------------------------------------------------

library(data.table)
library(ggplot2)

# FUNCTIONS --------------------------------------------------------------------

# Draw random 0-intercept models with standard normal slope
plot_random_slopes = function(x, y, n_slopes) {
    
    dt = data.table(x = x, y = y)
    p = ggplot(data = dt, aes(x = x, y = y)) +
        geom_point() +
        theme_bw()
    
    slopes = rnorm(n_slopes)
    for (i in seq_len(n_slopes)) {
        p = p +
            geom_abline(slope = slopes[[i]], intercept = 0, color = "darkgray")
    }
    p
    
}

# PLOTS ------------------------------------------------------------------------

set.seed(1234)
n = 50

x = seq(0, 7, length.out = n)
x = sample(x, n)
y = 0.5 * x + rnorm(n)
dt = data.table(x0 = rep(1, n), x = x, y = y)

ggsave(
    "../figure/bayes_lm/example.pdf",
    ggplot(dt, aes(x = x, y = y)) +
        geom_point() +
        theme_bw(),
    width = 4,
    height = 3
)

ggsave(
    "../figure/bayes_lm/random_slopes.pdf", 
    plot_random_slopes(x, y, 50), 
    width = 4, 
    height = 3
)
