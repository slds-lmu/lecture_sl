# ------------------------------------------------------------------------------
# PLOT FUNCTIONS ON DISCRETE DOMAINS
# ------------------------------------------------------------------------------

# PREREQ -----------------------------------------------------------------------

library(checkmate)
library(data.table)
library(ggplot2)
library(mvtnorm)

# FUNCTIONS --------------------------------------------------------------------

# Create weakly or strongly correlated covariance matrix
create_covmat = function(n, corr = "weak") {
    assert_choice(corr, c("weak", "strong"))
    if (corr == "weak") {
        cov_mat = diag(n)
    } else {
        cov_mat = matrix(0.99, nrow = n, ncol = n)
        diag(cov_mat) = 1
    }
    cov_mat
}

# Draw MV normal data with mean 1 and given covariance
draw_mvn_data = function(n, cov_mat) {
    c(rmvnorm(1, rep(1, n), sigma = cov_mat))
}

# Plot discrete functions for multiple vectors
plot_discr_fun = function(x, y, group, draw_line = TRUE, vert_bars = FALSE) {
    
    assert_true(all.equal(length(x), length(y), length(group)))
    dt = data.table(x = x, y = y, group = group)
    
    p = ggplot() +
        theme_bw()
    p = p + geom_point(
        dt,
        mapping = aes(x = x, y = y, group = group, color = group),
        shape = 19
    )
    if (draw_line) {
        p = p + geom_line(
            dt,
            mapping = aes(x = x, y = y, group = group, color = group),
            lty = 2
        )
    }
    if (vert_bars) {
        p = p + geom_segment(
            mapping = aes(x = x, y = y, xend = x, yend = -Inf),
            color = "darkgray",
            lty = 2
        )
    }
    p = p +
        scale_x_continuous(breaks = x, labels = round(x, 1)) +
        scale_y_continuous(breaks = NULL, labels = NULL) +
        scale_color_gradientn(colours = c(low = "gray", high = "blue")) +
        ylab("h(x)") +
        theme(
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none"
        )
    p
}

# Plot covariance as bivariate density or multivariate heatmap
plot_cov = function(mu, cov_mat) {
    
    assert_matrix(
        cov_mat, 
        mode = "numeric", 
        any.missing = FALSE, 
        min.rows = 2, 
        min.cols = 2
    )
    n = nrow(cov_mat)
    
    if (n == 2) {
        h1_min = mu[1] - 2 * cov_mat[1, 1]
        h1_max = mu[1] + 2 * cov_mat[1, 1]
        h2_min = mu[2] - 2 * cov_mat[2, 1]
        h2_max = mu[2] + 2 * cov_mat[2, 1]
        grid = expand.grid(
            h1 = seq(h1_min, h1_max, length.out = 100), 
            h2 = seq(h2_min, h2_max, length.out = 100)
        )
        probs = cbind(grid, density = dmvnorm(grid, mean = mu, sigma = cov_mat))
        p = ggplot() +
            labs(x = expression(h[1]), y = expression(h[2])) +
            geom_raster(data = probs, aes(x = h1, y = h2, fill = density)) +
            geom_contour(
                data = probs,
                aes(x = h1, y = h2, z = density),
                colour = "white",
                bins = 5
            ) +
            guides(fill = FALSE) +
            scale_fill_gradientn(
                colours = c(low = "white", high = "blue")
            ) +
            theme_bw()
    }
    p

}

# PLOTS ------------------------------------------------------------------------

set.seed(123)
input_sizes = c(2, 5, 10)
n_draws = 10

# Create plots of discrete functions for different input sizes, weak vs strong 
# correlation, multiple draws each

dt_list = lapply(
    input_sizes,
    function(i) {
        dt = lapply(
            c("weak", "strong"),
            function(j) {
                dt = lapply(
                    seq_len(n_draws),
                    function(k) {
                        x = seq(0, 1, length.out = i)
                        y = draw_mvn_data(length(x), create_covmat(i, corr = j))
                        data.table(x = x, y = y, group = k, corr = j)
                    }
                )
                do.call(rbind, dt)
            }
        )
        dt = do.call(rbind, dt)
        dt[, input_size := i]
    }
)
dt = do.call(rbind, dt_list)

for (i in input_sizes) {
    for (j in c("weak", "strong")) {
        dt_plot = dt[input_size == i & corr == j, .(x, y, group)]
        plot_discr_fun(dt_plot$x, dt_plot$y, dt_plot$group)
    }
}