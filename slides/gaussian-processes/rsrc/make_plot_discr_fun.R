# ------------------------------------------------------------------------------
# PLOT FUNCTIONS ON DISCRETE DOMAINS
# ------------------------------------------------------------------------------

# PREREQ -----------------------------------------------------------------------

library(checkmate)
library(data.table)
library(ggplot2)
library(mvtnorm)

# FUNCTIONS --------------------------------------------------------------------

# Create weakly or strongly correlated (exp decaying) covariance matrix
create_covmat = function(n, corr = "weak", min_corr = 0.01) {
    assert_choice(corr, c("weak", "strong"))
    cov_mat = matrix(min_corr, n, n)
    if (corr == "weak") {
        diag(cov_mat) = 1
    } else {
        for (i in seq_len(n)) {
            for (j in seq_len(n)) {
                decay_param = min_corr**(n - 1)
                cov_mat[i, j] = decay_param**abs(i - j)
            }
        }
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
plot_cov = function(cov_mat, mu = NULL) {
    
    assert_matrix(
        cov_mat, 
        mode = "numeric", 
        any.missing = FALSE, 
        min.rows = 2, 
        min.cols = 2
    )
    n = nrow(cov_mat)
    if (is.null(mu)) mu = rep(0, n)
    
    if (n == 2) {
        h1_min = mu[1] - 2 * cov_mat[1, 1]
        h1_max = mu[1] + 2 * cov_mat[1, 1]
        h2_min = mu[2] - 2 * cov_mat[2, 2]
        h2_max = mu[2] + 2 * cov_mat[2, 2]
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
                colours = c(low = "black", high = "white")
            ) +
            theme_bw() +
            theme(panel.grid = element_blank())
    } else {
        p = ggplot() +
            geom_tile(
                data = reshape2::melt(cov_mat), 
                aes(x = Var1, y = Var2, fill = value)
            ) +
            scale_x_reverse() +
            theme_bw() +
            scale_fill_gradientn(
                "covariance", colours = c(low = "white", high = "black")
            ) +
            theme(
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.border = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                panel.grid.major = element_blank()
            )
    }
    p

}

# PLOTS ------------------------------------------------------------------------

set.seed(123)
input_sizes = c(2, 5, 10)
corr_strengths = c("weak", "strong")
n_draws = 10

# Create plots of discrete functions for different input sizes, weak vs strong 
# correlation, multiple draws each

for (i in seq_along(input_sizes)) {
    size = input_sizes[[i]]
    for (j in seq_along(corr_strengths)) {
        corr = corr_strengths[[j]]
        cov_mat = create_covmat(size, corr, ifelse(corr == "strong", 0.99, 0.1))
        dt_list = lapply(
            seq_len(n_draws), 
            function(k) {
                x = seq(0, 1, length.out = size)
                y = draw_mvn_data(length(x), cov_mat)
                data.table(x = x, y = y, group = k, corr = corr)
            }
        )
        dt = do.call(rbind, dt_list)
        ggsave(
            sprintf("../figure/discrete/discr_%i_%s.pdf", size, corr),
            plot_discr_fun(dt$x, dt$y, dt$group),
            width = 6, 
            height = 4
        )
        ggsave(
            sprintf("../figure/discrete/discr_%i_%s_cov.pdf", size, corr),
            plot_cov(cov_mat),
            width = 5, 
            height = 4
        )
    }
}
