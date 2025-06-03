# ------------------------------------------------------------------------------
# PLOT FUNCTIONS ON DISCRETE DOMAINS
# ------------------------------------------------------------------------------

# Purpose: create plots of discrete functions sampled from MV Gaussians, 
# possibly along with visualizations of the corresponding covariance matrix

# PREREQ -----------------------------------------------------------------------

library(checkmate)
library(data.table)
library(ggplot2)
library(mvtnorm)

# FUNCTIONS --------------------------------------------------------------------

# Create identity covariance matrix
make_covmat_identity = function(n) diag(n)

# Create covariance matrix with exp decaying covariance
make_covmat_expdecay = function(n, min_cov = 0.99) {
    cov_mat = matrix(min_cov, n, n)
    decay_param = min_cov**(n - 1)
    for (i in seq_len(n)) {
        for (j in seq_len(n)) {
            cov_mat[i, j] = decay_param**abs(i - j)
        }
    }
    cov_mat
}

# Create covariance matrix with squared exponential kernel
make_covmat_sqexp = function(n, length_scale = 0.1) {
    sqexp_kernel = function(x1, x2, length_scale) {
        dist_mat = as.matrix(dist(c(x1, x2), method = "euclidean"))
        exp(-0.5 * dist_mat**2 / length_scale**2)
    }
    x = seq(0, 1, length.out = n)
    sqexp_kernel(x, x, length_scale)[1:n, 1:n]
}

# Create covariance matrix of chosen type
make_covmat = function(n, cov_type, ...) {
    assert_choice(cov_type, c("identity", "expdecay", "squaredexp"))
    switch(
        cov_type,
        identity = make_covmat_identity(n),
        expdecay = make_covmat_expdecay(n, ...),
        squaredexp = make_covmat_sqexp(n, ...)
    )
}

# Draw MV normal data with mean 1 and given covariance
draw_mvn_data = function(n, cov_mat) {
    c(rmvnorm(1, rep(1, n), sigma = cov_mat))
}

# Plot discrete functions for multiple vectors
plot_discr_fun = function(x, y, group, draw_line = TRUE, vert_bars = FALSE) {
    
    assert_true(all.equal(length(x), length(y), length(group)))
    dt = data.table(x = x, y = y, group = group)
    
    p = ggplot(dt, aes(x = x, y = y, group = group, color = group)) + 
        geom_point(shape = 19) +
        theme_bw()
    if (draw_line) {
        p = p + geom_line(
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
    if (nrow(dt) > 10) {
        p = p +
            scale_x_continuous(breaks = x, labels = NULL)
    } else {
        p = p +
            scale_x_continuous(breaks = x, labels = round(x, 1))
    }
    p = p +
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
            guides(fill = "none") +
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
input_sizes = c(2, 5, 10, 50)
cov_types = c("identity", "expdecay", "squaredexp")
n_draws = c(1, 10)

plot_cases = as.data.table(
    expand.grid(input_sizes[1:3], cov_types[1:2], n_draws, TRUE)
)
names(plot_cases) = c("size", "cov", "n", "save_plot_cov")
additional_cases = list(
    size = rep(50, 2),
    cov = c("identity", "squaredexp"),
    n = rep(1, 2),
    save_plot_cov = rep(FALSE, 2)
)
plot_cases = rbind(plot_cases, additional_cases)
plot_cases$cov = as.character(plot_cases$cov)

# Create plots of discrete functions for different input sizes, weak vs strong 
# correlation, multiple draws each

for (i in seq_len(nrow(plot_cases))) {
    
    dt = plot_cases[i, ]
    size = dt$size
    cov = dt$cov
    n = dt$n
    save_plot_cov = dt$save_plot_cov
    
    cov_mat = make_covmat(size, cov)
    if (save_plot_cov) {
        ggsave(
            sprintf("../figure/discrete/discr_%i_%s_cov.pdf", size, cov),
            plot_cov(cov_mat),
            width = 5, 
            height = 4
        )
    }
    
    dt_list = lapply(
        seq_len(n), 
        function(j) {
            x = seq(0, 1, length.out = size)
            y = draw_mvn_data(length(x), cov_mat)
            data.table(x = x, y = y, group = j, cov = cov)
        }
    )
    dt = do.call(rbind, dt_list)
    ggsave(
        sprintf(
            "../figure/discrete/discr_%i_%s_%in.pdf", size, cov, n
        ),
        plot_discr_fun(
            dt$x, 
            dt$y, 
            dt$group, 
            draw_line = ifelse(n == 1, FALSE, TRUE),
            vert_bars = FALSE  # ifelse(k == 1, TRUE, FALSE)
        ),
        width = 6, 
        height = 4
    )
    
}
