# ------------------------------------------------------------------------------
# PLOT FUNCTIONS ON DISCRETE DOMAINS
# ------------------------------------------------------------------------------

# Purpose: create plots of discrete functions sampled from MV Gaussians, 
# possibly along with visualizations of the corresponding covariance matrix
source("plot_functions.R")
# PREREQ -----------------------------------------------------------------------

library(checkmate)
library(data.table)
library(ggplot2)
library(mvtnorm)

source("covariance_functions.R")
source("plot_functions.R")

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
    get_kmat(x, x, "squaredexp", length_scale = length_scale)
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
        geom_point(shape = 15) +
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
        ylab("f(x)") +
        theme(
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none"
        )
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
            width = 2, 
            height = 2
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
            vert_bars = ifelse(n == 1, TRUE, FALSE)
        ),
        width = 2.5, 
        height = 2
    )
    
}
