# ------------------------------------------------------------------------------
# PLOT 2D BROWNIAN MOTION
# ------------------------------------------------------------------------------

# Purpose: create plots (mainly for covariance chapter) to visualize samples
# from GP priors characterized by different kernels

# PREREQ -----------------------------------------------------------------------

if (FALSE) install.packages("pak")

library(checkmate)
library(data.table)
library(ggplot2)
library(mvtnorm)
library(patchwork)
library(plotly)
# pak::pak("slds-lmu/vistool")
# library(vistool)

source("covariance_functions.R")

# FUNCTIONS --------------------------------------------------------------------

cov_brownian = function(s, t) pmin(s[[1]], t[[1]]) * pmin(s[[2]], t[[2]])
# also see https://www.uni-ulm.de/fileadmin/website_uni_ulm/mawi.inst.110/lehre/ws13/Stochastik_II/Skript_4.pdf

# PLOTS ------------------------------------------------------------------------

set.seed(123)
n_pts = 15
x1 = x2 = seq(0, 1, length.out = n_pts)
grid_x = expand.grid(x1 = x1, x2 = x2)
n_tuples = nrow(grid_x)
kmat = matrix(0, n_tuples, n_tuples)
# TODO make faster
for (i in seq_len(n_tuples)) {  # upper triangular
    for (j in i:n_tuples) {
        kmat[i, j] = cov_brownian(grid_x[i, ], grid_x[j, ])[[1]]
    }
}
kmat = kmat + t(kmat)  # symmetry
sample = rmvnorm(1, rep(0, n_tuples), kmat)
surface_mat <- matrix(sample, nrow = n_pts, byrow = FALSE)
p = plot_ly(x = x1, y = x2, z = surface_mat) %>%
    add_surface()
