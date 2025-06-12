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
n_pts = 25
x1 = seq(0, 5, length.out = n_pts)
x2 = seq(0, 10, length.out = n_pts)
grid_x = expand.grid(x1 = x1, x2 = x2)
n_tuples = nrow(grid_x)
# idx matrix for upper triangle (use fact that matrix is symmetric)
ij_pairs = which(
    upper.tri(matrix(NA, n_tuples, n_tuples), diag = TRUE), arr.ind = TRUE
)
cov_values = apply(
    ij_pairs, 
    1, 
    function(idx) cov_brownian(grid_x[idx[[1]], ], grid_x[idx[[2]], ])[[1]]
)
kmat =- matrix(0, n_tuples, n_tuples)
kmat[ij_pairs] = cov_values # upper triangle
kmat = kmat + t(kmat) - diag(diag(kmat)) # complete

sample = rmvnorm(1, rep(0, n_tuples), kmat)
surface_mat <- matrix(sample, nrow = n_pts, byrow = FALSE)


plot_ly(x = x1, y = x2, z = surface_mat) %>%
    add_surface() %>%
    layout(
        scene = list(
            xaxis = list(title = "x1"),
            yaxis = list(title = "x2"),
            zaxis = list(title = "f(x1, x2)"),
            camera = list(x = 2, y = 2, z = 2)
        )
    )
    
