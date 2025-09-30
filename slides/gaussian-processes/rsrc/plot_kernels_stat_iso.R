# ------------------------------------------------------------
# This script visualizes kernel properties with ggplot2.
# 
# Part 1: Stationary vs non-stationary kernels in 1D
#   - Isotropic Gaussian (stationary, depends only on x - x tilde)
#   - Polynomial degree 2 (non-stationary, depends on absolute x, x tilde)
#   - Plots k(x, x tilde) with x on the x-axis and x tilde on the y-axis
#
# Part 2: Isotropic vs anisotropic squared exponential kernels in 2D
#   - Isotropic SE: circular contours (stationary)
#   - Anisotropic SE: elliptical contours (still stationary)
#   - Plots k(d1, d2) with d1,d2 being differences
#
# Both parts save the resulting side-by-side figures as PDFs
# in the ../figure/ directory.
# ------------------------------------------------------------

library(ggplot2)
library(cowplot)

# ----------------------------
# Plot 1: Stationary vs Non-stationary (1D x)
# ----------------------------
n <- 200
x1 <- seq(-5, 5, length.out = n)
xtilde <- seq(-5, 5, length.out = n)
grid1 <- expand.grid(x1 = x1, xtilde = xtilde)

ell <- 1
grid1$k_se <- exp(-0.5 * ((grid1$x1 - grid1$xtilde)^2) / ell^2)

c <- 1
grid1$k_poly <- (grid1$x1 * grid1$xtilde + c)^2

p_se <- ggplot(grid1, aes(x1, xtilde, fill = k_se)) +
  geom_raster() +
  geom_contour(aes(z = k_se), color = "black", linewidth = 0.3) +
  scale_fill_viridis_c() +
  labs(title = "Stationary kernel (1D)",
       x = expression(x), y = expression(tilde(x))) +
  coord_equal() +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title  = element_text(size = 18),
    axis.text   = element_text(size = 16)
  )

p_poly <- ggplot(grid1, aes(x1, xtilde, fill = k_poly)) +
  geom_raster() +
  geom_contour(aes(z = k_poly), color = "black", linewidth = 0.3) +
  scale_fill_viridis_c() +
  labs(title = "Non-stationary kernel (1D)",
       x = expression(x), y = expression(tilde(x))) +
  coord_equal() +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title  = element_text(size = 18),
    axis.text   = element_text(size = 16)
  )

combined1 <- plot_grid(p_se, p_poly, ncol = 2)
print(combined1)
ggsave("../figure/stationary_vs_nonstationary.pdf", combined1, width = 14, height = 5)

# ----------------------------
# Plot 2: Isotropic vs Anisotropic SE (2D x in displacement coords)
# ----------------------------
n <- 200
d1 <- seq(-5, 5, length.out = n)
d2 <- seq(-5, 5, length.out = n)
grid2 <- expand.grid(d1 = d1, d2 = d2)

ell <- 1.5
grid2$k_iso <- exp(-0.5 * (grid2$d1^2 + grid2$d2^2) / ell^2)

ell1 <- 0.8
ell2 <- 2.5
grid2$k_aniso <- exp(-0.5 * (grid2$d1^2 / ell1^2 + grid2$d2^2 / ell2^2))

p_iso <- ggplot(grid2, aes(d1, d2, fill = k_iso)) +
  geom_raster() +
  geom_contour(aes(z = k_iso), color = "black", linewidth = 0.3) +
  scale_fill_viridis_c() +
  labs(title = "Isotropic+Stationary (2D)",
       x = expression(d[1] == x[1]-tilde(x)[1]),
       y = expression(d[2] == x[2]-tilde(x)[2])) +
  coord_equal() +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title  = element_text(size = 18),
    axis.text   = element_text(size = 16)
  )

p_aniso <- ggplot(grid2, aes(d1, d2, fill = k_aniso)) +
  geom_raster() +
  geom_contour(aes(z = k_aniso), color = "black", linewidth = 0.3) +
  scale_fill_viridis_c() +
  labs(title = "Anisotropic+Stationary (2D)",
       x = expression(d[1] == x[1]-tilde(x)[1]),
       y = expression(d[2] == x[2]-tilde(x)[2])) +
  coord_equal() +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.title  = element_text(size = 18),
    axis.text   = element_text(size = 16)
  )

combined2 <- plot_grid(p_iso, p_aniso, ncol = 2)
print(combined2)
ggsave("../figure/isotropic_vs_anisotropic.pdf", combined2, width = 14, height = 5)

