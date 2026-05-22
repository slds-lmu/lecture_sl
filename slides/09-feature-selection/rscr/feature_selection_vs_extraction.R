################################################################################
# VISUALIZATION: FEATURE SELECTION VS EXTRACTION
################################################################################

# Lisa Wimmer

# IDEA -------------------------------------------------------------------------

# Show how feature selection vs feature extraction works from a geometric
# intuition. Use mixture of two Gaussians, once projecting onto one feature,
# once onto first principal component.

# PREREQ -----------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(mvtnorm)
library(gridExtra)

# FICTIONAL DATA ---------------------------------------------------------------

set.seed(20200728)

mu_1 = c(8, 4)
mu_2 = c(4, 8)
Sigma = matrix(c(4, 3, 3, 4), ncol = 2)

cluster_1 = rmvnorm(10000, 
                    mean = mu_1, 
                    sigma = Sigma)

cluster_2 = rmvnorm(10000, 
                    mean = mu_2, 
                    sigma = Sigma)

data_bvnorm = data.frame(
  rbind(
    cbind(cluster_1, 1), 
    cbind(cluster_2, 2)
    )
  )

colnames(data_bvnorm) = c("x_1", "x_2", "cluster")

# PROJECTIONS ------------------------------------------------------------------

# Projection onto x_1

data_bvnorm$projection_x_1 = 
  apply(data_bvnorm[, c(1:2)], 1, function(i) {i %*% c(1, 0)})

# Projection onto hyperplane perpendicular to first principal component

data_bvnorm$projection_x_2 = -prcomp(data_bvnorm[, c(1:2)])$x[, 1]

# VISUALIZATION ----------------------------------------------------------------

colors = c("#99CC33", "#067B7F", "blue", "orange")

# Point cloud with projection hyperplanes

p_1 = ggplot(data_bvnorm, aes(x = x_1, y = x_2, col = as.factor(cluster)))
p_1 = p_1 + geom_point()
p_1 = p_1 + geom_hline(yintercept = min(data_bvnorm$x_2), 
                       size = 1.2,
                       col = colors[3])
p_1 = p_1 + geom_abline(intercept = 
                          median(data_bvnorm$x_1) + median(data_bvnorm$x_2), 
                        slope = -1, 
                        size = 1.2,
                        col = colors[4])
p_1 = p_1 + scale_color_manual(values = colors[1:2])
p_1 = p_1 + labs(title = "Original features", 
                 x = expression(x[1]),
                 y = expression(x[2]))
p_1 = p_1 + theme(legend.position = "none")

# Projection feature selection

p_2 = ggplot(data_bvnorm, aes(x = projection_x_1))
p_2 = p_2 + geom_histogram(data_bvnorm %>% filter(cluster == 1), 
                           mapping = aes(x = projection_x_1), 
                           bins = 100, 
                           fill = colors[1])
p_2 = p_2 + geom_histogram(data_bvnorm %>% filter(cluster == 2), 
                           mapping = aes(x = projection_x_1), 
                           bins = 100, 
                           fill = colors[2],
                           alpha = 0.8)
p_2 = p_2 + labs(title = expression("Feature selection: projection onto" ~
                                      x[1] ~ "axis"),
                 x = expression("Original feature " ~ x[1]),
                 y = "Frequency")
p_2 = p_2 + theme(plot.title = element_text(color = colors[3], hjust = 0),
                  axis.text = element_blank())

# Projection feature extraction

p_3 = ggplot(data_bvnorm, aes(x = projection_x_2))
p_3 = p_3 + geom_histogram(data_bvnorm %>% filter(cluster == 1), 
                           mapping = aes(x = projection_x_2), 
                           bins = 100, 
                           fill = colors[1])
p_3 = p_3 + geom_histogram(data_bvnorm %>% filter(cluster == 2), 
                           mapping = aes(x = projection_x_2), 
                           bins = 100, 
                           fill = colors[2],
                           alpha = 0.8)
p_3 = p_3 + labs(title = "Feature extraction: projection onto hyperplane 
                 perpendicular to direction of greatest variance",
                 x = expression("Newly created feature " ~ xi[1]),
                 y = "Frequency")
p_3 = p_3 + theme(plot.title = element_text(color = colors[4], hjust = 0),
                  axis.text = element_blank())

# Grid

p = grid.arrange(p_1, p_2, p_3, layout_matrix = rbind(c(1, 2), c(1, 3)))

ggsave("figure_man/feature_sel_vs_extr.png", p, width = 10, height = 6)



