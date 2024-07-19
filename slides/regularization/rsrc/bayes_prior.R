# Plot densities for zero-mean Gaussian and Laplacian priors
library(ggplot2)
library(gridExtra)
library(grid)
library(ExtDist)

# v = 2*b^2 
# b = sqrt(0.5 v)
xseq = seq(-3, 3, by = 0.1)
vars = c(2, 4, 8)
d = lapply(vars, function(v) data.frame(v = v, x = xseq, q = dnorm(x = xseq, sd = sqrt(v))))
d = do.call(rbind, d)
d$v = as.factor(d$v)

p1 = ggplot(data = d, aes(x = x, y = q, col = v)) +
  geom_line(linewidth=1.2) + 
  labs(col = expression(tau^2), x=expression(theta), y = "") + 
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    panel.border = element_blank(),
    axis.text = element_text(size = 14), 
    legend.text = element_text(size = 14), 
    legend.title = element_text(size = 16) 
  )

bs = sqrt(0.5*vars)
bs = round(bs, 2)
d = lapply(bs, function(b) data.frame(b = b, x = xseq, q = dLaplace(x = xseq, b = b)))
d = do.call(rbind, d)
d$b = as.factor(d$b)

p2 = ggplot(data = d, aes(x = x, y = q, col = b)) +
  geom_line(linewidth=1.2) +
  labs(x=expression(theta), y = "") + 
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    panel.border = element_blank(),
    axis.text = element_text(size = 14), 
    legend.text = element_text(size = 14), 
    legend.title = element_text(size = 16) 
  )

p = grid.arrange(p1, p2, nrow = 1)
# print(p)
ggsave(filename = paste0("../figure/bayes_prior.png"), plot = p, width = 12, height = 3)