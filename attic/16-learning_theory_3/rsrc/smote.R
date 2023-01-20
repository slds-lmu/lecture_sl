library(ggplot2)
library(gridExtra)

set.seed(123)
df = data.frame(x1 = runif(20, 0, 1), x2 = runif(20, 0, 1))

p1 = ggplot(df, aes(x = x1, y = x2)) + geom_point(size =3, shape = 21, color = "darkgrey") + xlim(c(0.2, 0.8)) + ylim(c(0.2, 1))

p1 = p1 + geom_point(aes(x = df[3, 1], y = df[3, 2]), color = "#F8766D", size = 2)  + ggtitle("Step 1")
p1

distance = as.matrix(dist(df[, 1:2]))
ndx = order(distance[, 3], decreasing = FALSE)[2:4]
neighbors = df[ndx, ]

p2 = p1 + geom_point(data = neighbors, aes(x = x1, y = x2), color = "#00C094", shape = 21, size = 2) + ggtitle("Step 2")
p2

p3 = p2 + geom_point(aes(x = neighbors[3, 1], y = neighbors[3, 2]), color = "#00C094", size = 2) + ggtitle("Step 3")

newpoint = df[3, ] + 0.6 * (neighbors[3, ] - df[3, ])

p4 = p3 + geom_segment(aes(x = df[3, 1], y = df[3, 2], xend = neighbors[3, 1], yend = neighbors[3, 2]), colour = "darkgrey") + geom_point(aes(x = newpoint[1, 1], y = newpoint[1, 2]), color = "#FF9900", size = 2) + ggtitle("Step 4")


p = grid.arrange(p1, p2, p3, p4, ncol = 2)
ggsave("figure_man/smote.pdf", p)
