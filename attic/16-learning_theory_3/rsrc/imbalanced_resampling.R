library(gridExtra)

data.imbal.train = rbind(
  data.frame(x = rnorm(100, mean = 1), class = "A"),
  data.frame(x = rnorm(5000, mean = 2), class = "B")
)
task = makeClassifTask(data = data.imbal.train, target = "class")
task.over = oversample(task, rate = 8)
task.under = undersample(task, rate = 1/8)

hist1 = ggplot() + stat_count(aes(x = getTaskTargets(task))) + ggtitle("Original") + xlab("") + ylim(c(0, 5000))
hist2 = ggplot() + stat_count(aes(x = getTaskTargets(task.over))) + ggtitle("Oversampling") + xlab("") + ylim(c(0, 5000))
hist3 = ggplot() + stat_count(aes(x = getTaskTargets(task.under))) + ggtitle("Undersampling") + xlab("") + ylim(c(0, 5000))

plot = grid.arrange(hist1, hist2, hist3, ncol=3)
ggsave("figure_man/imbalanced_resampling.pdf", plot)
