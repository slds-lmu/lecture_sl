# ------------------------------------------------------------------------------
# intro

# FIG: how MSE for training and test data change with
#      different feature numbers, and with different data sizes.

# DATA: from data_ozone_example.RData
# ------------------------------------------------------------------------------

library(ggplot2)
library(data.table)

theme_set(theme_minimal())

# DATA -------------------------------------------------------------------------

load("data_ozone_example.RData")

dfp <- setDT(df_incdata)[, .(mean.mse = median(value)), by = c("nobs", "variable")]

# PLOTS ------------------------------------------------------------------------

# data size
p1 <- ggplot(data = dfp, aes(x = nobs, y = mean.mse, colour = variable)) +
  geom_line(lwd = 1.2) + ylim(c(0, 100)) + labs(colour = " ") +
  scale_colour_discrete(labels = c("Train error", "Test error")) +
  xlab("Size of data set") + ylab("MSE") +
  scale_color_brewer(palette="Dark2") 

# feature number
p2 <- ggplot(data = df_incfeatures, aes(x = type, y = mean.mse, colour = variable)) +
  geom_line(lwd = 1.2) + labs(colour = " ") +
  scale_colour_discrete(labels = c("Train error", "Test error")) +
  xlab("Number of features") + ylab("MSE") +
  ylim(c(0, 150)) +
  scale_x_continuous(breaks = 0:12) +
  scale_color_brewer(palette="Dark2")

ggsave("../figure/avoid_overfitting_01.png", plot=p1, width=5, height=2.5)
ggsave("../figure/avoid_overfitting_02.png", plot=p2, width=5, height=2.5)
