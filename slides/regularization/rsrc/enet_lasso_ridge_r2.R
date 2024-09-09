# ------------------------------------------------------------------------------
# enetlogreg

# FIG: boxplot of R-squared for elasticnet, lasso and ridge
# LEFT: linear model with 5 non-Zero coefficients (sparse)
# RIGHT: linear model with 500 non-Zero coefficients
# ------------------------------------------------------------------------------

library(ggplot2)
library(gridExtra)
load("enet_exp.RData")

# PLOT -------------------------------------------------------------------------

q_values <- sapply(ba$task_id, function(task) {
  as.numeric(sub("q:(\\d+)", "\\1", task))
})

performance_df <- as.data.frame(ba)
performance_df$q <- q_values
performance_df$learner_id <- as.factor(gsub("\\.tuned", "", performance_df$learner_id))

# linear model with sparse features
df_5 <- performance_df[performance_df['q']==5,]

p1 <- ggplot(data = df_5, aes(x = regr.rsq, y = learner_id)) +
  geom_boxplot() +
  coord_flip() +
  ylab("") +
  labs(title="sparse") +
  xlab("R-squared")+
  xlim(0.95,1)+
  theme_minimal(base_size = 10) +
  theme(legend.position="none",
        axis.title.x=element_blank())

# linear model with dense features
df_500 <- performance_df[performance_df['q']==500,]

p2 <- ggplot(data = df_500, aes(x = regr.rsq, y = learner_id)) +
  geom_boxplot() +
  coord_flip() +
  ylab("") +
  xlab("R-squared")+
  labs(title="dense") +
  xlim(0.5,1)+
  theme_minimal(base_size = 10) +
  theme(legend.position="none",
        axis.title.x=element_blank())

p <- grid.arrange(p1, p2, nrow= 1)

ggsave("../figure/enet_lasso_ridge_r2.png", plot = p, width = 6, height = 2)
