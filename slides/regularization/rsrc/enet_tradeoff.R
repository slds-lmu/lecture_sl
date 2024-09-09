# ------------------------------------------------------------------------------
# enetlogreg

# FIG: boxplot and violinplot of coefficients for elasticnet, lasso and ridge.
#      only show first ten coefficients.
# LEFT: linear model with 5 non-Zero coefficients (sparse)
# RIGHT: linear model with 500 non-Zero coefficients
# ------------------------------------------------------------------------------

library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)

theme_set(theme_minimal())

# DATA -------------------------------------------------------------------------

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
df_5 <- df_5 %>% select(learner_id, betas)

df_5 <- df_5 %>%
  unnest_longer(betas)

beta_sam <- performance_df[1,'betas'][[1]]
index_column <- rep(1:length(beta_sam), length.out = nrow(df_5))
df_5$index <- index_column

df_5 <- df_5 %>% 
  filter(index <= 10)

p1 <- ggplot(data = df_5, aes(x=as.numeric(betas), y = as.numeric(index), group=as.numeric(index)))+
  geom_violin(position = "dodge", trim = FALSE, scale = "width") +
  geom_boxplot(width = 0.4, color = "gray50", alpha = 0.5) +
  coord_flip()+
  labs(title="sparse") +
  facet_grid(learner_id~.)+
  xlab("value") +
  ylab(expression('index of'~theta)) +
  scale_y_continuous(breaks=1:10)

# linear model with dense features
df_500 <- performance_df[performance_df['q']==500,]
df_500 <- df_500 %>% select(learner_id, betas)

df_500 <- df_500 %>%
  unnest_longer(betas)

beta_sam <- performance_df[1,'betas'][[1]]
index_column <- rep(1:length(beta_sam), length.out = nrow(df_500))
df_500$index <- index_column

df_500 <- df_500 %>% 
  filter(index <= 10)

p2 <- ggplot(data = df_500, aes(x=as.numeric(betas), y = as.numeric(index), group=as.numeric(index)))+
  geom_violin(position = "dodge", trim = FALSE, scale = "width") +
  geom_boxplot(width = 0.4, color = "gray50", alpha = 0.5) +
  coord_flip()+
  facet_grid(learner_id~.) +
  labs(title="dense") +
  xlab("value") +
  ylab(expression('index of'~theta)) +
  scale_y_continuous(breaks=1:10)

p <- grid.arrange(p1, p2, nrow=1)

ggsave("../figure/enet_tradeoff.png", plot = p, width = 8, height = 4.5)
