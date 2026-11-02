

library(mlr)
library(mlbench)
library(mvtnorm)
library(datasauRus)
library(energy)
library(dplyr)
library(tidyr)


cortable <- datasaurus_dozen %>% group_by(dataset) %>% 
  summarize(pearson = cor(x, y), spearman = cor(x,y, method = "spearman"), 
            distance = dcor(x, y), 
            minx = min(x), miny=median(y), maxy= max(y)) %>% 
  gather(key = "method", value = "value", -dataset, -minx, -miny, -maxy) %>% 
  mutate(label = recode(method, pearson = "r", spearman = "r[SP]", 
                        distance =  "r[d]"), 
         label = paste(label, round(value, 2), sep = "==")) %>% 
  group_by(dataset) %>% 
  summarize(label = paste("atop(", paste("atop(", label[1], ", ",label[2], ")"),", ", paste("atop(", label[3], ", ",label[4], ")"), ")"), minx=min(minx), maxy=min(maxy), 
            miny = min(miny))

ggplot(datasaurus_dozen, aes(x = x, y = y)) +
  geom_point(alpha = .3) +
  theme(legend.position = "none") +
  facet_wrap( ~ dataset, ncol = 3, labeller = function(...) "") + 
  geom_label(data = cortable, aes(x = 15, y = 50, 
                                  label = label), alpha = .7, col = "red", label.size = NA, nudge_x = 5, parse = TRUE, size = 5)

############################################################################