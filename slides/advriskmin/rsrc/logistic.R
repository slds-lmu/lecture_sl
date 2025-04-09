# ------------------------------------------------------------------------------
# classification bernoulli

# FIG: logistic function and its inverse logit function
# ------------------------------------------------------------------------------

library(ggplot2)
theme_set(theme_minimal())

# DATA -------------------------------------------------------------------------

logistic = function(f){
  1 / (1 + exp(-f))
}

logit = function(p){
  log(p / (1 - p))
}

f <- seq(-4, 4, by = 0.01)
y_log <- logistic(f)

pix <- seq(0, 1, by = 0.001)
y_logit <- logit(pix)

df_1 <- data.frame(x = f, y = y_log)
df_2 <- data.frame(x = pix, y = y_logit)

# PLOT -------------------------------------------------------------------------

p_1 <- ggplot(df_1, aes(x = x, y = y)) + 
  geom_line(size = 1.2) +
  xlab(expression(f(x))) +
  ylab(expression(pi(x)))

p_1 <- p_1 + 
  annotate(
    "text",
    x = 2,
    y = 0.3,
    label = bquote(pi ~ "=" ~ (1 + exp(-f(x)))^-1),
    size = 7)

p_1 <- p_1 + theme(text = element_text(size = 20))
p_1
ggsave("../figure/logistic.png", p_1, height = 4, width = 6)
       
p_2 <- ggplot(df_2, aes(x = x, y = y)) + 
  geom_line(size = 1.2) +
  xlab(expression(eta(x))) +
  ylab(expression(f^"*"*(x))) +
  theme(text = element_text(size = 20))
p_2
ggsave("../figure/logistic_inverse.png", p_2, height = 6, width = 8)
