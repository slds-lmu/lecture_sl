library(ggplot2)

x = seq(0, 10, len = 10)

f1 = function(x) 0.2 * x
f2 = function(x) -1 + 0.3 * x

d1 = data.frame(x = x, y = f1(x))
d2 = data.frame(x = x, y = f2(x))
d3 = data.frame(x = x, y = f1(x) + f2(x))

yl = c(-1, 4)

vcols = ggsci::pal_aaas()(3)

gg1 = ggplot() +
  geom_line(aes(x = x, y = f1(x)), color = vcols[1], linewidth = 1.2) +
  ylim(yl) +
  xlab("x") +
  ylab(expression(paste(b[j], "(", x, "|", theta^"[1]", ")"))) +
  theme_minimal()

ggsave("boosting-cwb-bl-add1.pdf", gg1, width = 1.8, height = 1.4)

gg2 = ggplot() +
  geom_line(aes(x = x, y = f2(x)), color = vcols[2], linewidth = 1.2) +
  ylim(yl) +
  xlab("x") +
  ylab(expression(paste(b[j], "(", x, "|", theta^"[2]", ")"))) +
  theme_minimal()

ggsave("boosting-cwb-bl-add2.pdf", gg2, width = 1.8, height = 1.4)

gg3 = ggplot() +
  geom_line(aes(x = x, y = f1(x)), color = vcols[1], alpha = 0.2, linewidth = 1.2) +
  geom_line(aes(x = x, y = f2(x)), color = vcols[2], alpha = 0.2, linewidth = 1.2) +
  geom_line(aes(x = x, y = f1(x) + f2(x)), color = vcols[3], linewidth = 1.2) +
  geom_line() +
  ylim(yl) +
  xlab("x") +
  ylab(expression(paste(b[j], "(", x, "|", theta^"[1]", "+",  theta^"[2]", ")"))) +
  theme_minimal()

ggsave("boosting-cwb-bl-add3.pdf", gg3, width = 1.8, height = 1.4)
