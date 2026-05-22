library(ggplot2)
library(patchwork)

x = seq(0, 10, len = 100)
yshift = seq(0, 0.4, len = 5)

fs = list(
  f1 = function(x) x + sin(x),
  f2 = function(x) 0.02 * x^2,
  f3 = function(x) 4 - 0.3 * x + 0.5 * sin(0.2 * x),
  f4 = function(x) 10 - x + sin(x))

invisible(lapply(seq_along(fs), function(i) {
  f = fs[[i]]
  y = f(x)
  dat = do.call(rbind, lapply(yshift, function(ys) {
    r = max(y) - min(y)
    data.frame(y = y + ys * r, x = x, yshift = ys)
  }))
  xl = sprintf("expression(x[%s])", i)
  yl = sprintf("expression(b[%s])", i)
  gg = ggplot(dat, aes(x = x, y = y, group = as.factor(yshift))) +
    geom_line() +
    theme_minimal() +
    xlab(eval(parse(text = xl))) +
    ylab(eval(parse(text = yl)))
  ggsave(sprintf("boosting-cwb-blpool%s.png", i), gg, width = 1.9, height = 1.3)
}))

set.seed(31415)
gg1 = ggplot() +
  geom_point(aes(x = x, y = fs[[1]](x) + rnorm(length(x), 0, 1)), alpha = 0.2) +
  geom_line(aes(x = x, y = fs[[1]](x)), linewidth = 1.1) +
  xlab(expression(x[1])) +
  ylab(expression(paste(b[1], "(", x[1], "|", theta["  1"]^"  [m]", ")"))) +
  theme_minimal()

ggsave("boosting-cwb-bl1-points.png", gg1, width = 1.9, height = 1.3)

gg1 = ggplot() +
  geom_point(aes(x = x, y = fs[[2]](x) + rnorm(length(x), 0, 1)), alpha = 0.2) +
  geom_line(aes(x = x, y = fs[[2]](x)), linewidth = 1.1) +
  xlab(expression(x[2])) +
  ylab(expression(paste(b[2], "(", x[2], "|", theta["  2"]^"  [m]", ")"))) +
  theme_minimal()

ggsave("boosting-cwb-bl2-points.png", gg1, width = 1.9, height = 1.3)

gg1 = ggplot() +
  geom_point(aes(x = x, y = fs[[3]](x) + rnorm(length(x), 0, 1)), alpha = 0.2) +
  geom_line(aes(x = x, y = fs[[3]](x)), linewidth = 1.1) +
  xlab(expression(x[3])) +
  ylab(expression(paste(b[3], "(", x[3], "|", theta["  3"]^"  [m]", ")"))) +
  theme_minimal()

ggsave("boosting-cwb-bl3-points.png", gg1, width = 1.9, height = 1.3)

