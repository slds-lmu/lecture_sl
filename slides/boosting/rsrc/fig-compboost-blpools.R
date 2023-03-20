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

