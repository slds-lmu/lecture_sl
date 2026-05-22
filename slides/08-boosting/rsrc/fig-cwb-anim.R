library(dplyr)
library(ggplot2)
library(ggsci)
library(patchwork)

devtools::load_all("~/repos/compboost")

REBUILD = TRUE
FIGURES = TRUE
ANIMATE = FALSE
ITER_MAX = 150L
RM_OLD   = TRUE
FIG_DIR  = "figure"

## Data: https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who
dat_raw = read.csv(here::here("data/who-life-expectancy-data.csv"))

countries = c("Germany", "United States of America", "Sweden", "South Africa", "Ethiopia")
country_codes = c("GER", "USA", "SWE", "ZAF", "ETH")
names(country_codes) = countries

dat = dat_raw %>%
  filter(Country %in% countries) %>%
  select("Country", "Year", "Life.expectancy", "BMI", "Adult.Mortality") %>%
  mutate(Country = as.factor(country_codes[Country])) %>%
  na.omit()

#dat %>% group_by(Country) %>% summarize(mean(Life.expectancy))
#tst = data.frame(Country = dat$Country, abb = country_codes[dat$Country])

target = "Life.expectancy"

if (ncol(dat) != 5) {
  stop("The code was intended to be used with EXACTLY 4 features!")
}

#cboost = boostSplines(dat, target, learning_rate = 0.05, df = 4, df_cat = 3, iterations = ITER_MAX)#, loss = LossBinomial$new())
cboost = boostLinear(dat, target, learning_rate = 0.05, iterations = ITER_MAX)#, loss = LossBinomial$new())

#plotBaselearnerTraces(cboost)
#plotRisk(cboost)
#getPlot(21)

getPlot = function(iter, add_points = FALSE, add_risk = FALSE, add_blpred = FALSE, add_risk_trace = TRUE) {
  #pr_range = quantile(cboost$response$getResponse() - cboost$getCoef()$offset, c(0.1, 0.9))
  #pr_range = range(cboost$response$getResponse() - cboost$getCoef()$offset)
  cboost$train(1)
  pr1 = cboost$loss$calculatePseudoResiduals(cboost$response$getResponse(), cbind(rep(cboost$getCoef()$offset, nrow(cboost$data))))
  pr_range = range(pr1)
  cboost$train(ITER_MAX)

  bltrace = cboost$getSelectedBaselearner()
  blnames = sort(unique(bltrace))
  risktrace = cboost$getInbagRisk()
  bltint  = vapply(bltrace, function(bn) which(bn == blnames), integer(1), USE.NAMES = FALSE)
  df_bl = data.frame(iter = seq_along(bltint), blsel = bltint, bln = bltrace)
  df_bl = df_bl[seq_len(iter), ]


  cols = pal_aaas()(length(blnames))
  df_cols = data.frame(bln = blnames, color = cols[c(1,3,2,4)])
  cols = df_cols$color
  names(cols) = df_cols$bln

  ilab = iter
  ggt = ggplot(df_bl, aes(xmin = iter - 1, xmax = iter, ymin = blsel - 0.4, ymax = blsel + 0.4))
  if (iter == 1) {
    ilab = 0
  } else {
    ggt = ggt + geom_rect(color = "white", show.legend = FALSE, linewidth = 0.05, fill = cols[bltrace[seq_len(iter)]])
  }
    #scale_color_aaas() +
    #scale_fill_aaas() +
  ggt = ggt + scale_fill_manual(values = cols) +
    scale_y_continuous(breaks = seq_along(blnames), labels = blnames, limits = c(0.5, length(blnames) + 0.5)) +
    xlim(c(0, ITER_MAX + 0.5)) +
    xlab("Iteration") +
    ylab("") +
    ggtitle("", sprintf("Model after %s iterations", ilab)) +
    theme_minimal()

  df_risk = data.frame(risk = risktrace[-1], bl = bltrace, color = cols[bltrace], iter = seq_along(bltrace))
  #ylabel = sprintf("expression(paste(R[emp], '(D | ', f^'[%s]', ')'))", iter)
  ggrt = ggplot(df_risk[seq_len(iter), ], aes(x = iter, y = risk)) +
    geom_point(color = df_risk$color[seq_len(iter)], shape = 20) +
    ylim(min(risktrace), max(risktrace)) +
    xlim(0, ITER_MAX) +
    xlab("Iteration") +
    ylab("") +
    ggtitle("", expression(paste(R[emp]))) +
    theme_minimal()
    #ggtitle("", subtitle = eval(parse(text = ylabel)))

  #ylabel = sprintf("expression(paste(R[emp], '(D | ', f^'[%s]', ')'))", iter)
  #ggplot() + geom_point(aes(x = 1:10, y = 1:10)) + ylab(eval(parse(text = ylabel)))

  cboost$train(iter)

  pbl = function(bln, cboost, npoints = 100L, add_points = FALSE, add_risk = FALSE, add_blpred = FALSE) {
    gg = try(plotBaselearner(cboost, bln), silent = TRUE)
    if (inherits(gg, "try-error")) {
      itmp = cboost$getCurrentIteration()
      cboost$train(ITER_MAX)
      dat = plotBaselearner(cboost, bln)$data
      dat$y = 0
      cboost$train(itmp)
      alpha = 0.3
    } else {
      dat = gg$data
      alpha = 1
      if (iter == 1) alpha = 0.3
    }
    if (iter == 1) dat$y = 0

    cl = df_cols$color[df_cols$bln == bln]
    f = cboost$baselearner_list[[bln]]$factory

    dato = data.frame(response = cboost$response$getResponse(), pred = cboost$response$getPrediction())
    pr = cboost$loss$calculatePseudoResiduals(cbind(dato$response), cbind(dato$pred))
    dato$pr = pr
    dato$x = cboost$data[[f$getFeatureName()]]

    if (iter == 1) {
      pold = cbind(rep(cboost$getCoef()$offset, nrow(dato)))
    } else {
      cboost$train(iter - 1)
      pold = cboost$response$getPrediction()
    }
    prold = cboost$loss$calculatePseudoResiduals(cbind(dato$response), pold)
    cboost$train(iter)
    X = f$getData()
    P = as.numeric(f$getMeta()$penalty) * f$getMeta()$penalty_mat
    if (nrow(X) > ncol(X)) X = t(X)
    blpred = t(X) %*% solve(X %*% t(X) + P) %*% X %*% prold
    dato$prold = prold
    dato$blpred = blpred

    ym = mean(dat$y)
    if (is.numeric(dat$x)) {
      xm = mean(dat$x)
      gg = ggplot() +
        geom_line(data = dat, mapping = aes(x = x, y = y, color = bln),
          show.legend = FALSE, alpha = alpha, linewidth = 1.2) +
        scale_color_manual(values = cl)

      if (add_points) {
        gg = gg + geom_point(data = dato, mapping = aes(x = x, y = pr, color = bln),
          alpha = 0.2, show.legend = FALSE)
      }
      if (add_blpred) {
        gg = gg + geom_line(data = dato, mapping = aes(x = x, y = blpred, color = bln),
          show.legend = FALSE, linetype = "dashed", alpha = 0.8)
      }
    } else {
      dat$i = seq_len(nrow(dat))
      dato = merge(dato, dat, by = "x")
      xm = mean(dat$i)

      gg = ggplot() +
        geom_segment(data = dat, mapping = aes(x = i - 0.4, xend = i + 0.4, y = y, yend = y, color = bln),
          show.legend = FALSE, alpha = alpha, linewidth = 1.2) +
        scale_color_manual(values = cl) +
        scale_x_continuous(breaks = dat$i, labels = dat$x)

      if (add_points) {
        set.seed(31415)
        gg = gg + geom_point(data = dato, mapping = aes(x = i, y = pr, color  = bln, shape = as.factor(i)),
          alpha = 0.2, show.legend = FALSE, position = position_jitter())
      }
      if (add_blpred) {
        dat_blpred = do.call(rbind, lapply(dat$x, function(xn) dato[which(dato$x == xn)[1],]))
        gg = gg + geom_segment(data = dat_blpred, mapping = aes(x = i - 0.4, xend = i + 0.4, y = blpred, yend = blpred, color = bln),
          show.legend = FALSE, linetype = "dashed", alpha = 0.8)
      }
    }
    llabs = factor(c("Partial feature effect", "Base learner fit to pseudo residuals"))
    levels(llabs) = as.character(llabs)
    gg = gg + #ggtitle(bln) +
      xlab(f$getFeatureName()) +
      ylab("Pseudo residuals") +
      theme_minimal() +
      geom_line(aes(x = rep(mean(xm), 2), y = rep(mean(ym), 2), linetype = llabs)) +
      labs(linetype = "") +
      theme(legend.position = "bottom", legend.justification = "left")

    if (add_risk) {
      sse = sum((dato$prold - dato$blpred)^2)
      blt = strsplit(bln, "_")[[1]][1]
      gg = gg + ggtitle(blt, sprintf("SSE=%s", round(sse, 4)))#geom_text(aes(x = -Inf, y = Inf, label = sprintf("SSE=%s", round(sse, 4))), hjust = -0.2, vjust = 1.2)
    }
    if (bln == bltrace[iter]) {
      gg = gg +
        theme(plot.subtitle = element_text(face = "bold", color = "red"))
        #theme(plot.subtitle = element_text(face = "bold", color = df_cols$color[df_cols$bln == bln]))
    }

    return(gg)
  }

  ggs = lapply(blnames, pbl, cboost = cboost, add_points = TRUE, add_risk = TRUE, add_blpred = TRUE)

  ggs = lapply(ggs, function(gg) gg + ylim(pr_range[1], pr_range[2]))
  ylaboffset = -150
  ggs[[1]] = ggs[[1]] + theme(axis.title.y = element_text(margin = margin(r = ylaboffset, unit = "pt")))
  ggs[[3]] = ggs[[3]] + theme(axis.title.y = element_text(margin = margin(r = ylaboffset, unit = "pt")))

  ggbls = Reduce("+", ggs) + plot_layout(nrow = 2)
  #ggbls = (ggs[[1]] | ggs[[2]]) / (ggs[[3]] | ggs[[4]])

  #ggbls = ( (ggs[[1]] + ylim(-1, 2)) | (ggs[[2]] + ylim(-0.125, 0.12))) /
    #((ggs[[3]] + ylim(-0.4, 0.75)) | (ggs[[4]] + ylim(-0.2, 0.2)))

  gg = (((ggt | ggrt) + plot_layout(widths = c(6, 1)))) /
    ((ggbls | plot_spacer()) + plot_layout(widths = c(6, 1))) +
    plot_layout(heights = c(1, 5), guides = "collect") & theme(legend.position = "bottom", legend.justification = "left")

  if (FALSE) {
    ggright = plot_spacer() / ggrt / plot_spacer() + plot_layout(heights = c(1,2,1))
    gg = (gg | ggright) + plot_layout(widths = c(5, 1))
  }

  cboost$train(ITER_MAX)

  return(gg)
}

if (! dir.exists(FIG_DIR)) dir.create(FIG_DIR)

if (RM_OLD) {
  fn = list.files(paste0(FIG_DIR, "/cwb-anim"), full.names = TRUE)
  fn = fn[grep("fig-iter", fn)]
  invisible(file.remove(fn))
}

if (FIGURES) {
  #iters = c(1, 2, 5, 10, 15, 20, 21, 30, 50, 70, 85, 87, 90, 110, 130, 140, 145, 150)
  iters = c(1, 2, 5, 10, 15, 16, 20, 30, 37, 38, 50, 70, 90, 110, 116, 117, 140, 150)
  slt = character()
  for (i in iters) {
    gg = getPlot(i) & plot_annotation(theme = theme(plot.background = element_rect(fill = "white")))
    fn = sprintf(paste0(FIG_DIR, "/cwb-anim/fig-iter-%s.png"), stringr::str_pad(i, 4, pad = 0))
    suppressMessages(suppressWarnings(ggsave(plot = gg, filename = fn, width = 8, height = 6)))

    if (i == iters[1]) {
      pc = "\\addtocounter{framenumber}{0}"
    } else {
      pc = "\\addtocounter{framenumber}{-1}"
    }
    message(sprintf("Save figure %s", fn))
    slt = c(slt, sprintf("\n\\begin{frame}{Example: Life expectancy}\n\t\\begin{figure}\n\t\t\\centering\n\t\t\\includegraphics[width=\\textwidth]{%s}\n\t\\end{figure}\n\t%s\n\\end{frame}\n", fn, pc))
    writeLines(slt, con = "tex/fig-cwb-anim.tex")
  }
}

