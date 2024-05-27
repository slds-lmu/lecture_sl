# ------------------------------------------------------------------------------
# FIG: COMPBOOST INTERCEPT HANDLING
# ------------------------------------------------------------------------------

# Purpose: visualize base learners

if (FALSE) {
  devtools::install_github("schalkdaniel/compboost", ref = "dev")
  install.packages("AmesHousing")
}

# HELPER  -------------------------------------------------------------------------
aggregator = function(cf) {
  bnames = names(cf)
  bnamesl = bnames[grepl("linear", bnames)]
  iidx = which(grepl("intercept", bnames))
  intercept = cf[["offset"]]
  if (length(iidx) == 0) {
    intercept = intercept + sum(vapply(bnamesl, FUN.VALUE = numeric(1), FUN = function(bn) cf[[bn]][1]))
    cfo = vapply(bnamesl, function(bn) cf[[bn]][2], numeric(1))
  } else {
    cfo = vapply(bnamesl, function(bn) cf[[bn]][1], numeric(1))
    intercept = intercept + cf[[iidx]]
  }
  names(cfo) = bnamesl
  cfo["Intercept"] = intercept
  return(cfo)
}

intInDF = function(cf_int, cf_noint) {
  cf1 = aggregator(cf_int)
  cf2 = aggregator(cf_noint)

  bln = names(cf1)
  if (! all(names(cf1) %in% names(cf2))) stop("Names are not equal")
  dfout = data.frame(bl = bln, with_intercept = unlist(cf1)[bln], without_intercept = unlist(cf2)[bln])
  rownames(dfout) = NULL
  return(dfout)
}

# DATA -------------------------------------------------------------------------

# Get data

dat = as.data.frame(AmesHousing::make_ames())

# TRAINING ---------------------------------------------------------------------

library(compboost)

# Instantiate & train model:
# - one intercept base learner
# - All other base learners are linear without intercept
n_iters = 10000L

fnames = c("Fireplaces", "Lot_Frontage", "Lot_Area", "Wood_Deck_SF")

# MODEL WITH INTERCEPT BL:
cboost_int = Compboost$new(data = dat, target = "Sale_Price", learning_rate = 0.01,
  loss = LossQuadratic$new())

cboost_int$addIntercept()

invisible(lapply(fnames, function (fn) {
  cboost_int$addBaselearner(fn, "linear", BaselearnerPolynomial, intercept = FALSE)
}))

cboost_int$train(n_iters, trace = 0)

# MODEL WITHOUT INTERCEPT BL:
cboost_noint = Compboost$new(data = dat, target = "Sale_Price", learning_rate = 0.01,
  loss = LossQuadratic$new())

invisible(lapply(fnames, function (fn) {
  cboost_noint$addBaselearner(fn, "linear", BaselearnerPolynomial)
}))

cboost_noint$train(n_iters, trace = 0)

# EXTRACT COEFFICIENTS  -------------------------------------------------------------------------
iters = seq(1, n_iters, length.out = 100L)
ll = list()
for (m in iters) {
  cboost_int$train(m)
  cboost_noint$train(m)

  cf_int = cboost_int$getCoef()
  cf_noint = cboost_noint$getCoef()

  dfin = cbind(intInDF(cf_int, cf_noint), iteration = m)
  ll = c(ll, list(dfin))
}
df_cf = do.call(rbind, ll)

# VISUALIZE -------------------------------------------------------------------------------------
library(ggplot2)

gg = ggplot(df_cf, aes(x = iteration, color = bl)) +
  geom_line(aes(y = with_intercept, linetype = "With intercept base learner")) +
  geom_line(aes(y = without_intercept, linetype = "Without intercept base learner")) +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(trans = "log") +
  labs(linetype = "", color = "Base learner") +
  xlab("Iteration") +
  ylab("Parameter value (log scale)")

ggsave(plot = gg, filename = here::here("slides/boosting/figure/compboost-intercept-handling.png"), height = 2.2, width = 7L)

