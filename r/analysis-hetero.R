if (requireNamespace("tidyverse", quietly = TRUE)) {
  library(tidyverse)
}
if (requireNamespace("bvhar", quietly = TRUE)) {
  library(bvhar)
}
if (requireNamespace("foreach", quietly = TRUE)) {
  library(foreach)
}
if (requireNamespace("knitr", quietly = TRUE)) {
  library(knitr)
}
if (requireNamespace("kableExtra", quietly = TRUE)) {
  library(kableExtra)
}
options(digits = 3)
options(knitr.kable.NA = "")
options(kableExtra.latex.load_packages = FALSE)
source("R/report-fns.R")
set.seed(1)
# Scale--------------------------------------------
# fit_vhar <- vhar_lm(etf_vix)
# etf_scale <- fit_vhar$y %*% chol2inv(chol(fit_vhar$covmat))
# etf_scale <- fit_vhar$y %*% diag(sqrt(diag(fit_vhar$covmat)))
# colnames(etf_scale) <- colnames(etf_vix)
# etf_scale <- fit_vhar$residuals %*% chol2inv(chol(fit_vhar$covmat))
# fit_bvhar2 <- bvhar_minnesota(etf_scale)
# data setting-------------------------------------
h <- 30
etf_split <-
  divide_ts(
    etf_vix %>% rename_with(~str_remove_all(., pattern = "CLS$")),
    h
  )
# etf_split <-
#   divide_ts(
#     as_tibble(etf_scale) %>%
#       rename_with(~str_remove_all(., pattern = "CLS$")),
#     h
#   )
etf_train <- etf_split$train
etf_test <- etf_split$test
# step_roll <- 1
var_lag <- 3
bvar_lag <- var_lag
fit_var <- var_lm(etf_train, var_lag)
# cache in analysis.rmd----------------------------
qwraps2::lazyload_cache_dir("R/analysis_cache/gfm")
fit_bvar_orginal <- bvar_optim$fit
fit_bvhar_original <- bvhar_var_optim$fit
fit_bvhar_vhar_original <- bvhar_vhar_optim$fit
# 1. VHAR------------------------------------------
fit_vhar <- vhar_lm(etf_train)
# 2. Scale-----------------------------------------
etf_train_sc <- fit_vhar$y %*% solve(chol(fit_vhar$covmat))
colnames(etf_train_sc) <- colnames(etf_vix)
# BVHAR-S------------------------------------------
cl <- parallel::makeCluster(8, type = "FORK")
n_asset <- ncol(etf_train_sc)
(bvhars_adj <- choose_bvhar(
  bayes_spec = set_bvhar(
    sigma = apply(etf_train_sc, 2, sd),
    lambda = .2,
    delta = rep(.1, n_asset)
  ), 
  lower = c(
    rep(1, n_asset), # sigma
    1e-4, # lambda
    rep(1e-2, n_asset) # delta
  ), 
  upper = c(
    rep(15, n_asset), # sigma
    Inf, # lambda
    rep(1, n_asset) # delta
  ), 
  y = etf_train_sc, 
  har = c(5, 22),
  include_mean = TRUE,
  parallel = list(cl = cl, forward = FALSE, loginfo = FALSE)
))
fit_bvhar <- bvhars_adj$fit
# BVHAR-L-------------------------------------------
(bvharl_adj <- choose_bvhar(
  bayes_spec = set_weight_bvhar(
    sigma = apply(etf_train_sc, 2, sd),
    lambda = .2,
    daily = rep(.1, n_asset),
    weekly = rep(.1, n_asset),
    monthly = rep(.1, n_asset)
  ), 
  lower = c(
    rep(1, n_asset), # sigma
    1e-4, # lambda
    rep(1e-2, n_asset), # daily
    rep(1e-2, n_asset), # weekly
    rep(1e-2, n_asset) # monthly
  ), 
  upper = c(
    rep(15, n_asset), # sigma
    Inf, # lambda
    rep(1, n_asset), # daily
    rep(1, n_asset), # weekly
    rep(1, n_asset) # monthly
  ), 
  y = etf_train_sc, 
  har = c(5, 22),
  include_mean = TRUE,
  parallel = list(cl = cl, forward = FALSE, loginfo = FALSE)
))
fit_bvhar_vhar <- bvharl_adj$fit
parallel::stopCluster(cl)
# Out-of-forecasting-------------------------------
fit_bvhar$process <- "BVHAR-S-adjusted"
fit_bvhar_vhar$process <- "BVHAR-L-adjusted"
mod_list <- list(
  fit_var,
  fit_bvhar,
  fit_bvhar_vhar
)
roll_list <- 
  parallel::mclapply(
    c(1, 5, 20),
    function(h_step) {
      mod_list %>% 
        lapply(
          function(mod) {
            if (is.varlse(mod) || is.vharlse(mod) || is.bvarmn(mod) || mod$process == "BVHAR_MN_VAR" || mod$process == "BVHAR_MN_VHAR") {
              pred <- forecast_roll(mod, h_step, etf_test)
            } else {
              pred <- forecast_roll(mod, h_step, as.matrix(etf_test) %*% solve(chol(fit_vhar$covmat)))
              pred$forecast <- pred$forecast %*% chol(fit_vhar$covmat)
            }
            pred
          }
        )
    },
    mc.cores = 8
  )
# Relative error------------------------------------
get_rmafetex_tr_2(
  roll_list, 
  etf_test,
  ahead_list = c("$h = 1$", "$h = 5$", "$h = 20$"), 
  benchmark_id = 1,
  caption = "Out-of-sample forecasting performance measures with VAR(3) as benchmark",
  label = "heteroloss"
)
