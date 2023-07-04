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
# data setting-------------------------------------
h <- 30
etf_split <-
  divide_ts(
    etf_vix[1:300,] %>%
      rename_with(~str_remove_all(., pattern = "CLS$")),
    h
  )
etf_train <- etf_split$train
etf_test <- etf_split$test
# step_roll <- 1
var_lag <- 3
bvar_lag <- 3
# cache in analysis.rmd----------------------------
# qwraps2::lazyload_cache_dir("R/analysis_cache/gfm")
# fit_bvar <- bvar_optim$fit
# fit_bvhar <- bvhar_var_optim$fit
# fit_bvhar_vhar <- bvhar_vhar_optim$fit
# New fit for BVHAR--------------------------------
# cl <- parallel::makeCluster(8, type = "FORK")
# n_asset <- ncol(etf_train)
# bvhar_init <- set_bvhar(
#   sigma = apply(etf_train, 2, sd),
#   lambda = .2,
#   delta = rep(.1, n_asset)
# )
# (bvhar_var_optim <- choose_bvhar(
#   bvhar_init,
#   lower = c(
#     rep(1, n_asset), # sigma
#     1e-4, # lambda
#     rep(1e-2, n_asset) # delta
#   ),
#   upper = c(
#     rep(15, n_asset), # sigma
#     Inf, # lambda
#     rep(1, n_asset) # delta
#   ),
#   y = etf_train,
#   har = c(5, 22),
#   include_mean = TRUE,
#   parallel = list(cl = cl, forward = FALSE, loginfo = FALSE)
# ))
# fit_bvhar <- bvhar_var_optim$fit
# bvhar_vhar_init <- set_weight_bvhar(
#   sigma = apply(etf_train, 2, sd),
#   lambda = .2,
#   daily = rep(.1, n_asset),
#   weekly = rep(.1, n_asset),
#   monthly = rep(.1, n_asset)
# )
# (bvhar_vhar_optim <- choose_bvhar(
#   bvhar_vhar_init,
#   lower = c(
#     rep(1, n_asset), # sigma
#     1e-4, # lambda
#     rep(1e-2, n_asset), # daily
#     rep(1e-2, n_asset), # weekly
#     rep(1e-2, n_asset) # monthly
#   ),
#   upper = c(
#     rep(15, n_asset), # sigma
#     Inf, # lambda
#     rep(1, n_asset), # daily
#     rep(1, n_asset), # weekly
#     rep(1, n_asset) # monthly
#   ),
#   y = etf_train,
#   har = c(5, 22),
#   include_mean = TRUE,
#   parallel = list(cl = cl, forward = FALSE, loginfo = FALSE)
# ))
# fit_bvhar_vhar <- bvhar_vhar_optim$fit
# parallel::stopCluster(cl)
# VHAR-SV------------------------------------------
fit_s_sv <- bvhar_sv(
  y = etf_train,
  num_iter = 100,
  # bayes_spec = bvhar_var_optim$spec,
  include_mean = TRUE,
  verbose = TRUE
)
fit_l_sv <- bvhar_sv(
  y = etf_train,
  num_iter = 100,
  # bayes_spec = bvhar_vhar_optim$spec,
  include_mean = TRUE,
  verbose = TRUE
)
# fit_var <- var_lm(etf_train, p = var_lag)
fit_vhar <- vhar_lm(etf_train)
mod_list <- list(
  # fit_var,
  fit_vhar,
  # fit_bvar,
  # fit_bvhar,
  # fit_bvhar_vhar
  fit_s_sv,
  fit_l_sv
)
# Out-of-sample forecasting--------------------------
roll_list <- 
  parallel::mclapply(
    c(1, 5, 20),
    function(h) {
      mod_list %>% 
        lapply(
          function(mod) {
            forecast_roll(mod, h, etf_test)
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
  label = "svloss"
)
