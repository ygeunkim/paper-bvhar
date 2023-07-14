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
    etf_vix[1:100,] %>%
    # etf_vix %>%
      rename_with(~str_remove_all(., pattern = "CLS$")),
    h
  )
etf_train <- etf_split$train
etf_test <- etf_split$test
var_lag <- 3
bvar_lag <- 3
# cache in analysis.rmd----------------------------
qwraps2::lazyload_cache_dir("R/analysis_cache/gfm")
fit_bvar <- bvar_optim$fit
fit_bvhar <- bvhar_var_optim$fit
fit_bvhar_vhar <- bvhar_vhar_optim$fit
# VHAR-SV------------------------------------------
fit_s_sv <- bvhar_sv(
  y = etf_train,
  num_iter = 3000,
  bayes_spec = set_bvhar(),
  include_mean = TRUE,
  verbose = TRUE,
  num_thread = 4
)
fit_l_sv <- bvhar_sv(
  y = etf_train,
  num_iter = 3000,
  bayes_spec = set_weight_bvhar(),
  include_mean = TRUE,
  verbose = TRUE,
  num_thread = 4
)
mod_list <- list(
  fit_bvhar,
  fit_bvhar_vhar,
  fit_s_sv,
  fit_l_sv
)
# Out-of-sample forecasting--------------------------
roll_list <-
  parallel::mclapply(
    c(1, 5, 10),
    function(h) {
      mod_list %>%
        lapply(
          function(mod) {
            forecast_roll(mod, h, etf_test, roll_thread = 2, mod_thread = 2)
          }
        )
    },
    mc.cores = 3
  )
# Relative error------------------------------------
get_rmafetex_tr_2(
  roll_list, 
  etf_test, 
  ahead_list = c("$h = 1$", "$h = 5$", "$h = 10$"), 
  benchmark_id = 1,
  caption = "Out-of-sample forecasting performance of BVHAR-SV models with BVHAR-S as benchmark.",
  label = "svloss"
)
