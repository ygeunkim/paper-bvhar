if (requireNamespace("tidyverse", quietly = TRUE)) {
  library(tidyverse)
}
if (requireNamespace("bvhar", quietly = TRUE)) {
  library(bvhar)
}
# VAR and VHAR coefs---------------------------------
dgp <- readRDS("data/processed/sim_coef.rds")
# simulation setting---------------------------------
# Generate 1030 (train 1000 + test 30)
# burn-in 300
#----------------------------------------------------
num_train <- 900
num_test <- 30
num_burin <- 500
n_small <- ncol(dgp$var_small_covmat)
n_medium <- ncol(dgp$var_medium_covmat)
n_large <- ncol(dgp$var_large_covmat)
# VAR---------------------------------------------------------------------------
# p = 5
var_lag <- dgp$var_lag
# SMALL--------------------------------------------
set.seed(1)
small_var <- sim_var(
  num_sim = num_train + num_test,
  num_burn = num_burin,
  var_coef = dgp$var_small_coef,
  var_lag = var_lag,
  sig_error = dgp$var_small_covmat,
  init = matrix(0L, nrow = var_lag, ncol = n_small)
) %>% 
  as.data.frame()
colnames(small_var) <- paste("asset", sprintf(1:n_small, fmt = "%02d"), sep = "_")
small_var_split <- divide_ts(small_var, num_test)
# MEDIUM-------------------------------------------
set.seed(1)
med_var <- sim_var(
  num_sim = num_train + num_test,
  num_burn = num_burin,
  var_coef = dgp$var_medium_coef,
  var_lag = var_lag,
  sig_error = dgp$var_medium_covmat,
  init = matrix(0L, nrow = var_lag, ncol = n_medium)
) %>% 
  as.data.frame()
colnames(med_var) <- paste("asset", sprintf(1:n_medium, fmt = "%02d"), sep = "_")
med_var_split <- divide_ts(med_var, num_test)
# LARGE--------------------------------------------
set.seed(1)
large_var <- sim_var(
  num_sim = num_train + num_test,
  num_burn = num_burin,
  var_coef = dgp$var_large_coef,
  var_lag = var_lag,
  sig_error = dgp$var_large_covmat,
  init = matrix(0L, nrow = var_lag, ncol = n_large)
) %>% 
  as.data.frame()
colnames(large_var) <- paste("asset", sprintf(1:n_large, fmt = "%02d"), sep = "_")
large_var_split <- divide_ts(large_var, num_test)
# VHAR--------------------------------------------------------------------------
har_order <- c(5, 22)
# SMALL--------------------------------------------
set.seed(1)
small_vhar <- sim_vhar(
  num_sim = num_train + num_test,
  num_burn = num_burin,
  vhar_coef = dgp$vhar_small_coef,
  week = har_order[1],
  month = har_order[2],
  sig_error = dgp$vhar_small_covmat,
  init = matrix(0L, nrow = har_order[2], ncol = n_small)
) %>% 
  as.data.frame()
colnames(small_vhar) <- paste("asset", sprintf(1:n_small, fmt = "%02d"), sep = "_")
small_vhar_split <- divide_ts(small_vhar, num_test)
# MEDIUM-------------------------------------------
set.seed(1)
med_vhar <- sim_vhar(
  num_sim = num_train + num_test,
  num_burn = num_burin,
  vhar_coef = dgp$vhar_medium_coef,
  week = har_order[1],
  month = har_order[2],
  sig_error = dgp$vhar_medium_covmat,
  init = matrix(0L, nrow = har_order[2], ncol = n_medium)
) %>% 
  as.data.frame()
colnames(med_vhar) <- paste("asset", sprintf(1:n_medium, fmt = "%02d"), sep = "_")
med_vhar_split <- divide_ts(med_vhar, num_test)
# LARGE--------------------------------------------
set.seed(1)
large_vhar <- sim_vhar(
  num_sim = num_train + num_test,
  num_burn = num_burin,
  vhar_coef = dgp$vhar_large_coef,
  week = har_order[1],
  month = har_order[2],
  sig_error = dgp$vhar_large_covmat,
  init = matrix(0L, nrow = har_order[2], ncol = n_large)
) %>% 
  as.data.frame()
colnames(large_vhar) <- paste("asset", sprintf(1:n_large, fmt = "%02d"), sep = "_")
large_vhar_split <- divide_ts(large_vhar, num_test)
# Save--------------------------------------------------------------------------
# in data/processed/
# File name: mts_sim.rds
#--------------------------------------------------
dgp_save <- list(
  var_lag = var_lag,
  har = har_order,
  # VAR----------------------------------
  var_small_train = small_var_split$train,
  var_small_test = small_var_split$test,
  var_medium_train = med_var_split$train,
  var_medium_test = med_var_split$test,
  var_large_train = large_var_split$train,
  var_large_test = large_var_split$test,
  # VHAR---------------------------------
  vhar_small_train = small_vhar_split$train,
  vhar_small_test = small_vhar_split$test,
  vhar_medium_train = med_vhar_split$train,
  vhar_medium_test = med_vhar_split$test,
  vhar_large_train = large_vhar_split$train,
  vhar_large_test = large_vhar_split$test
)
saveRDS(dgp_save, "data/processed/mts_sim.rds")
rm(list = ls())
ls()
