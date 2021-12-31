if (requireNamespace("tidyverse", quietly = TRUE)) {
  library(tidyverse)
}
if (requireNamespace("bvhar", quietly = TRUE)) {
  library(bvhar)
}
set.seed(1)
# Minnesota prior-------------------------------------
# p = 5
# SMALL: 3
# MEDIUM: 9
# LARGE: 12
# Random walk: delta = 1
# sigma = 1
# lambda = 3
#-----------------------------------------------------
bvar_lag <- 5
num_train <- 1000
num_test <- 100
num_burin <- 300
# SMALL-----------------------------------------------
n_small <- 3
bvar_small_spec <- set_bvar(
  sigma = runif(n_small, min = .01, max = .3),
  lambda = 5e-3,
  delta = runif(n_small, min = .01, max = .6)
)
# generate SMALL coef--------------------
set.seed(1)
bvar_small_coef <- sim_mncoef(bvar_lag, bvar_small_spec)
# SMALL dataset--------------------------
set.seed(1)
y_small <- sim_var(
  num_train + num_test,
  num_burin,
  bvar_small_coef$coefficients,
  bvar_lag,
  bvar_small_coef$covmat,
  matrix(0L, nrow = bvar_lag, ncol = n_small)
) %>% 
  as.data.frame()
colnames(y_small) <- paste("asset", sprintf(1:n_small, fmt = "%02d"), sep = "_")
# Split---------------------------------
y_small_split <- divide_ts(y_small, num_test)
# MEDIUM----------------------------------------------
n_medium <- 9
bvar_medium_spec <- set_bvar(
  sigma = runif(n_medium, min = .01, max = .3),
  lambda = 1e-3,
  delta = runif(n_medium, min = .01, max = .6),
  eps = .1
)
# generate MEDIUM coef----------------
set.seed(1)
bvar_medium_coef <- sim_mncoef(bvar_lag, bvar_medium_spec)
# MEDIUM dataset----------------------
set.seed(1)
y_medium <- sim_var(
  num_train + num_test,
  num_burin,
  bvar_medium_coef$coefficients,
  bvar_lag,
  bvar_medium_coef$covmat,
  matrix(0L, nrow = bvar_lag, ncol = n_medium)
) %>% 
  as.data.frame()
colnames(y_medium) <- paste("asset", sprintf(1:n_medium, fmt = "%02d"), sep = "_")
# Split---------------------------------
y_medium_split <- divide_ts(y_medium, num_test)
# LARGE----------------------------------------------
n_large <- 12
bvar_large_spec <- set_bvar(
  sigma = runif(n_large, min = .01, max = .3),
  lambda = 5e-4,
  delta = runif(n_large, min = .01, max = .6),
  eps = .1
)
# generate LARGE coef-----------------
set.seed(1)
bvar_large_coef <- sim_mncoef(bvar_lag, bvar_large_spec)
# LARGE dataset------------------------
set.seed(1)
y_large <- sim_var(
  num_train + num_test,
  num_burin,
  bvar_large_coef$coefficients,
  bvar_lag,
  bvar_large_coef$covmat,
  matrix(0L, nrow = bvar_lag, ncol = n_large)
) %>% 
  as.data.frame()
colnames(y_large) <- paste("asset", sprintf(1:n_large, fmt = "%02d"), sep = "_")
# Split---------------------------------
y_large_split <- divide_ts(y_large, num_test)
# Save----------------------------------------------
# in data/processed/
# File name: bvarsim_a_b.csv
# a: small, medium, large
# b: train, test
#---------------------------------------------------
dgp2 <- list(
  small_spec = bvar_small_spec,
  y_small_train = y_small_split$train,
  y_small_test = y_small_split$test,
  medium_spec = bvar_medium_spec,
  y_medium_train = y_medium_split$train,
  y_medium_test = y_medium_split$test,
  large_spec = bvar_large_spec,
  y_large_train = y_large_split$train,
  y_large_test = y_large_split$test
)
saveRDS(dgp2, "data/processed/bvarsim_dgp_rw.rds")
rm(list = ls())
ls()
