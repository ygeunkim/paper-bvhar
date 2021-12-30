if (requireNamespace("tidyverse", quietly = TRUE)) {
  library(tidyverse)
}
if (requireNamespace("bvhar", quietly = TRUE)) {
  library(bvhar)
}
# hyperparameter setting table------------------------
source("R/param-fns.R")
set.seed(1)
# Minnesota prior-------------------------------------
# SMALL: 3
# MEDIUM: 9
# LARGE: 12
# Random walk: delta = 1
# sigma = 1
# lambda = 3
#-----------------------------------------------------
num_train <- 1000
num_test <- 100
num_burin <- 300
# SMALL-----------------------------------------------
n_small <- 3
bvhar_small_spec <- set_bvhar(
  sigma = rep(.05, n_small),
  lambda = .2,
  delta = rep(0, n_small)
)
# generate SMALL coef--------------------
set.seed(1)
bvhar_small_coef <- sim_mnvhar_coef(bvhar_small_spec)
# SMALL dataset--------------------------
set.seed(1)
bvhar_small <- sim_vhar(
  num_train + num_test,
  num_burin,
  bvhar_small_coef$coefficients,
  bvhar_small_coef$covmat,
  matrix(0L, nrow = 22L, ncol = n_small)
) %>% 
  as.data.frame()
colnames(bvhar_small) <- paste("asset", sprintf(1:n_small, fmt = "%02d"), sep = "_")
# Split---------------------------------
bvhar_small_split <- divide_ts(bvhar_small, num_test)
# MEDIUM----------------------------------------------
n_medium <- 9
bvhar_medium_spec <- set_bvhar(
  sigma = rep(.05, n_medium),
  lambda = .1,
  delta = rep(0, n_medium)
)
# generate MEDIUM coef----------------
set.seed(1)
bvhar_medium_coef <- sim_mnvhar_coef(bvhar_medium_spec)
# MEDIUM dataset----------------------
set.seed(1)
bvhar_medium <- sim_vhar(
  num_train + num_test,
  num_burin,
  bvhar_medium_coef$coefficients,
  bvhar_medium_coef$covmat,
  matrix(0L, nrow = 22L, ncol = n_medium)
) %>% 
  as.data.frame()
colnames(bvhar_medium) <- paste("asset", sprintf(1:n_medium, fmt = "%02d"), sep = "_")
# Split---------------------------------
bvhar_medium_split <- divide_ts(bvhar_medium, num_test)
# LARGE----------------------------------------------
n_large <- 12
bvhar_large_spec <- set_bvhar(
  sigma = rep(.05, n_large),
  lambda = .03,
  delta = rep(0, n_large)
)
# generate LARGE coef-----------------
set.seed(1)
bvhar_large_coef <- sim_mnvhar_coef(bvhar_large_spec)
# LARGE dataset------------------------
set.seed(1)
bvhar_large <- sim_vhar(
  num_train + num_test,
  num_burin,
  bvhar_large_coef$coefficients,
  bvhar_large_coef$covmat,
  matrix(0L, nrow = 22L, ncol = n_large)
) %>% 
  as.data.frame()
colnames(bvhar_large) <- paste("asset", sprintf(1:n_large, fmt = "%02d"), sep = "_")
# Split---------------------------------
bvhar_large_split <- divide_ts(bvhar_large, num_test)
# Save----------------------------------------------
# in data/processed/
# File name: bvharsim_a_b.csv
# a: small, medium, large
# b: train, test
#---------------------------------------------------
dgp3 <- list(
  y_small_train = bvhar_small_split$train,
  y_small_test = bvhar_small_split$test,
  y_medium_train = bvhar_medium_split$train,
  y_medium_test = bvhar_medium_split$test,
  y_large_train = bvhar_large_split$train,
  y_large_test = bvhar_large_split$test
)
saveRDS(dgp3, "data/processed/bvharsim_dgp_wn.rds")
rm(list = ls())
ls()
