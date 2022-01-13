if (requireNamespace("tidyverse", quietly = TRUE)) {
  library(tidyverse)
}
if (requireNamespace("bvhar", quietly = TRUE)) {
  library(bvhar)
}
set.seed(1)
# Minnesota prior-------------------------------------
# SMALL: 3
# MEDIUM: 9
# LARGE: 12
# daily, weekly, monthly
# sigma
# lambda
#-----------------------------------------------------
num_train <- 1000
num_test <- 100
num_burin <- 300
# SMALL-----------------------------------------------
n_small <- 3
bvhar_small_spec <- set_weight_bvhar(
  sigma = runif(n_small, min = .9, max = 1.2),
  lambda = .1,
  daily = runif(n_small, min = 0, max = .5),
  weekly = runif(n_small, min = .5 / 5, max = 1 / 5),
  monthly = runif(n_small, min = 1 / 22, max = 1.5 / 22)
)
# generate SMALL coef--------------------
set.seed(1)
bvhar_small_coef <- sim_mnvhar_coef(bvhar_small_spec, full = FALSE)
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
bvhar_medium_spec <- set_weight_bvhar(
  sigma = runif(n_medium, min = .9, max = 1.2),
  lambda = 5e-2,
  daily = runif(n_medium, min = 0, max = .5),
  weekly = runif(n_medium, min = .5 / 5, max = 1 / 5),
  monthly = runif(n_medium, min = 1 / 22, max = 1.5 / 22)
)
# generate MEDIUM coef----------------
set.seed(1)
bvhar_medium_coef <- sim_mnvhar_coef(bvhar_medium_spec, full = FALSE)
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
bvhar_large_spec <- set_weight_bvhar(
  sigma = runif(n_large, min = .9, max = 1.2),
  lambda = 1e-2,
  daily = runif(n_large, min = 0, max = .5),
  weekly = runif(n_large, min = .5 / 5, max = 1 / 5),
  monthly = runif(n_large, min = 1 / 22, max = 1.5 / 22)
)
# generate LARGE coef-----------------
set.seed(1)
bvhar_large_coef <- sim_mnvhar_coef(bvhar_large_spec, full = FALSE)
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
# File name: bvharsim_dgp_l.rds
# saved objects: bvharspec and data
#---------------------------------------------------
dgp4 <- list(
  small_spec = bvhar_small_spec,
  y_small_train = bvhar_small_split$train,
  y_small_test = bvhar_small_split$test,
  medium_spec = bvhar_medium_spec,
  y_medium_train = bvhar_medium_split$train,
  y_medium_test = bvhar_medium_split$test,
  large_spec = bvhar_large_spec,
  y_large_train = bvhar_large_split$train,
  y_large_test = bvhar_large_split$test
)
saveRDS(dgp4, "data/processed/bvharsim_dgp_l.rds")
rm(list = ls())
ls()
