if (requireNamespace("tidyverse", quietly = TRUE)) {
  library(tidyverse)
}
if (requireNamespace("bvhar", quietly = TRUE)) {
  library(bvhar)
}
set.seed(1)
# Minnesota prior-------------------------------------
# SMALL: 5
# MEDIUM: 10
# LARGE: 50
# delta: zero prior mean
# sigma
# lambda
#-----------------------------------------------------
num_gen <- 1000
num_burin <- 300
# SMALL-----------------------------------------------
n_small <- 5
bvhar_small_spec <- set_bvhar(
  sigma = rep(1, n_small),
  lambda = .1,
  delta = rep(0, n_small)
)
# generate SMALL coef--------------------
set.seed(1)
bvhar_small_coef <- sim_mnvhar_coef(bvhar_small_spec, full = FALSE)
# SMALL dataset--------------------------
set.seed(1)
bvhar_small <- sim_vhar(
  num_sim = num_gen,
  num_burn = num_burin,
  vhar_coef = bvhar_small_coef$coefficients,
  week = 5,
  month = 22,
  sig_error = bvhar_small_coef$covmat,
  init = matrix(0L, nrow = 22L, ncol = n_small)
) %>% 
  as.data.frame()
colnames(bvhar_small) <- paste("asset", sprintf(1:n_small, fmt = "%02d"), sep = "_")
# MEDIUM----------------------------------------------
n_medium <- 10
bvhar_medium_spec <- set_bvhar(
  sigma = rep(1, n_medium),
  lambda = 5e-2,
  delta = rep(0, n_medium)
)
# generate MEDIUM coef----------------
set.seed(1)
bvhar_medium_coef <- sim_mnvhar_coef(bvhar_medium_spec, full = FALSE)
# MEDIUM dataset----------------------
set.seed(1)
bvhar_medium <- sim_vhar(
  num_sim = num_gen,
  num_burn = num_burin,
  vhar_coef = bvhar_medium_coef$coefficients,
  week = 5,
  month = 22,
  sig_error = bvhar_medium_coef$covmat,
  init = matrix(0L, nrow = 22L, ncol = n_medium)
) %>% 
  as.data.frame()
colnames(bvhar_medium) <- paste("asset", sprintf(1:n_medium, fmt = "%02d"), sep = "_")
# LARGE----------------------------------------------
n_large <- 50
bvhar_large_spec <- set_bvhar(
  sigma = rep(1, n_large),
  lambda = 1e-2,
  delta = rep(0, n_large)
)
# generate LARGE coef-----------------
set.seed(1)
bvhar_large_coef <- sim_mnvhar_coef(bvhar_large_spec, full = FALSE)
# LARGE dataset------------------------
set.seed(1)
bvhar_large <- sim_vhar(
  num_sim = num_gen,
  num_burn = num_burin,
  vhar_coef = bvhar_large_coef$coefficients,
  week = 5,
  month = 22,
  sig_error = bvhar_large_coef$covmat,
  init = matrix(0L, nrow = 22L, ncol = n_large)
) %>% 
  as.data.frame()
colnames(bvhar_large) <- paste("asset", sprintf(1:n_large, fmt = "%02d"), sep = "_")
# Save----------------------------------------------
# in data/processed/
# File name: bvharsim_dgp_s.rds
# saved objects: bvharspec and data
#---------------------------------------------------
dgp3 <- list(
  small_spec = bvhar_small_spec,
  y_small = bvhar_small,
  medium_spec = bvhar_medium_spec,
  y_medium = bvhar_medium,
  large_spec = bvhar_large_spec,
  y_large = bvhar_large
)
saveRDS(dgp3, "data/processed/dgp_bvhar_consistency.rds")
rm(list = ls())
ls()
