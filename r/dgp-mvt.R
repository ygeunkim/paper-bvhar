if (requireNamespace("tidyverse", quietly = TRUE)) {
  library(tidyverse)
}
if (requireNamespace("bvhar", quietly = TRUE)) {
  library(bvhar)
}
set.seed(1)
# Minnesota prior-------------------------------------
# SMALL: 10
# MEDIUM: 50
# LARGE: 100
# dimension vary with n (k = o(n))
# BUT same k / n
# small sample size: 40 (burn-in = 20), 80 (burn-in = 30), 120 (burn-in = 50)
# medium sample size: 200 (burn-in = 100), 400 (burn-in = 200), 600 (burn-in = 300)
# large sample size: 400 (burn-in = 200), 800 (burn-in = 300), 1200 (burn-in = 500)
# delta: zero prior mean => zero vector
# sigma
# lambda
mvt_df <- 3
generate_bvharlist <- function(num_dim, num, burn, coef_mat, sig, t_df) {
  lapply(
    1:3,
    function(id) {
      sim_vhar(
        num_sim = num[id],
        num_burn = burn[id],
        vhar_coef = coef_mat,
        week = 5,
        month = 22,
        sig_error = diag(1 / sqrt(diag(sig))) %*% sig %*% diag(1 / sqrt(diag(sig))), # correlation mat
        init = matrix(0L, nrow = 22L, ncol = num_dim),
        process = "student",
        t_param = t_df
      ) %>% 
        as.data.frame() %>% 
        setNames(paste0("asset", sprintf(1:num_dim, fmt = "%02d")))
    }
  )
}
# SMALL-----------------------------------------------
n_small <- 10
bvhar_small_spec <- set_bvhar(
  sigma = rep(1, n_small),
  lambda = .2,
  delta = rep(.1, n_small)
)
# generate SMALL coef--------------------
set.seed(1)
bvhar_small_coef <- sim_mnvhar_coef(bvhar_small_spec, full = TRUE)
# SMALL dataset--------------------------
num_small <- c(40, 80, 120)
num_small_burn <- c(20, 30, 50)
set.seed(1)
bvhar_small <- generate_bvharlist(n_small, num_small, num_small_burn, bvhar_small_coef$coefficients, bvhar_small_coef$covmat, mvt_df)
# MEDIUM----------------------------------------------
n_medium <- 50
bvhar_medium_spec <- set_bvhar(
  sigma = rep(1, n_medium),
  lambda = .1,
  delta = rep(.1, n_medium)
)
# generate MEDIUM coef----------------
set.seed(1)
bvhar_medium_coef <- sim_mnvhar_coef(bvhar_medium_spec, full = TRUE)
# MEDIUM dataset----------------------
num_medium <- c(200, 400, 600)
num_medium_burn <- c(100, 200, 300)
set.seed(1)
bvhar_medium <- generate_bvharlist(n_medium, num_medium, num_medium_burn, bvhar_medium_coef$coefficients, bvhar_medium_coef$covmat, mvt_df)
# LARGE----------------------------------------------
n_large <- 100
bvhar_large_spec <- set_bvhar(
  sigma = rep(1, n_large),
  lambda = 1e-2,
  delta = rep(.1, n_large)
)
# generate LARGE coef-----------------
set.seed(1)
bvhar_large_coef <- sim_mnvhar_coef(bvhar_large_spec, full = TRUE)
# LARGE dataset------------------------
num_large <- c(400, 800, 1200)
num_large_burn <- c(200, 300, 500)
set.seed(1)
bvhar_large <- generate_bvharlist(n_large, num_large, num_large_burn, bvhar_large_coef$coefficients, bvhar_large_coef$covmat, mvt_df)
# Save----------------------------------------------
# in data/processed/
# File name: bvharsim_dgp_s.rds
# saved objects: bvharspec and data
#---------------------------------------------------
dgp_consistency <- list(
  small_spec = bvhar_small_spec,
  small_coef = bvhar_small_coef,
  y_small_list = bvhar_small,
  medium_spec = bvhar_medium_spec,
  medium_coef = bvhar_medium_coef,
  y_medium_list = bvhar_medium,
  large_spec = bvhar_large_spec,
  large_coef = bvhar_large_coef,
  y_large_list = bvhar_large,
  t_param = mvt_df
)
saveRDS(dgp_consistency, "data/processed/dgp_bvhar_mvt.rds")
rm(list = ls())
ls()
