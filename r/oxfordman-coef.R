if (requireNamespace("tidyverse", quietly = TRUE)) {
  library(tidyverse)
}
if (requireNamespace("bvhar", quietly = TRUE)) {
  library(bvhar)
}
# SMALL--------------------------------------------
oxfordman_small <- 
  oxfordman_rk %>% 
  select(DJI, IXIC, SPX)
# MEDIUM-------------------------------------------
oxfordman_med <- 
  oxfordman_rk %>% 
  select(DJI, IXIC, SPX, FCHI, FTSE, GDAXI, GSPTSE, IBEX, OMXC20, OMXHPI, OMXSPI, OSEAX, RUT, SMSI, SSMI, STOXX50E)
# LARGE--------------------------------------------
oxfordman_large <- oxfordman_rk
# Fit models--------------------------------------------------------------------
# VAR(p)--------------------------------------------
var_lag <- 5
mean_include <- FALSE
var_small <- var_lm(oxfordman_small, p = var_lag, include_mean = mean_include)
var_med <- var_lm(oxfordman_med, p = var_lag, include_mean = mean_include)
var_large <- var_lm(oxfordman_large, p = var_lag, include_mean = mean_include)
# VHAR----------------------------------------------
har_order <- c(5, 22)
vhar_small <- vhar_lm(oxfordman_small, har = har_order, include_mean = mean_include)
vhar_med <- vhar_lm(oxfordman_med, har = har_order, include_mean = mean_include)
vhar_large <- vhar_lm(oxfordman_large, har = har_order, include_mean = mean_include)
# Save--------------------------------------------------------------------------
# From each VAR and VHAR
# Coefficient matrix and covariance matrix
#-------------------------------------------------------------------------------
dgp_coef <- list(
  var_lag = var_lag,
  var_small_coef = var_small$coefficients,
  var_small_covmat = var_small$covmat,
  var_medium_coef = var_med$coefficients,
  var_medium_covmat = var_med$covmat,
  var_large_coef = var_large$coefficients,
  var_large_covmat = var_large$covmat,
  vhar_small_coef = vhar_small$coefficients,
  vhar_small_covmat = vhar_small$covmat,
  vhar_medium_coef = vhar_med$coefficients,
  vhar_medium_covmat = vhar_med$covmat,
  vhar_large_coef = vhar_large$coefficients,
  vhar_large_covmat = vhar_large$covmat
)
saveRDS(dgp_coef, "data/processed/sim_coef.rds")
rm(list = ls())
ls()
