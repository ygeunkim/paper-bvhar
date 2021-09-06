library(tidyverse)
library(bvhar)
set.seed(1)
# VHAR----------------------------------------
# small: 3-dim
# medium: 7-dim
# large: 15-dim
#---------------------------------------------
mts_dim <- 15
small_dim <- 3
medium_dim <- 7
# sigma = diag(sig)
sig <- runif(mts_dim, .00001, .000045)
# lambda: shrinkage hyperparameter
lam_small <- .05
lam_medium <- .1
lam_large <- .1
# delta: white noise (0) vs random walk (1: litterman)
delta <- rep(.1, mts_dim)
# if HAR type
daily <- rep(.1, mts_dim)
weekly <- rep(.05, mts_dim)
monthly <- rep(.03, mts_dim)
# numbers: train + test
num_train <- 110 * 30
num_test <- 110
# Make as function----------------------------
sim_vhar_process <- function(N,
                             burnin, 
                             type = c("VAR", "VHAR"), 
                             mts_dim, 
                             sigma, 
                             lambda, 
                             delta, 
                             daily = NULL, 
                             weekly = NULL, 
                             monthly = NULL) {
  if (mts_dim != length(sigma)) stop("Length of sigma should be the same as mts_dim")
  if (mts_dim != length(delta)) stop("Length of delta should be the same as mts_dim")
  # coefficients-----------------
  BVHAR_SIM <- sim_mnvhar_coef(type, sigma, lambda, delta, daily, weekly, monthly)
  VHAR_COEF <- BVHAR_SIM$coefficients
  VHAR_SIG <- BVHAR_SIM$covmat
  # Generate VAR-----------------
  y_var <- sim_vhar(
    N,
    VHAR_COEF,
    diag(sigma),
    matrix(0L, nrow = var_lag, ncol = mts_dim)
  )
  colnames(y_var) <- paste("asset", sprintf(1:mts_dim, fmt = "%02d"), sep = "_")
  if (burnin > 0) return(y_var[-seq_len(burnin),])
  y_var
}
# SMALL MEDIUM LARGE--------------------------
set.seed(1)
y_small <- sim_vhar_process(
  N = num_train + num_test + 1000,
  burnin = 1000,
  mts_dim = small_dim, 
  sigma = sig[1:small_dim], 
  lambda = lam_small, 
  delta = delta[1:small_dim]
)
set.seed(1)
y_medium <- sim_vhar_process(
  N = num_train + num_test + 1000,
  burnin = 1000,
  mts_dim = medium_dim, 
  sigma = sig[1:medium_dim], 
  lambda = lam_medium, 
  delta = delta[1:medium_dim]
)
set.seed(1)
y_large <- sim_vhar_process(
  N = num_train + num_test + 1000,
  burnin = 1000,
  mts_dim = mts_dim, 
  sigma = sig, 
  lambda = lam_large, 
  delta = delta
)
# Plot----------------------------------------
y_medium %>% 
  as.data.frame() %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(-id, names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = id, y = value)) +
  geom_path() +
  facet_grid(variable ~ ., scales = "free_y") +
  labs(
    x = element_blank(),
    y = element_blank()
  )
# Split---------------------------------------
y_small_split <- divide_ts(y_small, num_test)
y_medium_split <- divide_ts(y_medium, num_test)
y_large_split <- divide_ts(y_large, num_test)
# # Save SMALL----------------------------------
# y_small_split$train %>%
#   as.data.frame() %>%
#   write_csv(file = "data/processed/vharsim_small_train.csv")
# y_small_split$test %>%
#   as.data.frame() %>%
#   write_csv(file = "data/processed/vharsim_small_test.csv")
# # Save MEDIUM--------------------------------
# y_medium_split$train %>%
#   as.data.frame() %>%
#   write_csv(file = "data/processed/vharsim_medium_train.csv")
# y_medium_split$test %>%
#   as.data.frame() %>%
#   write_csv(file = "data/processed/vharsim_medium_test.csv")
# # Save LARGE---------------------------------
# y_large_split$train %>%
#   as.data.frame() %>%
#   write_csv(file = "data/processed/vharsim_large_train.csv")
# y_large_split$test %>%
#   as.data.frame() %>%
#   write_csv(file = "data/processed/vharsim_large_test.csv")
