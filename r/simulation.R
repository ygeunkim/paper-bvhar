library(tidyverse)
library(bvhar)
set.seed(1)
# VAR(5)--------------------------------------
# small: 3-dim
# medium: 7-dim
# large: 15-dim
#---------------------------------------------
mts_dim <- 15
# sig <- c(.00003, .00002, .00015, .00001, .00025, .00005, .00035, .00004, .00045, .00002)
sig <- runif(mts_dim, .00001, .000045)
lam <- .1
delta <- rep(.1, mts_dim)
var_lag <- 22
# Make as function----------------------------
sim_process <- function(var_lag, mts_dim, num_train, num_test, sigma, lamda, delta) {
  if (mts_dim != length(sigma)) stop("Length of sigma should be the same as mts_dim")
  if (mts_dim != length(delta)) stop("Length of delta should be the same as mts_dim")
  # coefficients------------------------------
  BVAR_SIM <- sim_mncoef(var_lag, sigma, lamda, delta)
  VAR_COEF <- BVAR_SIM$coefficients
  VAR_SIG <- BVAR_SIM$covmat
  # Generate VAR------------------------------
  y_var <- sim_var(
    num_train + num_test,
    VAR_COEF,
    var_lag,
    diag(mts_dim),
    matrix(0L, nrow = var_lag, ncol = mts_dim)
  )
  colnames(y_var) <- paste("asset", sprintf(1:mts_dim, fmt = "%02d"), sep = "_")
  y_var
}
# SMALL MEDIUM LARGE--------------------------
set.seed(1)
y_small <- sim_process(
  var_lag, 3, num_train, num_test, sig[1:3], lam, delta[1:3]
)
set.seed(1)
y_medium <- sim_process(
  var_lag, 7, num_train, num_test, sig[1:7], lam, delta[1:7]
)
set.seed(1)
y_large <- sim_process(
  var_lag, 15, num_train, num_test, sig, lam, delta
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
#   write_csv(file = "data/processed/varsim_small_train.csv")
# y_small_split$test %>%
#   as.data.frame() %>%
#   write_csv(file = "data/processed/varsim_small_test.csv")
# # Save MEDIUM--------------------------------
# y_medium_split$train %>%
#   as.data.frame() %>%
#   write_csv(file = "data/processed/varsim_medium_train.csv")
# y_medium_split$test %>%
#   as.data.frame() %>%
#   write_csv(file = "data/processed/varsim_medium_test.csv")
# # Save LARGE---------------------------------
# y_large_split$train %>%
#   as.data.frame() %>%
#   write_csv(file = "data/processed/varsim_large_train.csv")
# y_large_split$test %>%
#   as.data.frame() %>%
#   write_csv(file = "data/processed/varsim_large_test.csv")
