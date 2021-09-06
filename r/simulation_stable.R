library(tidyverse)
library(bvhar)
set.seed(1)
# VAR(5)--------------------------------------
# small: 3-dim
# medium: 5-dim
# large: 10-dim
#---------------------------------------------
mts_dim <- 10
small_dim <- 3
medium_dim <- 5
sig <- runif(mts_dim, .00001, .000045)
lam <- .1
delta <- rep(.1, mts_dim)
var_lag <- 22
num_train <- 3000
num_test <- 50
# Make as function----------------------------
sim_stable_process <- function(var_lag, mts_dim, num_train, num_test, sigma, lamda, delta) {
  if (mts_dim != length(sigma)) stop("Length of sigma should be the same as mts_dim")
  if (mts_dim != length(delta)) stop("Length of delta should be the same as mts_dim")
  # coefficients-----------------
  BVAR_SIM <- sim_mncoef(var_lag, sigma, lamda, delta)
  VAR_COEF <- BVAR_SIM$coefficients
  VAR_SIG <- BVAR_SIM$covmat
  # Generate VAR-----------------
  y_var <- sim_stable_var(
    num_train + num_test,
    VAR_COEF,
    var_lag,
    diag(mts_dim)
  )
  colnames(y_var) <- paste("asset", sprintf(1:mts_dim, fmt = "%02d"), sep = "_")
  y_var
}
# SMALL MEDIUM LARGE--------------------------
set.seed(1)
y_small <- sim_stable_process(
  var_lag, small_dim, num_train, num_test, sig[1:small_dim], lam, delta[1:small_dim]
)
set.seed(1)
y_medium <- sim_stable_process(
  var_lag, medium_dim, num_train, num_test, sig[1:medium_dim], lam, delta[1:medium_dim]
)
set.seed(1)
y_large <- sim_stable_process(
  var_lag, mts_dim, num_train, num_test, sig, lam, delta
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
#   write_csv(file = "data/processed/varsim_stable_small_train.csv")
# y_small_split$test %>%
#   as.data.frame() %>%
#   write_csv(file = "data/processed/varsim_stable_small_test.csv")
# # Save MEDIUM--------------------------------
# y_medium_split$train %>%
#   as.data.frame() %>%
#   write_csv(file = "data/processed/varsim_stable_medium_train.csv")
# y_medium_split$test %>%
#   as.data.frame() %>%
#   write_csv(file = "data/processed/varsim_stable_medium_test.csv")
# # Save LARGE---------------------------------
# y_large_split$train %>%
#   as.data.frame() %>%
#   write_csv(file = "data/processed/varsim_stable_large_train.csv")
# y_large_split$test %>%
#   as.data.frame() %>%
#   write_csv(file = "data/processed/varsim_stable_large_test.csv")

