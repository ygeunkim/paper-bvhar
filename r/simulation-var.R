library(tidyverse)
library(bvhar)
set.seed(1)
# Simulate VAR--------------------------------
# Order: 25
# Use coefficient from fitting oxfordman.csv
# small: DJI, IXIC, RUT, SPX, AORD
# medium: add KS11, N225, SSEC, HSI
# large: add BFX, FCHI, FTMIB, FTSE, IBEX, SSMI
#---------------------------------------------
small_asset <- c("DJI", "IXIC", "RUT", "SPX", "AORD")
medium_asset <- c(small_asset, c("KS11", "N225", "SSEC", "HSI"))
large_asset <- c(medium_asset, c("BFX", "FCHI", "FTMIB", "FTSE", "IBEX", "SSMI"))
# p: VAR order
var_lag <- 5
# define coefficients-------------------------
small_fit <- 
  oxfordman_rk %>% 
  select(all_of(small_asset)) %>% 
  var_lm(., var_lag, include_mean = FALSE)
medium_fit <- 
  oxfordman_rk %>% 
  select(all_of(medium_asset)) %>% 
  var_lm(., var_lag, include_mean = FALSE)
large_fit <- 
  oxfordman_rk %>% 
  select(all_of(large_asset)) %>% 
  var_lm(., var_lag, include_mean = FALSE)
# Generate------------------------------------
# Train: 5000
# Test: 100
# Burn-in: 1000
#---------------------------------------------
# numbers: train + test
num_train <- 5000
num_test <- 100
num_burin <- 50
# SMALL---------------------------------------
small_coef <- coef(small_fit)
small_var <- diag(small_fit$covmat) %>% diag()
set.seed(1)
y_small <- sim_var(
  num_train + num_test,
  num_burin,
  small_coef,
  var_lag,
  small_var,
  matrix(0L, nrow = var_lag, ncol = length(small_asset))
)
colnames(y_small) <- paste("asset", sprintf(1:length(small_asset), fmt = "%02d"), sep = "_")
# MEDIUM--------------------------------------
medium_coef <- coef(medium_fit)
medium_var <- diag(medium_fit$covmat) %>% diag()
set.seed(1)
y_medium <- sim_var(
  num_train + num_test,
  num_burin,
  medium_coef,
  var_lag,
  medium_var,
  matrix(0L, nrow = var_lag, ncol = length(medium_asset))
)
colnames(y_medium) <- paste("asset", sprintf(1:length(medium_asset), fmt = "%02d"), sep = "_")
# LARGE---------------------------------------
large_coef <- coef(large_fit)
large_var <- diag(large_fit$covmat) %>% diag()
set.seed(1)
y_large <- sim_var(
  num_train + num_test,
  num_burin,
  large_coef,
  var_lag,
  large_var,
  matrix(0L, nrow = var_lag, ncol = length(large_asset))
)
colnames(y_large) <- paste("asset", sprintf(1:length(large_asset), fmt = "%02d"), sep = "_")
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
