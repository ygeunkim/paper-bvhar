library(tidyverse)
library(bvhar)
set.seed(1)
# Simulate VHAR-------------------------------
# Use coefficient from fitting oxfordman.csv
# small: DJI, IXIC, RUT, SPX, AORD
# medium: add KS11, N225, SSEC, HSI
# large: add BFX, FCHI, FTMIB, FTSE, IBEX, SSMI
#---------------------------------------------
small_asset <- c("DJI", "IXIC", "RUT", "SPX", "AORD")
medium_asset <- c(small_asset, c("KS11", "N225", "SSEC", "HSI"))
large_asset <- c(medium_asset, c("BFX", "FCHI", "FTMIB", "FTSE", "IBEX", "SSMI"))
oxfordman <- read_csv("data/processed/oxfordman.csv")
# define coefficients-------------------------
small_fit <- 
  oxfordman %>% 
  select(all_of(small_asset)) %>% 
  vhar_lm()
medium_fit <- 
  oxfordman %>% 
  select(all_of(medium_asset)) %>% 
  vhar_lm()
large_fit <- 
  oxfordman %>% 
  select(all_of(large_asset)) %>% 
  vhar_lm()
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
set.seed(1)
y_small <- sim_vhar(
  num_train + num_test,
  num_burin,
  coef(small_fit),
  small_fit$covmat,
  matrix(0L, nrow = 22L, ncol = length(small_asset))
)
colnames(y_small) <- paste("asset", sprintf(1:length(small_asset), fmt = "%02d"), sep = "_")
# MEDIUM--------------------------------------
set.seed(1)
y_medium <- sim_vhar(
  num_train + num_test,
  num_burin,
  coef(medium_fit),
  medium_fit$covmat,
  matrix(0L, nrow = 22L, ncol = length(medium_asset))
)
colnames(y_medium) <- paste("asset", sprintf(1:length(medium_asset), fmt = "%02d"), sep = "_")
# LARGE---------------------------------------
set.seed(1)
y_large <- sim_vhar(
  num_train + num_test,
  num_burin,
  coef(large_fit),
  large_fit$covmat,
  matrix(0L, nrow = 22L, ncol = length(large_asset))
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
