library(tidyverse)
library(bvhar)
set.seed(1)
# VAR(5)--------------------------------------
# 7-dim
#---------------------------------------------
mts_dim <- 7
# sig <- apply(starting_dat, 2, sd, na.rm = TRUE)
sig <- c(.00003, .00002, .00015, .00001, .00025, .00005, .00035)
lam <- .1
delta <- rep(.1, mts_dim)
bvar_lag <- 10
# simulation
BVAR_SIM <- sim_mncoef(bvar_lag, sig, lam, delta)
VAR_COEF <- BVAR_SIM$coefficients
VAR_SIG <- BVAR_SIM$covmat
# Generate VAR(10)-----------------------------
num_train <- 5000
num_test <- 100
y_var <- sim_var(
  num_train + num_test,
  VAR_COEF,
  bvar_lag,
  diag(mts_dim),
  matrix(0L, nrow = bvar_lag, ncol = mts_dim)
)
colnames(y_var) <- paste("asset", 1:mts_dim, sep = "_")
# Plot----------------------------------------
y_var %>% 
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
y_split <- divide_ts(y_var, num_test)
# Save----------------------------------------
# y_split$train %>% 
#   as.data.frame() %>% 
#   write_csv(file = "data/processed/varsim_train.csv")
# y_split$test %>% 
#   as.data.frame() %>% 
#   write_csv(file = "data/processed/varsim_test.csv")
