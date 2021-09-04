library(tidyverse)
library(bvhar)
#---------------------------------------------
y_train <- read_csv("data/processed/varsim_train.csv")
y_test <- read_csv("data/processed/varsim_test.csv")
num_test <- nrow(y_test)
# VAR-----------------------------------------
doMC::registerDoMC(16)
choose_var(y_train, 25, TRUE)
var_lag <- 2
fit_var <- var_lm(y_train, var_lag)
pred_var <- predict(fit_var, num_test)
mse_var <- mse(pred_var, y_test)
# VHAR----------------------------------------
fit_vhar <- vhar_lm(y_train)
pred_vhar <- predict(fit_vhar, num_test)
mse_vhar <- mse(pred_vhar, y_test)
# BVAR----------------------------------------
sig <- apply(y_train, 2, sd)
lam <- .5
del <- rep(.1, ncol(y_train))
fit_bvar <- bvar_minnesota(y_train, var_lag, sig, lam, del)
pred_bvar <- predict(fit_bvar, num_test)
mse_bvar <- mse(pred_bvar, y_test)
# BVHAR: VAR-type-----------------------------
fit_bvhar_v1 <- bvhar_minnesota(
  y_train,
  type = "VAR",
  sigma = sig,
  lambda = lam,
  delta = del,
  eps = 1e-04
)
pred_bvhar_v1 <- predict(fit_bvhar_v1, num_test)
mse_bvhar_v1 <- mse(pred_bvhar_v1, y_test)
# BVHAR: HAR-type-----------------------------
daily <- rep(.1, ncol(y_train))
weekly <- rep(.05, ncol(y_train))
monthly <- rep(.01, ncol(y_train))
fit_bvhar_v2 <- bvhar_minnesota(
  y_train,
  type = "VHAR",
  sigma = sig,
  lambda = lam,
  daily = daily,
  weekly = weekly,
  monthly = monthly,
  eps = 1e-04
)
pred_bvhar_v2 <- predict(fit_bvhar_v2, num_test)
mse_bvhar_v2 <- mse(pred_bvhar_v2, y_test)
# Plot the loss-------------------------------
list(
  pred_var,
  pred_vhar,
  pred_bvar,
  pred_bvhar_v1,
  pred_bvhar_v2
) %>% 
  plot_loss(y = y_test) +
  theme_minimal() +
  scale_y_log10()
