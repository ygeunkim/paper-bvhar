if (requireNamespace("tidyverse", quietly = TRUE)) {
  library(tidyverse)
}
if (requireNamespace("bvhar", quietly = TRUE)) {
  library(bvhar)
}
# if (requireNamespace("factorstochvol", quietly = TRUE)) {
#   library(factorstochvol)
# }
if (requireNamespace("foreach", quietly = TRUE)) {
  library(foreach)
}
if (requireNamespace("knitr", quietly = TRUE)) {
  library(knitr)
}
if (requireNamespace("kableExtra", quietly = TRUE)) {
  library(kableExtra)
}
options(digits = 3)
set.seed(1)
# data setting-------------------------------------
h <- 30
etf_split <-
  divide_ts(
    etf_vix %>%
      rename_with(~str_remove_all(., pattern = "CLS$")),
    h
  )
etf_train <- etf_split$train
etf_test <- etf_split$test
step_roll <- 1
var_lag <- 3
bvar_lag <- var_lag
# cache in analysis.rmd----------------------------
qwraps2::lazyload_cache_dir("R/analysis_cache/gfm")
fit_bvar <- bvar_optim$fit
fit_bvhar <- bvhar_var_optim$fit
fit_bvhar_vhar <- bvhar_vhar_optim$fit
# Cholesky decomposition of covmat-----------------
Rcpp::sourceCpp("src/misc-func.cpp")
# Out-of-forecasting-------------------------------
roll_shock <- function(y, bayes_spec, include_mean, step, y_test) {
  # shock_mat
  num_horizon <- nrow(y_test) - step + 1
  mat_roll <- y
  fit <- bvhar_minnesota(mat_roll, har = c(5, 22), bayes_spec = bayes_spec, include_mean = include_mean)
  shock_mat <- ldlt_inv_upper(summary(fit, num_iter = 5000L)$covmat)
  pred_fit <- bvhar:::forecast_bvharmn(fit, step, 1)
  res <- matrix(nrow = num_horizon, ncol = ncol(y_test))
  res[1,] <- last(pred_fit$posterior_mean %*% shock_mat)
  for (i in seq_len(num_horizon - 1)) {
    mat_roll[1:(nrow(y) - 1),] <- mat_roll[2:nrow(y),]
    mat_roll[nrow(y),] <- y_test[i,]
    fit <- bvhar_minnesota(mat_roll, har = c(5, 22), bayes_spec = bayes_spec, include_mean = include_mean)
    shock_mat <- ldlt_inv_upper(summary(fit, num_iter = 5000L)$covmat)
    pred_fit <- bvhar:::forecast_bvharmn(fit, step, 1)
    res[i + 1,] <- last(pred_fit$posterior_mean %*% shock_mat)
  }
  res_ls <- list(
    process = fit$process,
    forecast = res,
    eval_id = step:nrow(y_test),
    y = as.matrix(y)
  )
  class(res_ls) <- c("predbvhar_roll", "bvharcv")
  res_ls
}
# tmp_roll <- roll_shock(etf_train, bvhar_var_optim$spec, TRUE, 1, etf_test)

# 1-step, 5-step, and 20-step-------------------
doMC::registerDoMC(cores = 6)
pred_ls <- foreach(b_spec = 1:2, .combine = "rbind") %:%
  foreach(i = c(1, 5, 20), .combine = "rbind") %dopar% {
    set.seed(1)
    if (b_spec == 1) {
      bayes_spec <- bvhar_var_optim$spec
      pred_bvhar <- forecast_roll(fit_bvhar, i, etf_test)
    } else if (b_spec == 2) {
      bayes_spec <- bvhar_vhar_optim$spec
      pred_bvhar <- forecast_roll(fit_bvhar_vhar, i, etf_test)
    }
    # bayes_spec <- ifelse(b_spec == 1, bvhar_var_optim$spec, bvhar_vhar_optim$spec)
    pred_fit <- roll_shock(etf_train, bayes_spec, TRUE, i, etf_test)
    # pred_var <- forecast_roll(var_lm(etf_train, var_lag), i, etf_test)
    data.frame(
      BVHAR = ifelse(b_spec == 1, "BVHAR-S", "BVHAR-L"),
      h = i,
      RMAFE = rmafe(pred_fit, pred_bvhar, etf_test),
      RMSFE = rmsfe(pred_fit, pred_bvhar, etf_test),
      RMAPE = rmape(pred_fit, pred_bvhar, etf_test),
      RMASE = rmase(pred_fit, pred_bvhar, etf_test)
    )
  }


# pred_ls <- foreach(i = c(1, 5, 20), .combine = rbind) %dopar% {
#   set.seed(1)
#   # pred_fit <- roll_fsv(etf_train, i, etf_test, TRUE, 5000L, 2500L)
#   # pred_fit <- roll_shock(etf_train, )
#   pred_var <- forecast_roll(var_lm(etf_train, var_lag), i, etf_test)
#   
#   
#   data.frame(
#     h = i,
#     RMAFE = sum(abs(pred_fit - etf_test)) / sum(mae(pred_var, etf_test)),
#     # RMAFE = rmafe()
#     RMSFE = sum((pred_fit - etf_test)^2) / sum(mse(pred_var, etf_test)),
#     RMAPE = mean(
#       apply(100 * (pred_fit - etf_test) / etf_test, 2, function(pt) mean(abs(pt)))
#     ) / mean(mape(pred_var, etf_test)),
#     RMASE = mean(
#       apply(100 * (pred_fit - etf_test) / colMeans(abs(diff(as.matrix(etf_train)))), 2, function(qt) mean(abs(qt)))
#     ) / mean(mase(pred_var, etf_test))
#   )
# }

# bvhar:::VHARcoeftoVMA_ortho(
#   vhar_coef = mcmc_bvhars$coefficients,
#   vhar_covmat = mcmc_bvhars$covmat,
#   HARtrans_mat = fit_bvhar$HARtrans,
#   lag_max = 1,
#   month = fit_bvhar$month
# )

# mat_irf <- VHARcoeftoVMA(
#   vhar_coef = mat_coef,
#   HARtrans_mat = object$HARtrans,
#   lag_max = lag_max,
#   month = object$month
# )




# # Forecasting---------------------------------
# forecast_fsv <- function(fit, fit_vhar, n_ahead, lev = .05) {
#   pred_cov <- predcov(fit, ahead = n_ahead)
#   # pred_draws <- t(fit$beta)
#   dim_data <- ncol(fit$y)
#   # colnames(pred_draws) <- colnames(fit$y)
#   
#   # # fit_vhar <- vhar_lm(etf_train)
#   # # fit_vhar_fsv <- fsvsample(y = fit_vhar$residuals, draws = 5000L, burnin = 5000L, zeromean = FALSE)
#   # pred_vhar <- predict(fit_vhar, h)
#   # # pred_vhar_fsv <- predcov(fit_vhar_fsv, ahead = 1)
#   # # predict(fit_vhar, 2)$forecast
#   # pred_fsv_cov <- predcov(fit_vhar_fsv, h)
#   # pred_fsv <- array(dim = c(h, ncol(etf_test), 10000))
#   # for (i in 1:10000) {
#   #   pred_fsv[,, i] <- pred_vhar$forecast + matrix(rnorm(ncol(etf_test) * h), nrow = h) %*% chol(pred_fsv_cov[,, i, 1])
#   # }
#   # lower_quantile <- apply(pred_fsv, c(1, 2), quantile, probs = .05 / 2)
#   # upper_quantile <- apply(pred_fsv, c(1, 2), quantile, probs = (1 - .05 / 2))
#   
#   
#   # pred_vhar <- 
#   #   predict(fit_vhar, n_ahead)$forecast %>% 
#   #   last() %>% 
#   #   c()
#   pred_vhar <- predict(fit_vhar, n_ahead)
#   pred_draws <- array(dim = c(n_ahead, dim_data, dim(pred_cov)[3]))
#   for (i in seq_len(dim(pred_cov)[3])) {
#     # pred_draws[i,] <- pred_vhar + chol(pred_cov[,,i, 1]) %*% rnorm(dim_data)
#     pred_draws[,, i] <- pred_vhar$forecast + matrix(rnorm(dim_data * n_ahead), nrow = n_ahead) %*% chol(pred_cov[,, i, 1])
#   }
#   pred_point <- apply(pred_draws, c(1, 2), mean)
#   lower_quantile <- apply(pred_draws, c(1, 2), quantile, probs = lev / 2)
#   upper_quantile <- apply(pred_draws, c(1, 2), quantile, probs = (1 - lev / 2))
#   colnames(pred_point) <- colnames(fit$y)
#   colnames(lower_quantile) <- colnames(fit$y)
#   colnames(upper_quantile) <- colnames(fit$y)
#   list(
#     # forecast = colMeans(pred_draws),
#     forecast = pred_point,
#     predictive = pred_draws,
#     lower_joint = lower_quantile,
#     upper_joint = upper_quantile,
#     y = pred_vhar$y,
#     process = "VHAR-FSV"
#   )
# }
# # Out-of-step forecasting---------------------
# roll_fsv <- function(y, n_ahead, y_test, include_mean = TRUE, num_iter = 10000L, num_burn = 5000L) {
#   # num_horizon <- nrow(y_test) # 1-step
#   num_horizon <- nrow(y_test) - n_ahead + 1
#   mat_roll <- as.matrix(y)
#   y_test <- as.matrix(y_test)
#   fit_vhar <- vhar_lm(mat_roll, c(5, 22), include_mean)
#   fit <- fsvsample(fit_vhar$residuals, draws = num_iter - num_burn, burnin = num_burn, zeromean = FALSE, quiet = TRUE)
#   res_roll <- matrix(0L, nrow = num_horizon, ncol = ncol(y))
#   colnames(res_roll) <- colnames(y_test)
#   res_roll[1,] <- last(forecast_fsv(fit, fit_vhar, n_ahead)$forecast)
#   for (i in seq_len(num_horizon - 1)) {
#     mat_roll[1:(nrow(y) - 1),] <- mat_roll[2:nrow(y),]
#     mat_roll[nrow(y),] <- y_test[i,]
#     fit_vhar <- vhar_lm(mat_roll, c(5, 22), include_mean)
#     fit <- fsvsample(fit_vhar$residuals, draws = num_iter - num_burn, burnin = num_burn, zeromean = FALSE)
#     res_roll[i + 1,] <- last(forecast_fsv(fit, fit_vhar, n_ahead))
#   }
#   res_roll
# }
# # 1-step, 5-step, and 20-step-------------------
# doMC::registerDoMC(cores = 3)
# pred_ls <- foreach(i = c(1, 5, 20), .combine = rbind) %dopar% {
#   set.seed(1)
#   pred_fit <- roll_fsv(etf_train, i, etf_test, TRUE, 5000L, 2500L)
#   pred_var <- forecast_roll(var_lm(etf_train, var_lag), i, etf_test)
#   data.frame(
#     h = i,
#     RMAFE = sum(abs(pred_fit - etf_test)) / sum(mae(pred_var, etf_test)),
#     RMSFE = sum((pred_fit - etf_test)^2) / sum(mse(pred_var, etf_test)),
#     RMAPE = mean(
#       apply(100 * (pred_fit - etf_test) / etf_test, 2, function(pt) mean(abs(pt)))
#     ) / mean(mape(pred_var, etf_test)),
#     RMASE = mean(
#       apply(100 * (pred_fit - etf_test) / colMeans(abs(diff(as.matrix(etf_train)))), 2, function(qt) mean(abs(qt)))
#     ) / mean(mase(pred_var, etf_test))
#   )
# }

# Evaluations-----------------------------------
# pred_ls %>% 
#   mutate(h = str_c("$h = ", as.character(h), "$")) %>% 
#   column_to_rownames(var = "h") %>% 
#   kable(
#     format = "latex",
#     booktabs = TRUE,
#     escape = FALSE,
#     align = "c",
#     caption = "Out-of-sample forecasting of VHAR-FSV relative to VAR(3).",
#     label = "fsv"
#   ) %>% 
#   cat()
# pred_ls %>% 
#   mutate(
#     h = str_c("$h = ", as.character(h), "$"),
#     across(
#       where(is.numeric),
#       ~cell_spec(
#         paste0(
#           "\\num{",
#           str_remove(format(., nsmall = 3, scientific = -2), pattern = "0(?=\\.)"),
#           "}"
#         ),
#         format = "latex",
#         escape = FALSE
#       )
#     )
#   )
pred_tab <- 
  pred_ls %>% 
  pivot_longer(-c(BVHAR, h), names_to = "loss", values_to = "values") %>% 
  mutate(
    h = factor(str_c("$h = ", as.character(h), "$"), levels = c("$h = 1$", "$h = 5$", "$h = 20$")),
    values = cell_spec(
      paste0(
        "\\num{",
        str_remove(format(values, nsmall = 3) %>% as.numeric(), pattern = "0(?=\\.)"),
        "}"
      ),
      format = "latex",
      escape = FALSE
    ),
    loss = factor(loss, levels = c("RMAFE", "RMSFE", "RMAPE", "RMASE"))
  ) %>% 
  pivot_wider(names_from = c(loss, h), values_from = values, names_sort = TRUE)
colnames(pred_tab) <- str_remove_all(colnames(pred_tab), pattern = ".*\\_")
pred_tab %>% 
  kable(format = "latex", booktabs = TRUE, escape = FALSE, align = c("c|", rep("c", 2)), caption = "Out-of-sample forecasting relative to each BVHAR-S and BVHAR-L", label = "illustration") %>%
  kable_paper(full_width = FALSE, latex_options = c("scale_down")) %>%
  add_header_above(c(" " = 1, "RMAFE" = 3, "RMSFE" = 3, "RMAPE" = 3, "RMASE" = 3))



# Plots------------------------------------------
# fit_vhar <- vhar_lm(etf_train)
# fit_vhar_fsv <- fsvsample(fit_vhar$residuals, draws = 5000L, burnin = 5000L, zeromean = FALSE)
# pred_vhar_fsv <- forecast_fsv(fit_vhar_fsv, fit_vhar, h)
# pred_ls <- 
#   bvhar:::gather_predbvhar(pred_vhar_fsv) %>% 
#   filter(id >= 860) # for visualization
# 
# pred_ls %>% 
#   ggplot(aes(x = id, y = value_forecast)) +
#   geom_ribbon(
#     aes(ymin = value_lower_joint, ymax = value_upper_joint, fill = Model),
#     data = filter(pred_ls, forecast == TRUE),
#     alpha = .3,
#     show.legend = FALSE
#   ) +
#   geom_path() +
#   geom_eval(etf_test, colour = "#000000", alpha = .5) +
#   theme_minimal() +
#   theme(
#     panel.border = element_rect(fill = NA),
#     axis.text.x = element_blank(),
#     legend.position = "top",
#     text = element_text(family = "serif")
#   ) +
#   # geom_ribbon(aes(ymin = value_lower_joint, ymax = value_upper_joint, colour = Model)) +
#   facet_wrap(variable ~ ., scales = "free_y") +
#   labs(
#     x = element_blank(),
#     y = element_blank()
#   )


# fit_fsv <- fsvsample(y = as.matrix(etf_train), draws = 5000L, burnin = 5000L, zeromean = FALSE)
# pred_fsv <- predcond(fit_fsv, ahead = 1, each = 5)
# plot(pred_fsv)
# pred_draws <- matrix(rnorm(
#   length(pred_fsv$means[,,1]),
#   mean = pred_fsv$means[,,1],
#   sd = pred_fsv$vols[,,1]
# ), nrow = ncol(etf_train))
# pairs(t(pred_draws), col = rgb(0,0,0,.1), pch = 16)

# corimageplot(fit_fsv, plotCI = "circle", plotdatedist = 2, date.cex = 1.1)
# 
# facloadpairplot(fit_fsv)


# pred_vhar <- 
#   predict(fit_vhar, n_ahead)$forecast %>% 
#   last() %>% 
#   c()
# for (i in seq_len(nrow(pred_draws))) {
#   # pred_draws[i,] <- pred_draws[i,] + chol(pred_cov[,,i, 1]) %*% rnorm(dim_data)
#   pred_draws[i,] <- pred_vhar + chol(pred_cov[,,i, 1]) %*% rnorm(dim_data)
# }



# pred_vhar_fsv <- forecast_fsv(fit_vhar_fsv, fit_vhar, 1)
# pred_vhar <- predict(fit_vhar, h)

# parallel::mclapply(1:3, function(x) c(A = 1, B = 2, C = 3))


# LSD::heatpairs(
#   pred_draws,
#   labels = colnames(etf_train),
#   cor.cex = 1.5,
#   gap = 0.3,
#   xlim = quantile(pred_draws, c(0.01, 0.99)),
#   ylim = quantile(pred_draws, c(0.01, 0.99))
# )


# cov_draws <- matrix()
# plot.ts(pred_cov[1, 2,,])

# logvartimeplot(fit_fsv)
# voltimeplot(fit_fsv)
# plot(fit_fsv)

# predloglik(x = fit_fsv, y = as.matrix(etf_test), ahead = seq_len(h))


