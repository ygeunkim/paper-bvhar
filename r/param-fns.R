# Report parameter settings--------------------------------------
# returns kable
# use kableExtra
if (requireNamespace("tidyverse", quietly = TRUE)) {
  library(tidyverse)
}
if (requireNamespace("bvhar", quietly = TRUE)) {
  library(bvhar)
}
if (requireNamespace("foreach", quietly = TRUE)) {
  library(foreach)
}
if (requireNamespace("knitr", quietly = TRUE)) {
  library(knitr)
}
if (requireNamespace("kableExtra", quietly = TRUE)) {
  library(kableExtra)
}
#----------------------------------------------------------------
kable_hyperparam <- function(bayes_spec, kable = TRUE, format = "latex", ...) {
  mod_process <- bayes_spec$process
  mod_prior <- bayes_spec$prior
  lambda_fill <- c(rep(NA, length(bayes_spec$sigma) - 1))
  if (mod_prior == "MN_VHAR") {
    res_dt <- cbind(
      sigma = bayes_spec$sigma,
      lambda = c(bayes_spec$lambda, lambda_fill),
      daily = bayes_spec$daily,
      weekly = bayes_spec$weekly,
      monthly = bayes_spec$monthly
    ) %>% 
      t()
  } else if (mod_prior == "Minnesota" | mod_prior == "MN_VAR") {
    res_dt <- cbind(
      sigma = bayes_spec$sigma,
      lambda = c(bayes_spec$lambda, lambda_fill),
      delta = bayes_spec$delta
    ) %>% 
      t()
  } else {
    stop("Not yet defined")
  }
  if (!is.null(colnames(res_dt))) {
    colnames(res_dt) <- 
      stringr::str_remove_all(
        colnames(res_dt),
        pattern = "\\_"
      )
  }
  if (!kable) {
    res_dt %>% 
      as.data.frame()
  } else {
    # rownames(res_dt) <- c("$\\sigma$", "$\\lambda$", "$\\delta$")
    rownames(res_dt)[1:2] <- c("$\\sigma$", "$\\lambda$")
    if (mod_prior == "MN_VHAR") {
      rownames(res_dt)[3:5] <- c("$d_i$", "$w_i$", "$m_i$")
    } else {
      rownames(res_dt)[3] <- "$\\delta$"
    }
    res_dt %>% 
      kable(
        format = format,
        booktabs = TRUE,
        escape = FALSE,
        ...
      ) %>% 
      kable_paper(
        full_width = FALSE
      )
  }
}
# Report every hyperparameters---------------------------
report_hyperparam <- function(spec_list, caption = "Hyperparameter Lists", label = "hyperparamlist") {
  hyperparam_table <- foreach(i = seq_along(spec_list), .combine = rbind) %do% {
    error_table <- kable_hyperparam(spec_list[[i]], kable = FALSE)
    rownames(error_table)[1:2] <- c("$\\sigma$", "$\\lambda$")
    if (spec_list[[i]]$prior == "MN_VHAR") {
      rownames(error_table)[3:5] <- c("$d_i$", "$w_i$", "$m_i$")
    } else {
      rownames(error_table)[3] <- "$\\delta$ "
    }
    error_table
  }
  hyperparam_table %>% 
    as.data.frame() %>% 
    kable(
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      caption = caption,
      label = label
    ) %>% 
    kable_paper(full_width = FALSE) %>% 
    pack_rows(
      index = c("BVAR" = 3, "BVHAR-VAR" = 3, "BVHAR-VHAR" = 5)
    )
}
