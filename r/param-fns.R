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
#--------------------------------------------------------
get_hyperparam <- function(spec_list, report_true = FALSE) {
  if (report_true) {
    mod_nm <- paste0("DGP", 1:4)
  } else {
    mod_nm <- sapply(
      spec_list, function(x) {
        paste(x$process, x$prior, sep = "-") %>% 
          str_replace_all(pattern = "\\_", replacement = "-") %>% 
          str_remove_all(pattern = "-Minnesota$") %>% 
          str_replace_all(pattern = "MN-VAR$", replacement = "S") %>% 
          str_replace_all(pattern = "MN-VHAR$", replacement = "L")
      }
    )
  }
  if (length(spec_list) != length(mod_nm)) {
    stop("Wrong 'spec_list'")
  }
  hyperparam_table <- foreach(i = seq_along(spec_list), .combine = rbind) %do% {
    kable_hyperparam(spec_list[[i]], kable = FALSE) %>% 
      rownames_to_column(var = "Hyperparameters") %>% 
      add_column(Model = mod_nm[i], .before = 1)
  } %>% 
    mutate(
      Hyperparameters = case_when(
        Hyperparameters == "sigma" ~ "$\\sigma$",
        Hyperparameters == "lambda" ~ "$\\lambda$",
        Hyperparameters == "delta" ~ "$\\delta$ ",
        Hyperparameters == "daily" ~ "$d_i$",
        Hyperparameters == "weekly" ~ "$w_i$",
        Hyperparameters == "monthly" ~ "$m_i$"
      )
    )
  hyperparam_table %>% 
    as_tibble() %>% 
    rename_with(function(x) str_replace_all(x, pattern = "^V", replacement = "y"))
}
# Report every hyperparameters---------------------------
report_hyperparam <- function(spec_list, 
                              report_true = FALSE, 
                              size = NULL,
                              caption = "Hyperparameter Lists", 
                              label = "hyperparamlist") {
  if (length(spec_list) != 3) {
    stop("Wrong 'spec_list'.")
  }
  if (any(names(spec_list) != c("small", "medium", "large"))) {
    stop("Wrong names of 'spec_list'.")
  }
  size_id <- c("SMALL", "MEDIUM", "LARGE")
  large_dim <- length(spec_list$large[[1]]$sigma)
  cols_add <- list()
  dim_add <- 0
  hyperparam_table <- foreach(i = seq_along(spec_list), .combine = rbind) %do% {
    error_table <- get_hyperparam(spec_list[[i]], report_true = report_true) # small-medium-large
    dim_add <- large_dim - length(spec_list[[i]][[1]]$sigma)
    cols_add <- 
      rep(NA, dim_add) %>% 
      as.list()
    if (dim_add != 0) {
      names(cols_add) <- paste0("y", (length(spec_list[[i]][[1]]$sigma) + 1):large_dim)
    }
    error_table %>% 
      bind_cols(cols_add) %>% 
      add_column(size = size_id[i], .before = 1)
  }
  colnames(hyperparam_table)[1:3] <- c(" ", "  ", "   ")
  hyperparam_table %>% 
    kable(
      format = "latex",
      booktabs = TRUE,
      longtable = TRUE,
      escape = FALSE,
      # col.names = NULL,
      caption = caption,
      label = label
    ) %>% 
    kable_paper(
      full_width = FALSE,
      font_size = size,
      latex_options = c("repeat_header")
    ) %>% 
    collapse_rows(
      columns = 1:2,
      latex_hline = "custom",
      custom_latex_hline = 1:2,
      row_group_label_position = "stack"
    )
}
