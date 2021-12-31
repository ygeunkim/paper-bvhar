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
      col.names = NULL,
      caption = caption,
      label = label
    ) %>% 
    kable_paper(full_width = FALSE) %>% 
    pack_rows(
      index = c("BVAR" = 3, "BVHAR-VAR" = 3, "BVHAR-VHAR" = 5)
    )
}

report_true_hyperparam <- function(spec_list, 
                                   kable = TRUE, 
                                   model_name = "DGP1",
                                   caption = "Hyperparameter Lists", 
                                   label = "hyperparamlist") {
  if (length(spec_list) != 3) {
    stop("Wrong 'spec_list'.")
  }
  if (any(names(spec_list) != c("small", "medium", "large"))) {
    stop("Wrong names of 'spec_list'.")
  }
  size_id <- c("SMALL", "MEDIUM", "LARGE")
  large_dim <- length(spec_list$large$sigma)
  cols_add <- list()
  dim_add <- 0
  hyperparam_table <- foreach(i = seq_along(spec_list), .combine = rbind) %do% {
    error_table <- kable_hyperparam(spec_list[[i]], kable = FALSE)
    dim_add <- large_dim - length(spec_list[[i]]$sigma)
    cols_add <- 
      rep(NA, dim_add) %>% 
      as.list()
    if (dim_add != 0) {
      names(cols_add) <- paste0("V", (length(spec_list[[i]]$sigma) + 1):large_dim)
    }
    error_table %>% 
      bind_cols(cols_add) %>% 
      add_column(size = size_id[i], .before = 1) %>% 
      rownames_to_column(var = "hyperparam")
  } %>% 
    relocate(hyperparam, .after = size) %>% 
    mutate(
      hyperparam = case_when(
        hyperparam == "sigma" ~ "$\\sigma$",
        hyperparam == "lambda" ~ "$\\lambda$",
        hyperparam == "delta" ~ "$\\delta$ ",
        hyperparam == "daily" ~ "$d_i$",
        hyperparam == "weekly" ~ "$w_i$",
        hyperparam == "monthly" ~ "$m_i$"
      )
    )
  if (!kable) {
    return(hyperparam_table)
  }
  title_head <- c(ncol(hyperparam_table))
  names(title_head) <- model_name
  hyperparam_table %>% 
    kable(
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      col.names = NULL,
      align = "c",
      caption = caption,
      label = label
    ) %>% 
    kable_paper(full_width = FALSE) %>% 
    add_header_above(
      title_head,
      align = "l"
    ) %>%
    collapse_rows(
      columns = 1,
      valign = "middle",
      latex_hline = "major"
    )
}

