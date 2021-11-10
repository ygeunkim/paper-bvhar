# Compute loss for each variable and make kable------------------
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
make_kable <- function(mod_list, y, error = c("mse", "mae", "mape", "mase"), kable = TRUE, format = "latex") {
  # rbind loss------------------------------------
  error_type <- match.arg(error)
  error_table <- switch(
    error_type,
    "mse" = {
      foreach(mods = mod_list, .combine = rbind) %do% {
        mse(mods, y)
      }
    },
    "mae" = {
      foreach(mods = mod_list, .combine = rbind) %do% {
        mae(mods, y)
      }
    },
    "mape" = {
      foreach(mods = mod_list, .combine = rbind) %do% {
        mape(mods, y)
      }
    },
    "mase" = {
      foreach(mods = mod_list, .combine = rbind) %do% {
        mase(mods, y)
      }
    }
  )
  colnames(error_table) <- 
    stringr::str_remove_all(
      colnames(error_table),
      pattern = "\\_"
    )
  rownames(error_table) <- c("VAR", "VHAR", "BVAR", "BVHAR-VAR", "BVHAR-VHAR")
  # kable-----------------------------------------
  error_table <- as.data.frame(error_table)
  if (!kable) {
    return(error_table)
  }
  error_table %>% 
    mutate_all(
      function(x) {
        ifelse(
          x == min(x),
          cell_spec(
            format(x, nsmall = 3) %>% as.numeric(),
            color = "red"
          ),
          format(x, nsmall = 3) %>% as.numeric()
        )
      }
    ) %>% 
    t() %>% 
    kable(
      format = format,
      booktabs = TRUE,
      longtable = TRUE,
      escape = FALSE
    ) %>% 
    kable_styling(
      full_width = FALSE, 
      latex_options = c("striped", "HOLD_position")
    )
}

# Get latex code for above loss------------------------
# for each variable
#------------------------------------------------------
get_losstex <- function(mod_list, y) {
  dim_data <- seq_along(y)
  error_table <- foreach(error_type = c("mse", "mae", "mape", "mase"), .combine = rbind) %do% {
    mod_list %>% 
      make_kable(y_test, error_type, kable = FALSE) %>% 
      t() %>% 
      as.data.frame() %>% 
      mutate_if(
        is.numeric,
        function(x) {
          paste0(
            "\\numprint{",
            format(x, nsmall = 3),
            "}"
          )
        }
      ) %>% 
      rownames_to_column(var = "variable") %>% 
      add_column(Loss = error_type, .before = 1)
  }
  error_table %>% 
    kable(
      format = "latex",
      booktabs = TRUE,
      escape = FALSE
    ) %>% 
    kable_styling(
      full_width = FALSE,
      # latex_options = c("striped", "HOLD_position")
      latex_options = "HOLD_position"
    ) %>% 
    collapse_rows(
      columns = 1
    )
}

# Compute average loss---------------------------------
# returns kable
# use kableExtra
#------------------------------------------------------
kable_lossmean <- function(mod_list, y, kable = TRUE, format = "latex") {
  error_type <- c("mse", "mae", "mape", "mase")
  loss_mean <- foreach(error = error_type, .combine = rbind) %do% {
    switch(
      error,
      "mse" = {
        mod_list %>% 
          lapply(
            function(PRED) {
              mse(PRED, y) %>% 
                mean()
            }
          )
      },
      "mae" = {
        mod_list %>% 
          lapply(
            function(PRED) {
              mae(PRED, y) %>% 
                mean()
            }
          )
      },
      "mape" = {
        mod_list %>% 
          lapply(
            function(PRED) {
              mape(PRED, y) %>% 
                mean()
            }
          )
      },
      "mase" = {
        mod_list %>% 
          lapply(
            function(PRED) {
              mase(PRED, y) %>% 
                mean()
            }
          )
      }
    ) %>% 
      unlist()
  }
  rownames(loss_mean) <- error_type
  colnames(loss_mean) <- names(mod_list)
  loss_mean <- 
    loss_mean %>% 
    t() %>% 
    as.data.frame()
  if (!kable) {
    return(loss_mean)
  }
  loss_mean %>% 
    mutate_all(
      function(x) {
        ifelse(
          x == min(x),
          cell_spec(
            format(x, nsmall = 3) %>% as.numeric(),
            color = "red"
          ),
          format(x, nsmall = 3) %>% as.numeric()
        )
      }
    ) %>% 
    t() %>% 
    kable(
      format = format,
      booktabs = TRUE,
      longtable = TRUE,
      escape = FALSE
    ) %>% 
    kable_styling(
      full_width = FALSE, 
      latex_options = c("striped", "HOLD_position")
    )
}
