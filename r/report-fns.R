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
  if (!is.null(names(mod_list))) {
    rownames(error_table) <- names(mod_list)
  } else {
    rownames(error_table) <- 
      sapply(mod_list, function(x) x$process) %>% 
      str_replace_all(pattern = "\\_", replacement = "-")
  }
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
            format = format,
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
      escape = FALSE
    ) %>% 
    kable_styling(
      full_width = FALSE, 
      latex_options = c("striped", "HOLD_position")
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
  if (!is.null(names(mod_list))) {
    colnames(loss_mean) <- names(mod_list)
  } else {
    colnames(loss_mean) <- 
      sapply(mod_list, function(x) x$process) %>% 
      str_replace_all(pattern = "\\_", replacement = "-")
  }
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

# Get latex code for above loss------------------------
# put together
#------------------------------------------------------
get_losstex <- function(mod_list, y, caption = "Loss for SMALL Simulation", label = "smallerr") {
  dim_data <- seq_along(y)
  mean_table <- kable_lossmean(mod_list, y, kable = FALSE)
  error_table <- foreach(error_type = c("mse", "mae", "mape", "mase"), .combine = rbind) %do% {
    mod_list %>% 
      make_kable(y, error_type, kable = FALSE) %>% 
      cbind(Average = mean_table[,error_type]) %>% 
      mutate_all(
        function(x) {
          ifelse(
            x == min(x),
            cell_spec(
              # format(x, nsmall = 3) %>% as.numeric(),
              paste0("\\numprint{", format(x, nsmall = 3) %>% as.numeric(), "}"), # numprint
              format = "latex",
              escape = FALSE,
              color = "red"
            ),
            paste0("\\numprint{", format(x, nsmall = 3) %>% as.numeric(), "}") # numprint
          )
        }
      ) %>% 
      t() %>% 
      as.data.frame() %>%
      rownames_to_column(var = "variable") %>% 
      add_column(Loss = error_type, .before = 1)
  }
  error_table %>% 
    mutate(
      Loss = str_to_upper(Loss),
      variable = ifelse(
        variable == "Average",
        cell_spec(variable, format = "latex", background = "gray"), # mark the average cell
        variable
      )
    ) %>% 
    kable(
      format = "latex",
      booktabs = TRUE,
      longtable = TRUE,
      escape = FALSE,
      col.names = c("", "", "VAR", "VHAR", "Minnesota", "VAR-type", "VHAR-type"),
      caption = caption,
      label = label
    ) %>% 
    kable_paper(full_width = FALSE, latex_options = c("repeat_header")) %>% 
    add_header_above(
      c(
        " " = 1,
        " " = 1,
        "Frequentist" = 2,
        "BVAR" = 1,
        "BVHAR" = 2
      )
    ) %>% 
    collapse_rows(
      columns = 1
    )
}
