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
kable_lossmean <- function(mod_list, y, kable = TRUE, caption = "", label = "") {
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
  rownames(loss_mean) <- str_to_upper(error_type)
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
    kable(
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      caption = caption,
      label = label
    ) %>% 
    kable_paper(
      full_width = FALSE
    )
}

# Get latex code for above loss------------------------
# put together
#------------------------------------------------------
get_losstex <- function(mod_list, y, caption = "Loss for SMALL Simulation", label = "smallerr", size = NULL) {
  dim_data <- seq_along(y)
  mean_table <- kable_lossmean(mod_list, y, kable = FALSE)
  error_table <- foreach(error_type = c("mse", "mae", "mape", "mase"), .combine = rbind) %do% {
    mod_list %>% 
      make_kable(y, error_type, kable = FALSE) %>% 
      cbind(Average = mean_table[,str_to_upper(error_type)]) %>% 
      mutate_all(
        function(x) {
          ifelse(
            x == min(x),
            cell_spec(
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
    kable_paper(full_width = FALSE, font_size = size, latex_options = c("repeat_header")) %>% 
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

# Get RMSFE table--------------------------------------
# nested list: sizes - (1, 5, 20)
# returns kable
# use kableExtra
#------------------------------------------------------
get_rmfe <- function(mod_list, 
                     y, 
                     ahead_list = c("one", "five", "twenty"), 
                     benchmark_id,
                     error = c("rmafe", "rmsfe")) {
  error_type <- match.arg(error)
  if (length(mod_list) != length(ahead_list)) {
    stop("Wrong 'ahead_list'.")
  }
  # compute RMAFE--------------------------
  error_table <- foreach(ahead = seq_along(mod_list), .combine = rbind) %do% {
    err_vec <- 
      sapply(
        seq_along(mod_list[[ahead]])[-benchmark_id],
        function(id) {
          switch(
            error_type,
            "rmafe" = {
              rmafe(mod_list[[ahead]][[id]], mod_list[[ahead]][[benchmark_id]], y)
            },
            "rmsfe" = {
              rmsfe(mod_list[[ahead]][[id]], mod_list[[ahead]][[benchmark_id]], y)
            }
          )
        }
      )
    names(err_vec) <- sapply(
      mod_list[[ahead]][-benchmark_id],
      function(x) x$process
    ) %>% 
      str_replace_all(pattern = "\\_", replacement = "-")
    err_vec
  }
  rownames(error_table) <- ahead_list
  error_table %>% as.data.frame()
}
# Get latex code for SMALL, MEDIUM, and LARGE----------
# nested list of above (SMALL-MEDIUM-LARGE)
#------------------------------------------------------
get_rmafetex <- function(mod_list, 
                         y_list, 
                         ahead_list = c("One", "Five", "Twenty"), 
                         benchmark_id, 
                         caption = "", 
                         label = "",
                         font_size = NULL) {
  if (length(mod_list) != 3) {
    stop("Wrong 'mod_list'.")
  }
  if (length(y_list) != 3) {
    stop("Wrong 'y_list'.")
  }
  mod_lenth <- length(mod_list[[1]][[1]]) - 1
  error_list <- 
    c("rmafe", "rmsfe") %>% 
    lapply(
      function(error) {
        lapply(
          1:3,
          function(id) {
            get_rmfe(
              mod_list[[id]], 
              y_list[[id]], 
              ahead_list = ahead_list, 
              benchmark_id = benchmark_id, 
              error = error
            ) %>% 
              rownames_to_column(var = "h") %>% 
              mutate(size = case_when(
                id == 1 ~ "SMALL",
                id == 2 ~ "MEDIUM",
                id == 3 ~ "LARGE"
              ))
          }
        ) %>% 
          bind_rows() %>% 
          pivot_longer(-c(h, size), names_to = "model", values_to = "error") %>% 
          group_by(size, h) %>% 
          mutate(
            error = cell_spec(
              format(error, nsmall = 3) %>% as.numeric(),
              format = "latex",
              color = ifelse(error == min(error), "red", "black")
            )
          ) %>% 
          ungroup() %>% 
          pivot_wider(names_from = c(size, model), values_from = error)
      } %>% 
        bind_rows() %>% 
        add_column(Loss = str_to_upper(error), .before = 1)
    ) %>% 
    bind_rows()
  # kable------------------------------------------
  colnames(error_list) <- str_remove_all(colnames(error_list), pattern = ".*\\_")
  colnames(error_list)[1:2] <- c("", "")
  error_list %>% 
    kable(
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      caption = caption,
      label = label
    ) %>% 
    kable_paper(full_width = FALSE, font_size = font_size, latex_options = c("scale_down")) %>% 
    add_header_above(c(
      " " = 2,
      "SMALL" = mod_lenth,
      "MEDIUM" = mod_lenth,
      "LARGE" = mod_lenth
    )) %>% 
    collapse_rows(columns = 1)
}

