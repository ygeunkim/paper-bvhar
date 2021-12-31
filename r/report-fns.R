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
            paste0("\\num{", format(x, nsmall = 3) %>% as.numeric(), "}"), # siunitx
            format = "latex",
            escape = FALSE,
            color = "red"
          ),
          paste0("\\num{", format(x, nsmall = 3) %>% as.numeric(), "}") # siunitx
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
              paste0("\\num{", format(x, nsmall = 3) %>% as.numeric(), "}"), # siunitx
              format = "latex",
              escape = FALSE,
              color = "red"
            ),
            paste0("\\num{", format(x, nsmall = 3) %>% as.numeric(), "}") # siunitx
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
                         header_angle = NULL) {
  if (length(mod_list) != 3) {
    stop("Wrong 'mod_list'.")
  }
  if (length(y_list) != 3) {
    stop("Wrong 'y_list'.")
  }
  mod_length <- length(mod_list[[1]][[1]]) - 1 # change?
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
              paste0("\\num{", format(error, nsmall = 3) %>% as.numeric(), "}"), # siunitx
              format = "latex",
              escape = FALSE,
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
  align <- 
    c(
      c("c", "c|"),
      rep(
        c(
          rep("c", mod_length - 1), 
          "c|"
        ),
        3
      )
    )
  error_list %>% 
    kable(
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      align = align,
      caption = caption,
      label = label
    ) %>% 
    kable_paper(full_width = FALSE, latex_options = c("scale_down")) %>% 
    add_header_above(c(
      " " = 2,
      "SMALL" = mod_length,
      "MEDIUM" = mod_length,
      "LARGE" = mod_length
    )) %>% 
    row_spec(0, angle = header_angle) %>% 
    collapse_rows(columns = 1)
}
# Transposed table including MAPE and MASE--------------------------------------------
get_rmfe_tr <- function(mod_list, 
                        y, 
                        ahead_list = c("one", "five", "twenty"), 
                        benchmark_id,
                        error = c("rmafe", "rmsfe", "mape", "mase")) {
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
            },
            "mape" = {
              mean(mape(mod_list[[ahead]][[id]], y)) / mean(mape(mod_list[[ahead]][[benchmark_id]], y))
            },
            "mase" = {
              mean(mase(mod_list[[ahead]][[id]], y)) / mean(mase(mod_list[[ahead]][[benchmark_id]], y))
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
  colnames(error_table) <- str_remove_all(colnames(error_table), pattern = "-Minnesota$")
  colnames(error_table) <- str_replace_all(colnames(error_table), pattern = "MN-VAR$", replacement = "S")
  colnames(error_table) <- str_replace_all(colnames(error_table), pattern = "MN-VHAR$", replacement = "L")
  error_table %>% as.data.frame()
}
#--------------------------------------------
get_rmafetex_tr <- function(mod_list, 
                            y_list, 
                            ahead_list = c("One", "Five", "Twenty"), 
                            benchmark_id, 
                            caption = "", 
                            label = "",
                            header_angle = NULL) {
  if (length(mod_list) != 3) {
    stop("Wrong 'mod_list'.")
  }
  if (length(y_list) != 3) {
    stop("Wrong 'y_list'.")
  }
  ahead_length <- length(mod_list[[1]])
  error_list <- 
    c("rmafe", "rmsfe", "mase") %>% 
    lapply(
      function(er) {
        lapply(
          1:3,
          function(id) {
            get_rmfe_tr(
              mod_list[[id]], 
              y_list[[id]], 
              ahead_list = ahead_list, 
              benchmark_id = benchmark_id, 
              error = er
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
          add_column(Loss = str_to_upper(er), .before = 1) %>%
          pivot_longer(-c(h, size, Loss), names_to = "model", values_to = "error") %>%
          group_by(size, h) %>%
          mutate(
            error = cell_spec(
              # paste0("\\num{", format(error, nsmall = 3) %>% as.numeric(), "}"), # siunitx
              paste0("\\num{", format(error, nsmall = 3), "}"), # siunitx
              # paste0("\\num{", formatC(error, format = "e", digits = 3), "}"), # siunitx
              format = "latex",
              escape = FALSE,
              color = ifelse(error == min(error), "red", "black")
            )
          )
      } %>% 
        bind_rows()
    ) %>% 
    bind_rows() %>% 
    ungroup() %>% 
    pivot_wider(names_from = c(Loss, h), values_from = error)
  # kable------------------------------------------
  colnames(error_list) <- str_remove_all(colnames(error_list), pattern = ".*\\_")
  colnames(error_list)[1:2] <- c("", "")
  align <-
    c(
      c("c", "c|"),
      rep(
        c(
          rep("c", ahead_length - 1),
          "c|"
        ),
        3
      )
    )
  error_list %>% 
    kable(
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      align = align,
      caption = caption,
      label = label
    ) %>% 
    kable_paper(full_width = FALSE, latex_options = c("scale_down", "HOLD_position")) %>% 
    add_header_above(c(
      " " = 2,
      "RMAFE" = ahead_length,
      "RMSFE" = ahead_length,
      "RMASE" = ahead_length
    )) %>%
    row_spec(0, angle = header_angle) %>% 
    collapse_rows(columns = 1, latex_hline = "major")
}
