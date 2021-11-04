# Compute loss for each variable and make kable------------------
# returns kable
# use kableExtra
#----------------------------------------------------------------
make_kable <- function(mod_list, y, error = c("mse", "mae", "mape", "mase"), font_size = NULL, format = "latex") {
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
  error_table %>% 
    as.data.frame() %>% 
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
      latex_options = c("striped", "HOLD_position"),
      font_size = font_size
    )
}

# Compute average loss---------------------------------
# returns kable
# use kableExtra
#------------------------------------------------------
kable_lossmean <- function(mod_list, y, format = "latex") {
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
  loss_mean %>% 
    t() %>% 
    as.data.frame() %>% 
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
