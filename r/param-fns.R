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
report_hyperparam <- function(bayes_spec, kable = TRUE, font_size = NULL, format = "latex") {
  mod_process <- bayes_spec$process
  mod_prior <- bayes_spec$prior
  if (bayes_spec$prior == "MN_VHAR") {
    res_dt <- data.frame(
      sigma = bayes_spec$sigma,
      daily = bayes_spec$daily,
      weekly = bayes_spec$weekly,
      monthly = bayes_spec$monthly
    ) %>% 
      t()
  } else if (bayes_spec$prior == "Minnesota" | bayes_spec$prior == "MN_VAR") {
    res_dt <- data.frame(
      sigma = bayes_spec$sigma,
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
    list(
      dataframe = res_dt,
      lambda = bayes_spec$lambda,
      eps = bayes_spec$eps
    )
  } else {
    res_dt %>% 
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
}


