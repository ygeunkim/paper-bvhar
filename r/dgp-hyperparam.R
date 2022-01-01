if (requireNamespace("tidyverse", quietly = TRUE)) {
  library(tidyverse)
}
if (requireNamespace("bvhar", quietly = TRUE)) {
  library(bvhar)
}
source("R/param-fns.R")
#----------------------------------------------------
dgp1 <- readRDS("data/processed/bvarsim_dgp_wn.rds")
dgp2 <- readRDS("data/processed/bvarsim_dgp_rw.rds")
dgp3 <- readRDS("data/processed/bvharsim_dgp_s.rds")
dgp4 <- readRDS("data/processed/bvharsim_dgp_l.rds")
# List of true hyperparameters----------------------
dgp_true <- list(
  small = list(
    dgp1$small_spec,
    dgp2$small_spec,
    dgp3$small_spec,
    dgp4$small_spec
  ),
  medium = list(
    dgp1$medium_spec,
    dgp2$medium_spec,
    dgp3$medium_spec,
    dgp4$medium_spec
  ),
  large = list(
    dgp1$large_spec,
    dgp2$large_spec,
    dgp3$large_spec,
    dgp4$large_spec
  )
)
# Report-------------------------------------------
report_hyperparam(dgp_true, report_true = TRUE, size = 10, caption = "Hyperparameter Setting", label = "truehyperparam")
