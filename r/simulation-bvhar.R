if (requireNamespace("tidyverse", quietly = TRUE)) {
  library(tidyverse)
}
if (requireNamespace("bvhar", quietly = TRUE)) {
  library(bvhar)
}
# hyperparameter setting table------------------------
source("R/param-fns.R")
set.seed(1)
