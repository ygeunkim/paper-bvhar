Simulation for Consistency
================
Young Geun Kim
07 Jun, 2022

-   [Fit Models](#fit-models)
    -   [VHAR](#vhar)
        -   [SMALL](#small)
        -   [MEDIUM](#medium)
        -   [LARGE](#large)
    -   [BVHAR-S](#bvhar-s)
        -   [SMALL](#small-1)
        -   [MEDIUM](#medium-1)
        -   [LARGE](#large-1)
    -   [BVHAR-L](#bvhar-l)
        -   [SMALL](#small-2)
        -   [MEDIUM](#medium-2)
        -   [LARGE](#large-2)
-   [Compare](#compare)
    -   [VHAR](#vhar-1)
    -   [BVHAR](#bvhar)
    -   [Result](#result)

``` r
sim_data <- "../data/processed/dgp_bvhar_consistency.rds"
```

``` r
# tidyverse----------------------------
library(tidyverse)
# BVHAR custom package-----------------
library(bvhar)
# Set the number of processor cores----
# cl <- parallel::makeCluster(8, type = "FORK")
# latex table--------------------------
library(knitr)
library(kableExtra)
# set seed for reproducible result-----
set.seed(1)
# width of figure when save------------
fig_width <- 20
```

``` r
# result table-------------------------
source("report-fns.R")
# hyperparameter setting table---------
source("param-fns.R")
# Simulated data-----------------------
sim_consistency <- readRDS(sim_data)
```

# Fit Models

``` r
y_small <- sim_consistency$y_small_list
y_medium <- sim_consistency$y_medium_list
y_large <- sim_consistency$y_large_list
```

## VHAR

### SMALL

``` r
fit_vhar_small <- parallel::mclapply(
  1:3,
  function(id) {
    vhar_lm(
      y = y_small[[id]],
      har = c(5, 22),
      include_mean = FALSE
    )
  },
  mc.cores = 3
)
```

### MEDIUM

``` r
fit_vhar_medium <- parallel::mclapply(
  1:3,
  function(id) {
    vhar_lm(
      y = y_medium[[id]],
      har = c(5, 22),
      include_mean = FALSE
    )
  },
  mc.cores = 3
)
```

### LARGE

``` r
fit_vhar_large <- parallel::mclapply(
  1:3,
  function(id) {
    vhar_lm(
      y = y_large[[id]],
      har = c(5, 22),
      include_mean = FALSE
    )
  },
  mc.cores = 6
)
```

## BVHAR-S

### SMALL

``` r
fit_bvhars_small <- parallel::mclapply(
  1:3,
  function(id) {
    bvhar_minnesota(
      y = y_small[[id]],
      bayes_spec = sim_consistency$small_spec,
      include_mean = FALSE
    )
  },
  mc.cores = 3
)
```

### MEDIUM

``` r
fit_bvhars_medium <- parallel::mclapply(
  1:3,
  function(id) {
    bvhar_minnesota(
      y = y_medium[[id]],
      bayes_spec = sim_consistency$medium_spec,
      include_mean = FALSE
    )
  },
  mc.cores = 3
)
```

### LARGE

``` r
fit_bvhars_large <- parallel::mclapply(
  1:3,
  function(id) {
    bvhar_minnesota(
      y = y_large[[id]],
      bayes_spec = sim_consistency$large_spec,
      include_mean = FALSE
    )
  },
  mc.cores = 6
)
```

## BVHAR-L

``` r
n_small <- ncol(sim_consistency$y_small[[1]])
n_medium <- ncol(sim_consistency$y_medium[[1]])
n_large <- ncol(sim_consistency$y_large[[1]])
```

### SMALL

``` r
bvharl_small_spec <- set_weight_bvhar(
  sigma = sim_consistency$small_spec$sigma,
  lambda = sim_consistency$small_spec$lambda,
  daily = sim_consistency$small_spec$delta,
  weekly = sim_consistency$small_spec$delta,
  monthly = sim_consistency$small_spec$delta
)
```

``` r
fit_bvharl_small <- parallel::mclapply(
  1:3,
  function(id) {
    bvhar_minnesota(
      y = y_small[[id]],
      bayes_spec = bvharl_small_spec,
      include_mean = FALSE
    )
  },
  mc.cores = 3
)
```

### MEDIUM

``` r
bvharl_medium_spec <- set_weight_bvhar(
  sigma = sim_consistency$medium_spec$sigma,
  lambda = sim_consistency$medium_spec$lambda,
  daily = sim_consistency$medium_spec$delta,
  weekly = sim_consistency$medium_spec$delta,
  monthly = sim_consistency$medium_spec$delta
)
```

``` r
fit_bvharl_medium <- parallel::mclapply(
  1:3,
  function(id) {
    bvhar_minnesota(
      y = y_medium[[id]],
      bayes_spec = bvharl_medium_spec,
      include_mean = FALSE
    )
  },
  mc.cores = 3
)
```

### LARGE

``` r
bvharl_large_spec <- set_weight_bvhar(
  sigma = sim_consistency$large_spec$sigma,
  lambda = sim_consistency$large_spec$lambda,
  daily = sim_consistency$large_spec$delta,
  weekly = sim_consistency$large_spec$delta,
  monthly = sim_consistency$large_spec$delta
)
```

``` r
fit_bvharl_large <- parallel::mclapply(
  1:3,
  function(id) {
    bvhar_minnesota(
      y = y_large[[id]],
      bayes_spec = bvharl_large_spec,
      include_mean = FALSE
    )
  },
  mc.cores = 6
)
```

# Compare

## VHAR

``` r
small_vhar_norm <- parallel::pvec(
  1:3,
  function(id) {
    norm(sim_consistency$small_coef$coefficients - fit_vhar_small[[id]]$coef, type = "2") / norm(sim_consistency$small_coef$coefficients, type = "2")
  },
  mc.cores = 3
)
#---------------------------------
medium_vhar_norm <- parallel::pvec(
  1:3,
  function(id) {
    norm(sim_consistency$medium_coef$coefficients - fit_vhar_medium[[id]]$coef, type = "2") / norm(sim_consistency$medium_coef$coefficients, type = "2")
  },
  mc.cores = 3
)
#-----------------------------------
large_vhar_norm <- parallel::pvec(
  1:3,
  function(id) {
    norm(sim_consistency$large_coef$coefficients - fit_vhar_large[[id]]$coef, type = "2") / norm(sim_consistency$large_coef$coefficients, type = "2")
  },
  mc.cores = 3
)
```

## BVHAR

``` r
small_s_norm <- parallel::pvec(
  1:3,
  function(id) {
    norm(sim_consistency$small_coef$coefficients - fit_bvhars_small[[id]]$coef, type = "2") / norm(sim_consistency$small_coef$coefficients, type = "2")
  },
  mc.cores = 3
)
#----------------------------------
small_l_norm <- parallel::pvec(
  1:3,
  function(id) {
    norm(sim_consistency$small_coef$coefficients - fit_bvharl_small[[id]]$coef, type = "2") / norm(sim_consistency$small_coef$coefficients, type = "2")
  },
  mc.cores = 3
)
#----------------------------------
medium_s_norm <- parallel::pvec(
  1:3,
  function(id) {
    norm(sim_consistency$medium_coef$coefficients - fit_bvhars_medium[[id]]$coef, type = "2") / norm(sim_consistency$medium_coef$coefficients, type = "2")
  },
  mc.cores = 3
)
#----------------------------------
medium_l_norm <- parallel::pvec(
  1:3,
  function(id) {
    norm(sim_consistency$medium_coef$coefficients - fit_bvharl_medium[[id]]$coef, type = "2") / norm(sim_consistency$medium_coef$coefficients, type = "2")
  },
  mc.cores = 3
)
#-----------------------------------
large_s_norm <- parallel::pvec(
  1:3,
  function(id) {
    norm(sim_consistency$large_coef$coefficients - fit_bvhars_large[[id]]$coef, type = "2") / norm(sim_consistency$large_coef$coefficients, type = "2")
  },
  mc.cores = 3
)
#----------------------------------
large_l_norm <- parallel::pvec(
  1:3,
  function(id) {
    norm(sim_consistency$large_coef$coefficients - fit_bvharl_large[[id]]$coef, type = "2") / norm(sim_consistency$large_coef$coefficients, type = "2")
  },
  mc.cores = 3
)
```

## Result

``` r
errtibble <- tibble(
  size = gl(n = 3, k = 3, labels = c("SMALL", "MEDIUM", "LARGE")),
  sample_size = c(
    nrow(y_small[[1]]), nrow(y_small[[2]]), nrow(y_small[[3]]),
    nrow(y_medium[[1]]), nrow(y_medium[[2]]), nrow(y_medium[[3]]),
    nrow(y_large[[1]]), nrow(y_large[[2]]), nrow(y_large[[3]])
  ),
  vhar = c(small_vhar_norm, medium_vhar_norm, large_vhar_norm),
  bvhar_s = c(small_s_norm, medium_s_norm, large_s_norm),
  bvhar_l = c(small_s_norm, medium_s_norm, large_s_norm)
)
```

``` r
errtibble %>% 
  mutate_at(
    vars(vhar, bvhar_s, bvhar_l),
    ~cell_spec(
      paste0(
        "\\num{",
        format(., nsmall = 3, scientific = -2) %>% 
          str_remove(pattern = "0(?=\\.)"), # .xxx
        "}"
      ),
      format = "latex",
      align = "c",
      escape = FALSE
    )
  ) %>% 
  mutate(
    sample_size = cell_spec(
      sample_size, 
      format = "latex", 
      escape = FALSE, 
      align = "c|"
    )
  ) %>% 
  kable(
    format = "latex", 
    booktabs = TRUE,
    escape = FALSE,
    align = "c",
    col.names = c("$k$", "$T = n - 22$", "VHAR", "BVHAR-S", "BVHAR-L"),
    caption = "Relative Estimation Error",
    label = "simconsistency"
  ) %>% 
  collapse_rows(1, latex_hline = "major") %>% 
  writeLines()
\begin{table}

\caption{\label{tab:simconsistency}Relative Estimation Error}
\centering
\begin{tabular}[t]{ccccc}
\toprule
$k$ & $T = n - 22$ & VHAR & BVHAR-S & BVHAR-L\\
\midrule
 & \multicolumn{1}{c|}{40} & \multicolumn{1}{c}{\num{ 435.022}} & \multicolumn{1}{c}{\num{.966}} & \multicolumn{1}{c}{\num{.966}}\\

 & \multicolumn{1}{c|}{80} & \multicolumn{1}{c}{\num{  56.599}} & \multicolumn{1}{c}{\num{.879}} & \multicolumn{1}{c}{\num{.879}}\\

\multirow{-3}{*}{\centering\arraybackslash SMALL} & \multicolumn{1}{c|}{120} & \multicolumn{1}{c}{\num{  4.437}} & \multicolumn{1}{c}{\num{.795}} & \multicolumn{1}{c}{\num{.795}}\\
\cmidrule{1-5}
 & \multicolumn{1}{c|}{200} & \multicolumn{1}{c}{\num{ 187.085}} & \multicolumn{1}{c}{\num{.869}} & \multicolumn{1}{c}{\num{.869}}\\

 & \multicolumn{1}{c|}{400} & \multicolumn{1}{c}{\num{  6.274}} & \multicolumn{1}{c}{\num{.852}} & \multicolumn{1}{c}{\num{.852}}\\

\multirow{-3}{*}{\centering\arraybackslash MEDIUM} & \multicolumn{1}{c|}{600} & \multicolumn{1}{c}{\num{  39.176}} & \multicolumn{1}{c}{\num{.892}} & \multicolumn{1}{c}{\num{.892}}\\
\cmidrule{1-5}
 & \multicolumn{1}{c|}{400} & \multicolumn{1}{c}{\num{1986.343}} & \multicolumn{1}{c}{\num{.981}} & \multicolumn{1}{c}{\num{.981}}\\

 & \multicolumn{1}{c|}{800} & \multicolumn{1}{c}{\num{ 782.026}} & \multicolumn{1}{c}{\num{.975}} & \multicolumn{1}{c}{\num{.975}}\\

\multirow{-3}{*}{\centering\arraybackslash LARGE} & \multicolumn{1}{c|}{1200} & \multicolumn{1}{c}{\num{ 475.817}} & \multicolumn{1}{c}{\num{.967}} & \multicolumn{1}{c}{\num{.967}}\\
\bottomrule
\end{tabular}
\end{table}
```
