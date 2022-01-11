Simulating Minnesota VAR
================
Young Geun Kim
11 Jan, 2022

-   [BVAR Coefficient](#bvar-coefficient)
    -   [Minnesota prior](#minnesota-prior)
    -   [VAR(5)](#var5)
        -   [SMALL](#small)
        -   [MEDIUM](#medium)
        -   [LARGE](#large)
-   [Modeling](#modeling)
    -   [VAR](#var)
    -   [VHAR](#vhar)
    -   [BVAR](#bvar)
    -   [BVHAR-VAR](#bvhar-var)
    -   [BVHAR-VHAR](#bvhar-vhar)
    -   [Hyperparameters](#hyperparameters)
-   [Errors](#errors)
    -   [Rolling Windows](#rolling-windows)
    -   [Relative Errors](#relative-errors)
    -   [Piceswise Errors](#piceswise-errors)
        -   [SMALL](#small-1)
        -   [Tables](#tables)
        -   [MEDIUM](#medium-1)
        -   [LARGE](#large-1)
        -   [Average](#average)
-   [Additional](#additional)
-   [Coefficients](#coefficients)

``` r
sim_data <- "../data/processed/bvarsim_dgp_wn.rds"
```

``` r
# tidyverse----------------------------
library(tidyverse)
# BVHAR custom package-----------------
library(bvhar)
# Set the number of processor cores----
cl <- parallel::makeCluster(8, type = "FORK")
# set seed for reproducible result-----
set.seed(1)
```

``` r
# result table-------------------------
source("report-fns.R")
# hyperparameter setting table---------
source("param-fns.R")
# Simulation---------------------------
dgp <- readRDS(sim_data)
```

# BVAR Coefficient

## Minnesota prior

``` r
n_small <- length(bvar_small_spec$sigma)
n_medium <- length(bvar_medium_spec$sigma)
n_large <- length(bvar_large_spec$sigma)
```

## VAR(5)

### SMALL

``` r
y_small_train <- dgp$y_small_train
y_small_test <- dgp$y_small_test
```

``` r
y_small_train %>% 
  mutate(train = TRUE) %>% 
  bind_rows(y_small_test %>% mutate(train = FALSE)) %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(-c(id, train), names_to = "asset", values_to = "value") %>% 
  ggplot(aes(x = id, y = value)) +
  annotate(
    geom = "rect",
    xmin = nrow(y_small_train),
    xmax = nrow(y_small_train) + nrow(y_small_test),
    ymin = -Inf,
    ymax = Inf,
    alpha = .7,
    fill = "grey" # test set
  ) +
  # geom_path(aes(colour = asset), show.legend = FALSE, alpha = .5) +
  geom_path() +
  facet_grid(asset ~ ., scales = "free_y") +
  scale_x_continuous(
    breaks = c(nrow(y_small_train), nrow(y_small_train) + nrow(y_small_test))
  ) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA)) +
  labs(
    x = element_blank(),
    y = element_blank()
  )
```

<img src="../output/figs/DGP-1-smallplot-1.png" width="70%" style="display: block; margin: auto;" />

### MEDIUM

``` r
y_medium_train <- dgp$y_medium_train
y_medium_test <- dgp$y_medium_test
```

``` r
y_medium_train %>% 
  mutate(train = TRUE) %>% 
  bind_rows(y_medium_test %>% mutate(train = FALSE)) %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(-c(id, train), names_to = "asset", values_to = "value") %>% 
  ggplot(aes(x = id, y = value)) +
  annotate(
    geom = "rect",
    xmin = nrow(y_medium_train),
    xmax = nrow(y_medium_train) + nrow(y_medium_test),
    ymin = -Inf,
    ymax = Inf,
    alpha = .7,
    fill = "grey" # test set
  ) +
  geom_path() +
  facet_grid(asset ~ ., scales = "free_y") +
  scale_x_continuous(
    breaks = c(nrow(y_medium_train), nrow(y_medium_train) + nrow(y_medium_test))
  ) +
  theme_minimal() +
  theme(
    strip.text.y = element_text(size = 5), 
    panel.border = element_rect(fill = NA)
  ) +
  labs(
    x = element_blank(),
    y = element_blank()
  )
```

<img src="../output/figs/DGP-1-medplot-1.png" width="70%" style="display: block; margin: auto;" />

### LARGE

``` r
y_large_train <- dgp$y_large_train
y_large_test <- dgp$y_large_test
```

``` r
y_large_train %>% 
  mutate(train = TRUE) %>% 
  bind_rows(y_large_test %>% mutate(train = FALSE)) %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(-c(id, train), names_to = "asset", values_to = "value") %>% 
  ggplot(aes(x = id, y = value)) +
  annotate(
    geom = "rect",
    xmin = nrow(y_large_train),
    xmax = nrow(y_large_train) + nrow(y_large_test),
    ymin = -Inf,
    ymax = Inf,
    alpha = .7,
    fill = "grey" # test set
  ) +
  geom_path(size = .3) +
  facet_grid(asset ~ ., scales = "free_y") +
  scale_x_continuous(
    breaks = c(nrow(y_large_train), nrow(y_large_train) + nrow(y_large_test))
  ) +
  theme_minimal() +
  theme(
    strip.text.y = element_text(size = 5), 
    panel.border = element_rect(fill = NA),
    axis.text.y = element_text(size = 3)
  ) +
  labs(
    x = element_blank(),
    y = element_blank()
  )
```

<img src="../output/figs/DGP-1-largeplot-1.png" width="70%" style="display: block; margin: auto;" />

# Modeling

## VAR

``` r
(var_lag <- 5)
#> [1] 5
```

``` r
fit_var_small <- var_lm(y_small_train, var_lag, include_mean = FALSE)
fit_var_medium <- var_lm(y_medium_train, var_lag, include_mean = FALSE)
fit_var_large <- var_lm(y_large_train, var_lag, include_mean = FALSE)
```

## VHAR

``` r
fit_vhar_small <- vhar_lm(y_small_train, include_mean = FALSE)
fit_vhar_medium <- vhar_lm(y_medium_train, include_mean = FALSE)
fit_vhar_large <- vhar_lm(y_large_train, include_mean = FALSE)
```

## BVAR

``` r
(bvar_lag <- 5)
#> [1] 5
```

``` r
(bvar_small_optim <- choose_bvar(
  bvar_small_spec, 
  lower = c(
    rep(.5, n_small), # sigma
    1e-4, # lambda
    rep(0, n_small) # delta
  ), 
  upper = c(
    rep(2, n_small), # sigma
    Inf, # lambda
    rep(1, n_small) # delta
  ), 
  y = y_small_train, 
  p = bvar_lag, 
  include_mean = FALSE,
  parallel = list(cl = cl, forward = FALSE, loginfo = FALSE)
))
#> Model Specification for BVAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: Minnesota
#> # Type '?bvar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#> [1]  1.64  1.67  2.00
#> 
#> Setting for 'lambda':
#> [1]  0.136
#> 
#> Setting for 'delta':
#> [1]  0.0000  0.0301  0.0459
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
(bvar_medium_optim <- choose_bvar(
  bvar_medium_spec, 
  lower = c(
    rep(.5, n_medium), # sigma
    1e-4, # lambda
    rep(0, n_medium) # delta
  ), 
  upper = c(
    rep(2, n_medium), # sigma
    Inf, # lambda
    rep(1, n_medium) # delta
  ), 
  y = y_medium_train, 
  p = bvar_lag, 
  include_mean = FALSE,
  parallel = list(cl = cl, forward = FALSE, loginfo = FALSE)
))
#> Model Specification for BVAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: Minnesota
#> # Type '?bvar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#> [1]  1.35  2.00  2.00  2.00  2.00  2.00  1.20  2.00  2.00
#> 
#> Setting for 'lambda':
#> [1]  0.0731
#> 
#> Setting for 'delta':
#> [1]  0.0000  0.0234  0.0196  0.0082  0.0000  0.0000  0.1974  0.0000  0.0000
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
(bvar_large_optim <- choose_bvar(
  bvar_large_spec, 
  lower = c(
    rep(.5, n_large), # sigma
    1e-4, # lambda
    rep(0, n_large) # delta
  ), 
  upper = c(
    rep(2, n_large), # sigma
    Inf, # lambda
    rep(1, n_large) # delta
  ), 
  y = y_large_train, 
  p = bvar_lag, 
  include_mean = FALSE,
  parallel = list(cl = cl, forward = FALSE, loginfo = FALSE)
))
#> Model Specification for BVAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: Minnesota
#> # Type '?bvar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#>  [1]  2  2  2  2  2  2  2  2  2  2  2  2
#> 
#> Setting for 'lambda':
#> [1]  0.0078
#> 
#> Setting for 'delta':
#>  [1]  0.0000  0.0000  0.0000  0.0210  0.0048  0.0106  0.0000  0.0000  0.0000
#> [10]  0.0000  0.0000  0.0000
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
fit_small_bvar <- bvar_small_optim$fit
fit_medium_bvar <- bvar_medium_optim$fit
fit_large_bvar <- bvar_large_optim$fit
```

## BVHAR-VAR

``` r
bvhar_var_small_spec <- set_bvhar(
  sigma = bvar_small_spec$sigma,
  lambda = bvar_small_spec$lambda,
  delta = bvar_small_spec$delta
)
#----------------------------
bvhar_var_medium_spec <- set_bvhar(
  sigma = bvar_medium_spec$sigma,
  lambda = bvar_medium_spec$lambda,
  delta = bvar_medium_spec$delta
)
#----------------------------
bvhar_var_large_spec <- set_bvhar(
  sigma = bvar_large_spec$sigma,
  lambda = bvar_large_spec$lambda,
  delta = bvar_large_spec$delta
)
```

``` r
(bvhar_var_small_optim <- choose_bvhar(
  bvhar_var_small_spec, 
  lower = c(
    rep(.5, n_small), # sigma
    1e-4, # lambda
    rep(0, n_small) # delta
  ), 
  upper = c(
    rep(2, n_small), # sigma
    Inf, # lambda
    rep(1, n_small) # delta
  ), 
  y = y_small_train, 
  include_mean = FALSE,
  parallel = list(cl = cl, forward = FALSE, loginfo = FALSE)
))
#> Model Specification for BVHAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: MN_VAR
#> # Type '?bvhar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#> [1]  2.00  1.52  2.00
#> 
#> Setting for 'lambda':
#> [1]  0.115
#> 
#> Setting for 'delta':
#> [1]  0.0000  0.0383  0.0343
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
(bvhar_var_medium_optim <- choose_bvhar(
  bvhar_var_medium_spec, 
  lower = c(
    rep(.5, n_medium), # sigma
    1e-4, # lambda
    rep(0, n_medium) # delta
  ), 
  upper = c(
    rep(2, n_medium), # sigma
    Inf, # lambda
    rep(1, n_medium) # delta
  ), 
  y = y_medium_train, 
  include_mean = FALSE,
  parallel = list(cl = cl, forward = FALSE, loginfo = FALSE)
))
#> Model Specification for BVHAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: MN_VAR
#> # Type '?bvhar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#> [1]  1.72  2.00  2.00  2.00  2.00  2.00  1.36  2.00  2.00
#> 
#> Setting for 'lambda':
#> [1]  0.075
#> 
#> Setting for 'delta':
#> [1]  0.00000  0.03448  0.01650  0.00450  0.00000  0.00193  0.19707  0.00000
#> [9]  0.00000
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
(bvhar_var_large_optim <- choose_bvhar(
  bvhar_var_large_spec, 
  lower = c(
    rep(.5, n_large), # sigma
    1e-4, # lambda
    rep(0, n_large) # delta
  ), 
  upper = c(
    rep(2, n_large), # sigma
    Inf, # lambda
    rep(1, n_large) # delta
  ), 
  y = y_large_train, 
  include_mean = FALSE,
  parallel = list(cl = cl, forward = FALSE, loginfo = FALSE)
))
#> Model Specification for BVHAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: MN_VAR
#> # Type '?bvhar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#>  [1]  2  2  2  2  2  2  2  2  2  2  2  2
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'delta':
#>  [1]  0.00000  0.00000  0.00000  0.01767  0.01154  0.00517  0.00000  0.00347
#>  [9]  0.00000  0.00000  0.00000  0.00000
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
fit_bvhar_small_var <- bvhar_var_small_optim$fit
fit_bvhar_medium_var <- bvhar_var_medium_optim$fit
fit_bvhar_large_var <- bvhar_var_large_optim$fit
```

## BVHAR-VHAR

``` r
bvhar_vhar_small_spec <- set_weight_bvhar(
  sigma = bvar_small_spec$sigma,
  lambda = bvar_small_spec$lambda,
  daily = bvar_small_spec$delta,
  weekly = bvar_small_spec$delta,
  monthly = bvar_small_spec$delta
)
#-----------------------------------------
bvhar_vhar_medium_spec <- set_weight_bvhar(
  sigma = bvar_medium_spec$sigma,
  lambda = bvar_medium_spec$lambda,
  daily = bvar_medium_spec$delta,
  weekly = bvar_medium_spec$delta,
  monthly = bvar_medium_spec$delta
)
#-----------------------------------------
bvhar_vhar_large_spec <- set_weight_bvhar(
  sigma = bvar_large_spec$sigma,
  lambda = bvar_large_spec$lambda,
  daily = bvar_large_spec$delta,
  weekly = bvar_large_spec$delta,
  monthly = bvar_large_spec$delta
)
```

``` r
(bvhar_vhar_small_optim <- choose_bvhar(
  bvhar_vhar_small_spec, 
  lower = c(
    rep(.5, n_small), # sigma
    1e-4, # lambda
    rep(0, n_small), # daily
    rep(0, n_small), # weekly
    rep(0, n_small) # monthly
  ), 
  upper = c(
    rep(2, n_small), # sigma
    Inf, # lambda
    rep(1, n_small), # daily
    rep(1, n_small), # weekly
    rep(1, n_small) # monthly
  ), 
  y = y_small_train, 
  include_mean = FALSE,
  parallel = list(cl = cl, forward = FALSE, loginfo = FALSE)
))
#> Model Specification for BVHAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: MN_VHAR
#> # Type '?bvhar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#> [1]  2.00  1.51  2.00
#> 
#> Setting for 'lambda':
#> [1]  0.115
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.0000  0.0383  0.0308
#> 
#> Setting for 'weekly':
#> [1]  0  0  0
#> 
#> Setting for 'monthly':
#> [1]  0.000  0.000  0.262
```

``` r
(bvhar_vhar_medium_optim <- choose_bvhar(
  bvhar_vhar_medium_spec, 
  lower = c(
    rep(.5, n_medium), # sigma
    1e-4, # lambda
    rep(0, n_medium), # daily
    rep(0, n_medium), # weekly
    rep(0, n_medium) # monthly
  ), 
  upper = c(
    rep(2, n_medium), # sigma
    Inf, # lambda
    rep(1, n_medium), # daily
    rep(1, n_medium), # weekly
    rep(1, n_medium) # monthly
  ), 
  y = y_medium_train, 
  include_mean = FALSE,
  parallel = list(cl = cl, forward = FALSE, loginfo = FALSE)
))
#> Model Specification for BVHAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: MN_VHAR
#> # Type '?bvhar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#> [1]  1.68  2.00  2.00  2.00  2.00  2.00  1.37  2.00  2.00
#> 
#> Setting for 'lambda':
#> [1]  0.0752
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.00000  0.02019  0.01620  0.00441  0.00000  0.00166  0.18735  0.00000
#> [9]  0.00000
#> 
#> Setting for 'weekly':
#> [1]  0.3199  0.1378  0.0000  0.0000  0.0000  0.0000  0.0807  0.0722  0.0000
#> 
#> Setting for 'monthly':
#> [1]  0.00000  0.00000  0.00314  0.00000  0.01818  0.00917  0.00000  0.00000
#> [9]  0.07045
```

``` r
(bvhar_vhar_large_optim <- choose_bvhar(
  bvhar_vhar_large_spec, 
  lower = c(
    rep(.5, n_large), # sigma
    1e-4, # lambda
    rep(0, n_large), # daily
    rep(0, n_large), # weekly
    rep(0, n_large) # monthly
  ), 
  upper = c(
    rep(2, n_large), # sigma
    Inf, # lambda
    rep(1, n_large), # daily
    rep(1, n_large), # weekly
    rep(1, n_large) # monthly
  ), 
  y = y_large_train, 
  include_mean = FALSE,
  parallel = list(cl = cl, forward = FALSE, loginfo = FALSE)
))
#> Model Specification for BVHAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: MN_VHAR
#> # Type '?bvhar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#>  [1]  2  2  2  2  2  2  2  2  2  2  2  2
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#>  [1]  0.00000  0.00000  0.00000  0.01314  0.01158  0.00517  0.00000  0.00347
#>  [9]  0.00000  0.00000  0.00000  0.00000
#> 
#> Setting for 'weekly':
#>  [1]  0.0000  0.0000  0.0536  0.0433  0.0000  0.0000  0.0000  0.0000  0.0000
#> [10]  0.1264  0.0000  0.0000
#> 
#> Setting for 'monthly':
#>  [1]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00641  0.00000
#>  [9]  0.00000  0.00000  0.00000  0.00000
```

``` r
fit_bvhar_small_vhar <- bvhar_vhar_small_optim$fit
fit_bvhar_medium_vhar <- bvhar_vhar_medium_optim$fit
fit_bvhar_large_vhar <- bvhar_vhar_large_optim$fit
```

``` r
parallel::stopCluster(cl)
```

## Hyperparameters


    \begin{longtable}[t]{lllrrrrrrrrrrrr}
    \caption{\label{tab:empdgp1}Empirical Bayes Results for DGP1.}\\
    \toprule
     &    &     & y1 & y2 & y3 & y4 & y5 & y6 & y7 & y8 & y9 & y10 & y11 & y12\\
    \midrule
    \endfirsthead
    \caption[]{Empirical Bayes Results for DGP1. \textit{(continued)}}\\
    \toprule
      &    &     & y1 & y2 & y3 & y4 & y5 & y6 & y7 & y8 & y9 & y10 & y11 & y12\\
    \midrule
    \endhead

    \endfoot
    \bottomrule
    \endlastfoot
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{SMALL}}\\
    \hspace{1em} & BVAR & $\sigma$ & 1.644 & 1.670 & 2.000 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.136 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.000 & 0.030 & 0.046 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 2.000 & 1.522 & 2.000 &  &  &  &  &  &  &  &  & \\

    \hspace{1em}\hspace{1em} &  & $\lambda$ & 0.115 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.000 & 0.038 & 0.034 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 2.000 & 1.510 & 2.000 &  &  &  &  &  &  &  &  & \\

     &  & $\lambda$ & 0.115 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.000 & 0.038 & 0.031 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.000 & 0.000 & 0.000 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.000 & 0.000 & 0.262 &  &  &  &  &  &  &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{MEDIUM}}\\
    \hspace{1em} & BVAR & $\sigma$ & 1.354 & 2.000 & 2.000 & 2.000 & 2.000 & 2.000 & 1.204 & 2.000 & 2.00 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.073 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.000 & 0.023 & 0.020 & 0.008 & 0.000 & 0.000 & 0.197 & 0.000 & 0.00 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 1.725 & 2.000 & 2.000 & 2.000 & 2.000 & 2.000 & 1.361 & 2.000 & 2.00 &  &  & \\

    \hspace{1em}\hspace{1em} &  & $\lambda$ & 0.075 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.000 & 0.034 & 0.016 & 0.005 & 0.000 & 0.002 & 0.197 & 0.000 & 0.00 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 1.678 & 2.000 & 2.000 & 2.000 & 2.000 & 2.000 & 1.367 & 2.000 & 2.00 &  &  & \\

     &  & $\lambda$ & 0.075 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.000 & 0.020 & 0.016 & 0.004 & 0.000 & 0.002 & 0.187 & 0.000 & 0.00 &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.320 & 0.138 & 0.000 & 0.000 & 0.000 & 0.000 & 0.081 & 0.072 & 0.00 &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.000 & 0.000 & 0.003 & 0.000 & 0.018 & 0.009 & 0.000 & 0.000 & 0.07 &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{LARGE}}\\
    \hspace{1em} & BVAR & $\sigma$ & 2.000 & 2.000 & 2.000 & 2.000 & 2.000 & 2.000 & 2.000 & 2.000 & 2.00 & 2.000 & 2 & 2\\

    \hspace{1em} &  & $\lambda$ & 0.008 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.000 & 0.000 & 0.000 & 0.021 & 0.005 & 0.011 & 0.000 & 0.000 & 0.00 & 0.000 & 0 & 0\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 2.000 & 2.000 & 2.000 & 2.000 & 2.000 & 2.000 & 2.000 & 2.000 & 2.00 & 2.000 & 2 & 2\\

    \hspace{1em}\hspace{1em} &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.000 & 0.000 & 0.000 & 0.018 & 0.012 & 0.005 & 0.000 & 0.003 & 0.00 & 0.000 & 0 & 0\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 2.000 & 2.000 & 2.000 & 2.000 & 2.000 & 2.000 & 2.000 & 2.000 & 2.00 & 2.000 & 2 & 2\\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.000 & 0.000 & 0.000 & 0.013 & 0.012 & 0.005 & 0.000 & 0.003 & 0.00 & 0.000 & 0 & 0\\

    \hspace{1em} &  & $w_i$ & 0.000 & 0.000 & 0.054 & 0.043 & 0.000 & 0.000 & 0.000 & 0.000 & 0.00 & 0.126 & 0 & 0\\

    \hspace{1em} &  & $m_i$ & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.006 & 0.000 & 0.00 & 0.000 & 0 & 0\\*
    \end{longtable}

# Errors

## Rolling Windows

## Relative Errors

Set VAR as the benchmark model.

    \begin{table}[H]

    \caption{\label{tab:dgp1result}Out-of-sample forecasting performance measures for DGP1 with VAR(5) model as benchmark.}
    \centering
    \resizebox{\linewidth}{!}{
    \begin{tabular}[t]{cc|cccc|cccc|cccc|}
    \toprule
    \multicolumn{2}{c}{ } & \multicolumn{4}{c}{RMAFE} & \multicolumn{4}{c}{RMSFE} & \multicolumn{4}{c}{RMASE} \\
    \cmidrule(l{3pt}r{3pt}){3-6} \cmidrule(l{3pt}r{3pt}){7-10} \cmidrule(l{3pt}r{3pt}){11-14}
    \rotatebox{0}{} & \rotatebox{0}{} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 10$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 10$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 10$} & \rotatebox{0}{$h = 20$}\\
    \midrule
     & VHAR & \textcolor{black}{\num{.998}} & \textcolor{black}{\num{1.006}} & \textcolor{black}{\num{1.010}} & \textcolor{black}{\num{1.004}} & \textcolor{black}{\num{1.011}} & \textcolor{black}{\num{1.011}} & \textcolor{black}{\num{1.005}} & \textcolor{black}{\num{1.005}} & \textcolor{black}{\num{.998}} & \textcolor{black}{\num{1.006}} & \textcolor{black}{\num{1.010}} & \textcolor{black}{\num{1.004}}\\

     & BVAR & \textcolor{red}{\num{.986}} & \textcolor{black}{\num{.993}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{.987}} & \textcolor{red}{\num{.995}} & \textcolor{black}{\num{.999}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{.986}} & \textcolor{black}{\num{.993}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{1.000}}\\

     & BVHAR-S & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{.992}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{.996}} & \textcolor{red}{\num{.999}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{.992}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}}\\

    \multirow{-4}{*}{\centering\arraybackslash SMALL} & BVHAR-L & \textcolor{black}{\num{.988}} & \textcolor{red}{\num{.992}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.998}} & \textcolor{black}{\num{.996}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.988}} & \textcolor{red}{\num{.991}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}}\\
    \cmidrule{1-14}
     & VHAR & \textcolor{black}{\num{.998}} & \textcolor{black}{\num{.998}} & \textcolor{black}{\num{1.009}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{.995}} & \textcolor{black}{\num{1.013}} & \textcolor{red}{\num{.998}} & \textcolor{black}{\num{.998}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.008}} & \textcolor{black}{\num{1.002}}\\

     & BVAR & \textcolor{red}{\num{.984}} & \textcolor{black}{\num{.990}} & \textcolor{red}{\num{1.000}} & \textcolor{black}{\num{1.000}} & \textcolor{red}{\num{.971}} & \textcolor{black}{\num{.985}} & \textcolor{red}{\num{1.000}} & \textcolor{black}{\num{1.000}} & \textcolor{red}{\num{.987}} & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}}\\

     & BVHAR-S & \textcolor{black}{\num{.985}} & \textcolor{red}{\num{.990}} & \textcolor{black}{\num{1.000}} & \textcolor{red}{\num{1.000}} & \textcolor{black}{\num{.973}} & \textcolor{black}{\num{.984}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.988}} & \textcolor{red}{\num{.989}} & \textcolor{black}{\num{1.000}} & \textcolor{red}{\num{1.000}}\\

    \multirow{-4}{*}{\centering\arraybackslash MEDIUM} & BVHAR-L & \textcolor{black}{\num{.986}} & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.974}} & \textcolor{red}{\num{.984}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{.990}} & \textcolor{red}{\num{1.000}} & \textcolor{black}{\num{1.000}}\\
    \cmidrule{1-14}
     & VHAR & \textcolor{black}{\num{.987}} & \textcolor{black}{\num{1.001}} & \textcolor{black}{\num{1.004}} & \textcolor{black}{\num{1.008}} & \textcolor{black}{\num{.984}} & \textcolor{black}{\num{1.011}} & \textcolor{black}{\num{1.020}} & \textcolor{black}{\num{1.013}} & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{1.001}} & \textcolor{black}{\num{1.004}} & \textcolor{black}{\num{1.007}}\\

     & BVAR & \textcolor{black}{\num{.971}} & \textcolor{black}{\num{.996}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.939}} & \textcolor{black}{\num{.983}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.972}} & \textcolor{black}{\num{.995}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}}\\

     & BVHAR-S & \textcolor{black}{\num{.970}} & \textcolor{red}{\num{.996}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.939}} & \textcolor{red}{\num{.983}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.971}} & \textcolor{red}{\num{.995}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}}\\

    \multirow{-4}{*}{\centering\arraybackslash LARGE} & BVHAR-L & \textcolor{red}{\num{.970}} & \textcolor{black}{\num{.996}} & \textcolor{red}{\num{.999}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{.939}} & \textcolor{black}{\num{.983}} & \textcolor{red}{\num{.999}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{.971}} & \textcolor{black}{\num{.995}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{1.000}}\\
    \bottomrule
    \end{tabular}}
    \end{table}

## Piceswise Errors

### SMALL

Plots

<img src="../output/figs/DGP-1-smallcvonefig-1.png" width="70%" style="display: block; margin: auto;" />

<img src="../output/figs/DGP-1-smallcvfivefig-1.png" width="70%" style="display: block; margin: auto;" />

<img src="../output/figs/DGP-1-smallcvtwentyfig-1.png" width="70%" style="display: block; margin: auto;" />

### Tables

1-step:


    \begin{longtable}[t]{lllllll}
    \caption{\label{tab:smallone}SMALL Simulation - 1-step ahead Rolling Window Forecasting Loss}\\
    \toprule
    \multicolumn{1}{c}{ } & \multicolumn{1}{c}{ } & \multicolumn{2}{c}{Frequentist} & \multicolumn{1}{c}{BVAR} & \multicolumn{2}{c}{BVHAR} \\
    \cmidrule(l{3pt}r{3pt}){3-4} \cmidrule(l{3pt}r{3pt}){5-5} \cmidrule(l{3pt}r{3pt}){6-7}
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endfirsthead
    \caption[]{SMALL Simulation - 1-step ahead Rolling Window Forecasting Loss \textit{(continued)}}\\
    \toprule
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endhead

    \endfoot
    \bottomrule
    \endlastfoot
     & asset01 & \num{0.821} & \num{0.792} & \num{0.8} & \num{0.783} & \textcolor{red}{\num{0.783}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.922} & \num{0.963} & \textcolor{red}{\num{0.904}} & \num{0.929} & \num{0.929}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1.231}} & \num{1.251} & \num{1.232} & \num{1.255} & \num{1.257}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.991} & \num{1.002} & \textcolor{red}{\num{0.978}} & \num{0.989} & \num{0.99}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.721} & \num{0.708} & \num{0.712} & \textcolor{red}{\num{0.704}} & \num{0.704}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.741} & \num{0.755} & \textcolor{red}{\num{0.727}} & \num{0.737} & \num{0.737}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.908} & \num{0.902} & \textcolor{red}{\num{0.899}} & \num{0.901} & \num{0.901}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.79} & \num{0.788} & \textcolor{red}{\num{0.779}} & \num{0.781} & \num{0.781}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{139.841} & \num{141.485} & \num{134.783} & \textcolor{red}{\num{132.052}} & \num{132.094}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{129.024} & \num{118.302} & \num{108.535} & \num{100.519} & \textcolor{red}{\num{100.509}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{124.271} & \num{150.371} & \textcolor{red}{\num{116.551}} & \num{122.477} & \num{122.327}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{131.045} & \num{136.719} & \num{119.956} & \num{118.35} & \textcolor{red}{\num{118.31}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{58.786} & \num{57.743} & \num{58.072} & \textcolor{red}{\num{57.408}} & \num{57.409}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{60.27} & \num{61.413} & \textcolor{red}{\num{59.157}} & \num{60.028} & \num{60.028}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{73.688} & \num{73.171} & \textcolor{red}{\num{72.88}} & \num{73.082} & \num{73.043}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{64.248} & \num{64.109} & \textcolor{red}{\num{63.37}} & \num{63.506} & \num{63.493}\\*
    \end{longtable}

5-step:


    \begin{longtable}[t]{lllllll}
    \caption{\label{tab:smallfive}SMALL Simulation - 5-step ahead Rolling Window Forecasting Loss}\\
    \toprule
    \multicolumn{1}{c}{ } & \multicolumn{1}{c}{ } & \multicolumn{2}{c}{Frequentist} & \multicolumn{1}{c}{BVAR} & \multicolumn{2}{c}{BVHAR} \\
    \cmidrule(l{3pt}r{3pt}){3-4} \cmidrule(l{3pt}r{3pt}){5-5} \cmidrule(l{3pt}r{3pt}){6-7}
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endfirsthead
    \caption[]{SMALL Simulation - 5-step ahead Rolling Window Forecasting Loss \textit{(continued)}}\\
    \toprule
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endhead

    \endfoot
    \bottomrule
    \endlastfoot
     & asset01 & \textcolor{red}{\num{0.783}} & \num{0.793} & \num{0.789} & \num{0.791} & \num{0.79}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.998} & \num{1.018} & \num{0.979} & \textcolor{red}{\num{0.977}} & \num{0.977}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.275} & \num{1.278} & \textcolor{red}{\num{1.274}} & \num{1.277} & \num{1.277}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.019} & \num{1.03} & \textcolor{red}{\num{1.014}} & \num{1.015} & \num{1.015}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{0.692}} & \num{0.701} & \num{0.694} & \num{0.696} & \num{0.696}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.772} & \num{0.781} & \num{0.762} & \textcolor{red}{\num{0.76}} & \num{0.761}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.908} & \num{0.904} & \num{0.9} & \num{0.897} & \textcolor{red}{\num{0.896}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.791} & \num{0.796} & \num{0.785} & \num{0.784} & \textcolor{red}{\num{0.784}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{97.959}} & \num{105.174} & \num{98.336} & \num{100.167} & \num{100.009}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{104.154} & \num{116.482} & \textcolor{red}{\num{100.811}} & \num{100.963} & \num{100.94}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{117.4} & \num{126.863} & \num{101.997} & \num{100.186} & \textcolor{red}{\num{99.082}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{106.504} & \num{116.173} & \num{100.382} & \num{100.438} & \textcolor{red}{\num{100.01}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{55.575}} & \num{56.372} & \num{55.806} & \num{55.956} & \num{55.935}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{62.47} & \num{63.193} & \num{61.605} & \textcolor{red}{\num{61.488}} & \num{61.495}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{73.796} & \num{73.42} & \num{73.084} & \num{72.861} & \textcolor{red}{\num{72.773}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{63.947} & \num{64.329} & \num{63.498} & \num{63.435} & \textcolor{red}{\num{63.401}}\\*
    \end{longtable}

20-step:


    \begin{longtable}[t]{lllllll}
    \caption{\label{tab:smalltwenty}SMALL Simulation - 20-step ahead Rolling Window Forecasting Loss}\\
    \toprule
    \multicolumn{1}{c}{ } & \multicolumn{1}{c}{ } & \multicolumn{2}{c}{Frequentist} & \multicolumn{1}{c}{BVAR} & \multicolumn{2}{c}{BVHAR} \\
    \cmidrule(l{3pt}r{3pt}){3-4} \cmidrule(l{3pt}r{3pt}){5-5} \cmidrule(l{3pt}r{3pt}){6-7}
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endfirsthead
    \caption[]{SMALL Simulation - 20-step ahead Rolling Window Forecasting Loss \textit{(continued)}}\\
    \toprule
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endhead

    \endfoot
    \bottomrule
    \endlastfoot
     & asset01 & \num{0.797} & \num{0.798} & \num{0.796} & \num{0.796} & \textcolor{red}{\num{0.796}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.994}} & \num{1.001} & \num{0.994} & \num{0.995} & \num{0.995}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.202} & \num{1.21} & \num{1.199} & \textcolor{red}{\num{1.199}} & \num{1.203}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.997} & \num{1.003} & \num{0.997} & \textcolor{red}{\num{0.997}} & \num{0.998}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.703} & \num{0.704} & \num{0.703} & \num{0.703} & \textcolor{red}{\num{0.703}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.772}} & \num{0.778} & \num{0.772} & \num{0.773} & \num{0.773}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.871} & \num{0.887} & \textcolor{red}{\num{0.871}} & \num{0.871} & \num{0.871}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.782} & \num{0.79} & \textcolor{red}{\num{0.782}} & \num{0.782} & \num{0.782}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{99.476}} & \num{101.902} & \num{99.954} & \num{100.002} & \num{99.948}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{100.085} & \num{108.542} & \textcolor{red}{\num{100.003}} & \num{100.158} & \num{100.199}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{99.713}} & \num{128.014} & \num{99.976} & \num{100.274} & \num{99.824}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{99.758}} & \num{112.819} & \num{99.977} & \num{100.144} & \num{99.99}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{57.377} & \num{57.445} & \num{57.367} & \num{57.366} & \textcolor{red}{\num{57.355}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{62.77}} & \num{63.316} & \num{62.802} & \num{62.816} & \num{62.822}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{70.711} & \num{72.011} & \textcolor{red}{\num{70.684}} & \num{70.697} & \num{70.766}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{63.619} & \num{64.257} & \textcolor{red}{\num{63.617}} & \num{63.626} & \num{63.648}\\*
    \end{longtable}

### MEDIUM

Plots

``` r
cv_medium_list[[1]] %>% 
  gg_loss(
    y_medium_test, 
    mean_line = TRUE, 
    line_param = list(size = .3), 
    mean_param = list(alpha = .5, size = .3), 
    viridis = TRUE, 
    show.legend = FALSE
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -30, vjust = -1),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(.3, "cm")
  )
```

<img src="../output/figs/DGP-1-medcvonefig-1.png" width="70%" style="display: block; margin: auto;" />

``` r
cv_medium_list[[2]] %>% 
  gg_loss(
    y_medium_test, 
    mean_line = TRUE, 
    line_param = list(size = .3), 
    mean_param = list(alpha = .5, size = .3), 
    viridis = TRUE, 
    show.legend = FALSE
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -30, vjust = -1),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(.3, "cm")
  )
```

<img src="../output/figs/DGP-1-medcvfivefig-1.png" width="70%" style="display: block; margin: auto;" />

``` r
cv_medium_list[[3]] %>% 
  gg_loss(
    y_medium_test, 
    mean_line = TRUE, 
    line_param = list(size = .3), 
    mean_param = list(alpha = .5, size = .3), 
    viridis = TRUE, 
    show.legend = FALSE
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -30, vjust = -1),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(.3, "cm")
  )
```

<img src="../output/figs/DGP-1-medcvtwentyfig-1.png" width="70%" style="display: block; margin: auto;" />

Tables

1-step:


    \begin{longtable}[t]{lllllll}
    \caption{\label{tab:medone}MEDIUM Simulation - 1-step ahead Rolling Window Forecasting Loss}\\
    \toprule
    \multicolumn{1}{c}{ } & \multicolumn{1}{c}{ } & \multicolumn{2}{c}{Frequentist} & \multicolumn{1}{c}{BVAR} & \multicolumn{2}{c}{BVHAR} \\
    \cmidrule(l{3pt}r{3pt}){3-4} \cmidrule(l{3pt}r{3pt}){5-5} \cmidrule(l{3pt}r{3pt}){6-7}
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endfirsthead
    \caption[]{MEDIUM Simulation - 1-step ahead Rolling Window Forecasting Loss \textit{(continued)}}\\
    \toprule
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endhead

    \endfoot
    \bottomrule
    \endlastfoot
     & asset01 & \num{0.692} & \num{0.686} & \textcolor{red}{\num{0.659}} & \num{0.666} & \num{0.669}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.063} & \num{1.023} & \textcolor{red}{\num{1.013}} & \num{1.02} & \num{1.022}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.981} & \num{1.977} & \num{1.896} & \textcolor{red}{\num{1.891}} & \num{1.892}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{2.198} & \num{2.183} & \num{2.105} & \num{2.089} & \textcolor{red}{\num{2.089}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.505} & \textcolor{red}{\num{0.482}} & \num{0.495} & \num{0.493} & \num{0.493}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{2.003} & \num{1.93} & \textcolor{red}{\num{1.928}} & \num{1.964} & \num{1.963}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{0.484}} & \num{0.496} & \num{0.487} & \num{0.489} & \num{0.492}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.692} & \num{1.689} & \num{1.648} & \num{1.638} & \textcolor{red}{\num{1.637}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{1.606}} & \num{1.618} & \num{1.634} & \num{1.645} & \num{1.646}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.358} & \num{1.343} & \textcolor{red}{\num{1.318}} & \num{1.322} & \num{1.323}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.649} & \num{0.64} & \textcolor{red}{\num{0.627}} & \num{0.628} & \num{0.632}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.823} & \num{0.821} & \textcolor{red}{\num{0.813}} & \num{0.815} & \num{0.816}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.15} & \num{1.139} & \num{1.111} & \textcolor{red}{\num{1.108}} & \num{1.108}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.177} & \num{1.178} & \num{1.144} & \textcolor{red}{\num{1.142}} & \num{1.142}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.563} & \textcolor{red}{\num{0.551}} & \num{0.569} & \num{0.569} & \num{0.569}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.127} & \num{1.131} & \textcolor{red}{\num{1.102}} & \num{1.117} & \num{1.117}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{0.565}} & \num{0.568} & \num{0.567} & \num{0.566} & \num{0.568}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.069} & \num{1.065} & \num{1.047} & \num{1.043} & \textcolor{red}{\num{1.043}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{1.041}} & \num{1.053} & \num{1.053} & \num{1.055} & \num{1.055}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.907} & \num{0.905} & \textcolor{red}{\num{0.893}} & \num{0.894} & \num{0.894}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{144.228} & \num{150.599} & \num{115.526} & \textcolor{red}{\num{111.906}} & \num{130.273}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{151.82} & \num{144.623} & \num{128.735} & \textcolor{red}{\num{125.878}} & \num{128.462}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{213.079} & \num{176.717} & \num{144.55} & \textcolor{red}{\num{139.223}} & \num{139.829}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{317.713} & \num{193.834} & \num{167.496} & \textcolor{red}{\num{147.446}} & \num{148.459}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{237.232} & \textcolor{red}{\num{121.3}} & \num{183.033} & \num{167.765} & \num{167.371}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{128.609} & \num{133.824} & \num{110.802} & \textcolor{red}{\num{110.676}} & \num{111.13}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{188.165} & \num{167.866} & \num{161.13} & \num{162.408} & \textcolor{red}{\num{160.41}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{119.924} & \num{111.448} & \num{102.503} & \textcolor{red}{\num{102.064}} & \num{102.234}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{123.891} & \num{188.876} & \textcolor{red}{\num{119.065}} & \num{120.249} & \num{120.578}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{180.518} & \num{154.343} & \num{136.982} & \textcolor{red}{\num{131.957}} & \num{134.305}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{57.71} & \num{56.729} & \textcolor{red}{\num{55.717}} & \num{55.861} & \num{56.295}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{69.391} & \num{69.054} & \textcolor{red}{\num{68.432}} & \num{68.613} & \num{68.66}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{99.06} & \num{97.986} & \num{96.251} & \textcolor{red}{\num{96.049}} & \num{96.097}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{98.671} & \num{99.195} & \num{96.417} & \textcolor{red}{\num{96.215}} & \num{96.232}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{47.622} & \textcolor{red}{\num{46.564}} & \num{48.221} & \num{48.299} & \num{48.305}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{96.756} & \num{97.11} & \textcolor{red}{\num{94.804}} & \num{96.114} & \num{96.134}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{48.708}} & \num{49.294} & \num{49.398} & \num{49.353} & \num{49.52}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{89.921} & \num{90.108} & \num{88.644} & \num{88.452} & \textcolor{red}{\num{88.438}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{90.313}} & \num{90.616} & \num{90.89} & \num{90.857} & \num{90.864}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{77.572} & \num{77.406} & \textcolor{red}{\num{76.531}} & \num{76.646} & \num{76.727}\\*
    \end{longtable}

5-step:


    \begin{longtable}[t]{lllllll}
    \caption{\label{tab:medfive}MEDIUM Simulation - 5-step ahead Rolling Window Forecasting Loss}\\
    \toprule
    \multicolumn{1}{c}{ } & \multicolumn{1}{c}{ } & \multicolumn{2}{c}{Frequentist} & \multicolumn{1}{c}{BVAR} & \multicolumn{2}{c}{BVHAR} \\
    \cmidrule(l{3pt}r{3pt}){3-4} \cmidrule(l{3pt}r{3pt}){5-5} \cmidrule(l{3pt}r{3pt}){6-7}
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endfirsthead
    \caption[]{MEDIUM Simulation - 5-step ahead Rolling Window Forecasting Loss \textit{(continued)}}\\
    \toprule
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endhead

    \endfoot
    \bottomrule
    \endlastfoot
     & asset01 & \num{0.702} & \num{0.715} & \num{0.691} & \num{0.691} & \textcolor{red}{\num{0.688}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.092} & \num{1.111} & \num{1.062} & \textcolor{red}{\num{1.061}} & \num{1.062}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.912} & \num{1.937} & \num{1.912} & \textcolor{red}{\num{1.911}} & \num{1.912}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{2.21} & \num{2.199} & \num{2.145} & \textcolor{red}{\num{2.14}} & \num{2.14}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.482}} & \num{0.489} & \num{0.487} & \num{0.488} & \num{0.488}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{2.046} & \textcolor{red}{\num{1.923}} & \num{2.014} & \num{2.009} & \num{2.01}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.502} & \num{0.491} & \num{0.492} & \num{0.492} & \textcolor{red}{\num{0.49}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.69} & \num{1.69} & \num{1.655} & \num{1.651} & \textcolor{red}{\num{1.649}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.711} & \num{1.73} & \num{1.709} & \textcolor{red}{\num{1.709}} & \num{1.709}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.372} & \num{1.365} & \num{1.352} & \num{1.35} & \textcolor{red}{\num{1.35}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.651} & \num{0.65} & \textcolor{red}{\num{0.643}} & \num{0.643} & \num{0.644}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.825} & \num{0.838} & \num{0.817} & \textcolor{red}{\num{0.816}} & \num{0.817}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.112} & \num{1.128} & \textcolor{red}{\num{1.107}} & \num{1.107} & \num{1.108}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.185} & \num{1.172} & \num{1.156} & \textcolor{red}{\num{1.154}} & \num{1.154}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.559} & \textcolor{red}{\num{0.554}} & \num{0.563} & \num{0.564} & \num{0.564}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.144} & \textcolor{red}{\num{1.124}} & \num{1.127} & \num{1.126} & \num{1.127}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.572} & \textcolor{red}{\num{0.549}} & \num{0.559} & \num{0.558} & \num{0.557}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.048} & \num{1.049} & \num{1.039} & \num{1.038} & \textcolor{red}{\num{1.038}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{1.068}} & \num{1.087} & \num{1.072} & \num{1.073} & \num{1.073}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.907} & \num{0.905} & \num{0.898} & \textcolor{red}{\num{0.898}} & \num{0.898}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{108.096} & \num{114.96} & \textcolor{red}{\num{99.427}} & \num{100.882} & \num{109.976}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{105.524} & \num{131.003} & \num{99.281} & \num{98.335} & \textcolor{red}{\num{98.091}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{104.08} & \num{124.644} & \textcolor{red}{\num{100.244}} & \num{101.165} & \num{101.572}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{261.529} & \num{141.099} & \num{106.057} & \textcolor{red}{\num{101.804}} & \num{103.348}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{155.005} & \num{181.109} & \num{105.205} & \num{101.071} & \textcolor{red}{\num{100.296}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{107.047} & \num{129.266} & \num{100.476} & \textcolor{red}{\num{100.329}} & \num{101.043}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{117.571} & \num{106.128} & \textcolor{red}{\num{99.784}} & \num{100.041} & \num{100.041}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{104.814} & \num{105.602} & \num{100.402} & \textcolor{red}{\num{100.11}} & \num{100.729}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{117.883} & \num{136.139} & \num{101.846} & \num{100.074} & \textcolor{red}{\num{99.171}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{131.283} & \num{129.994} & \num{101.414} & \textcolor{red}{\num{100.423}} & \num{101.585}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{55.339} & \num{55.566} & \textcolor{red}{\num{54.826}} & \num{54.836} & \num{54.913}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{71.593} & \num{72.51} & \num{71.007} & \textcolor{red}{\num{70.921}} & \num{70.964}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{94.424} & \num{96.464} & \num{94.099} & \textcolor{red}{\num{94.096}} & \num{94.146}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{105.608} & \num{104.619} & \num{103.289} & \textcolor{red}{\num{103.114}} & \num{103.125}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{45.833}} & \num{45.975} & \num{46.315} & \num{46.39} & \num{46.408}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{98.972} & \textcolor{red}{\num{96.239}} & \num{97.044} & \num{96.937} & \num{96.986}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{49.11} & \textcolor{red}{\num{47.04}} & \num{47.719} & \num{47.646} & \num{47.541}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{88.057} & \num{88.33} & \num{87.092} & \num{86.974} & \textcolor{red}{\num{86.911}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{91.686}} & \num{93.281} & \num{92.213} & \num{92.282} & \num{92.272}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{77.847} & \num{77.78} & \num{77.067} & \textcolor{red}{\num{77.022}} & \num{77.03}\\*
    \end{longtable}

20-step:


    \begin{longtable}[t]{lllllll}
    \caption{\label{tab:medtwenty}MEDIUM Simulation - 20-step ahead Rolling Window Forecasting Loss}\\
    \toprule
    \multicolumn{1}{c}{ } & \multicolumn{1}{c}{ } & \multicolumn{2}{c}{Frequentist} & \multicolumn{1}{c}{BVAR} & \multicolumn{2}{c}{BVHAR} \\
    \cmidrule(l{3pt}r{3pt}){3-4} \cmidrule(l{3pt}r{3pt}){5-5} \cmidrule(l{3pt}r{3pt}){6-7}
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endfirsthead
    \caption[]{MEDIUM Simulation - 20-step ahead Rolling Window Forecasting Loss \textit{(continued)}}\\
    \toprule
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endhead

    \endfoot
    \bottomrule
    \endlastfoot
     & asset01 & \num{0.694} & \num{0.702} & \num{0.694} & \num{0.694} & \textcolor{red}{\num{0.694}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{1.066}} & \num{1.121} & \num{1.068} & \num{1.068} & \num{1.068}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.799} & \num{1.818} & \num{1.798} & \num{1.798} & \textcolor{red}{\num{1.797}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{2.243} & \num{2.296} & \num{2.236} & \num{2.236} & \textcolor{red}{\num{2.236}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.429}} & \num{0.431} & \num{0.429} & \num{0.429} & \num{0.429}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{2.031} & \textcolor{red}{\num{2.004}} & \num{2.026} & \num{2.026} & \num{2.026}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{0.461}} & \num{0.465} & \num{0.462} & \num{0.462} & \num{0.462}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{1.655}} & \num{1.699} & \num{1.655} & \num{1.655} & \num{1.655}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{1.765}} & \num{1.768} & \num{1.768} & \num{1.768} & \num{1.769}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.349} & \num{1.367} & \textcolor{red}{\num{1.349}} & \num{1.349} & \num{1.349}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{0.635}} & \num{0.636} & \num{0.636} & \num{0.636} & \num{0.636}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.818}} & \num{0.84} & \num{0.819} & \num{0.819} & \num{0.819}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.067} & \num{1.071} & \num{1.066} & \num{1.066} & \textcolor{red}{\num{1.066}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.189} & \num{1.211} & \textcolor{red}{\num{1.186}} & \num{1.186} & \num{1.186}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.54} & \textcolor{red}{\num{0.538}} & \num{0.541} & \num{0.541} & \num{0.541}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.129} & \num{1.135} & \textcolor{red}{\num{1.128}} & \num{1.128} & \num{1.128}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.533} & \textcolor{red}{\num{0.528}} & \num{0.534} & \num{0.534} & \num{0.534}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{1.031}} & \num{1.054} & \num{1.031} & \num{1.031} & \num{1.031}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{1.094}} & \num{1.096} & \num{1.094} & \num{1.094} & \num{1.094}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.893} & \num{0.901} & \textcolor{red}{\num{0.893}} & \num{0.893} & \num{0.893}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{99.511}} & \num{101.387} & \num{100} & \num{99.944} & \num{100.039}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{98.605}} & \num{121.49} & \num{99.983} & \num{100.103} & \num{100.023}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{100.789} & \num{106.919} & \num{100.003} & \textcolor{red}{\num{99.978}} & \num{99.988}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{103.879} & \num{138.845} & \textcolor{red}{\num{99.989}} & \num{100.21} & \num{100.27}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{100.562} & \num{190.655} & \textcolor{red}{\num{99.983}} & \num{100.775} & \num{101.078}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{100.26} & \num{115.285} & \textcolor{red}{\num{100.004}} & \num{100.047} & \num{100.038}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{99.243}} & \num{99.64} & \num{99.994} & \num{99.926} & \num{99.927}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{99.861}} & \num{109.082} & \num{100.001} & \num{100.049} & \num{100.049}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{99.99} & \num{108.368} & \num{100.005} & \num{99.938} & \textcolor{red}{\num{99.433}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{100.3} & \num{121.297} & \textcolor{red}{\num{99.996}} & \num{100.108} & \num{100.094}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{56.587} & \textcolor{red}{\num{56.476}} & \num{56.647} & \num{56.643} & \num{56.636}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{67.989}} & \num{69.278} & \num{68.08} & \num{68.085} & \num{68.083}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{91.454} & \textcolor{red}{\num{91.257}} & \num{91.397} & \num{91.395} & \num{91.39}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{99.285} & \num{100.636} & \textcolor{red}{\num{99.072}} & \num{99.078} & \num{99.075}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{45.655} & \textcolor{red}{\num{45.562}} & \num{45.738} & \num{45.734} & \num{45.735}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{98.36} & \num{99.445} & \textcolor{red}{\num{98.248}} & \num{98.261} & \num{98.26}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{46.43} & \textcolor{red}{\num{46.137}} & \num{46.525} & \num{46.524} & \num{46.526}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{86.629}} & \num{89.024} & \num{86.667} & \num{86.674} & \num{86.673}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{93.733} & \textcolor{red}{\num{93.716}} & \num{93.785} & \num{93.779} & \num{93.782}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{76.236}} & \num{76.837} & \num{76.24} & \num{76.241} & \num{76.24}\\*
    \end{longtable}

### LARGE

Plots

``` r
cv_large_list[[1]] %>% 
  gg_loss(
    y_large_test, 
    mean_line = TRUE, 
    line_param = list(size = .3),
    mean_param = list(alpha = .5, size = .3), 
    viridis = TRUE, 
    show.legend = FALSE
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -30, vjust = -1),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(.3, "cm")
  )
```

<img src="../output/figs/DGP-1-largecvonefig-1.png" width="70%" style="display: block; margin: auto;" />

``` r
cv_large_list[[2]] %>% 
  gg_loss(
    y_large_test, 
    mean_line = TRUE, 
    line_param = list(size = .3),
    mean_param = list(alpha = .5, size = .3), 
    viridis = TRUE, 
    show.legend = FALSE
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -30, vjust = -1),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(.3, "cm")
  )
```

<img src="../output/figs/DGP-1-largecvfivefig-1.png" width="70%" style="display: block; margin: auto;" />

``` r
cv_large_list[[3]] %>% 
  gg_loss(
    y_large_test, 
    mean_line = TRUE, 
    line_param = list(size = .3),
    mean_param = list(alpha = .5, size = .3), 
    viridis = TRUE, 
    show.legend = FALSE
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -30, vjust = -1),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(.3, "cm")
  )
```

<img src="../output/figs/DGP-1-largecvtwentyfig-1.png" width="70%" style="display: block; margin: auto;" />

Tables

1-step:


    \begin{longtable}[t]{lllllll}
    \caption{\label{tab:largeone}LARGE Simulation - 1-step ahead Rolling Window Forecasting Loss}\\
    \toprule
    \multicolumn{1}{c}{ } & \multicolumn{1}{c}{ } & \multicolumn{2}{c}{Frequentist} & \multicolumn{1}{c}{BVAR} & \multicolumn{2}{c}{BVHAR} \\
    \cmidrule(l{3pt}r{3pt}){3-4} \cmidrule(l{3pt}r{3pt}){5-5} \cmidrule(l{3pt}r{3pt}){6-7}
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endfirsthead
    \caption[]{LARGE Simulation - 1-step ahead Rolling Window Forecasting Loss \textit{(continued)}}\\
    \toprule
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endhead

    \endfoot
    \bottomrule
    \endlastfoot
     & asset01 & \textcolor{red}{\num{1.404}} & \num{1.498} & \num{1.427} & \num{1.428} & \num{1.428}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{2.035} & \num{1.958} & \num{1.803} & \textcolor{red}{\num{1.802}} & \textcolor{red}{\num{1.802}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.994} & \num{2.001} & \num{1.968} & \num{1.968} & \textcolor{red}{\num{1.959}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.241} & \textcolor{red}{\num{1.218}} & \num{1.233} & \num{1.235} & \num{1.233}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{1.198} & \num{1.151} & \textcolor{red}{\num{1.105}} & \num{1.106} & \num{1.106}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.722} & \num{1.72} & \num{1.674} & \textcolor{red}{\num{1.674}} & \num{1.674}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1.736} & \num{1.759} & \textcolor{red}{\num{1.583}} & \num{1.583} & \num{1.583}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.727} & \num{1.733} & \num{1.6} & \textcolor{red}{\num{1.597}} & \num{1.597}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.017} & \textcolor{red}{\num{0.987}} & \num{0.988} & \num{0.988} & \num{0.988}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.93} & \num{0.958} & \num{0.887} & \textcolor{red}{\num{0.887}} & \num{0.89}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{2.255} & \num{2.021} & \num{1.932} & \textcolor{red}{\num{1.931}} & \textcolor{red}{\num{1.931}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.623} & \num{0.599} & \num{0.598} & \textcolor{red}{\num{0.598}} & \textcolor{red}{\num{0.598}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.49} & \num{1.467} & \num{1.4} & \num{1.4} & \textcolor{red}{\num{1.399}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{0.964}} & \num{0.983} & \num{0.965} & \num{0.966} & \num{0.966}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.161} & \num{1.12} & \num{1.095} & \textcolor{red}{\num{1.094}} & \textcolor{red}{\num{1.094}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.081} & \num{1.068} & \num{1.049} & \num{1.049} & \textcolor{red}{\num{1.045}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.892} & \textcolor{red}{\num{0.885}} & \num{0.899} & \num{0.899} & \num{0.899}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.864} & \num{0.856} & \textcolor{red}{\num{0.842}} & \num{0.843} & \num{0.843}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.043} & \num{1.021} & \num{1.02} & \textcolor{red}{\num{1.019}} & \num{1.019}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1.051} & \num{1.064} & \num{1.013} & \textcolor{red}{\num{1.013}} & \num{1.013}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.056} & \num{1.053} & \num{1.005} & \textcolor{red}{\num{1.004}} & \num{1.004}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.795} & \textcolor{red}{\num{0.771}} & \num{0.776} & \num{0.776} & \num{0.776}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.762} & \num{0.774} & \num{0.744} & \num{0.744} & \textcolor{red}{\num{0.742}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{1.171} & \textcolor{red}{\num{1.121}} & \num{1.121} & \num{1.121} & \num{1.121}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.651} & \num{0.631} & \num{0.623} & \textcolor{red}{\num{0.623}} & \textcolor{red}{\num{0.623}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.958} & \num{0.946} & \num{0.929} & \num{0.929} & \textcolor{red}{\num{0.929}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{118.919} & \num{128.158} & \num{100.191} & \textcolor{red}{\num{100}} & \textcolor{red}{\num{100}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{131.584} & \num{109.564} & \num{100.078} & \textcolor{red}{\num{100}} & \textcolor{red}{\num{100}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{144.849} & \num{150.711} & \num{100.157} & \textcolor{red}{\num{100}} & \num{100.223}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{173.803} & \num{130.505} & \num{98.944} & \num{99.049} & \textcolor{red}{\num{98.282}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{158.647} & \num{136.765} & \num{100.613} & \textcolor{red}{\num{100.483}} & \num{100.484}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{121.925} & \num{133.829} & \textcolor{red}{\num{99.737}} & \num{99.97} & \num{99.97}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{331.522} & \num{185.951} & \num{101.565} & \num{100} & \textcolor{red}{\num{99.895}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{122.234} & \num{115.386} & \num{100.15} & \textcolor{red}{\num{99.641}} & \num{99.641}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{398.165} & \num{371.634} & \num{100.115} & \textcolor{red}{\num{100}} & \textcolor{red}{\num{100}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{137.254} & \num{122.618} & \num{99.934} & \num{100} & \textcolor{red}{\num{98.803}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{130.637} & \num{139.156} & \num{100.013} & \textcolor{red}{\num{100}} & \textcolor{red}{\num{100}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{219.955} & \num{183.471} & \num{100.372} & \textcolor{red}{\num{100}} & \textcolor{red}{\num{100}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{182.458} & \num{158.979} & \num{100.156} & \num{99.929} & \textcolor{red}{\num{99.775}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{74.812}} & \num{76.498} & \num{75.354} & \num{75.373} & \num{75.373}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{89.729} & \num{86.43} & \num{85.002} & \textcolor{red}{\num{84.977}} & \textcolor{red}{\num{84.977}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{83.442} & \num{82.52} & \num{80.259} & \num{80.237} & \textcolor{red}{\num{79.927}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{70.515} & \textcolor{red}{\num{69.993}} & \num{71.352} & \num{71.386} & \num{71.362}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{67.79} & \num{67.663} & \textcolor{red}{\num{66.041}} & \num{66.112} & \num{66.112}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{82.584} & \num{80.831} & \num{80.773} & \textcolor{red}{\num{80.744}} & \num{80.744}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{81.721} & \num{82.303} & \num{78.414} & \textcolor{red}{\num{78.385}} & \num{78.386}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{82.865} & \num{82.593} & \num{78.747} & \textcolor{red}{\num{78.656}} & \num{78.656}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{62.229} & \textcolor{red}{\num{61.037}} & \num{61.08} & \num{61.07} & \num{61.07}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{59.42} & \num{60.582} & \num{58.179} & \num{58.182} & \textcolor{red}{\num{58.022}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{92.125} & \textcolor{red}{\num{88.258}} & \num{88.802} & \num{88.787} & \num{88.787}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{50.834} & \num{48.921} & \num{48.562} & \textcolor{red}{\num{48.553}} & \textcolor{red}{\num{48.553}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{74.839} & \num{73.969} & \num{72.714} & \num{72.705} & \textcolor{red}{\num{72.664}}\\*
    \end{longtable}

5-step:


    \begin{longtable}[t]{lllllll}
    \caption{\label{tab:largefive}LARGE Simulation - 5-step ahead Rolling Window Forecasting Loss}\\
    \toprule
    \multicolumn{1}{c}{ } & \multicolumn{1}{c}{ } & \multicolumn{2}{c}{Frequentist} & \multicolumn{1}{c}{BVAR} & \multicolumn{2}{c}{BVHAR} \\
    \cmidrule(l{3pt}r{3pt}){3-4} \cmidrule(l{3pt}r{3pt}){5-5} \cmidrule(l{3pt}r{3pt}){6-7}
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endfirsthead
    \caption[]{LARGE Simulation - 5-step ahead Rolling Window Forecasting Loss \textit{(continued)}}\\
    \toprule
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endhead

    \endfoot
    \bottomrule
    \endlastfoot
     & asset01 & \textcolor{red}{\num{1.316}} & \num{1.429} & \num{1.332} & \num{1.332} & \num{1.332}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.895} & \num{1.913} & \num{1.85} & \num{1.85} & \textcolor{red}{\num{1.85}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.936} & \num{1.989} & \num{1.921} & \num{1.921} & \textcolor{red}{\num{1.919}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.309} & \textcolor{red}{\num{1.261}} & \num{1.279} & \num{1.279} & \num{1.279}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{1.078}} & \num{1.104} & \num{1.104} & \num{1.104} & \num{1.104}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.703} & \num{1.763} & \num{1.678} & \textcolor{red}{\num{1.678}} & \num{1.678}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1.628} & \num{1.649} & \num{1.581} & \num{1.581} & \textcolor{red}{\num{1.581}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.705} & \num{1.692} & \num{1.644} & \num{1.644} & \textcolor{red}{\num{1.644}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.001} & \textcolor{red}{\num{0.975}} & \num{1.012} & \num{1.012} & \num{1.012}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.888} & \num{0.926} & \textcolor{red}{\num{0.884}} & \num{0.884} & \num{0.887}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{2.007} & \num{1.961} & \num{1.913} & \num{1.913} & \textcolor{red}{\num{1.913}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.604} & \num{0.597} & \num{0.588} & \num{0.588} & \textcolor{red}{\num{0.588}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.422} & \num{1.438} & \num{1.399} & \textcolor{red}{\num{1.399}} & \num{1.399}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.957} & \num{0.977} & \num{0.948} & \textcolor{red}{\num{0.948}} & \num{0.948}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.121} & \num{1.115} & \num{1.112} & \num{1.112} & \textcolor{red}{\num{1.112}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.034} & \num{1.052} & \num{1.033} & \num{1.033} & \textcolor{red}{\num{1.033}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.934} & \textcolor{red}{\num{0.906}} & \num{0.92} & \num{0.92} & \num{0.92}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.829}} & \num{0.832} & \num{0.834} & \num{0.834} & \num{0.834}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{1.011}} & \num{1.029} & \num{1.016} & \num{1.016} & \num{1.016}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1.019} & \num{1.022} & \num{1.008} & \num{1.008} & \textcolor{red}{\num{1.008}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.021} & \num{1.035} & \textcolor{red}{\num{1.02}} & \num{1.02} & \num{1.02}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.781} & \textcolor{red}{\num{0.764}} & \num{0.788} & \num{0.788} & \num{0.788}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.735}} & \num{0.752} & \num{0.738} & \num{0.738} & \num{0.739}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{1.13} & \textcolor{red}{\num{1.112}} & \num{1.118} & \num{1.118} & \num{1.118}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.622} & \num{0.615} & \num{0.613} & \textcolor{red}{\num{0.613}} & \num{0.613}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.933} & \num{0.934} & \num{0.929} & \textcolor{red}{\num{0.929}} & \num{0.929}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{107.952} & \num{114.082} & \textcolor{red}{\num{99.999}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{110.373} & \num{100.058} & \num{100.005} & \textcolor{red}{\num{100}} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{106.167} & \num{114.455} & \num{100.002} & \num{100} & \textcolor{red}{\num{99.931}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{121.305} & \num{127.544} & \num{100.005} & \textcolor{red}{\num{100}} & \num{100.054}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{118.102} & \num{103.1} & \num{100} & \textcolor{red}{\num{100}} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{111.507} & \num{133.767} & \textcolor{red}{\num{99.989}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{156.682} & \num{136.001} & \num{100.009} & \num{100} & \textcolor{red}{\num{99.924}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{103.007} & \num{103.962} & \textcolor{red}{\num{99.995}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{125.597} & \num{182.372} & \num{100.023} & \textcolor{red}{\num{100}} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{102.219} & \num{105.072} & \textcolor{red}{\num{99.986}} & \num{100} & \num{101.387}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{104.326} & \num{110.327} & \textcolor{red}{\num{100}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{135.069} & \num{108.815} & \textcolor{red}{\num{99.99}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{116.859} & \num{119.963} & \num{100} & \textcolor{red}{\num{100}} & \num{100.108}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{75.415} & \num{76.851} & \num{74.864} & \textcolor{red}{\num{74.864}} & \num{74.864}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{86.172} & \num{85.786} & \num{85.451} & \num{85.45} & \textcolor{red}{\num{85.45}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{78.866} & \num{80.156} & \num{78.791} & \num{78.791} & \textcolor{red}{\num{78.749}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{71.263} & \textcolor{red}{\num{68.891}} & \num{70.333} & \num{70.332} & \num{70.304}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{65.102}} & \num{65.272} & \num{65.585} & \num{65.585} & \num{65.585}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{78.979}} & \num{80.453} & \num{79.67} & \num{79.671} & \num{79.671}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{79.445} & \num{79.6} & \num{78.132} & \num{78.131} & \textcolor{red}{\num{78.13}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{82.285} & \num{83.231} & \textcolor{red}{\num{81.781}} & \num{81.782} & \num{81.782}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{61.561} & \textcolor{red}{\num{60.492}} & \num{61.958} & \num{61.958} & \num{61.958}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{55.803}} & \num{57.086} & \num{56.104} & \num{56.105} & \num{56.219}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{88.414} & \textcolor{red}{\num{87.187}} & \num{87.394} & \num{87.394} & \num{87.394}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{48.112} & \num{47.412} & \num{47.325} & \textcolor{red}{\num{47.324}} & \num{47.324}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{72.618} & \num{72.701} & \num{72.282} & \textcolor{red}{\num{72.282}} & \num{72.286}\\*
    \end{longtable}

20-step:


    \begin{longtable}[t]{lllllll}
    \caption{\label{tab:largetwenty}LARGE Simulation - 20-step ahead Rolling Window Forecasting Loss}\\
    \toprule
    \multicolumn{1}{c}{ } & \multicolumn{1}{c}{ } & \multicolumn{2}{c}{Frequentist} & \multicolumn{1}{c}{BVAR} & \multicolumn{2}{c}{BVHAR} \\
    \cmidrule(l{3pt}r{3pt}){3-4} \cmidrule(l{3pt}r{3pt}){5-5} \cmidrule(l{3pt}r{3pt}){6-7}
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endfirsthead
    \caption[]{LARGE Simulation - 20-step ahead Rolling Window Forecasting Loss \textit{(continued)}}\\
    \toprule
     &  & VAR & VHAR & Minnesota & VAR-type & VHAR-type\\
    \midrule
    \endhead

    \endfoot
    \bottomrule
    \endlastfoot
     & asset01 & \num{1.377} & \num{1.432} & \num{1.375} & \textcolor{red}{\num{1.375}} & \num{1.375}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.862} & \num{1.893} & \num{1.85} & \num{1.85} & \textcolor{red}{\num{1.85}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1.93}} & \num{1.983} & \num{1.93} & \num{1.93} & \num{1.93}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.26} & \textcolor{red}{\num{1.238}} & \num{1.266} & \num{1.266} & \num{1.266}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.979} & \textcolor{red}{\num{0.969}} & \num{0.975} & \num{0.975} & \num{0.975}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{1.751}} & \num{1.85} & \num{1.753} & \num{1.753} & \num{1.753}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1.416} & \num{1.458} & \num{1.411} & \textcolor{red}{\num{1.411}} & \num{1.411}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.645} & \num{1.668} & \num{1.638} & \num{1.638} & \textcolor{red}{\num{1.638}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.05} & \num{1.066} & \num{1.048} & \textcolor{red}{\num{1.048}} & \num{1.048}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.879}} & \num{0.898} & \num{0.881} & \num{0.881} & \num{0.881}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{1.906}} & \num{1.923} & \num{1.911} & \num{1.911} & \num{1.911}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.574} & \num{0.585} & \num{0.573} & \textcolor{red}{\num{0.573}} & \num{0.573}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.386} & \num{1.414} & \num{1.384} & \num{1.384} & \textcolor{red}{\num{1.384}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.972} & \num{0.98} & \num{0.97} & \textcolor{red}{\num{0.97}} & \num{0.97}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.105} & \num{1.109} & \num{1.101} & \textcolor{red}{\num{1.101}} & \num{1.101}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.047} & \num{1.049} & \num{1.045} & \num{1.045} & \textcolor{red}{\num{1.045}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.907} & \textcolor{red}{\num{0.903}} & \num{0.911} & \num{0.911} & \num{0.911}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.793} & \textcolor{red}{\num{0.784}} & \num{0.792} & \num{0.792} & \num{0.792}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{1.045}} & \num{1.064} & \num{1.046} & \num{1.046} & \num{1.046}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.966} & \num{0.983} & \num{0.966} & \num{0.966} & \textcolor{red}{\num{0.966}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.011} & \num{1.022} & \num{1.01} & \num{1.01} & \textcolor{red}{\num{1.01}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.806} & \num{0.805} & \num{0.804} & \num{0.804} & \textcolor{red}{\num{0.804}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.735}} & \num{0.737} & \num{0.736} & \num{0.736} & \num{0.736}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{1.12} & \textcolor{red}{\num{1.111}} & \num{1.119} & \num{1.119} & \num{1.119}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.606} & \num{0.612} & \num{0.606} & \textcolor{red}{\num{0.606}} & \num{0.606}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.926} & \num{0.93} & \num{0.925} & \num{0.925} & \textcolor{red}{\num{0.925}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{100.565} & \num{104.019} & \num{100} & \textcolor{red}{\num{100}} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{100.701} & \num{101.824} & \num{100} & \textcolor{red}{\num{100}} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{100.537} & \num{107.41} & \num{100} & \num{100} & \textcolor{red}{\num{99.998}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{99.086}} & \num{131.921} & \num{100} & \num{100} & \num{99.996}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{102.325} & \num{100.018} & \num{100} & \textcolor{red}{\num{100}} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{99.494}} & \num{107.983} & \num{100} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{102.144} & \num{140.761} & \num{100} & \num{100} & \textcolor{red}{\num{99.989}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{100.187} & \num{102.54} & \num{100} & \textcolor{red}{\num{100}} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{103.429} & \num{171.228} & \textcolor{red}{\num{100}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{99.121}} & \num{102.629} & \num{100} & \num{100} & \num{99.993}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{101.652} & \num{101.297} & \num{100} & \textcolor{red}{\num{100}} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{104.579} & \num{130.929} & \textcolor{red}{\num{100}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{101.152} & \num{116.88} & \num{100} & \num{100} & \textcolor{red}{\num{99.998}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{74.495} & \num{75.121} & \num{74.328} & \textcolor{red}{\num{74.328}} & \num{74.328}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{86.168} & \num{86.431} & \num{85.838} & \textcolor{red}{\num{85.838}} & \num{85.838}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{81.423} & \num{81.607} & \num{81.36} & \num{81.36} & \textcolor{red}{\num{81.36}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{72.621} & \textcolor{red}{\num{72.212}} & \num{72.911} & \num{72.911} & \num{72.91}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{61.355} & \textcolor{red}{\num{60.775}} & \num{61.306} & \num{61.306} & \num{61.306}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{80.231}} & \num{81.51} & \num{80.377} & \num{80.377} & \num{80.377}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{75.759}} & \num{77.51} & \num{75.763} & \num{75.763} & \num{75.761}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{78.579} & \num{79.604} & \num{78.477} & \num{78.477} & \textcolor{red}{\num{78.477}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{61.251} & \num{61.229} & \num{61.081} & \num{61.081} & \textcolor{red}{\num{61.081}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{56.679}} & \num{56.861} & \num{56.77} & \num{56.77} & \num{56.77}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{89.307} & \textcolor{red}{\num{88.332}} & \num{89.277} & \num{89.277} & \num{89.277}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{47.051} & \num{47.556} & \num{47.032} & \textcolor{red}{\num{47.032}} & \num{47.032}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{72.077} & \num{72.396} & \num{72.043} & \num{72.043} & \textcolor{red}{\num{72.043}}\\*
    \end{longtable}

### Average

SMALL

1-step:

    \begin{table}

    \caption{\label{tab:smallonemean}SMALL Simulation - 1-step ahead Rolling Window Forecasting Average Loss}
    \centering
    \begin{tabular}[t]{llllll}
    \toprule
      & VAR & VHAR & BVAR-Minnesota & BVHAR-MN-VAR & BVHAR-MN-VHAR\\
    \midrule
    MSE & \num{0.991} & \num{1.002} & \textcolor{red}{\num{0.978}} & \num{0.989} & \num{0.99}\\
    MAE & \num{0.79} & \num{0.788} & \textcolor{red}{\num{0.779}} & \num{0.781} & \num{0.781}\\
    MAPE & \num{131.045} & \num{136.719} & \num{119.956} & \num{118.35} & \textcolor{red}{\num{118.31}}\\
    MASE & \num{64.248} & \num{64.109} & \textcolor{red}{\num{63.37}} & \num{63.506} & \num{63.493}\\
    \bottomrule
    \end{tabular}
    \end{table}

5-step:

    \begin{table}

    \caption{\label{tab:smallfivemean}SMALL Simulation - 5-step ahead Rolling Window Forecasting Average Loss}
    \centering
    \begin{tabular}[t]{llllll}
    \toprule
      & VAR & VHAR & BVAR-Minnesota & BVHAR-MN-VAR & BVHAR-MN-VHAR\\
    \midrule
    MSE & \num{1.019} & \num{1.03} & \textcolor{red}{\num{1.014}} & \num{1.015} & \num{1.015}\\
    MAE & \num{0.791} & \num{0.796} & \num{0.785} & \num{0.784} & \textcolor{red}{\num{0.784}}\\
    MAPE & \num{106.504} & \num{116.173} & \num{100.382} & \num{100.438} & \textcolor{red}{\num{100.01}}\\
    MASE & \num{63.947} & \num{64.329} & \num{63.498} & \num{63.435} & \textcolor{red}{\num{63.401}}\\
    \bottomrule
    \end{tabular}
    \end{table}

20-step:

    \begin{table}

    \caption{\label{tab:smalltwentyemean}SMALL Simulation - 20-step ahead Rolling Window Forecasting Average Loss}
    \centering
    \begin{tabular}[t]{llllll}
    \toprule
      & VAR & VHAR & BVAR-Minnesota & BVHAR-MN-VAR & BVHAR-MN-VHAR\\
    \midrule
    MSE & \textcolor{red}{\num{0.995}} & \num{1.004} & \num{0.997} & \num{0.997} & \num{0.996}\\
    MAE & \num{0.782} & \num{0.788} & \num{0.782} & \num{0.782} & \textcolor{red}{\num{0.782}}\\
    MAPE & \num{100.653} & \num{111.045} & \num{100.007} & \num{99.961} & \textcolor{red}{\num{99.873}}\\
    MASE & \num{66.223} & \num{66.62} & \num{66.225} & \num{66.222} & \textcolor{red}{\num{66.217}}\\
    \bottomrule
    \end{tabular}
    \end{table}

MEDIUM

1-step:

    \begin{table}

    \caption{\label{tab:medonemean}MEDIUM Simulation - 1-step ahead Rolling Window Forecasting Average Loss}
    \centering
    \begin{tabular}[t]{llllll}
    \toprule
      & VAR & VHAR & BVAR-Minnesota & BVHAR-MN-VAR & BVHAR-MN-VHAR\\
    \midrule
    MSE & \num{1.358} & \num{1.343} & \textcolor{red}{\num{1.318}} & \num{1.322} & \num{1.323}\\
    MAE & \num{0.907} & \num{0.905} & \textcolor{red}{\num{0.893}} & \num{0.894} & \num{0.894}\\
    MAPE & \num{180.518} & \num{154.343} & \num{136.982} & \textcolor{red}{\num{131.957}} & \num{134.305}\\
    MASE & \num{77.572} & \num{77.406} & \textcolor{red}{\num{76.531}} & \num{76.646} & \num{76.727}\\
    \bottomrule
    \end{tabular}
    \end{table}

5-step:

    \begin{table}

    \caption{\label{tab:medfivemean}MEDIUM Simulation - 5-step ahead Rolling Window Forecasting Average Loss}
    \centering
    \begin{tabular}[t]{llllll}
    \toprule
      & VAR & VHAR & BVAR-Minnesota & BVHAR-MN-VAR & BVHAR-MN-VHAR\\
    \midrule
    MSE & \num{1.372} & \num{1.365} & \num{1.352} & \num{1.35} & \textcolor{red}{\num{1.35}}\\
    MAE & \num{0.907} & \num{0.905} & \num{0.898} & \textcolor{red}{\num{0.898}} & \num{0.898}\\
    MAPE & \num{131.283} & \num{129.994} & \num{101.414} & \textcolor{red}{\num{100.423}} & \num{101.585}\\
    MASE & \num{77.847} & \num{77.78} & \num{77.067} & \textcolor{red}{\num{77.022}} & \num{77.03}\\
    \bottomrule
    \end{tabular}
    \end{table}

20-step:

    \begin{table}

    \caption{\label{tab:medtwentyemean}MEDIUM Simulation - 20-step ahead Rolling Window Forecasting Average Loss}
    \centering
    \begin{tabular}[t]{llllll}
    \toprule
      & VAR & VHAR & BVAR-Minnesota & BVHAR-MN-VAR & BVHAR-MN-VHAR\\
    \midrule
    MSE & \num{1.349} & \num{1.367} & \textcolor{red}{\num{1.349}} & \num{1.349} & \num{1.349}\\
    MAE & \num{0.893} & \num{0.901} & \textcolor{red}{\num{0.893}} & \num{0.893} & \num{0.893}\\
    MAPE & \num{100.3} & \num{121.297} & \textcolor{red}{\num{99.996}} & \num{100.108} & \num{100.094}\\
    MASE & \textcolor{red}{\num{76.236}} & \num{76.837} & \num{76.24} & \num{76.241} & \num{76.24}\\
    \bottomrule
    \end{tabular}
    \end{table}

LARGE

1-step:

    \begin{table}

    \caption{\label{tab:largeonemean}LARGE Simulation - 1-step ahead Rolling Window Forecasting Average Loss}
    \centering
    \begin{tabular}[t]{llllll}
    \toprule
      & VAR & VHAR & BVAR-Minnesota & BVHAR-MN-VAR & BVHAR-MN-VHAR\\
    \midrule
    MSE & \num{1.49} & \num{1.467} & \num{1.4} & \num{1.4} & \textcolor{red}{\num{1.399}}\\
    MAE & \num{0.958} & \num{0.946} & \num{0.929} & \num{0.929} & \textcolor{red}{\num{0.929}}\\
    MAPE & \num{182.458} & \num{158.979} & \num{100.156} & \num{99.929} & \textcolor{red}{\num{99.775}}\\
    MASE & \num{74.839} & \num{73.969} & \num{72.714} & \num{72.705} & \textcolor{red}{\num{72.664}}\\
    \bottomrule
    \end{tabular}
    \end{table}

5-step:

    \begin{table}

    \caption{\label{tab:largefivemean}LARGE Simulation - 5-step ahead Rolling Window Forecasting Average Loss}
    \centering
    \begin{tabular}[t]{llllll}
    \toprule
      & VAR & VHAR & BVAR-Minnesota & BVHAR-MN-VAR & BVHAR-MN-VHAR\\
    \midrule
    MSE & \num{1.422} & \num{1.438} & \num{1.399} & \textcolor{red}{\num{1.399}} & \num{1.399}\\
    MAE & \num{0.933} & \num{0.934} & \num{0.929} & \textcolor{red}{\num{0.929}} & \num{0.929}\\
    MAPE & \num{116.859} & \num{119.963} & \num{100} & \textcolor{red}{\num{100}} & \num{100.108}\\
    MASE & \num{72.618} & \num{72.701} & \num{72.282} & \textcolor{red}{\num{72.282}} & \num{72.286}\\
    \bottomrule
    \end{tabular}
    \end{table}

20-step:

    \begin{table}

    \caption{\label{tab:largetwentyemean}LARGE Simulation - 20-step ahead Rolling Window Forecasting Average Loss}
    \centering
    \begin{tabular}[t]{llllll}
    \toprule
      & VAR & VHAR & BVAR-Minnesota & BVHAR-MN-VAR & BVHAR-MN-VHAR\\
    \midrule
    MSE & \num{1.386} & \num{1.414} & \num{1.384} & \num{1.384} & \textcolor{red}{\num{1.384}}\\
    MAE & \num{0.926} & \num{0.93} & \num{0.925} & \num{0.925} & \textcolor{red}{\num{0.925}}\\
    MAPE & \num{101.152} & \num{116.88} & \num{100} & \num{100} & \textcolor{red}{\num{99.998}}\\
    MASE & \num{72.077} & \num{72.396} & \num{72.043} & \num{72.043} & \textcolor{red}{\num{72.043}}\\
    \bottomrule
    \end{tabular}
    \end{table}

# Additional

<img src="../output/figs/DGP-1-modlegend-1.png" width="70%" style="display: block; margin: auto;" />

# Coefficients

``` r
bayes_small_mod <- mod_small_list[3:5]
names(bayes_small_mod) <- sapply(bayes_small_mod, function(x) x$process)
small_vec <- names(y_small_train)
names(small_vec) <- names(y_small_train)
# draw---------------------
set.seed(1)
bayes_small_plot <- 
  bayes_small_mod %>% 
  lapply(
    function(mods) {
      lapply(
        small_vec,
        function(x) {
          autoplot(summary(mods), var_name = x) +
            theme_minimal()
        }
      )
    }
  )
# save---------------------
lapply(
  seq_along(bayes_small_plot),
  function(x) {
    process_name <- names(bayes_small_plot)[x] # one of the processes
    p_list <- bayes_small_plot[[process_name]]
    asset_name <- names(p_list) # character vector: asset_01, asset_02, ...
    lapply(
      asset_name,
      function(y) {
        file_name <- 
          paste0(
            "../output/dgp01-figs/small-coef/", 
            str_replace_all(process_name, pattern = "\\_", replacement = "\\-"), 
            "-", 
            str_remove_all(y, pattern = "\\_"),
            ".pdf"
          )
        ggsave(filename = file_name, plot = p_list[[y]])
      }
    )
  }
)
```

``` r
bayes_medium_mod <- mod_medium_list[3:5]
names(bayes_medium_mod) <- sapply(bayes_medium_mod, function(x) x$process)
medium_vec <- names(y_medium_train)
names(medium_vec) <- names(y_medium_train)
# draw---------------------
set.seed(1)
bayes_medium_plot <- 
  bayes_medium_mod %>% 
  lapply(
    function(mods) {
      lapply(
        medium_vec,
        function(x) {
          autoplot(summary(mods), var_name = x) +
            theme_minimal()
        }
      )
    }
  )
# save---------------------
lapply(
  seq_along(bayes_medium_plot),
  function(x) {
    process_name <- names(bayes_medium_plot)[x] # one of the processes
    p_list <- bayes_medium_plot[[process_name]]
    asset_name <- names(p_list) # character vector: asset_01, asset_02, ...
    lapply(
      asset_name,
      function(y) {
        file_name <- 
          paste0(
            "../output/dgp01-figs/med-coef/", 
            str_replace_all(process_name, pattern = "\\_", replacement = "\\-"), 
            "-", 
            str_remove_all(y, pattern = "\\_"),
            ".pdf"
          )
        ggsave(filename = file_name, plot = p_list[[y]])
      }
    )
  }
)
```

``` r
bayes_large_mod <- mod_large_list[3:5]
names(bayes_large_mod) <- sapply(bayes_large_mod, function(x) x$process)
large_vec <- names(y_large_train)
names(large_vec) <- names(y_large_train)
# draw---------------------
set.seed(1)
bayes_large_plot <- 
  bayes_large_mod %>% 
  lapply(
    function(mods) {
      lapply(
        large_vec,
        function(x) {
          autoplot(summary(mods), var_name = x) +
            theme_minimal()
        }
      )
    }
  )
# save---------------------
lapply(
  seq_along(bayes_large_plot),
  function(x) {
    process_name <- names(bayes_large_plot)[x] # one of the processes
    p_list <- bayes_large_plot[[process_name]]
    asset_name <- names(p_list) # character vector: asset_01, asset_02, ...
    lapply(
      asset_name,
      function(y) {
        file_name <- 
          paste0(
            "../output/dgp01-figs/large-coef/", 
            str_replace_all(process_name, pattern = "\\_", replacement = "\\-"), 
            "-", 
            str_remove_all(y, pattern = "\\_"),
            ".pdf"
          )
        ggsave(filename = file_name, plot = p_list[[y]])
      }
    )
  }
)
```
