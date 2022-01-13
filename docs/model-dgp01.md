Simulating Minnesota VAR
================
Young Geun Kim
12 Jan, 2022

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
    rep(5, n_small), # sigma
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
#> [1]  2.04  1.93  3.12
#> 
#> Setting for 'lambda':
#> [1]  0.175
#> 
#> Setting for 'delta':
#> [1]  0.0000  0.0305  0.0546
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
    rep(5, n_medium), # sigma
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
#> [1]  2.66  3.22  3.67  3.99  3.13  4.60  2.68  3.49  3.83
#> 
#> Setting for 'lambda':
#> [1]  0.143
#> 
#> Setting for 'delta':
#> [1]  0.00283  0.02904  0.02964  0.02269  0.00000  0.00448  0.15343  0.00371
#> [9]  0.00000
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
    rep(5, n_large), # sigma
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
#>  [1]  4.18  3.86  4.18  4.20  3.86  3.99  4.48  3.88  3.70  3.51  4.27  3.49
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'delta':
#>  [1]  0.00000  0.00000  0.00000  0.02244  0.00555  0.01107  0.00000  0.00000
#>  [9]  0.00000  0.00000  0.00000  0.00000
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
    rep(5, n_small), # sigma
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
#> [1]  2.63  1.70  2.70
#> 
#> Setting for 'lambda':
#> [1]  0.15
#> 
#> Setting for 'delta':
#> [1]  0.0000  0.0411  0.0429
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
    rep(5, n_medium), # sigma
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
#> [1]  2.83  3.27  3.65  4.30  3.12  4.37  2.64  3.58  3.70
#> 
#> Setting for 'lambda':
#> [1]  0.136
#> 
#> Setting for 'delta':
#> [1]  0.00000  0.04036  0.02612  0.01686  0.00000  0.00933  0.15206  0.00000
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
    rep(5, n_large), # sigma
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
#>  [1]  4.18  3.90  4.19  4.20  3.87  4.00  4.40  3.86  3.74  3.43  4.25  3.49
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'delta':
#>  [1]  0.00000  0.00000  0.00000  0.01914  0.01214  0.00582  0.00000  0.00544
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
    rep(5, n_small), # sigma
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
#> [1]  2.64  1.71  2.70
#> 
#> Setting for 'lambda':
#> [1]  0.151
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.0000  0.0411  0.0391
#> 
#> Setting for 'weekly':
#> [1]  0  0  0
#> 
#> Setting for 'monthly':
#> [1]  0.000  0.000  0.269
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
    rep(5, n_medium), # sigma
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
#> [1]  2.81  3.26  3.62  4.29  3.12  4.36  2.65  3.57  3.70
#> 
#> Setting for 'lambda':
#> [1]  0.135
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.00000  0.02556  0.02573  0.01681  0.00000  0.00955  0.14502  0.00000
#> [9]  0.00000
#> 
#> Setting for 'weekly':
#> [1]  0.3428  0.1458  0.0000  0.0000  0.0000  0.0000  0.0647  0.0715  0.0000
#> 
#> Setting for 'monthly':
#> [1]  0.00000  0.00000  0.00281  0.00000  0.00000  0.00000  0.00000  0.00000
#> [9]  0.02285
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
    rep(5, n_large), # sigma
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
#>  [1]  4.19  3.88  4.19  4.21  3.86  4.00  4.44  3.85  3.73  3.45  4.27  3.50
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#>  [1]  0.00000  0.00000  0.00000  0.01494  0.01218  0.00585  0.00000  0.00541
#>  [9]  0.00000  0.00000  0.00000  0.00000
#> 
#> Setting for 'weekly':
#>  [1]  0.0000  0.0000  0.0565  0.0396  0.0000  0.0000  0.0000  0.0000  0.0000
#> [10]  0.1274  0.0000  0.0000
#> 
#> Setting for 'monthly':
#>  [1]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000  0.00465  0.00000
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
    \hspace{1em} & BVAR & $\sigma$ & 2.035 & 1.926 & 3.118 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.175 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.000 & 0.030 & 0.055 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 2.632 & 1.701 & 2.700 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.150 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.000 & 0.041 & 0.043 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 2.639 & 1.707 & 2.701 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.151 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.000 & 0.041 & 0.039 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.000 & 0.000 & 0.000 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.000 & 0.000 & 0.269 &  &  &  &  &  &  &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{MEDIUM}}\\
    \hspace{1em} & BVAR & $\sigma$ & 2.658 & 3.223 & 3.674 & 3.994 & 3.126 & 4.600 & 2.681 & 3.485 & 3.835 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.143 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.003 & 0.029 & 0.030 & 0.023 & 0.000 & 0.004 & 0.153 & 0.004 & 0.000 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 2.830 & 3.272 & 3.647 & 4.303 & 3.116 & 4.371 & 2.644 & 3.579 & 3.697 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.136 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.000 & 0.040 & 0.026 & 0.017 & 0.000 & 0.009 & 0.152 & 0.000 & 0.000 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 2.814 & 3.264 & 3.618 & 4.289 & 3.118 & 4.362 & 2.646 & 3.565 & 3.700 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.135 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.000 & 0.026 & 0.026 & 0.017 & 0.000 & 0.010 & 0.145 & 0.000 & 0.000 &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.343 & 0.146 & 0.000 & 0.000 & 0.000 & 0.000 & 0.065 & 0.072 & 0.000 &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.000 & 0.000 & 0.003 & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.023 &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{LARGE}}\\
    \hspace{1em} & BVAR & $\sigma$ & 4.175 & 3.861 & 4.183 & 4.202 & 3.859 & 3.990 & 4.477 & 3.875 & 3.704 & 3.508 & 4.27 & 3.49\\

    \hspace{1em}\hspace{1em}\hspace{1em} &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.000 & 0.000 & 0.000 & 0.022 & 0.006 & 0.011 & 0.000 & 0.000 & 0.000 & 0.000 & 0.00 & 0.00\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 4.185 & 3.896 & 4.188 & 4.202 & 3.872 & 4.004 & 4.400 & 3.857 & 3.741 & 3.432 & 4.25 & 3.48\\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.000 & 0.000 & 0.000 & 0.019 & 0.012 & 0.006 & 0.000 & 0.005 & 0.000 & 0.000 & 0.00 & 0.00\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 4.189 & 3.881 & 4.191 & 4.214 & 3.860 & 3.995 & 4.438 & 3.845 & 3.728 & 3.451 & 4.27 & 3.50\\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.000 & 0.000 & 0.000 & 0.015 & 0.012 & 0.006 & 0.000 & 0.005 & 0.000 & 0.000 & 0.00 & 0.00\\

    \hspace{1em} &  & $w_i$ & 0.000 & 0.000 & 0.057 & 0.040 & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.127 & 0.00 & 0.00\\

    \hspace{1em} &  & $m_i$ & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.005 & 0.000 & 0.000 & 0.000 & 0.00 & 0.00\\*
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
     & VHAR & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{1.007}} & \textcolor{black}{\num{1.009}} & \textcolor{black}{\num{1.004}} & \textcolor{black}{\num{1.009}} & \textcolor{black}{\num{1.012}} & \textcolor{black}{\num{1.005}} & \textcolor{black}{\num{1.005}} & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{1.006}} & \textcolor{black}{\num{1.009}} & \textcolor{black}{\num{1.004}}\\

     & BVAR & \textcolor{red}{\num{.987}} & \textcolor{black}{\num{.993}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{.985}} & \textcolor{red}{\num{.996}} & \textcolor{black}{\num{.999}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{.987}} & \textcolor{black}{\num{.993}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{1.000}}\\

     & BVHAR-S & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{.992}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.994}} & \textcolor{black}{\num{.997}} & \textcolor{red}{\num{.999}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{.992}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}}\\

    \multirow{-4}{*}{\centering\arraybackslash SMALL} & BVHAR-L & \textcolor{black}{\num{.988}} & \textcolor{red}{\num{.992}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.995}} & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.988}} & \textcolor{red}{\num{.992}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}}\\
    \cmidrule{1-14}
     & VHAR & \textcolor{black}{\num{.996}} & \textcolor{black}{\num{.998}} & \textcolor{black}{\num{1.009}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.987}} & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{1.014}} & \textcolor{red}{\num{.998}} & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{.998}} & \textcolor{black}{\num{1.009}} & \textcolor{black}{\num{1.001}}\\

     & BVAR & \textcolor{red}{\num{.984}} & \textcolor{black}{\num{.990}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{.968}} & \textcolor{black}{\num{.986}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}} & \textcolor{red}{\num{.985}} & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{1.000}} & \textcolor{red}{\num{1.000}}\\

     & BVHAR-S & \textcolor{black}{\num{.986}} & \textcolor{red}{\num{.990}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.973}} & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.988}} & \textcolor{red}{\num{.990}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}}\\

    \multirow{-4}{*}{\centering\arraybackslash MEDIUM} & BVHAR-L & \textcolor{black}{\num{.987}} & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.974}} & \textcolor{red}{\num{.984}} & \textcolor{red}{\num{1.000}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{.990}} & \textcolor{red}{\num{1.000}} & \textcolor{black}{\num{1.000}}\\
    \cmidrule{1-14}
     & VHAR & \textcolor{black}{\num{.987}} & \textcolor{black}{\num{1.001}} & \textcolor{black}{\num{1.004}} & \textcolor{black}{\num{1.008}} & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{1.011}} & \textcolor{black}{\num{1.020}} & \textcolor{black}{\num{1.014}} & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{1.001}} & \textcolor{black}{\num{1.004}} & \textcolor{black}{\num{1.008}}\\

     & BVAR & \textcolor{black}{\num{.971}} & \textcolor{red}{\num{.996}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.941}} & \textcolor{black}{\num{.984}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.971}} & \textcolor{red}{\num{.996}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.000}}\\

     & BVHAR-S & \textcolor{black}{\num{.971}} & \textcolor{black}{\num{.996}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.941}} & \textcolor{red}{\num{.984}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.971}} & \textcolor{black}{\num{.996}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.000}}\\

    \multirow{-4}{*}{\centering\arraybackslash LARGE} & BVHAR-L & \textcolor{red}{\num{.970}} & \textcolor{black}{\num{.996}} & \textcolor{red}{\num{.999}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{.940}} & \textcolor{black}{\num{.984}} & \textcolor{red}{\num{.999}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{.970}} & \textcolor{black}{\num{.996}} & \textcolor{red}{\num{.999}} & \textcolor{red}{\num{1.000}}\\
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
     & asset01 & \num{0.877} & \num{0.846} & \num{0.852} & \textcolor{red}{\num{0.836}} & \num{0.836}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.892} & \num{0.93} & \textcolor{red}{\num{0.873}} & \num{0.899} & \num{0.899}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1.041}} & \num{1.057} & \num{1.042} & \num{1.059} & \num{1.062}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.937} & \num{0.945} & \textcolor{red}{\num{0.922}} & \num{0.931} & \num{0.932}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.745} & \num{0.732} & \num{0.736} & \textcolor{red}{\num{0.729}} & \num{0.729}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.729} & \num{0.742} & \textcolor{red}{\num{0.714}} & \num{0.725} & \num{0.725}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.835} & \num{0.829} & \num{0.828} & \num{0.828} & \textcolor{red}{\num{0.828}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.77} & \num{0.768} & \textcolor{red}{\num{0.759}} & \num{0.761} & \num{0.76}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{660.872} & \num{623.918} & \num{573.074} & \textcolor{red}{\num{535.264}} & \num{535.745}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{126.548} & \num{119.558} & \num{106.98} & \textcolor{red}{\num{100.865}} & \num{100.876}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{120.129} & \num{118.246} & \num{109.318} & \num{106.991} & \textcolor{red}{\num{106.724}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{302.516} & \num{287.241} & \num{263.124} & \textcolor{red}{\num{247.706}} & \num{247.782}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{61.906} & \num{60.856} & \num{61.128} & \textcolor{red}{\num{60.524}} & \num{60.525}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{60.631} & \num{61.709} & \textcolor{red}{\num{59.44}} & \num{60.327} & \num{60.326}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{69.422} & \num{68.913} & \num{68.805} & \num{68.825} & \textcolor{red}{\num{68.791}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{63.986} & \num{63.826} & \textcolor{red}{\num{63.124}} & \num{63.225} & \num{63.214}\\*
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
     & asset01 & \textcolor{red}{\num{0.836}} & \num{0.848} & \num{0.843} & \num{0.845} & \num{0.844}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.968} & \num{0.988} & \num{0.95} & \textcolor{red}{\num{0.949}} & \num{0.949}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.077} & \num{1.08} & \textcolor{red}{\num{1.076}} & \num{1.079} & \num{1.078}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.961} & \num{0.972} & \textcolor{red}{\num{0.957}} & \num{0.957} & \num{0.957}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{0.715}} & \num{0.725} & \num{0.718} & \num{0.719} & \num{0.719}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.761} & \num{0.77} & \num{0.751} & \textcolor{red}{\num{0.75}} & \num{0.75}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.835} & \num{0.831} & \num{0.827} & \num{0.824} & \textcolor{red}{\num{0.823}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.77} & \num{0.775} & \num{0.765} & \num{0.764} & \textcolor{red}{\num{0.764}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{118.698} & \num{107.378} & \num{107.217} & \num{101.181} & \textcolor{red}{\num{100.863}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{105.523} & \num{115.956} & \textcolor{red}{\num{101.04}} & \num{101.257} & \num{101.247}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{113.795} & \num{111.037} & \num{103.187} & \num{100.162} & \textcolor{red}{\num{99.429}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{112.672} & \num{111.457} & \num{103.815} & \num{100.867} & \textcolor{red}{\num{100.513}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{59.23}} & \num{60.056} & \num{59.461} & \num{59.602} & \num{59.577}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{63.129} & \num{63.884} & \num{62.271} & \textcolor{red}{\num{62.192}} & \num{62.197}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{69.437} & \num{69.088} & \num{68.8} & \num{68.532} & \textcolor{red}{\num{68.44}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{63.932} & \num{64.343} & \num{63.511} & \num{63.442} & \textcolor{red}{\num{63.405}}\\*
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
     & asset01 & \num{0.85} & \num{0.851} & \num{0.849} & \num{0.849} & \textcolor{red}{\num{0.849}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.963}} & \num{0.971} & \num{0.964} & \num{0.965} & \num{0.965}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.015} & \num{1.022} & \num{1.013} & \textcolor{red}{\num{1.013}} & \num{1.016}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.943} & \num{0.948} & \num{0.942} & \textcolor{red}{\num{0.942}} & \num{0.943}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.726} & \num{0.726} & \num{0.726} & \num{0.726} & \textcolor{red}{\num{0.726}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.76}} & \num{0.766} & \num{0.761} & \num{0.761} & \num{0.761}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.8} & \num{0.815} & \textcolor{red}{\num{0.8}} & \num{0.8} & \num{0.801}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.762} & \num{0.769} & \textcolor{red}{\num{0.762}} & \num{0.762} & \num{0.762}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{108.246} & \num{125.12} & \num{100.917} & \textcolor{red}{\num{99.739}} & \num{100.3}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{100.063} & \num{108.833} & \textcolor{red}{\num{99.989}} & \num{100.176} & \num{100.199}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{100.3} & \num{112.893} & \num{100.031} & \num{99.729} & \textcolor{red}{\num{99.188}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{102.869} & \num{115.615} & \num{100.312} & \textcolor{red}{\num{99.881}} & \num{99.896}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{60.303} & \num{60.311} & \num{60.28} & \num{60.276} & \textcolor{red}{\num{60.266}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{63.222}} & \num{63.749} & \num{63.26} & \num{63.277} & \num{63.285}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{66.496} & \num{67.711} & \textcolor{red}{\num{66.462}} & \num{66.473} & \num{66.538}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{63.341} & \num{63.924} & \textcolor{red}{\num{63.334}} & \num{63.342} & \num{63.363}\\*
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
     & asset01 & \num{0.972} & \num{0.961} & \textcolor{red}{\num{0.926}} & \num{0.935} & \num{0.939}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.993} & \num{0.953} & \textcolor{red}{\num{0.939}} & \num{0.945} & \num{0.946}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.49} & \num{1.483} & \textcolor{red}{\num{1.425}} & \num{1.425} & \num{1.426}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.64} & \num{1.622} & \num{1.562} & \textcolor{red}{\num{1.561}} & \num{1.561}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.793} & \textcolor{red}{\num{0.765}} & \num{0.78} & \num{0.777} & \num{0.777}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.481} & \num{1.434} & \textcolor{red}{\num{1.421}} & \num{1.449} & \num{1.449}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.704} & \num{0.722} & \textcolor{red}{\num{0.702}} & \num{0.711} & \num{0.715}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.288} & \num{1.276} & \num{1.245} & \textcolor{red}{\num{1.24}} & \num{1.241}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{1.311}} & \num{1.322} & \num{1.332} & \num{1.34} & \num{1.341}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.186} & \num{1.171} & \textcolor{red}{\num{1.148}} & \num{1.154} & \num{1.155}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.769} & \num{0.757} & \textcolor{red}{\num{0.742}} & \num{0.745} & \num{0.749}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.795} & \num{0.794} & \textcolor{red}{\num{0.784}} & \num{0.786} & \num{0.788}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.998} & \num{0.987} & \num{0.97} & \textcolor{red}{\num{0.967}} & \num{0.967}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.017} & \num{1.016} & \textcolor{red}{\num{0.989}} & \num{0.991} & \num{0.991}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.706} & \textcolor{red}{\num{0.693}} & \num{0.715} & \num{0.716} & \num{0.716}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.968} & \num{0.974} & \textcolor{red}{\num{0.943}} & \num{0.956} & \num{0.956}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.681} & \num{0.684} & \textcolor{red}{\num{0.679}} & \num{0.682} & \num{0.684}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.933} & \num{0.926} & \num{0.911} & \num{0.909} & \textcolor{red}{\num{0.909}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.941}} & \num{0.95} & \num{0.948} & \num{0.95} & \num{0.95}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.868} & \num{0.864} & \textcolor{red}{\num{0.853}} & \num{0.856} & \num{0.856}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{223.279} & \num{246.866} & \num{155.969} & \textcolor{red}{\num{139.923}} & \num{199.353}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{135.112} & \num{132.52} & \textcolor{red}{\num{121.075}} & \num{121.209} & \num{125.338}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{151.199} & \num{124.075} & \num{121.9} & \num{114.513} & \textcolor{red}{\num{114.479}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{151.512} & \num{132.267} & \num{120.478} & \textcolor{red}{\num{116.246}} & \num{116.263}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{136.008} & \textcolor{red}{\num{112.056}} & \num{117.914} & \num{113.607} & \num{113.604}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{204.696} & \num{195.019} & \num{137.592} & \textcolor{red}{\num{130.959}} & \num{131.074}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1113.238} & \num{908.231} & \textcolor{red}{\num{530.61}} & \num{656.071} & \num{650.409}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{118.729} & \num{111.528} & \num{101.927} & \textcolor{red}{\num{101.693}} & \num{101.875}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{126.407} & \num{135.15} & \textcolor{red}{\num{107.69}} & \num{107.826} & \num{107.739}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{262.242} & \num{233.079} & \textcolor{red}{\num{168.351}} & \num{178.005} & \num{184.459}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{65.673} & \num{64.51} & \textcolor{red}{\num{63.258}} & \num{63.477} & \num{63.883}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{66.429} & \num{66.254} & \textcolor{red}{\num{65.431}} & \num{65.62} & \num{65.741}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{84.254} & \num{83.299} & \num{82.125} & \textcolor{red}{\num{81.936}} & \num{81.939}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{84.933} & \num{84.999} & \textcolor{red}{\num{82.816}} & \num{83.005} & \num{83.007}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{58.965} & \textcolor{red}{\num{57.88}} & \num{59.806} & \num{59.896} & \num{59.898}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{81.472} & \num{82.091} & \textcolor{red}{\num{79.624}} & \num{80.755} & \num{80.755}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{57.446}} & \num{57.854} & \num{57.523} & \num{57.826} & \num{57.98}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{77.864} & \num{77.473} & \num{76.18} & \num{76.091} & \textcolor{red}{\num{76.081}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{79.522}} & \num{79.997} & \num{79.902} & \num{79.944} & \num{79.948}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{72.951} & \num{72.706} & \textcolor{red}{\num{71.852}} & \num{72.061} & \num{72.137}\\*
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
     & asset01 & \num{0.991} & \num{1.012} & \num{0.976} & \num{0.975} & \textcolor{red}{\num{0.971}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.014} & \num{1.031} & \num{0.986} & \textcolor{red}{\num{0.984}} & \num{0.986}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.437} & \num{1.456} & \num{1.435} & \textcolor{red}{\num{1.435}} & \num{1.435}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.659} & \num{1.65} & \num{1.612} & \textcolor{red}{\num{1.609}} & \num{1.609}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.762}} & \num{0.774} & \num{0.771} & \num{0.772} & \num{0.773}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.516} & \textcolor{red}{\num{1.426}} & \num{1.494} & \num{1.49} & \num{1.491}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.729} & \num{0.715} & \num{0.714} & \num{0.714} & \textcolor{red}{\num{0.712}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.286} & \num{1.283} & \num{1.256} & \num{1.253} & \textcolor{red}{\num{1.252}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.398} & \num{1.415} & \num{1.397} & \num{1.396} & \textcolor{red}{\num{1.396}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.199} & \num{1.196} & \num{1.182} & \num{1.181} & \textcolor{red}{\num{1.18}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.774} & \num{0.772} & \num{0.761} & \textcolor{red}{\num{0.761}} & \num{0.762}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.796} & \num{0.808} & \num{0.789} & \textcolor{red}{\num{0.788}} & \num{0.79}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.965} & \num{0.977} & \num{0.962} & \textcolor{red}{\num{0.962}} & \num{0.962}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.029} & \num{1.019} & \num{1.005} & \textcolor{red}{\num{1.004}} & \num{1.005}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.704} & \textcolor{red}{\num{0.699}} & \num{0.709} & \num{0.71} & \num{0.71}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.98} & \textcolor{red}{\num{0.964}} & \num{0.968} & \num{0.967} & \num{0.967}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.687} & \textcolor{red}{\num{0.661}} & \num{0.671} & \num{0.67} & \num{0.669}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.915} & \num{0.914} & \num{0.906} & \num{0.905} & \textcolor{red}{\num{0.905}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.964}} & \num{0.982} & \num{0.969} & \num{0.969} & \num{0.969}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.868} & \num{0.866} & \num{0.86} & \textcolor{red}{\num{0.859}} & \num{0.86}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{130.411} & \num{156.877} & \textcolor{red}{\num{98.259}} & \num{103.051} & \num{129.605}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{102.323} & \num{121.225} & \num{99.556} & \textcolor{red}{\num{98.425}} & \num{100.609}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{103.51} & \num{115.392} & \textcolor{red}{\num{100.091}} & \num{100.392} & \num{100.716}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{123.504} & \num{113.575} & \textcolor{red}{\num{99.159}} & \num{99.765} & \num{99.416}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{110.667} & \num{110.304} & \textcolor{red}{\num{99.196}} & \num{100.117} & \num{100.483}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{104.265} & \num{140.76} & \num{100.553} & \textcolor{red}{\num{99.859}} & \num{100.885}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{367.37} & \num{362.924} & \num{110.509} & \textcolor{red}{\num{100.598}} & \num{122.155}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{104.201} & \num{105.57} & \num{100.358} & \textcolor{red}{\num{100.07}} & \num{100.541}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{101.403} & \num{120.907} & \textcolor{red}{\num{99.532}} & \num{100.347} & \num{100.42}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{138.628} & \num{149.726} & \num{100.801} & \textcolor{red}{\num{100.292}} & \num{106.092}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{64.8} & \num{64.874} & \num{63.802} & \textcolor{red}{\num{63.801}} & \num{63.907}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{67.148} & \num{68.086} & \num{66.592} & \textcolor{red}{\num{66.503}} & \num{66.657}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{80.798} & \num{82.024} & \num{80.507} & \textcolor{red}{\num{80.503}} & \num{80.543}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{87.9} & \num{87.076} & \num{85.998} & \textcolor{red}{\num{85.923}} & \num{85.929}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{58.376} & \textcolor{red}{\num{58.145}} & \num{58.847} & \num{58.915} & \num{58.936}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{82.774} & \textcolor{red}{\num{81.11}} & \num{81.558} & \num{81.47} & \num{81.519}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{57.891} & \textcolor{red}{\num{55.684}} & \num{56.35} & \num{56.257} & \num{56.173}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{76.606} & \num{76.619} & \num{75.789} & \num{75.692} & \textcolor{red}{\num{75.66}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{81.062}} & \num{82.536} & \num{81.506} & \num{81.549} & \num{81.546}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{73.039} & \num{72.906} & \num{72.328} & \textcolor{red}{\num{72.29}} & \num{72.319}\\*
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
     & asset01 & \num{0.978} & \num{0.99} & \num{0.977} & \num{0.977} & \textcolor{red}{\num{0.977}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.994}} & \num{1.046} & \num{0.996} & \num{0.996} & \num{0.996}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.348} & \num{1.362} & \num{1.348} & \num{1.348} & \textcolor{red}{\num{1.348}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.688} & \num{1.73} & \num{1.682} & \num{1.682} & \textcolor{red}{\num{1.682}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.678}} & \num{0.681} & \num{0.679} & \num{0.679} & \num{0.679}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.507} & \textcolor{red}{\num{1.489}} & \num{1.504} & \num{1.504} & \num{1.504}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{0.667}} & \num{0.674} & \num{0.67} & \num{0.67} & \num{0.67}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.254} & \num{1.285} & \num{1.254} & \textcolor{red}{\num{1.254}} & \num{1.254}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{1.441}} & \num{1.443} & \num{1.444} & \num{1.444} & \num{1.444}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.173} & \num{1.189} & \num{1.173} & \num{1.173} & \textcolor{red}{\num{1.173}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{0.75}} & \num{0.754} & \num{0.751} & \num{0.751} & \num{0.751}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.792}} & \num{0.813} & \num{0.793} & \num{0.793} & \num{0.793}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.927} & \num{0.928} & \num{0.927} & \num{0.927} & \textcolor{red}{\num{0.927}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.035} & \num{1.054} & \textcolor{red}{\num{1.033}} & \num{1.033} & \num{1.033}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.679} & \textcolor{red}{\num{0.677}} & \num{0.68} & \num{0.68} & \num{0.68}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.97} & \num{0.976} & \textcolor{red}{\num{0.969}} & \num{0.969} & \num{0.969}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.639} & \textcolor{red}{\num{0.636}} & \num{0.64} & \num{0.64} & \num{0.64}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{0.897}} & \num{0.917} & \num{0.898} & \num{0.898} & \num{0.898}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.987}} & \num{0.99} & \num{0.988} & \num{0.988} & \num{0.988}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{0.853}} & \num{0.861} & \num{0.853} & \num{0.853} & \num{0.853}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{100.505} & \num{121.325} & \num{100.014} & \textcolor{red}{\num{99.84}} & \num{100.392}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{98.274}} & \num{119.42} & \num{99.987} & \num{100.019} & \num{99.925}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{100.134} & \textcolor{red}{\num{99.978}} & \num{100.001} & \num{99.982} & \num{100.026}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{100.013} & \num{106.053} & \textcolor{red}{\num{99.998}} & \num{100.035} & \num{100.025}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{100.094} & \num{112.376} & \num{99.994} & \num{99.852} & \textcolor{red}{\num{99.823}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{99.313}} & \num{115.961} & \num{99.997} & \num{99.998} & \num{100.004}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{126.85} & \num{283.922} & \textcolor{red}{\num{99.989}} & \num{100.799} & \num{101.023}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{99.989}} & \num{108.69} & \num{100.001} & \num{100.038} & \num{100.034}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{100.005} & \num{108.58} & \textcolor{red}{\num{99.999}} & \num{100.02} & \num{100.049}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{102.797} & \num{130.7} & \textcolor{red}{\num{99.998}} & \num{100.065} & \num{100.145}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{64.025}} & \num{64.276} & \num{64.096} & \num{64.093} & \num{64.085}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{65.73}} & \num{67.318} & \num{65.867} & \num{65.871} & \num{65.868}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{78.084} & \textcolor{red}{\num{77.993}} & \num{78.065} & \num{78.064} & \num{78.059}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{86.374} & \num{87.774} & \textcolor{red}{\num{86.194}} & \num{86.199} & \num{86.197}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{56.818} & \textcolor{red}{\num{56.658}} & \num{56.894} & \num{56.891} & \num{56.891}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{82.481} & \num{83.224} & \textcolor{red}{\num{82.408}} & \num{82.415} & \num{82.416}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{54.116} & \textcolor{red}{\num{53.952}} & \num{54.179} & \num{54.176} & \num{54.177}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{74.857}} & \num{76.706} & \num{74.874} & \num{74.878} & \num{74.878}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{82.916}} & \num{83.073} & \num{82.964} & \num{82.961} & \num{82.965}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{71.711}} & \num{72.33} & \num{71.727} & \num{71.728} & \num{71.726}\\*
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
     & asset01 & \textcolor{red}{\num{1.111}} & \num{1.187} & \num{1.132} & \num{1.132} & \num{1.132}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.665} & \num{1.604} & \num{1.475} & \num{1.475} & \textcolor{red}{\num{1.475}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.597} & \num{1.603} & \num{1.575} & \num{1.575} & \textcolor{red}{\num{1.568}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.991} & \textcolor{red}{\num{0.974}} & \num{0.988} & \num{0.988} & \num{0.987}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{1.023} & \num{0.983} & \textcolor{red}{\num{0.944}} & \num{0.945} & \num{0.945}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.346} & \num{1.344} & \num{1.31} & \textcolor{red}{\num{1.309}} & \num{1.309}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1.254} & \num{1.27} & \num{1.144} & \textcolor{red}{\num{1.144}} & \num{1.144}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.437} & \num{1.441} & \num{1.329} & \textcolor{red}{\num{1.327}} & \num{1.327}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.065} & \textcolor{red}{\num{1.034}} & \num{1.036} & \num{1.036} & \num{1.036}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{1.021} & \num{1.053} & \textcolor{red}{\num{0.972}} & \num{0.972} & \num{0.976}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{1.712} & \num{1.534} & \num{1.465} & \num{1.465} & \textcolor{red}{\num{1.465}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.809} & \num{0.778} & \num{0.775} & \num{0.775} & \textcolor{red}{\num{0.775}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.253} & \num{1.233} & \num{1.179} & \num{1.179} & \textcolor{red}{\num{1.178}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{0.858}} & \num{0.874} & \num{0.859} & \num{0.859} & \num{0.859}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.05} & \num{1.013} & \num{0.99} & \num{0.99} & \textcolor{red}{\num{0.99}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.967} & \num{0.956} & \num{0.939} & \num{0.939} & \textcolor{red}{\num{0.936}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.797} & \textcolor{red}{\num{0.791}} & \num{0.805} & \num{0.805} & \num{0.804}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.799} & \num{0.791} & \textcolor{red}{\num{0.778}} & \num{0.779} & \num{0.779}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.922} & \num{0.903} & \num{0.902} & \textcolor{red}{\num{0.901}} & \num{0.901}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.893} & \num{0.904} & \textcolor{red}{\num{0.86}} & \num{0.86} & \num{0.86}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.963} & \num{0.961} & \num{0.917} & \textcolor{red}{\num{0.916}} & \num{0.916}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.813} & \textcolor{red}{\num{0.789}} & \num{0.795} & \num{0.795} & \num{0.795}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.799} & \num{0.811} & \num{0.779} & \num{0.779} & \textcolor{red}{\num{0.777}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{1.02} & \textcolor{red}{\num{0.976}} & \num{0.977} & \num{0.977} & \num{0.977}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.742} & \num{0.718} & \num{0.709} & \num{0.709} & \textcolor{red}{\num{0.709}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.885} & \num{0.874} & \num{0.859} & \num{0.859} & \textcolor{red}{\num{0.859}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{116.823} & \num{126.187} & \textcolor{red}{\num{100}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{133.433} & \num{110.803} & \num{100} & \num{100} & \textcolor{red}{\num{100}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{136.961} & \num{137.368} & \num{100} & \num{100} & \textcolor{red}{\num{99.718}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{481.211} & \num{162.415} & \num{130.678} & \num{125.583} & \textcolor{red}{\num{123.491}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{158.918} & \num{137.623} & \textcolor{red}{\num{100.231}} & \num{100.506} & \num{100.508}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{121.62} & \num{127.348} & \textcolor{red}{\num{99.936}} & \num{99.966} & \num{99.966}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{231.241} & \num{162.648} & \textcolor{red}{\num{100}} & \num{100} & \num{100.062}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{121.088} & \num{114.279} & \num{100} & \textcolor{red}{\num{99.551}} & \num{99.553}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{324.139} & \num{298.93} & \textcolor{red}{\num{100}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{136.068} & \num{120.081} & \num{100} & \num{100} & \textcolor{red}{\num{98.463}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{127.535} & \num{135.547} & \textcolor{red}{\num{100}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{203.77} & \num{163.775} & \num{100} & \num{100} & \textcolor{red}{\num{100}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{191.067} & \num{149.75} & \num{102.57} & \num{102.134} & \textcolor{red}{\num{101.813}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{70.738}} & \num{72.221} & \num{71.052} & \num{71.052} & \num{71.052}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{86.33} & \num{83.265} & \num{81.56} & \num{81.56} & \textcolor{red}{\num{81.56}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{79.567} & \num{78.711} & \num{76.991} & \num{76.991} & \textcolor{red}{\num{76.685}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{66.284} & \textcolor{red}{\num{65.762}} & \num{67.025} & \num{67.03} & \num{66.999}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{66.361} & \num{65.915} & \textcolor{red}{\num{64.664}} & \num{64.742} & \num{64.743}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{76.806} & \num{75.196} & \num{75.171} & \textcolor{red}{\num{75.13}} & \num{75.13}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{73.847} & \num{74.521} & \textcolor{red}{\num{71.034}} & \num{71.034} & \num{71.035}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{80.054} & \num{79.799} & \num{76.134} & \textcolor{red}{\num{76.038}} & \num{76.039}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{67.335} & \textcolor{red}{\num{65.696}} & \num{65.979} & \num{65.979} & \num{65.979}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{66.053} & \num{67.186} & \num{64.451} & \num{64.451} & \textcolor{red}{\num{64.265}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{84.726} & \textcolor{red}{\num{81.079}} & \num{81.399} & \num{81.399} & \num{81.399}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{61.447} & \num{59.355} & \num{58.673} & \num{58.673} & \textcolor{red}{\num{58.673}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{73.296} & \num{72.392} & \num{71.178} & \num{71.173} & \textcolor{red}{\num{71.13}}\\*
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
     & asset01 & \textcolor{red}{\num{1.043}} & \num{1.132} & \num{1.055} & \num{1.055} & \num{1.055}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.551} & \num{1.567} & \num{1.515} & \num{1.515} & \textcolor{red}{\num{1.515}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.55} & \num{1.594} & \num{1.538} & \num{1.538} & \textcolor{red}{\num{1.537}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.048} & \textcolor{red}{\num{1.01}} & \num{1.024} & \num{1.024} & \num{1.024}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.922}} & \num{0.944} & \num{0.943} & \num{0.943} & \num{0.943}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.33} & \num{1.378} & \num{1.312} & \textcolor{red}{\num{1.312}} & \num{1.312}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1.178} & \num{1.191} & \num{1.143} & \num{1.143} & \textcolor{red}{\num{1.143}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.417} & \num{1.407} & \num{1.367} & \num{1.367} & \textcolor{red}{\num{1.367}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.05} & \textcolor{red}{\num{1.023}} & \num{1.062} & \num{1.062} & \num{1.062}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.974} & \num{1.015} & \textcolor{red}{\num{0.969}} & \num{0.969} & \num{0.973}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{1.523} & \num{1.488} & \num{1.452} & \num{1.452} & \textcolor{red}{\num{1.452}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.784} & \num{0.775} & \num{0.764} & \num{0.764} & \textcolor{red}{\num{0.764}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.197} & \num{1.21} & \num{1.179} & \textcolor{red}{\num{1.179}} & \num{1.179}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.851} & \num{0.869} & \num{0.843} & \textcolor{red}{\num{0.843}} & \num{0.843}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.015} & \num{1.009} & \num{1.006} & \num{1.006} & \textcolor{red}{\num{1.006}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.926} & \num{0.943} & \num{0.926} & \num{0.926} & \textcolor{red}{\num{0.925}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.835} & \textcolor{red}{\num{0.811}} & \num{0.823} & \num{0.823} & \num{0.823}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.767}} & \num{0.769} & \num{0.771} & \num{0.771} & \num{0.771}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.894}} & \num{0.91} & \num{0.899} & \num{0.899} & \num{0.899}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.866} & \num{0.867} & \num{0.856} & \num{0.856} & \textcolor{red}{\num{0.856}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.932} & \num{0.945} & \textcolor{red}{\num{0.931}} & \num{0.931} & \num{0.931}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.801} & \textcolor{red}{\num{0.783}} & \num{0.807} & \num{0.807} & \num{0.807}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.77}} & \num{0.787} & \num{0.772} & \num{0.772} & \num{0.774}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.984} & \textcolor{red}{\num{0.969}} & \num{0.975} & \num{0.975} & \num{0.975}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.708} & \num{0.7} & \num{0.697} & \textcolor{red}{\num{0.697}} & \num{0.697}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.862} & \num{0.864} & \textcolor{red}{\num{0.859}} & \num{0.859} & \num{0.859}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{107.44} & \num{114.074} & \num{100} & \textcolor{red}{\num{100}} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{110.34} & \num{100.627} & \num{100} & \textcolor{red}{\num{100}} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{104.114} & \num{110.956} & \num{100} & \num{100} & \textcolor{red}{\num{99.898}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{190.748} & \num{284.332} & \num{100} & \textcolor{red}{\num{100}} & \num{101.246}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{117.981} & \num{102.74} & \num{100} & \textcolor{red}{\num{100}} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{108.806} & \num{127.244} & \textcolor{red}{\num{100}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{144.841} & \num{122.484} & \num{100} & \textcolor{red}{\num{100}} & \num{100.049}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{103.209} & \num{103.952} & \textcolor{red}{\num{100}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{129.431} & \num{160.85} & \textcolor{red}{\num{100}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{102.747} & \num{104.302} & \textcolor{red}{\num{100}} & \num{100} & \num{100.956}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{103.212} & \num{111.216} & \num{100} & \num{100} & \textcolor{red}{\num{100}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{126.142} & \num{109.483} & \textcolor{red}{\num{100}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{120.751} & \num{129.355} & \num{100} & \textcolor{red}{\num{100}} & \num{100.179}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{70.716} & \num{72.219} & \num{70.143} & \textcolor{red}{\num{70.143}} & \num{70.143}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{83.565} & \num{83.137} & \num{82.867} & \num{82.867} & \textcolor{red}{\num{82.867}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{75.849} & \num{77.14} & \num{75.794} & \num{75.794} & \textcolor{red}{\num{75.754}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{68.569} & \textcolor{red}{\num{66.505}} & \num{67.64} & \num{67.64} & \num{67.611}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{63.721}} & \num{63.883} & \num{64.114} & \num{64.114} & \num{64.114}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{74.006}} & \num{75.386} & \num{74.547} & \num{74.547} & \num{74.547}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{71.565} & \num{71.71} & \num{70.579} & \num{70.579} & \textcolor{red}{\num{70.579}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{78.439} & \num{79.443} & \textcolor{red}{\num{78.15}} & \num{78.15} & \num{78.15}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{66.482} & \textcolor{red}{\num{65.194}} & \num{66.975} & \num{66.975} & \num{66.975}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{62.95}} & \num{64.422} & \num{63.247} & \num{63.247} & \num{63.374}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{81.657} & \textcolor{red}{\num{80.466}} & \num{80.779} & \num{80.779} & \num{80.779}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{58.415} & \num{57.686} & \num{57.499} & \textcolor{red}{\num{57.499}} & \num{57.499}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{71.328} & \num{71.433} & \textcolor{red}{\num{71.028}} & \num{71.028} & \num{71.033}\\*
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
     & asset01 & \num{1.092} & \num{1.135} & \textcolor{red}{\num{1.09}} & \num{1.09} & \num{1.09}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.525} & \num{1.549} & \textcolor{red}{\num{1.515}} & \num{1.515} & \num{1.515}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1.545}} & \num{1.589} & \num{1.545} & \num{1.545} & \num{1.545}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.008} & \textcolor{red}{\num{0.99}} & \num{1.013} & \num{1.013} & \num{1.013}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.836} & \textcolor{red}{\num{0.828}} & \num{0.833} & \num{0.833} & \num{0.833}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{1.368}} & \num{1.446} & \num{1.37} & \num{1.37} & \num{1.37}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1.023} & \num{1.053} & \textcolor{red}{\num{1.019}} & \num{1.019} & \num{1.019}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.367} & \num{1.387} & \textcolor{red}{\num{1.362}} & \num{1.362} & \num{1.362}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.102} & \num{1.119} & \textcolor{red}{\num{1.1}} & \num{1.1} & \num{1.1}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.965}} & \num{0.985} & \num{0.967} & \num{0.967} & \num{0.967}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{1.448}} & \num{1.46} & \num{1.451} & \num{1.451} & \num{1.451}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.746} & \num{0.76} & \textcolor{red}{\num{0.744}} & \num{0.744} & \num{0.744}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.169} & \num{1.192} & \num{1.167} & \num{1.167} & \textcolor{red}{\num{1.167}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.865} & \num{0.872} & \textcolor{red}{\num{0.863}} & \num{0.863} & \num{0.863}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1} & \num{1.003} & \textcolor{red}{\num{0.996}} & \num{0.996} & \num{0.996}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.937} & \num{0.94} & \num{0.936} & \num{0.936} & \textcolor{red}{\num{0.936}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.811} & \textcolor{red}{\num{0.808}} & \num{0.815} & \num{0.815} & \num{0.815}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.733} & \textcolor{red}{\num{0.724}} & \num{0.732} & \num{0.732} & \num{0.732}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.924}} & \num{0.941} & \num{0.925} & \num{0.925} & \num{0.925}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.821} & \num{0.836} & \num{0.82} & \num{0.82} & \textcolor{red}{\num{0.82}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.923} & \num{0.933} & \textcolor{red}{\num{0.921}} & \num{0.921} & \num{0.921}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.826} & \num{0.824} & \num{0.824} & \textcolor{red}{\num{0.824}} & \num{0.824}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.769}} & \num{0.771} & \num{0.77} & \num{0.77} & \num{0.77}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.976} & \textcolor{red}{\num{0.969}} & \num{0.975} & \num{0.975} & \num{0.975}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.69} & \num{0.698} & \textcolor{red}{\num{0.69}} & \num{0.69} & \num{0.69}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.856} & \num{0.86} & \num{0.856} & \num{0.856} & \textcolor{red}{\num{0.856}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{100.545} & \num{103.538} & \textcolor{red}{\num{100}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{100.704} & \num{102.565} & \textcolor{red}{\num{100}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{100.229} & \num{103.986} & \num{100} & \num{100} & \textcolor{red}{\num{99.999}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{103.593} & \num{276.214} & \num{100} & \num{100} & \textcolor{red}{\num{99.988}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{102.473} & \textcolor{red}{\num{98.691}} & \num{100} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{99.408}} & \num{106.726} & \num{100} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{101.676} & \num{120.157} & \num{100} & \textcolor{red}{\num{100}} & \num{100.028}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{100.069} & \num{101.998} & \textcolor{red}{\num{100}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{101.406} & \num{143.388} & \num{100} & \num{100} & \textcolor{red}{\num{100}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{99.054}} & \num{101.959} & \num{100} & \num{100} & \num{99.994}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{101.298} & \num{100.952} & \num{100} & \textcolor{red}{\num{100}} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{101.186} & \num{126.249} & \textcolor{red}{\num{100}} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{100.97} & \num{123.869} & \num{100} & \textcolor{red}{\num{100}} & \num{100.001}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{71.089} & \num{71.59} & \textcolor{red}{\num{70.934}} & \num{70.934} & \num{70.934}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{82.666} & \num{82.941} & \textcolor{red}{\num{82.362}} & \num{82.362} & \num{82.362}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{77.896} & \num{78.079} & \num{77.829} & \num{77.829} & \textcolor{red}{\num{77.829}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{67.851} & \textcolor{red}{\num{67.5}} & \num{68.104} & \num{68.104} & \num{68.104}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{60.534} & \textcolor{red}{\num{59.836}} & \num{60.464} & \num{60.464} & \num{60.464}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{75.514}} & \num{76.816} & \num{75.644} & \num{75.644} & \num{75.644}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{68.041} & \num{69.472} & \num{68.016} & \num{68.016} & \textcolor{red}{\num{68.015}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{76.376} & \num{77.274} & \textcolor{red}{\num{76.262}} & \num{76.262} & \num{76.262}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{67.56} & \num{67.456} & \num{67.381} & \textcolor{red}{\num{67.381}} & \num{67.381}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{63.285}} & \num{63.492} & \num{63.376} & \num{63.376} & \num{63.376}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{81.547} & \textcolor{red}{\num{80.818}} & \num{81.499} & \num{81.499} & \num{81.499}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{57.033} & \num{57.698} & \textcolor{red}{\num{57.021}} & \num{57.021} & \num{57.021}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{70.783} & \num{71.081} & \num{70.741} & \num{70.741} & \textcolor{red}{\num{70.741}}\\*
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
    MSE & \num{0.937} & \num{0.945} & \textcolor{red}{\num{0.922}} & \num{0.931} & \num{0.932}\\
    MAE & \num{0.77} & \num{0.768} & \textcolor{red}{\num{0.759}} & \num{0.761} & \num{0.76}\\
    MAPE & \num{302.516} & \num{287.241} & \num{263.124} & \textcolor{red}{\num{247.706}} & \num{247.782}\\
    MASE & \num{63.986} & \num{63.826} & \textcolor{red}{\num{63.124}} & \num{63.225} & \num{63.214}\\
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
    MSE & \num{0.961} & \num{0.972} & \textcolor{red}{\num{0.957}} & \num{0.957} & \num{0.957}\\
    MAE & \num{0.77} & \num{0.775} & \num{0.765} & \num{0.764} & \textcolor{red}{\num{0.764}}\\
    MAPE & \num{112.672} & \num{111.457} & \num{103.815} & \num{100.867} & \textcolor{red}{\num{100.513}}\\
    MASE & \num{63.932} & \num{64.343} & \num{63.511} & \num{63.442} & \textcolor{red}{\num{63.405}}\\
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
    MSE & \textcolor{red}{\num{0.941}} & \num{0.952} & \num{0.942} & \num{0.942} & \num{0.942}\\
    MAE & \textcolor{red}{\num{0.762}} & \num{0.768} & \num{0.762} & \num{0.762} & \num{0.762}\\
    MAPE & \num{99.994} & \num{113.747} & \textcolor{red}{\num{99.964}} & \num{100.382} & \num{100.815}\\
    MASE & \textcolor{red}{\num{63.743}} & \num{64.23} & \num{63.749} & \num{63.747} & \num{63.745}\\
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
    MSE & \num{1.186} & \num{1.171} & \textcolor{red}{\num{1.148}} & \num{1.154} & \num{1.155}\\
    MAE & \num{0.868} & \num{0.864} & \textcolor{red}{\num{0.853}} & \num{0.856} & \num{0.856}\\
    MAPE & \num{262.242} & \num{233.079} & \textcolor{red}{\num{168.351}} & \num{178.005} & \num{184.459}\\
    MASE & \num{72.951} & \num{72.706} & \textcolor{red}{\num{71.852}} & \num{72.061} & \num{72.137}\\
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
    MSE & \num{1.199} & \num{1.196} & \num{1.182} & \num{1.181} & \textcolor{red}{\num{1.18}}\\
    MAE & \num{0.868} & \num{0.866} & \num{0.86} & \textcolor{red}{\num{0.859}} & \num{0.86}\\
    MAPE & \num{138.628} & \num{149.726} & \num{100.801} & \textcolor{red}{\num{100.292}} & \num{106.092}\\
    MASE & \num{73.039} & \num{72.906} & \num{72.328} & \textcolor{red}{\num{72.29}} & \num{72.319}\\
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
    MSE & \num{1.173} & \num{1.189} & \num{1.173} & \num{1.173} & \textcolor{red}{\num{1.173}}\\
    MAE & \textcolor{red}{\num{0.853}} & \num{0.861} & \num{0.853} & \num{0.853} & \num{0.853}\\
    MAPE & \num{102.797} & \num{130.7} & \textcolor{red}{\num{99.998}} & \num{100.065} & \num{100.145}\\
    MASE & \textcolor{red}{\num{71.711}} & \num{72.33} & \num{71.727} & \num{71.728} & \num{71.726}\\
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
    MSE & \num{1.253} & \num{1.233} & \num{1.179} & \num{1.179} & \textcolor{red}{\num{1.178}}\\
    MAE & \num{0.885} & \num{0.874} & \num{0.859} & \num{0.859} & \textcolor{red}{\num{0.859}}\\
    MAPE & \num{191.067} & \num{149.75} & \num{102.57} & \num{102.134} & \textcolor{red}{\num{101.813}}\\
    MASE & \num{73.296} & \num{72.392} & \num{71.178} & \num{71.173} & \textcolor{red}{\num{71.13}}\\
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
    MSE & \num{1.197} & \num{1.21} & \num{1.179} & \textcolor{red}{\num{1.179}} & \num{1.179}\\
    MAE & \num{0.862} & \num{0.864} & \textcolor{red}{\num{0.859}} & \num{0.859} & \num{0.859}\\
    MAPE & \num{120.751} & \num{129.355} & \num{100} & \textcolor{red}{\num{100}} & \num{100.179}\\
    MASE & \num{71.328} & \num{71.433} & \textcolor{red}{\num{71.028}} & \num{71.028} & \num{71.033}\\
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
    MSE & \num{1.169} & \num{1.192} & \num{1.167} & \num{1.167} & \textcolor{red}{\num{1.167}}\\
    MAE & \num{0.856} & \num{0.86} & \num{0.856} & \num{0.856} & \textcolor{red}{\num{0.856}}\\
    MAPE & \num{100.97} & \num{123.869} & \num{100} & \textcolor{red}{\num{100}} & \num{100.001}\\
    MASE & \num{70.783} & \num{71.081} & \num{70.741} & \num{70.741} & \textcolor{red}{\num{70.741}}\\
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
