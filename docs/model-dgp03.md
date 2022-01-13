Simulating VAR-type Minnesota BVHAR
================
Young Geun Kim
12 Jan, 2022

-   [BVHAR Coefficient](#bvhar-coefficient)
    -   [VAR-type Minnesota prior](#var-type-minnesota-prior)
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
    -   [Relative Error](#relative-error)
    -   [Piecewise Errors](#piecewise-errors)
        -   [SMALL](#small-1)
        -   [MEDIUM](#medium-1)
        -   [LARGE](#large-1)
        -   [Average](#average)
-   [Coefficients](#coefficients)

``` r
sim_data <- "../data/processed/bvharsim_dgp_s.rds"
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

# BVHAR Coefficient

## VAR-type Minnesota prior

``` r
n_small <- length(bvhar_small_spec$sigma)
n_medium <- length(bvhar_medium_spec$sigma)
n_large <- length(bvhar_large_spec$sigma)
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

<img src="../output/figs/DGP-3-smallplot-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-3-medplot-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-3-largeplot-1.png" width="70%" style="display: block; margin: auto;" />

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
bvar_small_spec <- set_bvar(
  sigma = bvhar_small_spec$sigma,
  lambda = bvhar_small_spec$lambda,
  delta = bvhar_small_spec$delta
)
#----------------------------
bvar_medium_spec <- set_bvar(
  sigma = bvhar_medium_spec$sigma,
  lambda = bvhar_medium_spec$lambda,
  delta = bvhar_medium_spec$delta
)
#----------------------------
bvar_large_spec <- set_bvar(
  sigma = bvhar_large_spec$sigma,
  lambda = bvhar_large_spec$lambda,
  delta = bvhar_large_spec$delta
)
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
#> [1]  2.38  1.85  2.84
#> 
#> Setting for 'lambda':
#> [1]  0.134
#> 
#> Setting for 'delta':
#> [1]  0.845  0.202  0.976
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
#> [1]  2.59  3.24  3.69  4.06  3.11  4.51  2.60  3.67  3.82
#> 
#> Setting for 'lambda':
#> [1]  0.128
#> 
#> Setting for 'delta':
#> [1]  0.4911  0.5739  0.0454  0.4623  0.9338  0.8030  0.3235  0.2900  0.5417
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
#>  [1]  4.17  3.90  4.15  4.21  3.91  4.02  4.39  3.91  3.75  3.40  4.20  3.52
#> 
#> Setting for 'lambda':
#> [1]  0.0246
#> 
#> Setting for 'delta':
#>  [1]  0.18968  0.37024  0.24504  0.45083  0.37815  0.35357  0.00537  0.55585
#>  [9]  0.15658  0.86306  0.74093  0.78909
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
(bvhar_var_small_optim <- choose_bvhar(
  bvhar_small_spec, 
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
#> [1]  2.44  1.73  2.86
#> 
#> Setting for 'lambda':
#> [1]  0.136
#> 
#> Setting for 'delta':
#> [1]  0.855  0.202  0.977
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
(bvhar_var_medium_optim <- choose_bvhar(
  bvhar_medium_spec, 
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
#> [1]  2.58  3.30  3.63  4.26  3.18  4.46  2.61  3.66  3.65
#> 
#> Setting for 'lambda':
#> [1]  0.13
#> 
#> Setting for 'delta':
#> [1]  0.4842  0.5887  0.0419  0.4601  0.9428  0.8087  0.3258  0.2881  0.5450
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
(bvhar_var_large_optim <- choose_bvhar(
  bvhar_large_spec, 
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
#>  [1]  4.17  3.89  4.17  4.24  3.88  4.00  4.42  3.84  3.74  3.39  4.22  3.52
#> 
#> Setting for 'lambda':
#> [1]  0.0218
#> 
#> Setting for 'delta':
#>  [1]  0.19383  0.36950  0.24559  0.44830  0.38315  0.34936  0.00571  0.55976
#>  [9]  0.16317  0.86462  0.74444  0.78996
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
  sigma = bvhar_small_spec$sigma,
  lambda = bvhar_small_spec$lambda,
  daily = bvhar_small_spec$delta,
  weekly = bvhar_small_spec$delta,
  monthly = bvhar_small_spec$delta
)
#-----------------------------------------
bvhar_vhar_medium_spec <- set_weight_bvhar(
  sigma = bvhar_medium_spec$sigma,
  lambda = bvhar_medium_spec$lambda,
  daily = bvhar_medium_spec$delta,
  weekly = bvhar_medium_spec$delta,
  monthly = bvhar_medium_spec$delta
)
#-----------------------------------------
bvhar_vhar_large_spec <- set_weight_bvhar(
  sigma = bvhar_large_spec$sigma,
  lambda = bvhar_large_spec$lambda,
  daily = bvhar_large_spec$delta,
  weekly = bvhar_large_spec$delta,
  monthly = bvhar_large_spec$delta
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
#> [1]  2.41  1.71  2.91
#> 
#> Setting for 'lambda':
#> [1]  0.133
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.851  0.202  0.952
#> 
#> Setting for 'weekly':
#> [1]  0.0174  0.0000  0.0982
#> 
#> Setting for 'monthly':
#> [1]  0.00000  0.00000  0.00478
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
#> [1]  2.56  3.28  3.61  4.18  3.31  4.44  2.59  3.71  3.66
#> 
#> Setting for 'lambda':
#> [1]  0.128
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.454  0.565  0.042  0.460  0.875  0.809  0.327  0.267  0.542
#> 
#> Setting for 'weekly':
#> [1]  0.178  0.119  0.000  0.000  0.206  0.000  0.000  0.160  0.000
#> 
#> Setting for 'monthly':
#> [1]  0.0000  0.0000  0.0229  0.0000  0.0000  0.0000  0.0000  0.0000  0.1065
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
#>  [1]  4.19  3.94  4.20  4.26  3.94  4.05  4.39  3.91  3.81  3.44  4.27  3.55
#> 
#> Setting for 'lambda':
#> [1]  0.0206
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#>  [1]  0.1934  0.3686  0.2283  0.4443  0.3826  0.3491  0.0027  0.5596  0.1631
#> [10]  0.8440  0.7303  0.7900
#> 
#> Setting for 'weekly':
#>  [1]  0.0000  0.0000  0.1214  0.0218  0.0000  0.0000  0.0000  0.0000  0.0000
#> [10]  0.0563  0.0471  0.0000
#> 
#> Setting for 'monthly':
#>  [1]  0.00  0.00  0.00  0.00  0.00  0.00  0.16  0.00  0.00  0.00  0.00  0.00
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
    \caption{\label{tab:empdgp3}Empirical Bayes Results for DGP3.}\\
    \toprule
     &    &     & y1 & y2 & y3 & y4 & y5 & y6 & y7 & y8 & y9 & y10 & y11 & y12\\
    \midrule
    \endfirsthead
    \caption[]{Empirical Bayes Results for DGP3. \textit{(continued)}}\\
    \toprule
      &    &     & y1 & y2 & y3 & y4 & y5 & y6 & y7 & y8 & y9 & y10 & y11 & y12\\
    \midrule
    \endhead

    \endfoot
    \bottomrule
    \endlastfoot
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{SMALL}}\\
    \hspace{1em} & BVAR & $\sigma$ & 2.377 & 1.849 & 2.845 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.134 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.845 & 0.202 & 0.976 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 2.442 & 1.734 & 2.859 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.136 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.855 & 0.202 & 0.977 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 2.409 & 1.706 & 2.908 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.133 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.851 & 0.202 & 0.952 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.017 & 0.000 & 0.098 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.000 & 0.000 & 0.005 &  &  &  &  &  &  &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{MEDIUM}}\\
    \hspace{1em} & BVAR & $\sigma$ & 2.591 & 3.241 & 3.685 & 4.061 & 3.114 & 4.509 & 2.601 & 3.674 & 3.820 &  &  & \\

    \hspace{1em}\hspace{1em} &  & $\lambda$ & 0.128 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.491 & 0.574 & 0.045 & 0.462 & 0.934 & 0.803 & 0.324 & 0.290 & 0.542 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 2.580 & 3.303 & 3.632 & 4.260 & 3.178 & 4.462 & 2.613 & 3.664 & 3.646 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.130 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.484 & 0.589 & 0.042 & 0.460 & 0.943 & 0.809 & 0.326 & 0.288 & 0.545 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 2.562 & 3.281 & 3.609 & 4.180 & 3.307 & 4.435 & 2.591 & 3.706 & 3.663 &  &  & \\

     &  & $\lambda$ & 0.128 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.454 & 0.565 & 0.042 & 0.460 & 0.875 & 0.809 & 0.327 & 0.267 & 0.542 &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.178 & 0.119 & 0.000 & 0.000 & 0.206 & 0.000 & 0.000 & 0.160 & 0.000 &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.000 & 0.000 & 0.023 & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.107 &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{LARGE}}\\
    \hspace{1em} & BVAR & $\sigma$ & 4.165 & 3.900 & 4.148 & 4.213 & 3.911 & 4.017 & 4.392 & 3.910 & 3.751 & 3.401 & 4.202 & 3.519\\

    \hspace{1em} &  & $\lambda$ & 0.025 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.190 & 0.370 & 0.245 & 0.451 & 0.378 & 0.354 & 0.005 & 0.556 & 0.157 & 0.863 & 0.741 & 0.789\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 4.168 & 3.885 & 4.174 & 4.236 & 3.878 & 4.003 & 4.421 & 3.844 & 3.745 & 3.392 & 4.218 & 3.521\\

    \hspace{1em} &  & $\lambda$ & 0.022 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.194 & 0.369 & 0.246 & 0.448 & 0.383 & 0.349 & 0.006 & 0.560 & 0.163 & 0.865 & 0.744 & 0.790\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 4.194 & 3.945 & 4.199 & 4.259 & 3.935 & 4.052 & 4.392 & 3.910 & 3.806 & 3.445 & 4.271 & 3.554\\

    \hspace{1em} &  & $\lambda$ & 0.021 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.193 & 0.369 & 0.228 & 0.444 & 0.383 & 0.349 & 0.003 & 0.560 & 0.163 & 0.844 & 0.730 & 0.790\\

    \hspace{1em} &  & $w_i$ & 0.000 & 0.000 & 0.121 & 0.022 & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.056 & 0.047 & 0.000\\

    \hspace{1em} &  & $m_i$ & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.160 & 0.000 & 0.000 & 0.000 & 0.000 & 0.000\\*
    \end{longtable}

# Errors

## Rolling Windows

``` r
# model lists-------------------------
mod_small_list <- list(
  fit_var_small,
  fit_vhar_small,
  fit_small_bvar,
  fit_bvhar_small_var,
  fit_bvhar_small_vhar
)
mod_medium_list <- list(
  fit_var_medium,
  fit_vhar_medium,
  fit_medium_bvar,
  fit_bvhar_medium_var,
  fit_bvhar_medium_vhar
)
mod_large_list <- list(
  fit_var_large,
  fit_vhar_large,
  fit_large_bvar,
  fit_bvhar_large_var,
  fit_bvhar_large_vhar
)
# SMALL-------------------------------
cv_small_list <- 
  parallel::mclapply(
    c(1, 5, 10, 20),
    function(h) {
      mod_small_list %>% 
        lapply(
          function(mod) {
            forecast_roll(mod, h, y_small_test)
          }
        )
    },
    mc.cores = 8
  )
# MEDIUM------------------------------
cv_medium_list <- 
  parallel::mclapply(
    c(1, 5, 10, 20),
    function(h) {
      mod_medium_list %>% 
        lapply(
          function(mod) {
            forecast_roll(mod, h, y_medium_test)
          }
        )
    },
    mc.cores = 8
  )
# LARGE-------------------------------
cv_large_list <- 
  parallel::mclapply(
    c(1, 5, 10, 20),
    function(h) {
      mod_large_list %>% 
        lapply(
          function(mod) {
            forecast_roll(mod, h, y_large_test)
          }
        )
    },
    mc.cores = 8
  )
```

## Relative Error

Set VAR as the benchmark model.

    \begin{table}[H]

    \caption{\label{tab:dgp3result}Out-of-sample forecasting performance measures for DGP3 with VAR(5) model as benchmark.}
    \centering
    \resizebox{\linewidth}{!}{
    \begin{tabular}[t]{cc|cccc|cccc|cccc|}
    \toprule
    \multicolumn{2}{c}{ } & \multicolumn{4}{c}{RMAFE} & \multicolumn{4}{c}{RMSFE} & \multicolumn{4}{c}{RMASE} \\
    \cmidrule(l{3pt}r{3pt}){3-6} \cmidrule(l{3pt}r{3pt}){7-10} \cmidrule(l{3pt}r{3pt}){11-14}
    \rotatebox{0}{} & \rotatebox{0}{} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 10$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 10$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 10$} & \rotatebox{0}{$h = 20$}\\
    \midrule
     & VHAR & \textcolor{black}{\num{.992}} & \textcolor{black}{\num{.996}} & \textcolor{black}{\num{1.038}} & \textcolor{black}{\num{1.104}} & \textcolor{black}{\num{.993}} & \textcolor{red}{\num{.965}} & \textcolor{black}{\num{1.031}} & \textcolor{black}{\num{1.228}} & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{.995}} & \textcolor{black}{\num{1.038}} & \textcolor{black}{\num{1.103}}\\

     & BVAR & \textcolor{black}{\num{.985}} & \textcolor{red}{\num{.989}} & \textcolor{red}{\num{.994}} & \textcolor{red}{\num{.973}} & \textcolor{red}{\num{.982}} & \textcolor{black}{\num{.984}} & \textcolor{red}{\num{.975}} & \textcolor{red}{\num{.937}} & \textcolor{black}{\num{.985}} & \textcolor{red}{\num{.989}} & \textcolor{red}{\num{.994}} & \textcolor{red}{\num{.973}}\\

     & BVHAR-S & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{1.004}} & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{.984}} & \textcolor{black}{\num{.978}} & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{.996}} & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{1.004}} & \textcolor{black}{\num{.996}}\\

    \multirow{-4}{*}{\centering\arraybackslash SMALL} & BVHAR-L & \textcolor{red}{\num{.984}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{1.007}} & \textcolor{black}{\num{1.007}} & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{.976}} & \textcolor{black}{\num{.993}} & \textcolor{black}{\num{1.022}} & \textcolor{red}{\num{.984}} & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{1.008}} & \textcolor{black}{\num{1.007}}\\
    \cmidrule{1-14}
     & VHAR & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{1.004}} & \textcolor{black}{\num{1.021}} & \textcolor{black}{\num{1.028}} & \textcolor{black}{\num{.987}} & \textcolor{black}{\num{.994}} & \textcolor{black}{\num{1.034}} & \textcolor{black}{\num{1.055}} & \textcolor{black}{\num{.995}} & \textcolor{black}{\num{1.004}} & \textcolor{black}{\num{1.021}} & \textcolor{black}{\num{1.028}}\\

     & BVAR & \textcolor{black}{\num{.981}} & \textcolor{black}{\num{.980}} & \textcolor{black}{\num{.986}} & \textcolor{red}{\num{.994}} & \textcolor{red}{\num{.961}} & \textcolor{red}{\num{.967}} & \textcolor{black}{\num{.971}} & \textcolor{red}{\num{.986}} & \textcolor{black}{\num{.981}} & \textcolor{black}{\num{.980}} & \textcolor{black}{\num{.986}} & \textcolor{black}{\num{.993}}\\

     & BVHAR-S & \textcolor{red}{\num{.981}} & \textcolor{red}{\num{.980}} & \textcolor{red}{\num{.983}} & \textcolor{black}{\num{.994}} & \textcolor{black}{\num{.963}} & \textcolor{black}{\num{.969}} & \textcolor{red}{\num{.968}} & \textcolor{black}{\num{.989}} & \textcolor{red}{\num{.981}} & \textcolor{red}{\num{.980}} & \textcolor{red}{\num{.982}} & \textcolor{red}{\num{.993}}\\

    \multirow{-4}{*}{\centering\arraybackslash MEDIUM} & BVHAR-L & \textcolor{black}{\num{.982}} & \textcolor{black}{\num{.983}} & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{.963}} & \textcolor{black}{\num{.973}} & \textcolor{black}{\num{.979}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.981}} & \textcolor{black}{\num{.984}} & \textcolor{black}{\num{.987}} & \textcolor{black}{\num{.999}}\\
    \cmidrule{1-14}
     & VHAR & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{1.019}} & \textcolor{black}{\num{1.021}} & \textcolor{black}{\num{1.016}} & \textcolor{black}{\num{.981}} & \textcolor{black}{\num{1.022}} & \textcolor{black}{\num{1.033}} & \textcolor{black}{\num{1.031}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{1.020}} & \textcolor{black}{\num{1.019}} & \textcolor{black}{\num{1.016}}\\

     & BVAR & \textcolor{black}{\num{.969}} & \textcolor{red}{\num{.983}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{.938}} & \textcolor{red}{\num{.953}} & \textcolor{black}{\num{.982}} & \textcolor{black}{\num{1.001}} & \textcolor{black}{\num{.970}} & \textcolor{red}{\num{.982}} & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{.999}}\\

     & BVHAR-S & \textcolor{black}{\num{.969}} & \textcolor{black}{\num{.983}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{.938}} & \textcolor{black}{\num{.953}} & \textcolor{black}{\num{.982}} & \textcolor{black}{\num{1.001}} & \textcolor{black}{\num{.970}} & \textcolor{black}{\num{.983}} & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{.999}}\\

    \multirow{-4}{*}{\centering\arraybackslash LARGE} & BVHAR-L & \textcolor{red}{\num{.969}} & \textcolor{black}{\num{.984}} & \textcolor{red}{\num{.989}} & \textcolor{red}{\num{.998}} & \textcolor{red}{\num{.937}} & \textcolor{black}{\num{.954}} & \textcolor{red}{\num{.982}} & \textcolor{red}{\num{1.001}} & \textcolor{red}{\num{.970}} & \textcolor{black}{\num{.983}} & \textcolor{red}{\num{.990}} & \textcolor{red}{\num{.999}}\\
    \bottomrule
    \end{tabular}}
    \end{table}

## Piecewise Errors

### SMALL

Plots

<img src="../output/figs/DGP-3-smallcvonefig-1.png" width="70%" style="display: block; margin: auto;" />

<img src="../output/figs/DGP-3-smallcvfivefig-1.png" width="70%" style="display: block; margin: auto;" />

<img src="../output/figs/DGP-3-smallcvtwentyfig-1.png" width="70%" style="display: block; margin: auto;" />

Tables

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
     & asset01 & \num{0.888} & \textcolor{red}{\num{0.837}} & \num{0.851} & \num{0.838} & \num{0.84}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.889} & \num{0.917} & \textcolor{red}{\num{0.874}} & \num{0.895} & \num{0.895}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.051} & \num{1.054} & \num{1.052} & \textcolor{red}{\num{1.05}} & \num{1.05}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.942} & \num{0.936} & \textcolor{red}{\num{0.926}} & \num{0.927} & \num{0.928}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.748} & \textcolor{red}{\num{0.727}} & \num{0.733} & \num{0.728} & \num{0.728}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.727} & \num{0.734} & \textcolor{red}{\num{0.714}} & \num{0.721} & \num{0.721}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.838} & \num{0.833} & \num{0.83} & \num{0.829} & \textcolor{red}{\num{0.826}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.771} & \num{0.765} & \num{0.759} & \num{0.759} & \textcolor{red}{\num{0.759}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{99.937}} & \num{110.038} & \num{105.998} & \num{111.933} & \num{112.035}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{193.86} & \num{209.744} & \num{187.767} & \num{184.189} & \textcolor{red}{\num{184.034}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{165.99} & \num{173.409} & \textcolor{red}{\num{164.07}} & \num{165} & \num{164.682}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{153.263} & \num{164.397} & \textcolor{red}{\num{152.612}} & \num{153.707} & \num{153.583}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{80.465} & \textcolor{red}{\num{78.147}} & \num{78.921} & \num{78.313} & \num{78.359}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{76.481} & \num{77.248} & \textcolor{red}{\num{75.06}} & \num{75.898} & \num{75.899}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{88.488} & \num{87.867} & \num{87.662} & \num{87.499} & \textcolor{red}{\num{87.283}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{81.811} & \num{81.087} & \num{80.548} & \num{80.57} & \textcolor{red}{\num{80.513}}\\*
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
     & asset01 & \num{3.595} & \textcolor{red}{\num{3.324}} & \num{3.538} & \num{3.494} & \num{3.492}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.079} & \num{1.092} & \textcolor{red}{\num{1.066}} & \num{1.071} & \num{1.071}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{4.508} & \num{4.449} & \num{4.43} & \num{4.416} & \textcolor{red}{\num{4.396}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{3.061} & \textcolor{red}{\num{2.955}} & \num{3.012} & \num{2.994} & \num{2.986}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{1.48} & \textcolor{red}{\num{1.424}} & \num{1.47} & \num{1.468} & \num{1.466}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.772} & \num{0.786} & \num{0.763} & \textcolor{red}{\num{0.762}} & \num{0.763}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.731} & \num{1.757} & \textcolor{red}{\num{1.707}} & \num{1.716} & \num{1.712}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{1.328} & \num{1.322} & \textcolor{red}{\num{1.313}} & \num{1.315} & \num{1.313}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{211.203} & \num{240.58} & \num{198.546} & \num{194.952} & \textcolor{red}{\num{194.056}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{161.773} & \num{182.969} & \textcolor{red}{\num{156.318}} & \num{157.004} & \num{157.424}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{346.238} & \num{303.46} & \num{307.517} & \textcolor{red}{\num{294.622}} & \num{303.345}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{239.738} & \num{242.336} & \num{220.794} & \textcolor{red}{\num{215.526}} & \num{218.275}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{158.351} & \textcolor{red}{\num{152.053}} & \num{157.162} & \num{156.964} & \num{156.707}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{82.432} & \num{83.969} & \num{81.526} & \textcolor{red}{\num{81.496}} & \num{81.518}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{184.129} & \num{186.963} & \textcolor{red}{\num{181.708}} & \num{182.626} & \num{182.239}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{141.637} & \num{140.995} & \textcolor{red}{\num{140.132}} & \num{140.362} & \num{140.155}\\*
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
     & asset01 & \num{3.609} & \textcolor{red}{\num{3.361}} & \num{3.657} & \num{3.63} & \num{3.592}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.045} & \textcolor{red}{\num{1.03}} & \num{1.055} & \num{1.054} & \num{1.055}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{8.37} & \num{9.038} & \textcolor{red}{\num{7.987}} & \num{8.147} & \num{8.284}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{4.341} & \num{4.476} & \textcolor{red}{\num{4.233}} & \num{4.277} & \num{4.31}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{1.571} & \textcolor{red}{\num{1.539}} & \num{1.584} & \num{1.586} & \num{1.58}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.742}} & \num{0.749} & \num{0.745} & \num{0.745} & \num{0.745}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{2.345} & \num{2.549} & \textcolor{red}{\num{2.303}} & \num{2.344} & \num{2.367}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{1.553} & \num{1.612} & \textcolor{red}{\num{1.544}} & \num{1.558} & \num{1.564}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{164.2} & \textcolor{red}{\num{108.56}} & \num{162.857} & \num{155.966} & \num{155.74}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{126.264} & \num{147.976} & \textcolor{red}{\num{123.996}} & \num{125.22} & \num{126.228}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{488.603} & \num{545.377} & \textcolor{red}{\num{463.39}} & \num{486.056} & \num{500.161}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{259.689} & \num{267.304} & \textcolor{red}{\num{250.081}} & \num{255.747} & \num{260.71}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{167.551} & \textcolor{red}{\num{164.113}} & \num{168.893} & \num{169.156} & \num{168.491}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{78.187}} & \num{78.845} & \num{78.555} & \num{78.536} & \num{78.627}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{248.438} & \num{269.98} & \textcolor{red}{\num{243.999}} & \num{248.479} & \num{250.883}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{164.726} & \num{170.98} & \textcolor{red}{\num{163.815}} & \num{165.39} & \num{166}\\*
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

<img src="../output/figs/DGP-3-medcvonefig-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-3-medcvfivefig-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-3-medcvtwentyfig-1.png" width="70%" style="display: block; margin: auto;" />

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
     & asset01 & \num{0.973} & \num{0.97} & \textcolor{red}{\num{0.941}} & \num{0.957} & \num{0.958}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.994} & \num{0.976} & \textcolor{red}{\num{0.93}} & \num{0.933} & \num{0.94}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.477} & \num{1.453} & \num{1.406} & \textcolor{red}{\num{1.404}} & \num{1.405}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.639} & \num{1.639} & \textcolor{red}{\num{1.557}} & \num{1.558} & \num{1.559}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.803} & \textcolor{red}{\num{0.788}} & \num{0.795} & \num{0.802} & \num{0.788}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.494} & \num{1.408} & \textcolor{red}{\num{1.402}} & \num{1.405} & \num{1.405}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.719} & \num{0.703} & \num{0.693} & \textcolor{red}{\num{0.693}} & \num{0.693}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.278} & \num{1.276} & \num{1.236} & \textcolor{red}{\num{1.227}} & \num{1.228}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{1.302}} & \num{1.324} & \num{1.305} & \num{1.309} & \num{1.31}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.186} & \num{1.171} & \textcolor{red}{\num{1.14}} & \num{1.143} & \num{1.143}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.771} & \num{0.763} & \textcolor{red}{\num{0.751}} & \num{0.757} & \num{0.76}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.797} & \num{0.809} & \textcolor{red}{\num{0.779}} & \num{0.779} & \num{0.783}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.999} & \num{0.987} & \num{0.968} & \textcolor{red}{\num{0.968}} & \num{0.968}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.018} & \num{1.012} & \textcolor{red}{\num{0.98}} & \num{0.981} & \num{0.981}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.714} & \textcolor{red}{\num{0.71}} & \num{0.719} & \num{0.724} & \num{0.719}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.967} & \num{0.957} & \num{0.939} & \textcolor{red}{\num{0.936}} & \num{0.936}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.688} & \num{0.678} & \num{0.681} & \num{0.678} & \textcolor{red}{\num{0.678}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.928} & \num{0.933} & \num{0.916} & \textcolor{red}{\num{0.911}} & \num{0.912}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.937}} & \num{0.947} & \num{0.94} & \num{0.941} & \num{0.941}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.869} & \num{0.866} & \num{0.853} & \textcolor{red}{\num{0.853}} & \num{0.853}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{202.69} & \num{191.213} & \num{185.303} & \textcolor{red}{\num{178.771}} & \num{179.423}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{267.542} & \num{351.578} & \num{259.873} & \textcolor{red}{\num{250.981}} & \num{276.669}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{149.097} & \num{136.566} & \num{118.923} & \num{118.746} & \textcolor{red}{\num{118.45}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{147.514} & \num{147.795} & \textcolor{red}{\num{145.77}} & \num{145.875} & \num{145.92}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{146.653} & \num{153.085} & \num{138.805} & \num{139.677} & \textcolor{red}{\num{138.565}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{265.329} & \num{254.303} & \num{247.287} & \num{243.445} & \textcolor{red}{\num{243.22}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{148.963} & \num{149.704} & \num{139.446} & \num{136.47} & \textcolor{red}{\num{136.352}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{338.925} & \textcolor{red}{\num{218.449}} & \num{255.332} & \num{250.69} & \num{273.319}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{156.793} & \num{176.697} & \num{153.848} & \textcolor{red}{\num{152.831}} & \num{153.423}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{202.612} & \num{197.71} & \num{182.732} & \textcolor{red}{\num{179.721}} & \num{185.038}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{81.44} & \num{80.599} & \textcolor{red}{\num{79.313}} & \num{79.986} & \num{80.369}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{82.487} & \num{83.44} & \num{80.128} & \textcolor{red}{\num{80.088}} & \num{80.413}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{102.161} & \num{101.09} & \num{99.34} & \textcolor{red}{\num{99.337}} & \num{99.344}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{102.136} & \num{101.332} & \textcolor{red}{\num{98.41}} & \num{98.516} & \num{98.514}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{73.613} & \textcolor{red}{\num{73.162}} & \num{74.257} & \num{74.765} & \num{74.413}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{101.139} & \num{99.511} & \num{97.676} & \textcolor{red}{\num{97.157}} & \num{97.181}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{70.861} & \textcolor{red}{\num{69.668}} & \num{70.214} & \num{69.829} & \num{69.826}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{94.74} & \num{95.375} & \num{93.523} & \textcolor{red}{\num{93.077}} & \num{93.149}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{96.723}} & \num{97.385} & \num{96.886} & \num{96.942} & \num{96.992}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{89.478} & \num{89.062} & \num{87.75} & \textcolor{red}{\num{87.744}} & \num{87.8}\\*
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
     & asset01 & \num{1.244} & \num{1.315} & \num{1.211} & \textcolor{red}{\num{1.209}} & \num{1.224}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.706} & \num{1.889} & \textcolor{red}{\num{1.656}} & \num{1.657} & \num{1.666}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1.427}} & \num{1.452} & \num{1.452} & \num{1.458} & \num{1.456}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{2.184} & \num{2.277} & \num{2.145} & \num{2.147} & \textcolor{red}{\num{2.145}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{2.345} & \num{2.587} & \textcolor{red}{\num{2.291}} & \num{2.355} & \num{2.402}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{5.43} & \textcolor{red}{\num{4.627}} & \num{5.061} & \num{5.041} & \num{5.044}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.772} & \num{0.755} & \num{0.747} & \textcolor{red}{\num{0.747}} & \num{0.747}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.367} & \num{1.421} & \num{1.334} & \num{1.329} & \textcolor{red}{\num{1.328}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{2.007} & \num{2.054} & \num{1.982} & \textcolor{red}{\num{1.972}} & \num{1.976}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{2.054} & \num{2.042} & \textcolor{red}{\num{1.987}} & \num{1.991} & \num{1.999}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.902} & \num{0.933} & \num{0.895} & \textcolor{red}{\num{0.894}} & \num{0.902}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.057} & \num{1.11} & \textcolor{red}{\num{1.036}} & \num{1.037} & \num{1.047}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.975}} & \num{0.982} & \num{0.977} & \num{0.978} & \num{0.978}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.164} & \num{1.176} & \textcolor{red}{\num{1.145}} & \num{1.147} & \num{1.146}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{1.261} & \num{1.348} & \textcolor{red}{\num{1.225}} & \num{1.238} & \num{1.254}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.742} & \textcolor{red}{\num{1.63}} & \num{1.647} & \num{1.63} & \num{1.635}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.697} & \textcolor{red}{\num{0.677}} & \num{0.68} & \num{0.679} & \num{0.679}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.936} & \num{0.953} & \num{0.929} & \num{0.927} & \textcolor{red}{\num{0.926}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.152} & \textcolor{red}{\num{1.121}} & \num{1.157} & \num{1.157} & \num{1.154}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{1.098} & \num{1.103} & \num{1.077} & \textcolor{red}{\num{1.076}} & \num{1.08}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{138.046} & \num{139.502} & \num{122.165} & \textcolor{red}{\num{118.76}} & \num{124.519}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{299.695} & \num{393.36} & \textcolor{red}{\num{230.825}} & \num{245.81} & \num{291.893}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{121.601} & \num{137.782} & \num{117.3} & \num{116.356} & \textcolor{red}{\num{115.755}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{99.403} & \num{107.471} & \textcolor{red}{\num{98.701}} & \num{98.849} & \num{99.137}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{245.563} & \num{322.881} & \textcolor{red}{\num{240.846}} & \num{244.922} & \num{260.17}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{260.888} & \num{270.542} & \num{199.153} & \textcolor{red}{\num{188.56}} & \num{190.467}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{113.289} & \num{107.937} & \textcolor{red}{\num{100.469}} & \num{100.876} & \num{101.113}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{315.396} & \textcolor{red}{\num{284.54}} & \num{379.442} & \num{386.483} & \num{361.371}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{105.421}} & \num{115.424} & \num{110.129} & \num{111.764} & \num{110.877}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{188.811} & \num{208.827} & \textcolor{red}{\num{177.67}} & \num{179.153} & \num{183.922}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{93.624} & \num{96.729} & \num{92.779} & \textcolor{red}{\num{92.587}} & \num{93.394}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{111.116} & \num{115.795} & \textcolor{red}{\num{109.019}} & \num{109.129} & \num{110.082}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{99.425}} & \num{100.241} & \num{99.502} & \num{99.587} & \num{99.562}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{122.83} & \num{124.724} & \textcolor{red}{\num{121.44}} & \num{121.67} & \num{121.598}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{130.089} & \num{138.663} & \textcolor{red}{\num{126.878}} & \num{128.393} & \num{130.009}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{177.901} & \num{167.083} & \num{167.775} & \textcolor{red}{\num{166.199}} & \num{166.696}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{72.516} & \textcolor{red}{\num{70.529}} & \num{70.592} & \num{70.535} & \num{70.554}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{94.547} & \num{96.354} & \num{93.823} & \num{93.619} & \textcolor{red}{\num{93.495}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{118.123} & \textcolor{red}{\num{114.023}} & \num{118.445} & \num{118.327} & \num{117.985}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{113.352} & \num{113.793} & \num{111.139} & \textcolor{red}{\num{111.116}} & \num{111.486}\\*
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
     & asset01 & \num{1.18} & \num{1.19} & \num{1.167} & \textcolor{red}{\num{1.166}} & \num{1.167}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.706} & \num{1.94} & \num{1.679} & \num{1.685} & \textcolor{red}{\num{1.678}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.411} & \num{1.42} & \textcolor{red}{\num{1.404}} & \num{1.405} & \num{1.405}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{2.314}} & \num{2.5} & \num{2.328} & \num{2.336} & \num{2.334}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{4.206} & \num{5.15} & \textcolor{red}{\num{3.993}} & \num{4.003} & \num{4.246}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{7.209} & \textcolor{red}{\num{6.469}} & \num{6.859} & \num{6.776} & \num{6.777}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.724} & \num{0.721} & \textcolor{red}{\num{0.715}} & \num{0.715} & \num{0.716}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{1.339}} & \num{1.432} & \num{1.358} & \num{1.354} & \num{1.354}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{2.188} & \num{2.214} & \num{2.139} & \textcolor{red}{\num{2.132}} & \num{2.136}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{2.475} & \num{2.56} & \num{2.405} & \textcolor{red}{\num{2.397}} & \num{2.424}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.874} & \num{0.876} & \num{0.868} & \num{0.868} & \textcolor{red}{\num{0.868}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.05} & \num{1.119} & \num{1.043} & \num{1.044} & \textcolor{red}{\num{1.042}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.955} & \num{0.956} & \num{0.955} & \num{0.955} & \textcolor{red}{\num{0.954}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{1.19}} & \num{1.227} & \num{1.194} & \num{1.197} & \num{1.196}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{1.725} & \num{1.915} & \num{1.656} & \textcolor{red}{\num{1.637}} & \num{1.695}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.991} & \textcolor{red}{\num{1.913}} & \num{1.933} & \num{1.916} & \num{1.917}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.659} & \textcolor{red}{\num{0.644}} & \num{0.655} & \num{0.655} & \num{0.655}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{0.924}} & \num{0.958} & \num{0.932} & \num{0.93} & \num{0.93}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.215} & \num{1.2} & \num{1.2} & \num{1.197} & \textcolor{red}{\num{1.195}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{1.176} & \num{1.201} & \num{1.16} & \textcolor{red}{\num{1.155}} & \num{1.161}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{101.098} & \num{104.081} & \num{99.06} & \num{98.511} & \textcolor{red}{\num{97.888}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{129.41} & \num{186.779} & \textcolor{red}{\num{118.482}} & \num{128.691} & \num{122.11}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{123.756} & \num{135.434} & \num{120.572} & \textcolor{red}{\num{119.38}} & \num{120.006}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{105.172}} & \num{108.913} & \num{106.078} & \num{106.574} & \num{106.523}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{496} & \num{467.326} & \num{435.255} & \textcolor{red}{\num{423.073}} & \num{453.475}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{177.552} & \num{214.375} & \num{147.67} & \textcolor{red}{\num{142.698}} & \num{145.949}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{105.075} & \textcolor{red}{\num{99.129}} & \num{101.135} & \num{100.66} & \num{100.67}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{304.056} & \num{385.021} & \textcolor{red}{\num{253.537}} & \num{259.631} & \num{270.136}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{100.017} & \num{115.744} & \num{98.843} & \textcolor{red}{\num{98.792}} & \num{98.805}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{182.46} & \num{201.867} & \num{164.515} & \textcolor{red}{\num{164.223}} & \num{168.396}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{92.041} & \num{92.067} & \num{91.335} & \num{91.365} & \textcolor{red}{\num{91.307}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{108.348} & \num{115.28} & \num{107.7} & \num{107.777} & \textcolor{red}{\num{107.554}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{96.986} & \num{96.956} & \num{96.861} & \num{96.852} & \textcolor{red}{\num{96.774}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{119.847}} & \num{123.395} & \num{120.439} & \num{120.73} & \num{120.623}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{177.011} & \num{197.44} & \num{169.833} & \textcolor{red}{\num{167.827}} & \num{173.633}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{205.857} & \num{198.041} & \num{199.667} & \textcolor{red}{\num{197.849}} & \num{197.961}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{68.331} & \textcolor{red}{\num{66.901}} & \num{67.953} & \num{67.952} & \num{67.979}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{93.982}} & \num{97.289} & \num{94.782} & \num{94.524} & \num{94.529}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{124.513} & \num{122.541} & \num{122.895} & \num{122.611} & \textcolor{red}{\num{122.306}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{120.768} & \num{123.323} & \num{119.052} & \textcolor{red}{\num{118.61}} & \num{119.185}\\*
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

<img src="../output/figs/DGP-3-largecvonefig-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-3-largecvfivefig-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-3-largecvtwentyfig-1.png" width="70%" style="display: block; margin: auto;" />

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
     & asset01 & \num{1.168} & \num{1.223} & \textcolor{red}{\num{1.113}} & \num{1.114} & \num{1.114}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.659} & \num{1.551} & \num{1.467} & \num{1.466} & \textcolor{red}{\num{1.466}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1.572}} & \num{1.574} & \num{1.589} & \num{1.589} & \num{1.578}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.991} & \textcolor{red}{\num{0.989}} & \num{0.993} & \num{0.994} & \num{0.992}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{1.057} & \num{0.981} & \textcolor{red}{\num{0.944}} & \num{0.945} & \num{0.945}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.351} & \num{1.327} & \num{1.314} & \textcolor{red}{\num{1.313}} & \num{1.313}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1.263} & \num{1.27} & \num{1.154} & \textcolor{red}{\num{1.153}} & \num{1.153}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.425} & \num{1.441} & \num{1.327} & \textcolor{red}{\num{1.326}} & \num{1.326}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.06} & \num{1.046} & \num{1.037} & \textcolor{red}{\num{1.035}} & \num{1.035}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{1.08} & \num{1.067} & \num{0.971} & \textcolor{red}{\num{0.971}} & \num{0.971}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{1.659} & \num{1.534} & \num{1.465} & \num{1.466} & \textcolor{red}{\num{1.465}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.8} & \num{0.794} & \num{0.778} & \num{0.778} & \textcolor{red}{\num{0.778}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.257} & \num{1.233} & \num{1.179} & \num{1.179} & \textcolor{red}{\num{1.178}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.877} & \num{0.886} & \num{0.858} & \num{0.858} & \textcolor{red}{\num{0.858}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.056} & \num{0.999} & \num{0.986} & \num{0.985} & \textcolor{red}{\num{0.985}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.96} & \num{0.952} & \num{0.946} & \num{0.946} & \textcolor{red}{\num{0.94}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.793}} & \num{0.809} & \num{0.808} & \num{0.808} & \num{0.807}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.812} & \num{0.792} & \textcolor{red}{\num{0.777}} & \num{0.778} & \num{0.778}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.928} & \textcolor{red}{\num{0.902}} & \num{0.906} & \num{0.905} & \num{0.905}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.897} & \num{0.919} & \num{0.876} & \textcolor{red}{\num{0.875}} & \num{0.876}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.958} & \num{0.959} & \num{0.917} & \textcolor{red}{\num{0.916}} & \num{0.916}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.81} & \num{0.798} & \num{0.797} & \textcolor{red}{\num{0.796}} & \num{0.796}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.819} & \num{0.821} & \textcolor{red}{\num{0.775}} & \num{0.775} & \num{0.777}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{1.012} & \num{0.976} & \textcolor{red}{\num{0.974}} & \num{0.975} & \num{0.976}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.738} & \num{0.723} & \num{0.71} & \num{0.71} & \textcolor{red}{\num{0.71}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.888} & \num{0.878} & \num{0.861} & \num{0.861} & \textcolor{red}{\num{0.861}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{156.69} & \num{142.94} & \textcolor{red}{\num{124.259}} & \num{125.024} & \num{125.039}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{159.966} & \num{136.069} & \num{122.884} & \num{122.477} & \textcolor{red}{\num{122.351}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{210.789} & \num{201.955} & \num{144.344} & \num{144.175} & \textcolor{red}{\num{143.34}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{249.673}} & \num{335.664} & \num{345.239} & \num{345.815} & \num{344.664}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{201.364}} & \num{223.1} & \num{205.821} & \num{207.619} & \num{207.389}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{427.624} & \num{334.702} & \num{264.447} & \num{261.489} & \textcolor{red}{\num{261.094}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{241.405} & \num{239.34} & \num{105.003} & \num{103.188} & \textcolor{red}{\num{102.996}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{169.618}} & \num{180.024} & \num{171.944} & \num{172.295} & \num{172.262}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{184.442} & \num{183.569} & \textcolor{red}{\num{133.651}} & \num{134.85} & \num{134.529}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{221.24} & \num{223.478} & \num{198.103} & \textcolor{red}{\num{197.782}} & \num{200.986}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{151.519}} & \num{159.563} & \num{155.5} & \num{155.796} & \num{156.736}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{213.557} & \num{226.359} & \textcolor{red}{\num{206.032}} & \num{206.368} & \num{206.407}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{215.657} & \num{215.564} & \num{181.436} & \textcolor{red}{\num{181.406}} & \num{181.483}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{86.835} & \num{87.811} & \num{85.244} & \num{85.23} & \textcolor{red}{\num{85.23}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{104.655} & \num{99.073} & \num{98.133} & \num{98.104} & \textcolor{red}{\num{98.095}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{96.238} & \num{95.611} & \num{94.425} & \num{94.41} & \textcolor{red}{\num{93.964}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{79.589}} & \num{81.355} & \num{81.392} & \num{81.399} & \num{81.332}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{81.905} & \num{80.194} & \textcolor{red}{\num{78.452}} & \num{78.504} & \num{78.502}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{92.309} & \textcolor{red}{\num{89.906}} & \num{90.275} & \num{90.234} & \num{90.23}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{89.136} & \num{90.845} & \num{86.866} & \textcolor{red}{\num{86.839}} & \num{86.911}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{95.785} & \num{95.649} & \num{91.584} & \textcolor{red}{\num{91.498}} & \num{91.509}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{82.107} & \num{81.558} & \num{81.261} & \textcolor{red}{\num{81.159}} & \num{81.163}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{82.076} & \num{81.939} & \textcolor{red}{\num{77.331}} & \num{77.35} & \num{77.509}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{101.729} & \num{98.631} & \textcolor{red}{\num{98.571}} & \num{98.632} & \num{98.678}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{73} & \num{71.335} & \textcolor{red}{\num{70.295}} & \num{70.304} & \num{70.301}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{88.78} & \num{87.826} & \num{86.152} & \num{86.139} & \textcolor{red}{\num{86.119}}\\*
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
     & asset01 & \num{1.104} & \num{1.236} & \textcolor{red}{\num{1.036}} & \num{1.037} & \num{1.037}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.633} & \num{1.684} & \num{1.602} & \num{1.602} & \textcolor{red}{\num{1.601}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1.764}} & \num{1.874} & \num{1.78} & \num{1.78} & \num{1.773}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.447} & \textcolor{red}{\num{1.347}} & \num{1.404} & \num{1.404} & \num{1.4}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{1.149} & \num{1.122} & \num{1.112} & \textcolor{red}{\num{1.112}} & \num{1.112}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.406} & \num{1.483} & \num{1.393} & \textcolor{red}{\num{1.393}} & \num{1.393}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1.196} & \num{1.207} & \textcolor{red}{\num{1.147}} & \num{1.148} & \num{1.148}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{2.263} & \num{2.242} & \textcolor{red}{\num{2.144}} & \num{2.144} & \num{2.145}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.136} & \textcolor{red}{\num{1.122}} & \num{1.164} & \num{1.164} & \num{1.165}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{3.397} & \num{3.225} & \num{2.641} & \textcolor{red}{\num{2.636}} & \num{2.67}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{3.534}} & \num{3.973} & \num{3.538} & \num{3.552} & \num{3.538}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{2.078}} & \num{2.078} & \num{2.096} & \num{2.097} & \num{2.097}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.842} & \num{1.883} & \textcolor{red}{\num{1.755}} & \num{1.756} & \num{1.757}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.879} & \num{0.924} & \num{0.849} & \num{0.849} & \textcolor{red}{\num{0.849}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.053} & \num{1.061} & \num{1.052} & \num{1.052} & \textcolor{red}{\num{1.052}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.007} & \num{1.045} & \num{1.005} & \textcolor{red}{\num{1.005}} & \num{1.006}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.953} & \textcolor{red}{\num{0.925}} & \num{0.943} & \num{0.943} & \num{0.941}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.803} & \num{0.806} & \textcolor{red}{\num{0.795}} & \num{0.795} & \num{0.795}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.912} & \num{0.946} & \textcolor{red}{\num{0.91}} & \num{0.91} & \num{0.91}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.874} & \num{0.892} & \textcolor{red}{\num{0.869}} & \num{0.87} & \num{0.87}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.215} & \num{1.224} & \textcolor{red}{\num{1.2}} & \num{1.201} & \num{1.201}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.842} & \textcolor{red}{\num{0.828}} & \num{0.853} & \num{0.853} & \num{0.854}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{1.466} & \num{1.454} & \num{1.313} & \textcolor{red}{\num{1.312}} & \num{1.32}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{1.505}} & \num{1.605} & \num{1.514} & \num{1.517} & \num{1.518}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{1.154} & \num{1.19} & \textcolor{red}{\num{1.142}} & \num{1.143} & \num{1.143}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{1.055} & \num{1.075} & \textcolor{red}{\num{1.037}} & \num{1.037} & \num{1.038}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{132.006} & \num{156.761} & \textcolor{red}{\num{99.704}} & \num{99.755} & \num{99.759}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{105.644} & \num{105.293} & \num{99.63} & \num{99.289} & \textcolor{red}{\num{99.195}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{135.616} & \num{140.101} & \num{100.69} & \textcolor{red}{\num{100.542}} & \num{106.622}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{165.384} & \num{250.433} & \num{117.317} & \textcolor{red}{\num{115.659}} & \num{121.731}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{133.034} & \num{167.34} & \textcolor{red}{\num{99.3}} & \num{99.848} & \num{99.903}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{190.741} & \num{174.788} & \textcolor{red}{\num{100.163}} & \num{100.283} & \num{100.414}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{144.064} & \num{157.207} & \textcolor{red}{\num{98.841}} & \num{99.328} & \num{103.551}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{112.708} & \num{123.693} & \textcolor{red}{\num{105.016}} & \num{105.1} & \num{105.15}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{117.43} & \num{128.305} & \textcolor{red}{\num{97.469}} & \num{97.812} & \num{98.009}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{417.217} & \num{358.555} & \num{315.628} & \textcolor{red}{\num{314.321}} & \num{328.486}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{144.562} & \num{167.564} & \num{117.877} & \textcolor{red}{\num{117.646}} & \num{122.714}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{147.919} & \num{200.564} & \textcolor{red}{\num{123.329}} & \num{124.332} & \num{124.633}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{162.194} & \num{177.55} & \num{122.914} & \textcolor{red}{\num{122.826}} & \num{125.847}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{89.038} & \num{93.558} & \num{85.959} & \num{85.958} & \textcolor{red}{\num{85.957}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{106.416} & \num{107.236} & \num{106.085} & \num{106.09} & \textcolor{red}{\num{106.083}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{101.093} & \num{104.605} & \num{100.613} & \textcolor{red}{\num{100.606}} & \num{100.72}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{94.727} & \textcolor{red}{\num{92.266}} & \num{93.519} & \num{93.469} & \num{93.352}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{80.017} & \num{80.106} & \textcolor{red}{\num{78.916}} & \num{78.95} & \num{78.957}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{90.401} & \num{93.604} & \textcolor{red}{\num{90.2}} & \num{90.217} & \num{90.225}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{87.061} & \num{89.265} & \textcolor{red}{\num{86.421}} & \num{86.467} & \num{86.524}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{121.071} & \num{122.224} & \textcolor{red}{\num{119.71}} & \num{119.782} & \num{119.808}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{85.689} & \textcolor{red}{\num{84.694}} & \num{86.794} & \num{86.849} & \num{86.868}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{145.895} & \num{144.067} & \num{130.864} & \textcolor{red}{\num{130.768}} & \num{131.589}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{148.449}} & \num{158.757} & \num{149.39} & \num{149.691} & \num{149.871}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{114.72} & \num{118.856} & \textcolor{red}{\num{113.618}} & \num{113.648} & \num{113.664}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{105.381} & \num{107.437} & \textcolor{red}{\num{103.508}} & \num{103.541} & \num{103.635}\\*
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
     & asset01 & \num{1.089} & \num{1.216} & \textcolor{red}{\num{1.074}} & \num{1.074} & \num{1.074}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.611} & \num{1.649} & \textcolor{red}{\num{1.599}} & \num{1.599} & \num{1.599}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1.749}} & \num{1.844} & \num{1.768} & \num{1.768} & \num{1.768}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.375} & \textcolor{red}{\num{1.327}} & \num{1.413} & \num{1.412} & \num{1.412}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.905} & \num{0.897} & \num{0.89} & \num{0.89} & \textcolor{red}{\num{0.89}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{1.454}} & \num{1.594} & \num{1.454} & \num{1.454} & \num{1.454}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1.066} & \num{1.078} & \num{1.038} & \textcolor{red}{\num{1.038}} & \num{1.038}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{2.225} & \num{2.213} & \textcolor{red}{\num{2.187}} & \num{2.187} & \num{2.187}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.219} & \num{1.243} & \num{1.202} & \num{1.201} & \textcolor{red}{\num{1.201}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{3.316} & \num{3.287} & \num{2.945} & \textcolor{red}{\num{2.943}} & \num{2.952}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{3.468}} & \num{3.946} & \num{3.529} & \num{3.536} & \num{3.514}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{2.496} & \textcolor{red}{\num{2.4}} & \num{2.479} & \num{2.478} & \num{2.478}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.831} & \num{1.891} & \num{1.798} & \num{1.798} & \textcolor{red}{\num{1.797}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.875} & \num{0.919} & \textcolor{red}{\num{0.87}} & \num{0.87} & \num{0.87}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.061} & \num{1.065} & \textcolor{red}{\num{1.049}} & \num{1.049} & \num{1.049}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1}} & \num{1.022} & \num{1.002} & \num{1.002} & \num{1.002}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.934} & \textcolor{red}{\num{0.924}} & \num{0.947} & \num{0.947} & \num{0.947}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.735} & \num{0.738} & \num{0.73} & \num{0.73} & \textcolor{red}{\num{0.73}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.936} & \num{0.977} & \num{0.936} & \textcolor{red}{\num{0.936}} & \num{0.936}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.843} & \num{0.858} & \num{0.836} & \num{0.836} & \textcolor{red}{\num{0.836}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.227} & \num{1.233} & \textcolor{red}{\num{1.214}} & \num{1.215} & \num{1.215}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.872} & \num{0.869} & \num{0.866} & \textcolor{red}{\num{0.866}} & \num{0.866}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{1.413} & \num{1.439} & \num{1.316} & \textcolor{red}{\num{1.316}} & \num{1.317}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{1.482}} & \num{1.593} & \num{1.497} & \num{1.499} & \num{1.496}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{1.245} & \num{1.248} & \num{1.226} & \num{1.226} & \textcolor{red}{\num{1.226}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{1.052} & \num{1.074} & \num{1.041} & \num{1.041} & \textcolor{red}{\num{1.041}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{100.589} & \num{131.729} & \num{100.004} & \num{99.987} & \textcolor{red}{\num{99.985}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{107.672} & \num{110.028} & \textcolor{red}{\num{99.029}} & \num{99.343} & \num{99.359}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{104.605} & \num{135.971} & \num{99.852} & \num{99.892} & \textcolor{red}{\num{99.602}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{109.521} & \num{258.546} & \num{100.871} & \textcolor{red}{\num{100.253}} & \num{100.455}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{104.185} & \num{136.74} & \textcolor{red}{\num{99.232}} & \num{99.672} & \num{99.671}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{105.407} & \num{149.136} & \textcolor{red}{\num{99.228}} & \num{99.549} & \num{99.609}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{107.253} & \num{149.741} & \textcolor{red}{\num{99.716}} & \num{99.979} & \num{103.339}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{119.891} & \num{131.193} & \num{99.415} & \num{99.337} & \textcolor{red}{\num{99.304}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{107.796} & \num{108.122} & \textcolor{red}{\num{99.66}} & \num{99.747} & \num{99.769}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{227.38} & \num{185.183} & \textcolor{red}{\num{180.207}} & \num{180.438} & \num{192.583}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{114.752} & \num{147.754} & \textcolor{red}{\num{103.371}} & \num{103.623} & \num{105.854}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{101.472}} & \num{147.258} & \num{113.849} & \num{114.735} & \num{114.794}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{117.543} & \num{149.283} & \textcolor{red}{\num{107.869}} & \num{108.046} & \num{109.527}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{85.333} & \num{89.357} & \textcolor{red}{\num{84.845}} & \num{84.847} & \num{84.846}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{106.348} & \num{106.847} & \textcolor{red}{\num{105.111}} & \num{105.129} & \num{105.131}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{101.223}} & \num{103.385} & \num{101.398} & \num{101.4} & \num{101.379}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{94.469} & \textcolor{red}{\num{92.82}} & \num{95.974} & \num{95.948} & \num{95.935}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{72.769} & \num{73.076} & \num{72.297} & \num{72.298} & \textcolor{red}{\num{72.296}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{92.087} & \num{95.898} & \num{91.99} & \textcolor{red}{\num{91.988}} & \num{91.989}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{84.214} & \num{86.611} & \num{83.672} & \num{83.666} & \textcolor{red}{\num{83.658}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{122.276} & \num{122.893} & \textcolor{red}{\num{120.873}} & \num{120.894} & \num{120.897}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{84.427} & \num{83.988} & \num{83.816} & \textcolor{red}{\num{83.806}} & \num{83.807}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{142.489} & \num{144.145} & \num{133.083} & \textcolor{red}{\num{133.008}} & \num{133.138}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{151.927}} & \num{162.371} & \num{153.722} & \num{153.921} & \num{153.56}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{124.087} & \num{124.331} & \num{121.926} & \num{121.882} & \textcolor{red}{\num{121.872}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{105.137} & \num{107.143} & \num{104.059} & \num{104.066} & \textcolor{red}{\num{104.042}}\\*
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
    MSE & \num{0.942} & \num{0.936} & \textcolor{red}{\num{0.926}} & \num{0.927} & \num{0.928}\\
    MAE & \num{0.771} & \num{0.765} & \num{0.759} & \num{0.759} & \textcolor{red}{\num{0.759}}\\
    MAPE & \num{153.263} & \num{164.397} & \textcolor{red}{\num{152.612}} & \num{153.707} & \num{153.583}\\
    MASE & \num{81.811} & \num{81.087} & \num{80.548} & \num{80.57} & \textcolor{red}{\num{80.513}}\\
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
    MSE & \num{3.061} & \textcolor{red}{\num{2.955}} & \num{3.012} & \num{2.994} & \num{2.986}\\
    MAE & \num{1.328} & \num{1.322} & \textcolor{red}{\num{1.313}} & \num{1.315} & \num{1.313}\\
    MAPE & \num{239.738} & \num{242.336} & \num{220.794} & \textcolor{red}{\num{215.526}} & \num{218.275}\\
    MASE & \num{141.637} & \num{140.995} & \textcolor{red}{\num{140.132}} & \num{140.362} & \num{140.155}\\
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
    MSE & \num{4.341} & \num{4.476} & \textcolor{red}{\num{4.233}} & \num{4.277} & \num{4.31}\\
    MAE & \num{1.553} & \num{1.612} & \textcolor{red}{\num{1.544}} & \num{1.558} & \num{1.564}\\
    MAPE & \num{259.689} & \num{267.304} & \textcolor{red}{\num{250.081}} & \num{255.747} & \num{260.71}\\
    MASE & \num{164.726} & \num{170.98} & \textcolor{red}{\num{163.815}} & \num{165.39} & \num{166}\\
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
    MSE & \num{1.186} & \num{1.171} & \textcolor{red}{\num{1.14}} & \num{1.143} & \num{1.143}\\
    MAE & \num{0.869} & \num{0.866} & \num{0.853} & \textcolor{red}{\num{0.853}} & \num{0.853}\\
    MAPE & \num{202.612} & \num{197.71} & \num{182.732} & \textcolor{red}{\num{179.721}} & \num{185.038}\\
    MASE & \num{89.478} & \num{89.062} & \num{87.75} & \textcolor{red}{\num{87.744}} & \num{87.8}\\
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
    MSE & \num{2.054} & \num{2.042} & \textcolor{red}{\num{1.987}} & \num{1.991} & \num{1.999}\\
    MAE & \num{1.098} & \num{1.103} & \num{1.077} & \textcolor{red}{\num{1.076}} & \num{1.08}\\
    MAPE & \num{188.811} & \num{208.827} & \textcolor{red}{\num{177.67}} & \num{179.153} & \num{183.922}\\
    MASE & \num{113.352} & \num{113.793} & \num{111.139} & \textcolor{red}{\num{111.116}} & \num{111.486}\\
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
    MSE & \num{2.475} & \num{2.56} & \num{2.405} & \textcolor{red}{\num{2.397}} & \num{2.424}\\
    MAE & \num{1.176} & \num{1.201} & \num{1.16} & \textcolor{red}{\num{1.155}} & \num{1.161}\\
    MAPE & \num{182.46} & \num{201.867} & \num{164.515} & \textcolor{red}{\num{164.223}} & \num{168.396}\\
    MASE & \num{120.768} & \num{123.323} & \num{119.052} & \textcolor{red}{\num{118.61}} & \num{119.185}\\
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
    MSE & \num{1.257} & \num{1.233} & \num{1.179} & \num{1.179} & \textcolor{red}{\num{1.178}}\\
    MAE & \num{0.888} & \num{0.878} & \num{0.861} & \num{0.861} & \textcolor{red}{\num{0.861}}\\
    MAPE & \num{215.657} & \num{215.564} & \num{181.436} & \textcolor{red}{\num{181.406}} & \num{181.483}\\
    MASE & \num{88.78} & \num{87.826} & \num{86.152} & \num{86.139} & \textcolor{red}{\num{86.119}}\\
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
    MSE & \num{1.842} & \num{1.883} & \textcolor{red}{\num{1.755}} & \num{1.756} & \num{1.757}\\
    MAE & \num{1.055} & \num{1.075} & \textcolor{red}{\num{1.037}} & \num{1.037} & \num{1.038}\\
    MAPE & \num{162.194} & \num{177.55} & \num{122.914} & \textcolor{red}{\num{122.826}} & \num{125.847}\\
    MASE & \num{105.381} & \num{107.437} & \textcolor{red}{\num{103.508}} & \num{103.541} & \num{103.635}\\
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
    MSE & \num{1.831} & \num{1.891} & \num{1.798} & \num{1.798} & \textcolor{red}{\num{1.797}}\\
    MAE & \num{1.052} & \num{1.074} & \num{1.041} & \num{1.041} & \textcolor{red}{\num{1.041}}\\
    MAPE & \num{117.543} & \num{149.283} & \textcolor{red}{\num{107.869}} & \num{108.046} & \num{109.527}\\
    MASE & \num{105.137} & \num{107.143} & \num{104.059} & \num{104.066} & \textcolor{red}{\num{104.042}}\\
    \bottomrule
    \end{tabular}
    \end{table}

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
            "../output/dgp03-figs/small-coef/", 
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
            "../output/dgp03-figs/med-coef/", 
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
            "../output/dgp03-figs/large-coef/", 
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
