Simulating VHAR-type Minnesota BVHAR
================
Young Geun Kim
13 Jan, 2022

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
sim_data <- "../data/processed/bvharsim_dgp_l.rds"
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

<img src="../output/figs/DGP-4-smallplot-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-4-medplot-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-4-largeplot-1.png" width="70%" style="display: block; margin: auto;" />

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
  delta = bvhar_small_spec$daily
)
#----------------------------
bvar_medium_spec <- set_bvar(
  sigma = bvhar_medium_spec$sigma,
  lambda = bvhar_medium_spec$lambda,
  delta = bvhar_medium_spec$daily
)
#----------------------------
bvar_large_spec <- set_bvar(
  sigma = bvhar_large_spec$sigma,
  lambda = bvhar_large_spec$lambda,
  delta = bvhar_large_spec$daily
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
#> [1]  2.06  2.36  2.74
#> 
#> Setting for 'lambda':
#> [1]  0.163
#> 
#> Setting for 'delta':
#> [1]  0.420  0.123  0.556
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
#> [1]  2.47  3.13  3.79  4.17  3.25  4.55  2.56  3.53  3.87
#> 
#> Setting for 'lambda':
#> [1]  0.16
#> 
#> Setting for 'delta':
#> [1]  0.2797  0.3278  0.0449  0.2768  0.4822  0.4372  0.2749  0.1718  0.2914
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
#>  [1]  4.18  3.91  4.16  4.23  3.93  4.05  4.37  3.92  3.77  3.45  4.24  3.49
#> 
#> Setting for 'lambda':
#> [1]  0.0335
#> 
#> Setting for 'delta':
#>  [1]  0.0959  0.2011  0.1231  0.2781  0.2157  0.2160  0.0000  0.3154  0.0864
#> [10]  0.4816  0.3903  0.4529
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
  sigma = bvhar_small_spec$sigma,
  lambda = bvhar_small_spec$lambda,
  delta = bvhar_small_spec$daily
)
#-----------------------------------------
bvhar_var_medium_spec <- set_bvhar(
  sigma = bvhar_medium_spec$sigma,
  lambda = bvhar_medium_spec$lambda,
  delta = bvhar_medium_spec$daily
)
#-----------------------------------------
bvhar_var_large_spec <- set_bvhar(
  sigma = bvhar_large_spec$sigma,
  lambda = bvhar_large_spec$lambda,
  delta = bvhar_large_spec$daily
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
#> [1]  2.34  2.09  2.72
#> 
#> Setting for 'lambda':
#> [1]  0.18
#> 
#> Setting for 'delta':
#> [1]  0.439  0.131  0.564
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
#> [1]  2.58  3.28  3.71  4.36  3.12  4.42  2.67  3.64  3.74
#> 
#> Setting for 'lambda':
#> [1]  0.141
#> 
#> Setting for 'delta':
#> [1]  0.2748  0.3517  0.0393  0.2799  0.4975  0.4549  0.2851  0.1729  0.3054
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
#>  [1]  4.19  3.87  4.21  4.23  3.85  4.00  4.43  3.84  3.71  3.48  4.26  3.50
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'delta':
#>  [1]  0.1006  0.2003  0.1251  0.2755  0.2231  0.2113  0.0000  0.3228  0.0943
#> [10]  0.4817  0.3972  0.4558
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
(bvhar_vhar_small_optim <- choose_bvhar(
  bvhar_small_spec, 
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
#> [1]  2.45  1.91  2.74
#> 
#> Setting for 'lambda':
#> [1]  0.149
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.405  0.131  0.534
#> 
#> Setting for 'weekly':
#> [1]  0.256  0.000  0.196
#> 
#> Setting for 'monthly':
#> [1]  0.000  0.000  0.101
```

``` r
(bvhar_vhar_medium_optim <- choose_bvhar(
  bvhar_medium_spec, 
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
#> [1]  2.66  3.28  3.65  4.31  3.20  4.43  2.63  3.64  3.70
#> 
#> Setting for 'lambda':
#> [1]  0.131
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.209  0.308  0.030  0.275  0.444  0.436  0.258  0.132  0.298
#> 
#> Setting for 'weekly':
#> [1]  0.4508  0.2836  0.0735  0.0357  0.2725  0.1141  0.2043  0.3287  0.0000
#> 
#> Setting for 'monthly':
#> [1]  0.0000  0.0000  0.1317  0.0000  0.0635  0.0000  0.0000  0.0000  0.3474
```

``` r
(bvhar_vhar_large_optim <- choose_bvhar(
  bvhar_large_spec, 
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
#>  [1]  4.19  3.87  4.20  4.23  3.86  4.00  4.42  3.85  3.72  3.45  4.26  3.49
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#>  [1]  0.0905  0.1859  0.0869  0.2405  0.2163  0.1978  0.0000  0.3114  0.0942
#> [10]  0.4014  0.3726  0.4500
#> 
#> Setting for 'weekly':
#>  [1]  0.0788  0.1111  0.2925  0.2252  0.0516  0.1018  0.0950  0.0707  0.0000
#> [10]  0.3646  0.1402  0.0320
#> 
#> Setting for 'monthly':
#>  [1]  0.0373  0.0000  0.0000  0.0000  0.0000  0.0000  0.2825  0.0000  0.0000
#> [10]  0.0000  0.0000  0.0000
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
    \caption{\label{tab:empdgp4}Empirical Bayes Results for DGP4.}\\
    \toprule
     &    &     & y1 & y2 & y3 & y4 & y5 & y6 & y7 & y8 & y9 & y10 & y11 & y12\\
    \midrule
    \endfirsthead
    \caption[]{Empirical Bayes Results for DGP4. \textit{(continued)}}\\
    \toprule
      &    &     & y1 & y2 & y3 & y4 & y5 & y6 & y7 & y8 & y9 & y10 & y11 & y12\\
    \midrule
    \endhead

    \endfoot
    \bottomrule
    \endlastfoot
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{SMALL}}\\
    \hspace{1em} & BVAR & $\sigma$ & 2.056 & 2.358 & 2.738 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.163 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.420 & 0.123 & 0.556 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 2.335 & 2.090 & 2.717 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.180 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.439 & 0.131 & 0.564 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 2.446 & 1.907 & 2.745 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.149 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.405 & 0.131 & 0.534 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.256 & 0.000 & 0.196 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.000 & 0.000 & 0.101 &  &  &  &  &  &  &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{MEDIUM}}\\
    \hspace{1em} & BVAR & $\sigma$ & 2.470 & 3.127 & 3.785 & 4.168 & 3.251 & 4.550 & 2.557 & 3.528 & 3.869 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.160 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.280 & 0.328 & 0.045 & 0.277 & 0.482 & 0.437 & 0.275 & 0.172 & 0.291 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 2.583 & 3.280 & 3.711 & 4.363 & 3.122 & 4.419 & 2.675 & 3.642 & 3.742 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.141 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.275 & 0.352 & 0.039 & 0.280 & 0.498 & 0.455 & 0.285 & 0.173 & 0.305 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 2.661 & 3.285 & 3.646 & 4.311 & 3.196 & 4.425 & 2.628 & 3.638 & 3.704 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.131 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.209 & 0.308 & 0.030 & 0.275 & 0.444 & 0.436 & 0.258 & 0.132 & 0.298 &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.451 & 0.284 & 0.073 & 0.036 & 0.273 & 0.114 & 0.204 & 0.329 & 0.000 &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.000 & 0.000 & 0.132 & 0.000 & 0.064 & 0.000 & 0.000 & 0.000 & 0.347 &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{LARGE}}\\
    \hspace{1em} & BVAR & $\sigma$ & 4.175 & 3.910 & 4.155 & 4.231 & 3.935 & 4.045 & 4.369 & 3.919 & 3.770 & 3.451 & 4.240 & 3.493\\

    \hspace{1em} &  & $\lambda$ & 0.033 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.096 & 0.201 & 0.123 & 0.278 & 0.216 & 0.216 & 0.000 & 0.315 & 0.086 & 0.482 & 0.390 & 0.453\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 4.190 & 3.868 & 4.209 & 4.233 & 3.849 & 3.997 & 4.431 & 3.845 & 3.712 & 3.481 & 4.259 & 3.498\\

    \hspace{1em}\hspace{1em} &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.101 & 0.200 & 0.125 & 0.276 & 0.223 & 0.211 & 0.000 & 0.323 & 0.094 & 0.482 & 0.397 & 0.456\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 4.191 & 3.874 & 4.200 & 4.226 & 3.856 & 4.001 & 4.423 & 3.851 & 3.720 & 3.446 & 4.256 & 3.494\\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.090 & 0.186 & 0.087 & 0.241 & 0.216 & 0.198 & 0.000 & 0.311 & 0.094 & 0.401 & 0.373 & 0.450\\

    \hspace{1em} &  & $w_i$ & 0.079 & 0.111 & 0.292 & 0.225 & 0.052 & 0.102 & 0.095 & 0.071 & 0.000 & 0.365 & 0.140 & 0.032\\

    \hspace{1em} &  & $m_i$ & 0.037 & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.282 & 0.000 & 0.000 & 0.000 & 0.000 & 0.000\\*
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

    \caption{\label{tab:dgp4result}Out-of-sample forecasting performance measures for DGP4 with VAR(5) model as benchmark.}
    \centering
    \resizebox{\linewidth}{!}{
    \begin{tabular}[t]{cc|cccc|cccc|cccc|}
    \toprule
    \multicolumn{2}{c}{ } & \multicolumn{4}{c}{RMAFE} & \multicolumn{4}{c}{RMSFE} & \multicolumn{4}{c}{RMASE} \\
    \cmidrule(l{3pt}r{3pt}){3-6} \cmidrule(l{3pt}r{3pt}){7-10} \cmidrule(l{3pt}r{3pt}){11-14}
    \rotatebox{0}{} & \rotatebox{0}{} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 10$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 10$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 10$} & \rotatebox{0}{$h = 20$}\\
    \midrule
     & VHAR & \textcolor{black}{\num{1.004}} & \textcolor{black}{\num{1.018}} & \textcolor{black}{\num{1.028}} & \textcolor{black}{\num{1.008}} & \textcolor{black}{\num{1.018}} & \textcolor{black}{\num{1.024}} & \textcolor{black}{\num{1.034}} & \textcolor{black}{\num{1.015}} & \textcolor{black}{\num{1.003}} & \textcolor{black}{\num{1.018}} & \textcolor{black}{\num{1.027}} & \textcolor{black}{\num{1.008}}\\

     & BVAR & \textcolor{red}{\num{.988}} & \textcolor{red}{\num{.998}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{.989}} & \textcolor{black}{\num{1.005}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{.988}} & \textcolor{red}{\num{.998}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{1.000}}\\

     & BVHAR-S & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.003}} & \textcolor{black}{\num{1.001}} & \textcolor{black}{\num{.996}} & \textcolor{red}{\num{1.005}} & \textcolor{black}{\num{1.004}} & \textcolor{black}{\num{1.001}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.003}} & \textcolor{black}{\num{1.001}}\\

    \multirow{-4}{*}{\centering\arraybackslash SMALL} & BVHAR-L & \textcolor{black}{\num{.993}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.008}} & \textcolor{black}{\num{1.003}} & \textcolor{black}{\num{1.003}} & \textcolor{black}{\num{1.008}} & \textcolor{black}{\num{1.013}} & \textcolor{black}{\num{1.004}} & \textcolor{black}{\num{.993}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.008}} & \textcolor{black}{\num{1.003}}\\
    \cmidrule{1-14}
     & VHAR & \textcolor{black}{\num{1.001}} & \textcolor{black}{\num{1.001}} & \textcolor{black}{\num{1.017}} & \textcolor{red}{\num{1.000}} & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{1.003}} & \textcolor{black}{\num{1.033}} & \textcolor{red}{\num{.993}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.001}} & \textcolor{black}{\num{1.017}} & \textcolor{black}{\num{1.000}}\\

     & BVAR & \textcolor{red}{\num{.985}} & \textcolor{black}{\num{.986}} & \textcolor{red}{\num{.998}} & \textcolor{black}{\num{1.000}} & \textcolor{red}{\num{.966}} & \textcolor{red}{\num{.980}} & \textcolor{black}{\num{.996}} & \textcolor{black}{\num{1.000}} & \textcolor{red}{\num{.985}} & \textcolor{black}{\num{.987}} & \textcolor{red}{\num{.997}} & \textcolor{black}{\num{1.000}}\\

     & BVHAR-S & \textcolor{black}{\num{.988}} & \textcolor{red}{\num{.986}} & \textcolor{black}{\num{.998}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.974}} & \textcolor{black}{\num{.980}} & \textcolor{red}{\num{.996}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.989}} & \textcolor{red}{\num{.986}} & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{1.000}}\\

    \multirow{-4}{*}{\centering\arraybackslash MEDIUM} & BVHAR-L & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{.998}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.972}} & \textcolor{black}{\num{.981}} & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{1.001}} & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{.998}} & \textcolor{red}{\num{1.000}}\\
    \cmidrule{1-14}
     & VHAR & \textcolor{black}{\num{.994}} & \textcolor{black}{\num{1.012}} & \textcolor{black}{\num{1.013}} & \textcolor{black}{\num{1.011}} & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{1.021}} & \textcolor{black}{\num{1.031}} & \textcolor{black}{\num{1.022}} & \textcolor{black}{\num{.994}} & \textcolor{black}{\num{1.013}} & \textcolor{black}{\num{1.013}} & \textcolor{black}{\num{1.011}}\\

     & BVAR & \textcolor{black}{\num{.976}} & \textcolor{red}{\num{.990}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.949}} & \textcolor{red}{\num{.974}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.001}} & \textcolor{black}{\num{.977}} & \textcolor{red}{\num{.990}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.000}}\\

     & BVHAR-S & \textcolor{black}{\num{.976}} & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.949}} & \textcolor{black}{\num{.974}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.001}} & \textcolor{black}{\num{.977}} & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.000}}\\

    \multirow{-4}{*}{\centering\arraybackslash LARGE} & BVHAR-L & \textcolor{red}{\num{.974}} & \textcolor{black}{\num{.992}} & \textcolor{red}{\num{.999}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{.945}} & \textcolor{black}{\num{.976}} & \textcolor{red}{\num{.999}} & \textcolor{red}{\num{1.001}} & \textcolor{red}{\num{.975}} & \textcolor{black}{\num{.992}} & \textcolor{red}{\num{.999}} & \textcolor{red}{\num{1.000}}\\
    \bottomrule
    \end{tabular}}
    \end{table}

## Piecewise Errors

### SMALL

Plots

<img src="../output/figs/DGP-4-smallcvonefig-1.png" width="70%" style="display: block; margin: auto;" />

<img src="../output/figs/DGP-4-smallcvfivefig-1.png" width="70%" style="display: block; margin: auto;" />

<img src="../output/figs/DGP-4-smallcvtwentyfig-1.png" width="70%" style="display: block; margin: auto;" />

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
     & asset01 & \num{0.867} & \num{0.877} & \textcolor{red}{\num{0.852}} & \num{0.858} & \num{0.871}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.89} & \num{0.91} & \textcolor{red}{\num{0.872}} & \num{0.882} & \num{0.881}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1.048}} & \num{1.068} & \num{1.05} & \num{1.053} & \num{1.06}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.935} & \num{0.952} & \textcolor{red}{\num{0.925}} & \num{0.931} & \num{0.937}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.74} & \num{0.745} & \num{0.733} & \textcolor{red}{\num{0.733}} & \num{0.743}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.729} & \num{0.732} & \textcolor{red}{\num{0.713}} & \num{0.715} & \num{0.714}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.839} & \num{0.84} & \textcolor{red}{\num{0.834}} & \num{0.835} & \num{0.834}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.769} & \num{0.772} & \textcolor{red}{\num{0.76}} & \num{0.761} & \num{0.764}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{170.308}} & \num{181.224} & \num{171.756} & \num{174.236} & \num{179.559}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{191.667} & \num{199.277} & \num{176.752} & \textcolor{red}{\num{159.921}} & \num{160.806}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{157.982} & \num{152.81} & \num{156.181} & \num{155.691} & \textcolor{red}{\num{152.663}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{173.319} & \num{177.77} & \num{168.23} & \textcolor{red}{\num{163.283}} & \num{164.343}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{73.014} & \num{73.366} & \num{72.367} & \textcolor{red}{\num{72.289}} & \num{73.222}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{70.854} & \num{71.099} & \textcolor{red}{\num{69.306}} & \num{69.483} & \num{69.452}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{81.739} & \num{81.892} & \textcolor{red}{\num{81.239}} & \num{81.352} & \num{81.385}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{75.202} & \num{75.452} & \textcolor{red}{\num{74.304}} & \num{74.375} & \num{74.686}\\*
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
     & asset01 & \textcolor{red}{\num{1.272}} & \num{1.324} & \num{1.298} & \num{1.302} & \num{1.332}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.994} & \num{1.015} & \textcolor{red}{\num{0.98}} & \num{0.983} & \num{0.984}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.535} & \num{1.553} & \num{1.543} & \num{1.534} & \textcolor{red}{\num{1.516}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{1.267}} & \num{1.297} & \num{1.274} & \num{1.273} & \num{1.278}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{0.879}} & \num{0.889} & \num{0.884} & \num{0.886} & \num{0.887}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.752} & \num{0.769} & \textcolor{red}{\num{0.745}} & \num{0.746} & \num{0.748}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.974} & \num{0.994} & \num{0.972} & \num{0.973} & \textcolor{red}{\num{0.967}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.869} & \num{0.884} & \textcolor{red}{\num{0.867}} & \num{0.868} & \num{0.867}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{105.217} & \num{134.456} & \textcolor{red}{\num{103.767}} & \num{106.618} & \num{117.254}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{124.793} & \num{124.295} & \textcolor{red}{\num{104.97}} & \num{109.037} & \num{113.26}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{105.356} & \num{120.872} & \textcolor{red}{\num{101.147}} & \num{101.482} & \num{104.082}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{111.789} & \num{126.541} & \textcolor{red}{\num{103.295}} & \num{105.713} & \num{111.532}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{85.824}} & \num{86.897} & \num{86.348} & \num{86.483} & \num{86.751}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{73.77} & \num{75.428} & \textcolor{red}{\num{73.008}} & \num{73.183} & \num{73.318}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{95.725} & \num{97.561} & \num{95.524} & \num{95.555} & \textcolor{red}{\num{95.016}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{85.106} & \num{86.629} & \textcolor{red}{\num{84.96}} & \num{85.074} & \num{85.029}\\*
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
     & asset01 & \textcolor{red}{\num{1.263}} & \num{1.28} & \num{1.264} & \num{1.267} & \num{1.268}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.989}} & \num{0.998} & \num{0.99} & \num{0.99} & \num{0.991}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.448} & \num{1.548} & \textcolor{red}{\num{1.446}} & \num{1.457} & \num{1.489}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{1.233}} & \num{1.275} & \num{1.233} & \num{1.238} & \num{1.249}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.878} & \num{0.882} & \textcolor{red}{\num{0.878}} & \num{0.88} & \num{0.879}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.747}} & \num{0.76} & \num{0.748} & \num{0.748} & \num{0.749}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.936} & \num{0.989} & \textcolor{red}{\num{0.935}} & \num{0.941} & \num{0.954}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.854} & \num{0.877} & \textcolor{red}{\num{0.853}} & \num{0.856} & \num{0.861}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{101.038} & \num{115.051} & \textcolor{red}{\num{99.877}} & \num{102.101} & \num{105.501}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{102.485}} & \num{154.436} & \num{103.876} & \num{109.727} & \num{116.014}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{100.92} & \num{123.42} & \textcolor{red}{\num{100.818}} & \num{102.043} & \num{104.648}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{101.481}} & \num{130.969} & \num{101.524} & \num{104.624} & \num{108.721}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{86.728} & \num{87.129} & \textcolor{red}{\num{86.716}} & \num{86.867} & \num{86.785}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{72.679}} & \num{73.943} & \num{72.744} & \num{72.815} & \num{72.9}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{91.972} & \num{97.082} & \textcolor{red}{\num{91.848}} & \num{92.47} & \num{93.804}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{83.793} & \num{86.051} & \textcolor{red}{\num{83.769}} & \num{84.051} & \num{84.496}\\*
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

<img src="../output/figs/DGP-4-medcvonefig-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-4-medcvfivefig-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-4-medcvtwentyfig-1.png" width="70%" style="display: block; margin: auto;" />

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
     & asset01 & \num{0.972} & \num{0.991} & \textcolor{red}{\num{0.941}} & \num{0.968} & \num{0.971}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.991} & \num{0.973} & \textcolor{red}{\num{0.93}} & \num{0.936} & \num{0.943}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.483} & \num{1.494} & \num{1.437} & \num{1.44} & \textcolor{red}{\num{1.436}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.649} & \num{1.664} & \textcolor{red}{\num{1.583}} & \num{1.599} & \num{1.594}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.799} & \textcolor{red}{\num{0.786}} & \num{0.8} & \num{0.809} & \num{0.798}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.49} & \num{1.422} & \textcolor{red}{\num{1.416}} & \num{1.445} & \num{1.432}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.712} & \num{0.696} & \num{0.68} & \textcolor{red}{\num{0.673}} & \num{0.681}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.289} & \num{1.277} & \num{1.241} & \num{1.231} & \textcolor{red}{\num{1.229}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.307} & \textcolor{red}{\num{1.293}} & \num{1.301} & \num{1.307} & \num{1.311}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.188} & \num{1.177} & \textcolor{red}{\num{1.148}} & \num{1.157} & \num{1.155}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.769} & \num{0.774} & \textcolor{red}{\num{0.753}} & \num{0.762} & \num{0.77}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.794} & \num{0.804} & \textcolor{red}{\num{0.78}} & \num{0.782} & \num{0.786}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.999} & \num{0.997} & \num{0.978} & \num{0.977} & \textcolor{red}{\num{0.974}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.017} & \num{1.02} & \textcolor{red}{\num{0.993}} & \num{0.999} & \num{0.997}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.711} & \textcolor{red}{\num{0.71}} & \num{0.722} & \num{0.731} & \num{0.727}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.968} & \num{0.973} & \textcolor{red}{\num{0.944}} & \num{0.956} & \num{0.955}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.685} & \num{0.675} & \num{0.673} & \textcolor{red}{\num{0.666}} & \num{0.67}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.934} & \num{0.931} & \num{0.917} & \num{0.911} & \textcolor{red}{\num{0.909}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.94} & \textcolor{red}{\num{0.939}} & \num{0.939} & \num{0.942} & \num{0.944}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.869} & \num{0.869} & \textcolor{red}{\num{0.856}} & \num{0.858} & \num{0.859}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{150.509} & \num{134.134} & \num{131.195} & \textcolor{red}{\num{124.334}} & \num{127.743}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{197.023} & \num{200.966} & \num{191.961} & \textcolor{red}{\num{189.667}} & \num{192.902}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{157.134} & \num{152.267} & \num{128.01} & \num{130.005} & \textcolor{red}{\num{125.745}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{162.85}} & \num{201.81} & \num{201.697} & \num{211.147} & \num{212.472}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{138.017}} & \num{160.983} & \num{146.947} & \num{153.797} & \num{152.957}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{252.301}} & \num{311.379} & \num{266.002} & \num{275.868} & \num{278.243}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{169.064} & \num{157.966} & \num{147.356} & \num{141.894} & \textcolor{red}{\num{141.538}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{212.525} & \num{157.36} & \num{154.998} & \num{142.073} & \textcolor{red}{\num{137.881}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{143.998} & \num{160.685} & \textcolor{red}{\num{138.783}} & \num{139.084} & \num{141.881}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{175.936} & \num{181.95} & \textcolor{red}{\num{167.439}} & \num{167.541} & \num{167.929}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{74.934} & \num{75.36} & \textcolor{red}{\num{73.298}} & \num{74.139} & \num{75.113}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{75.805} & \num{76.586} & \textcolor{red}{\num{74.327}} & \num{74.422} & \num{74.79}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{94.953} & \num{94.765} & \num{93.246} & \num{93.195} & \textcolor{red}{\num{92.964}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{95.023} & \num{95.435} & \textcolor{red}{\num{93.131}} & \num{93.795} & \num{93.553}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{67.614} & \textcolor{red}{\num{67.397}} & \num{68.766} & \num{69.666} & \num{69.435}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{93.192} & \num{93.136} & \textcolor{red}{\num{90.4}} & \num{91.554} & \num{91.485}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{65.268} & \num{64.626} & \num{64.5} & \textcolor{red}{\num{63.812}} & \num{64.274}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{88.297} & \num{88.337} & \num{86.902} & \num{86.519} & \textcolor{red}{\num{86.252}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{89.877} & \textcolor{red}{\num{89.339}} & \num{89.574} & \num{89.716} & \num{89.838}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{82.774} & \num{82.776} & \textcolor{red}{\num{81.572}} & \num{81.869} & \num{81.967}\\*
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
     & asset01 & \num{1.063} & \num{1.127} & \num{1.043} & \textcolor{red}{\num{1.041}} & \num{1.054}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.237} & \num{1.327} & \num{1.201} & \textcolor{red}{\num{1.198}} & \num{1.204}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1.445}} & \num{1.476} & \num{1.457} & \num{1.459} & \num{1.463}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.897} & \num{1.945} & \num{1.844} & \num{1.838} & \textcolor{red}{\num{1.833}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.893} & \num{0.915} & \textcolor{red}{\num{0.881}} & \num{0.889} & \num{0.91}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{2.296} & \textcolor{red}{\num{2.051}} & \num{2.213} & \num{2.222} & \num{2.202}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.722} & \textcolor{red}{\num{0.696}} & \num{0.704} & \num{0.704} & \num{0.699}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.327} & \num{1.357} & \num{1.292} & \num{1.288} & \textcolor{red}{\num{1.279}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.538} & \num{1.56} & \textcolor{red}{\num{1.534}} & \num{1.536} & \num{1.544}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.38} & \num{1.384} & \textcolor{red}{\num{1.352}} & \num{1.353} & \num{1.354}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.822} & \num{0.853} & \textcolor{red}{\num{0.82}} & \num{0.82} & \num{0.831}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.898} & \num{0.929} & \num{0.877} & \textcolor{red}{\num{0.875}} & \num{0.888}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.978} & \num{0.987} & \num{0.974} & \textcolor{red}{\num{0.973}} & \num{0.973}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.075} & \num{1.082} & \num{1.056} & \num{1.052} & \textcolor{red}{\num{1.052}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.783} & \num{0.79} & \textcolor{red}{\num{0.771}} & \num{0.776} & \num{0.78}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.198} & \textcolor{red}{\num{1.124}} & \num{1.156} & \num{1.151} & \num{1.151}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.682} & \textcolor{red}{\num{0.657}} & \num{0.666} & \num{0.664} & \num{0.661}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.919} & \num{0.929} & \num{0.915} & \num{0.915} & \textcolor{red}{\num{0.908}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{1.009}} & \num{1.017} & \num{1.015} & \num{1.018} & \num{1.021}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.929} & \num{0.93} & \num{0.917} & \textcolor{red}{\num{0.916}} & \num{0.918}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{104.584} & \num{114.931} & \num{97.62} & \textcolor{red}{\num{97.095}} & \num{108.276}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{123.383} & \num{126.691} & \num{103.867} & \textcolor{red}{\num{101.744}} & \num{123.031}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{114.762} & \num{120.212} & \num{103.704} & \num{101.666} & \textcolor{red}{\num{98.939}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{130.144} & \num{134.703} & \num{105.444} & \textcolor{red}{\num{101.122}} & \num{105.035}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{114.416} & \num{155.083} & \textcolor{red}{\num{99.988}} & \num{100.851} & \num{109.072}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{179.485} & \num{226.612} & \num{140.487} & \textcolor{red}{\num{121.282}} & \num{136.841}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{121.806} & \num{117.119} & \num{102.095} & \textcolor{red}{\num{100.964}} & \num{102.691}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{130.94} & \num{124.801} & \num{105.063} & \textcolor{red}{\num{103.088}} & \num{120.902}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{98.695}} & \num{120.258} & \num{99.615} & \num{99.82} & \num{102.76}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{124.246} & \num{137.823} & \num{106.431} & \textcolor{red}{\num{103.07}} & \num{111.95}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{78.489} & \num{81.588} & \textcolor{red}{\num{78.244}} & \num{78.295} & \num{79.351}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{87.11} & \num{89.759} & \num{85.177} & \textcolor{red}{\num{85.04}} & \num{86.207}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{92.287} & \num{93.395} & \num{91.852} & \textcolor{red}{\num{91.766}} & \num{91.796}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{104.93} & \num{105.913} & \num{103.289} & \num{103} & \textcolor{red}{\num{102.916}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{72.685} & \num{73.734} & \textcolor{red}{\num{71.592}} & \num{72.083} & \num{72.436}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{114.397} & \textcolor{red}{\num{107.159}} & \num{110.423} & \num{110.002} & \num{109.936}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{65.596} & \textcolor{red}{\num{63.203}} & \num{63.883} & \num{63.73} & \num{63.406}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{86.038} & \num{87.185} & \num{85.721} & \num{85.686} & \textcolor{red}{\num{85.036}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{96.292}} & \num{96.668} & \num{96.933} & \num{97.213} & \num{97.389}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{88.647} & \num{88.734} & \num{87.457} & \textcolor{red}{\num{87.424}} & \num{87.608}\\*
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
     & asset01 & \num{1.033} & \num{1.073} & \num{1.032} & \num{1.032} & \textcolor{red}{\num{1.029}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.211} & \num{1.34} & \num{1.212} & \num{1.212} & \textcolor{red}{\num{1.21}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.36} & \num{1.368} & \num{1.357} & \num{1.358} & \textcolor{red}{\num{1.352}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.936} & \num{2.064} & \num{1.929} & \num{1.929} & \textcolor{red}{\num{1.926}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.814} & \num{0.885} & \textcolor{red}{\num{0.806}} & \num{0.807} & \num{0.825}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{2.358} & \textcolor{red}{\num{2.298}} & \num{2.331} & \num{2.33} & \num{2.325}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.675} & \num{0.673} & \num{0.67} & \textcolor{red}{\num{0.669}} & \num{0.672}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{1.279}} & \num{1.343} & \num{1.28} & \num{1.279} & \num{1.281}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.603} & \num{1.626} & \num{1.6} & \textcolor{red}{\num{1.599}} & \num{1.607}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.363} & \num{1.408} & \num{1.357} & \textcolor{red}{\num{1.357}} & \num{1.359}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.813} & \num{0.829} & \num{0.812} & \num{0.813} & \textcolor{red}{\num{0.812}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.886} & \num{0.931} & \num{0.887} & \num{0.887} & \textcolor{red}{\num{0.886}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.935} & \num{0.938} & \num{0.934} & \num{0.935} & \textcolor{red}{\num{0.933}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.086} & \num{1.124} & \num{1.086} & \num{1.086} & \textcolor{red}{\num{1.085}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.757} & \num{0.786} & \textcolor{red}{\num{0.75}} & \num{0.75} & \num{0.76}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.177} & \num{1.174} & \num{1.168} & \num{1.168} & \textcolor{red}{\num{1.167}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.641} & \textcolor{red}{\num{0.629}} & \num{0.639} & \num{0.639} & \num{0.639}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{0.906}} & \num{0.935} & \num{0.907} & \num{0.907} & \num{0.907}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.042} & \num{1.042} & \num{1.04} & \num{1.04} & \textcolor{red}{\num{1.038}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.916} & \num{0.932} & \textcolor{red}{\num{0.914}} & \num{0.914} & \num{0.914}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{100.211} & \num{114.014} & \textcolor{red}{\num{99.908}} & \num{100.421} & \num{100.354}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{100.471} & \num{125.687} & \num{99.736} & \num{99.724} & \textcolor{red}{\num{98.923}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{99.411}} & \num{115.454} & \num{100.14} & \num{100.287} & \num{99.709}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{108.305} & \num{192.47} & \textcolor{red}{\num{101.231}} & \num{102.071} & \num{103.552}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{105.162} & \num{147.02} & \num{100.771} & \textcolor{red}{\num{100.363}} & \num{103.014}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{106.775} & \num{198.363} & \textcolor{red}{\num{97.847}} & \num{99.249} & \num{100.425}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{101.184} & \textcolor{red}{\num{99.485}} & \num{99.816} & \num{99.875} & \num{99.6}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{99.676}} & \num{120.197} & \num{100.271} & \num{100.095} & \num{101.275}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{100.103} & \num{116.296} & \num{100.016} & \textcolor{red}{\num{99.866}} & \num{100.73}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{102.367} & \num{136.554} & \textcolor{red}{\num{99.971}} & \num{100.217} & \num{100.843}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{78.824} & \num{80.254} & \num{78.755} & \num{78.801} & \textcolor{red}{\num{78.675}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{83.955}} & \num{88.05} & \num{84.043} & \num{84.048} & \num{83.972}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{88.67} & \num{88.736} & \num{88.593} & \num{88.608} & \textcolor{red}{\num{88.468}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{101.772} & \num{105.108} & \num{101.689} & \num{101.697} & \textcolor{red}{\num{101.64}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{72.966} & \num{75.734} & \textcolor{red}{\num{72.192}} & \num{72.213} & \num{73.201}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{112.807} & \num{113.224} & \num{111.855} & \num{111.841} & \textcolor{red}{\num{111.825}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{61.528} & \textcolor{red}{\num{60.51}} & \num{61.372} & \num{61.344} & \num{61.411}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{85.459}} & \num{88.427} & \num{85.591} & \num{85.575} & \num{85.569}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{99.251} & \num{98.898} & \num{99.063} & \num{99.03} & \textcolor{red}{\num{98.814}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{87.248} & \num{88.771} & \textcolor{red}{\num{87.017}} & \num{87.017} & \num{87.064}\\*
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

<img src="../output/figs/DGP-4-largecvonefig-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-4-largecvfivefig-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-4-largecvtwentyfig-1.png" width="70%" style="display: block; margin: auto;" />

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
     & asset01 & \num{1.138} & \num{1.205} & \num{1.12} & \num{1.122} & \textcolor{red}{\num{1.12}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.66} & \num{1.593} & \num{1.479} & \textcolor{red}{\num{1.477}} & \num{1.482}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1.593}} & \num{1.626} & \num{1.641} & \num{1.646} & \num{1.611}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.997} & \textcolor{red}{\num{0.992}} & \num{1.005} & \num{1.008} & \num{0.995}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{1.037} & \num{0.988} & \num{0.959} & \num{0.959} & \textcolor{red}{\num{0.957}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.357} & \num{1.322} & \num{1.312} & \num{1.314} & \textcolor{red}{\num{1.309}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1.25} & \num{1.315} & \num{1.179} & \textcolor{red}{\num{1.178}} & \num{1.181}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.425} & \num{1.44} & \num{1.299} & \textcolor{red}{\num{1.294}} & \num{1.301}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.059} & \num{1.036} & \num{1.031} & \textcolor{red}{\num{1.028}} & \num{1.028}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{1.058} & \num{1.071} & \num{1.003} & \num{1.004} & \textcolor{red}{\num{0.997}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{1.683} & \num{1.576} & \num{1.512} & \num{1.514} & \textcolor{red}{\num{1.498}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.808} & \num{0.766} & \num{0.76} & \textcolor{red}{\num{0.758}} & \num{0.759}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.255} & \num{1.244} & \num{1.192} & \num{1.192} & \textcolor{red}{\num{1.187}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.866} & \num{0.886} & \textcolor{red}{\num{0.859}} & \num{0.86} & \num{0.86}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.054} & \num{1.005} & \num{0.989} & \textcolor{red}{\num{0.988}} & \num{0.989}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.963} & \num{0.967} & \num{0.965} & \num{0.966} & \textcolor{red}{\num{0.951}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.796}} & \num{0.803} & \num{0.805} & \num{0.806} & \num{0.802}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.807} & \num{0.796} & \num{0.79} & \num{0.79} & \textcolor{red}{\num{0.788}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.927} & \textcolor{red}{\num{0.9}} & \num{0.907} & \num{0.907} & \num{0.905}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.893} & \num{0.939} & \num{0.882} & \num{0.881} & \textcolor{red}{\num{0.881}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.957} & \num{0.961} & \num{0.915} & \textcolor{red}{\num{0.912}} & \num{0.913}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.811} & \textcolor{red}{\num{0.791}} & \num{0.795} & \num{0.793} & \num{0.793}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.811} & \num{0.812} & \num{0.786} & \num{0.788} & \textcolor{red}{\num{0.786}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{1.013} & \num{0.995} & \num{0.989} & \num{0.99} & \textcolor{red}{\num{0.986}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.741} & \num{0.717} & \num{0.705} & \textcolor{red}{\num{0.704}} & \num{0.704}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.887} & \num{0.881} & \num{0.866} & \num{0.865} & \textcolor{red}{\num{0.863}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{215.703} & \num{263.572} & \num{158.229} & \num{163.003} & \textcolor{red}{\num{157.49}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{184.269} & \num{147.305} & \num{147.721} & \num{147.498} & \textcolor{red}{\num{146.278}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{216.906} & \num{168.911} & \num{126.256} & \textcolor{red}{\num{124.555}} & \num{135.568}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{522.867} & \num{497.428} & \num{241.969} & \textcolor{red}{\num{236.265}} & \num{264.143}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{195.777} & \num{187.544} & \num{147.97} & \num{148.856} & \textcolor{red}{\num{147.619}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{136.212} & \textcolor{red}{\num{124.034}} & \num{125.755} & \num{125.024} & \num{126.768}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{238.888} & \num{213.854} & \num{105.916} & \textcolor{red}{\num{100}} & \num{106.952}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{494.391} & \num{349.338} & \num{286.318} & \num{285.619} & \textcolor{red}{\num{281.171}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{139.728} & \num{141.046} & \num{106.345} & \num{105.758} & \textcolor{red}{\num{105.743}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{321.611} & \textcolor{red}{\num{127.633}} & \num{232.191} & \num{230.241} & \num{212.36}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{124.192} & \textcolor{red}{\num{117.592}} & \num{118.498} & \num{118.689} & \num{120.617}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{305.903} & \textcolor{red}{\num{219.266}} & \num{222.674} & \num{221.921} & \num{222.7}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{258.037} & \num{213.127} & \num{168.32} & \textcolor{red}{\num{167.286}} & \num{168.951}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{79.843} & \num{81.768} & \textcolor{red}{\num{79.511}} & \num{79.581} & \num{79.563}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{97.309} & \num{92.868} & \num{91.653} & \textcolor{red}{\num{91.601}} & \num{91.648}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{89.473} & \num{89.906} & \num{89.248} & \num{89.317} & \textcolor{red}{\num{88.052}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{74.471}} & \num{75.156} & \num{75.432} & \num{75.594} & \num{75.203}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{75.642} & \num{74.906} & \num{73.862} & \num{73.9} & \textcolor{red}{\num{73.686}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{86.28} & \textcolor{red}{\num{83.923}} & \num{84.705} & \num{84.717} & \num{84.531}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{82.823} & \num{86.617} & \num{81.578} & \num{81.488} & \textcolor{red}{\num{81.454}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{89.117} & \num{89.519} & \num{85.226} & \textcolor{red}{\num{85.002}} & \num{85.043}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{76.076} & \num{74.886} & \num{74.918} & \textcolor{red}{\num{74.757}} & \num{74.76}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{75.516} & \num{75.425} & \num{73.028} & \num{73.142} & \textcolor{red}{\num{73.023}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{94.688} & \num{93.29} & \num{93.1} & \num{93.237} & \textcolor{red}{\num{92.728}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{68.592} & \num{66.107} & \num{65.231} & \textcolor{red}{\num{65.113}} & \num{65.132}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{82.486} & \num{82.031} & \num{80.624} & \num{80.621} & \textcolor{red}{\num{80.402}}\\*
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
     & asset01 & \num{1.068} & \num{1.183} & \num{1.043} & \num{1.044} & \textcolor{red}{\num{1.042}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.527} & \num{1.583} & \num{1.499} & \textcolor{red}{\num{1.498}} & \num{1.504}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.71} & \num{1.784} & \num{1.695} & \num{1.695} & \textcolor{red}{\num{1.685}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.231} & \textcolor{red}{\num{1.157}} & \num{1.189} & \num{1.188} & \num{1.177}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{1.004} & \textcolor{red}{\num{0.989}} & \num{0.998} & \num{0.999} & \num{0.998}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.334} & \num{1.417} & \num{1.322} & \textcolor{red}{\num{1.322}} & \num{1.325}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1.216} & \num{1.236} & \textcolor{red}{\num{1.172}} & \num{1.172} & \num{1.175}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.657} & \num{1.696} & \num{1.603} & \textcolor{red}{\num{1.603}} & \num{1.61}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.087} & \textcolor{red}{\num{1.079}} & \num{1.111} & \num{1.111} & \num{1.111}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{1.382} & \num{1.357} & \num{1.196} & \textcolor{red}{\num{1.195}} & \num{1.241}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{1.835} & \num{1.949} & \textcolor{red}{\num{1.81}} & \num{1.811} & \num{1.815}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{1.062} & \textcolor{red}{\num{1.025}} & \num{1.049} & \num{1.05} & \num{1.046}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.343} & \num{1.371} & \textcolor{red}{\num{1.307}} & \num{1.307} & \num{1.311}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.862} & \num{0.901} & \num{0.845} & \textcolor{red}{\num{0.845}} & \num{0.846}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.01} & \num{1.019} & \num{1.004} & \textcolor{red}{\num{1.004}} & \num{1.007}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.986} & \num{1.016} & \num{0.982} & \num{0.982} & \textcolor{red}{\num{0.981}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.891} & \textcolor{red}{\num{0.852}} & \num{0.875} & \num{0.874} & \num{0.869}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.783} & \num{0.774} & \num{0.774} & \num{0.774} & \textcolor{red}{\num{0.773}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.899} & \num{0.924} & \num{0.895} & \textcolor{red}{\num{0.895}} & \num{0.897}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.879} & \num{0.906} & \textcolor{red}{\num{0.875}} & \num{0.875} & \num{0.876}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.032} & \num{1.056} & \textcolor{red}{\num{1.025}} & \num{1.025} & \num{1.027}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.819} & \textcolor{red}{\num{0.804}} & \num{0.83} & \num{0.83} & \num{0.83}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.976} & \num{0.978} & \num{0.916} & \textcolor{red}{\num{0.916}} & \num{0.933}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{1.094}} & \num{1.143} & \num{1.112} & \num{1.113} & \num{1.115}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.838} & \num{0.829} & \num{0.828} & \num{0.828} & \textcolor{red}{\num{0.828}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.922} & \num{0.933} & \textcolor{red}{\num{0.913}} & \num{0.914} & \num{0.915}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{118.795} & \num{266.33} & \textcolor{red}{\num{99.911}} & \num{100} & \num{106.671}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{111.176} & \num{111.552} & \num{100.039} & \textcolor{red}{\num{100.039}} & \num{102.625}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{122.902} & \num{128.268} & \textcolor{red}{\num{99.999}} & \num{100} & \num{102.849}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{120.865} & \num{222.222} & \num{99.573} & \textcolor{red}{\num{99.331}} & \num{118.349}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{133.503} & \num{134.354} & \num{100.24} & \textcolor{red}{\num{100.044}} & \num{100.846}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{116.641} & \num{120.359} & \num{100.153} & \textcolor{red}{\num{100.045}} & \num{101.838}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{207.077} & \num{210.107} & \num{100.043} & \textcolor{red}{\num{100}} & \num{103.784}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{208.515} & \num{170.303} & \textcolor{red}{\num{99.387}} & \num{99.96} & \num{103.615}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{110.222} & \num{102.187} & \textcolor{red}{\num{99.919}} & \num{100.001} & \num{100.001}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{226.605} & \num{405.654} & \num{128.293} & \textcolor{red}{\num{125.736}} & \num{228.019}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{97.303}} & \num{121.644} & \num{100.065} & \num{100.393} & \num{102.448}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{171.472} & \num{174.628} & \textcolor{red}{\num{98.444}} & \num{99.053} & \num{100.245}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{145.423} & \num{180.634} & \num{102.172} & \textcolor{red}{\num{102.05}} & \num{114.274}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{80.832} & \num{84.48} & \num{79.319} & \textcolor{red}{\num{79.318}} & \num{79.34}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{94.291} & \num{95.334} & \num{93.72} & \textcolor{red}{\num{93.716}} & \num{93.96}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{91.397} & \num{93.912} & \num{90.807} & \num{90.803} & \textcolor{red}{\num{90.72}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{82.098} & \textcolor{red}{\num{78.523}} & \num{80.591} & \num{80.562} & \num{80.15}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{72.917} & \num{71.831} & \num{71.877} & \num{71.877} & \textcolor{red}{\num{71.813}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{83.275} & \num{85.523} & \num{83.035} & \textcolor{red}{\num{83.03}} & \num{83.212}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{81.579} & \num{84.393} & \textcolor{red}{\num{81.036}} & \num{81.046} & \num{81.176}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{97.013} & \num{99.359} & \textcolor{red}{\num{96.235}} & \num{96.251} & \num{96.445}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{77.14} & \textcolor{red}{\num{76.082}} & \num{78.075} & \num{78.09} & \num{78.09}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{90.313} & \num{90.322} & \num{84.681} & \textcolor{red}{\num{84.64}} & \num{86.555}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{101.533}} & \num{106.258} & \num{103.202} & \num{103.275} & \num{103.503}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{77.067} & \num{76.429} & \num{76.365} & \num{76.391} & \textcolor{red}{\num{76.329}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{85.788} & \num{86.871} & \textcolor{red}{\num{84.912}} & \num{84.917} & \num{85.108}\\*
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
     & asset01 & \num{1.087} & \num{1.175} & \num{1.08} & \num{1.08} & \textcolor{red}{\num{1.08}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.514} & \num{1.549} & \textcolor{red}{\num{1.502}} & \num{1.502} & \num{1.502}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1.671}} & \num{1.754} & \num{1.682} & \num{1.682} & \num{1.681}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.169} & \textcolor{red}{\num{1.144}} & \num{1.186} & \num{1.186} & \num{1.184}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.837} & \textcolor{red}{\num{0.821}} & \num{0.83} & \num{0.83} & \num{0.83}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{1.378}} & \num{1.508} & \num{1.381} & \num{1.381} & \num{1.381}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1.057} & \num{1.093} & \num{1.044} & \textcolor{red}{\num{1.044}} & \num{1.045}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.64} & \num{1.673} & \textcolor{red}{\num{1.631}} & \num{1.631} & \num{1.631}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.154} & \num{1.18} & \num{1.146} & \textcolor{red}{\num{1.146}} & \num{1.146}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{1.161}} & \num{1.194} & \num{1.166} & \num{1.166} & \num{1.166}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{1.778}} & \num{1.846} & \num{1.779} & \num{1.779} & \num{1.778}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{1.039} & \textcolor{red}{\num{1.033}} & \num{1.038} & \num{1.038} & \num{1.039}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.29} & \num{1.331} & \num{1.289} & \num{1.289} & \textcolor{red}{\num{1.289}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.872} & \num{0.9} & \textcolor{red}{\num{0.867}} & \num{0.867} & \num{0.867}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.003} & \num{1.008} & \textcolor{red}{\num{0.998}} & \num{0.998} & \num{0.998}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.981} & \num{0.996} & \num{0.981} & \num{0.981} & \textcolor{red}{\num{0.98}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.864} & \textcolor{red}{\num{0.86}} & \num{0.871} & \num{0.871} & \num{0.87}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.724} & \textcolor{red}{\num{0.716}} & \num{0.72} & \num{0.72} & \num{0.72}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.918}} & \num{0.95} & \num{0.92} & \num{0.92} & \num{0.92}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.84} & \num{0.861} & \num{0.836} & \num{0.836} & \textcolor{red}{\num{0.835}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.031} & \num{1.044} & \textcolor{red}{\num{1.029}} & \num{1.029} & \num{1.029}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.844} & \textcolor{red}{\num{0.836}} & \num{0.842} & \num{0.842} & \num{0.842}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.888}} & \num{0.903} & \num{0.895} & \num{0.895} & \num{0.896}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{1.1} & \num{1.128} & \num{1.095} & \num{1.095} & \textcolor{red}{\num{1.095}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.82} & \num{0.825} & \textcolor{red}{\num{0.819}} & \num{0.819} & \num{0.819}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.907} & \num{0.919} & \num{0.906} & \num{0.906} & \textcolor{red}{\num{0.906}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{102.119} & \num{133.566} & \textcolor{red}{\num{99.989}} & \num{100} & \num{100.224}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{101.258} & \textcolor{red}{\num{99.849}} & \num{99.997} & \num{100} & \num{99.968}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{104.954} & \num{123.522} & \num{100.001} & \textcolor{red}{\num{100}} & \num{100.386}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{108.069} & \num{189.246} & \num{100.009} & \num{100} & \textcolor{red}{\num{99.77}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{103.255} & \num{123.227} & \num{100.001} & \num{100} & \textcolor{red}{\num{99.999}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{98.599}} & \num{110.791} & \num{99.998} & \num{100} & \num{99.97}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{111.537} & \num{172.161} & \num{100.002} & \textcolor{red}{\num{100}} & \num{100.95}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{111.274} & \num{135.293} & \num{99.974} & \num{99.992} & \textcolor{red}{\num{99.577}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{99.456} & \textcolor{red}{\num{98.451}} & \num{99.999} & \num{100} & \num{100}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{195.881} & \num{206.283} & \textcolor{red}{\num{99.265}} & \num{99.421} & \num{118.983}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{104.832} & \num{119.513} & \num{99.995} & \num{99.996} & \textcolor{red}{\num{99.931}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{106.881} & \num{238.491} & \num{99.978} & \num{99.969} & \textcolor{red}{\num{99.953}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{112.343} & \num{145.866} & \textcolor{red}{\num{99.934}} & \num{99.948} & \num{101.643}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{79.627} & \num{82.081} & \textcolor{red}{\num{79.195}} & \num{79.196} & \num{79.197}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{93.42} & \num{94.029} & \textcolor{red}{\num{92.946}} & \num{92.946} & \num{92.954}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{92.189} & \num{93.637} & \num{92.183} & \num{92.183} & \textcolor{red}{\num{92.14}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{81.412} & \textcolor{red}{\num{80.639}} & \num{82.061} & \num{82.061} & \num{81.993}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{66.793} & \textcolor{red}{\num{66.153}} & \num{66.442} & \num{66.442} & \num{66.442}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{84.052}} & \num{86.738} & \num{84.172} & \num{84.173} & \num{84.173}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{78.067} & \num{80.712} & \num{77.806} & \num{77.806} & \textcolor{red}{\num{77.779}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{95.509} & \num{96.887} & \textcolor{red}{\num{95.29}} & \num{95.29} & \num{95.291}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{76.588} & \textcolor{red}{\num{75.764}} & \num{76.44} & \num{76.44} & \num{76.44}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{83.008}} & \num{84.204} & \num{83.696} & \num{83.696} & \num{83.704}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{103.601} & \num{105.873} & \num{103.199} & \num{103.201} & \textcolor{red}{\num{103.182}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{75.936} & \num{76.286} & \textcolor{red}{\num{75.853}} & \num{75.853} & \num{75.866}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{84.183} & \num{85.25} & \num{84.107} & \num{84.107} & \textcolor{red}{\num{84.097}}\\*
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
    MSE & \num{0.935} & \num{0.952} & \textcolor{red}{\num{0.925}} & \num{0.931} & \num{0.937}\\
    MAE & \num{0.769} & \num{0.772} & \textcolor{red}{\num{0.76}} & \num{0.761} & \num{0.764}\\
    MAPE & \num{173.319} & \num{177.77} & \num{168.23} & \textcolor{red}{\num{163.283}} & \num{164.343}\\
    MASE & \num{75.202} & \num{75.452} & \textcolor{red}{\num{74.304}} & \num{74.375} & \num{74.686}\\
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
    MSE & \textcolor{red}{\num{1.267}} & \num{1.297} & \num{1.274} & \num{1.273} & \num{1.278}\\
    MAE & \num{0.869} & \num{0.884} & \textcolor{red}{\num{0.867}} & \num{0.868} & \num{0.867}\\
    MAPE & \num{111.789} & \num{126.541} & \textcolor{red}{\num{103.295}} & \num{105.713} & \num{111.532}\\
    MASE & \num{85.106} & \num{86.629} & \textcolor{red}{\num{84.96}} & \num{85.074} & \num{85.029}\\
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
    MSE & \textcolor{red}{\num{1.233}} & \num{1.275} & \num{1.233} & \num{1.238} & \num{1.249}\\
    MAE & \num{0.854} & \num{0.877} & \textcolor{red}{\num{0.853}} & \num{0.856} & \num{0.861}\\
    MAPE & \textcolor{red}{\num{101.481}} & \num{130.969} & \num{101.524} & \num{104.624} & \num{108.721}\\
    MASE & \num{83.793} & \num{86.051} & \textcolor{red}{\num{83.769}} & \num{84.051} & \num{84.496}\\
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
    MSE & \num{1.188} & \num{1.177} & \textcolor{red}{\num{1.148}} & \num{1.157} & \num{1.155}\\
    MAE & \num{0.869} & \num{0.869} & \textcolor{red}{\num{0.856}} & \num{0.858} & \num{0.859}\\
    MAPE & \num{175.936} & \num{181.95} & \textcolor{red}{\num{167.439}} & \num{167.541} & \num{167.929}\\
    MASE & \num{82.774} & \num{82.776} & \textcolor{red}{\num{81.572}} & \num{81.869} & \num{81.967}\\
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
    MSE & \num{1.38} & \num{1.384} & \textcolor{red}{\num{1.352}} & \num{1.353} & \num{1.354}\\
    MAE & \num{0.929} & \num{0.93} & \num{0.917} & \textcolor{red}{\num{0.916}} & \num{0.918}\\
    MAPE & \num{124.246} & \num{137.823} & \num{106.431} & \textcolor{red}{\num{103.07}} & \num{111.95}\\
    MASE & \num{88.647} & \num{88.734} & \num{87.457} & \textcolor{red}{\num{87.424}} & \num{87.608}\\
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
    MSE & \num{1.363} & \num{1.408} & \num{1.357} & \textcolor{red}{\num{1.357}} & \num{1.359}\\
    MAE & \num{0.916} & \num{0.932} & \textcolor{red}{\num{0.914}} & \num{0.914} & \num{0.914}\\
    MAPE & \num{102.367} & \num{136.554} & \textcolor{red}{\num{99.971}} & \num{100.217} & \num{100.843}\\
    MASE & \num{87.248} & \num{88.771} & \textcolor{red}{\num{87.017}} & \num{87.017} & \num{87.064}\\
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
    MSE & \num{1.255} & \num{1.244} & \num{1.192} & \num{1.192} & \textcolor{red}{\num{1.187}}\\
    MAE & \num{0.887} & \num{0.881} & \num{0.866} & \num{0.865} & \textcolor{red}{\num{0.863}}\\
    MAPE & \num{258.037} & \num{213.127} & \num{168.32} & \textcolor{red}{\num{167.286}} & \num{168.951}\\
    MASE & \num{82.486} & \num{82.031} & \num{80.624} & \num{80.621} & \textcolor{red}{\num{80.402}}\\
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
    MSE & \num{1.343} & \num{1.371} & \textcolor{red}{\num{1.307}} & \num{1.307} & \num{1.311}\\
    MAE & \num{0.922} & \num{0.933} & \textcolor{red}{\num{0.913}} & \num{0.914} & \num{0.915}\\
    MAPE & \num{145.423} & \num{180.634} & \num{102.172} & \textcolor{red}{\num{102.05}} & \num{114.274}\\
    MASE & \num{85.788} & \num{86.871} & \textcolor{red}{\num{84.912}} & \num{84.917} & \num{85.108}\\
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
    MSE & \num{1.29} & \num{1.331} & \num{1.289} & \num{1.289} & \textcolor{red}{\num{1.289}}\\
    MAE & \num{0.907} & \num{0.919} & \num{0.906} & \num{0.906} & \textcolor{red}{\num{0.906}}\\
    MAPE & \num{112.343} & \num{145.866} & \textcolor{red}{\num{99.934}} & \num{99.948} & \num{101.643}\\
    MASE & \num{84.183} & \num{85.25} & \num{84.107} & \num{84.107} & \textcolor{red}{\num{84.097}}\\
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
