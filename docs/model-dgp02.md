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
    -   [Piecewise Errors](#piecewise-errors)
        -   [SMALL](#small-1)
        -   [MEDIUM](#medium-1)
        -   [LARGE](#large-1)
        -   [Average](#average)
-   [Coefficients](#coefficients)

``` r
sim_data <- "../data/processed/bvarsim_dgp_rw.rds"
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
#> [1]  2.56  1.54  2.87
#> 
#> Setting for 'lambda':
#> [1]  0.126
#> 
#> Setting for 'delta':
#> [1]  0.829  0.223  0.959
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
#> [1]  2.69  3.13  3.63  3.88  3.20  4.61  2.66  3.64  3.76
#> 
#> Setting for 'lambda':
#> [1]  0.141
#> 
#> Setting for 'delta':
#> [1]  0.4885  0.5669  0.0503  0.4754  0.9218  0.7872  0.3159  0.2874  0.5355
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
#>  [1]  4.17  3.92  4.14  4.22  3.94  4.03  4.36  3.93  3.78  3.31  4.20  3.53
#> 
#> Setting for 'lambda':
#> [1]  0.0282
#> 
#> Setting for 'delta':
#>  [1]  0.18960  0.36716  0.24237  0.44974  0.37745  0.35061  0.00645  0.55656
#>  [9]  0.15945  0.86193  0.74107  0.79091
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
#> [1]  2.56  1.55  2.85
#> 
#> Setting for 'lambda':
#> [1]  0.132
#> 
#> Setting for 'delta':
#> [1]  0.832  0.220  0.957
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
#> [1]  2.67  3.27  3.71  4.24  3.22  4.55  2.65  3.73  3.66
#> 
#> Setting for 'lambda':
#> [1]  0.137
#> 
#> Setting for 'delta':
#> [1]  0.4829  0.5862  0.0505  0.4778  0.9323  0.7891  0.3186  0.2846  0.5352
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
#>  [1]  4.16  3.92  4.17  4.23  3.91  4.03  4.37  3.88  3.78  3.31  4.20  3.52
#> 
#> Setting for 'lambda':
#> [1]  0.0258
#> 
#> Setting for 'delta':
#>  [1]  0.19357  0.36636  0.24275  0.44717  0.38259  0.34624  0.00674  0.56039
#>  [9]  0.16630  0.86360  0.74476  0.79182
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
#> [1]  2.54  1.53  2.89
#> 
#> Setting for 'lambda':
#> [1]  0.129
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.832  0.220  0.935
#> 
#> Setting for 'weekly':
#> [1]  0.000  0.000  0.085
#> 
#> Setting for 'monthly':
#> [1]  0.00000  0.00000  0.00796
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
#> [1]  2.62  3.15  3.59  4.15  3.33  4.47  2.59  3.68  3.60
#> 
#> Setting for 'lambda':
#> [1]  0.132
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.4528  0.5655  0.0503  0.4782  0.8675  0.7891  0.3164  0.2636  0.5330
#> 
#> Setting for 'weekly':
#> [1]  0.1780  0.1073  0.0000  0.0000  0.2013  0.0000  0.0211  0.1592  0.0000
#> 
#> Setting for 'monthly':
#> [1]  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000  0.0791
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
#>  [1]  3.81  3.78  3.82  3.89  3.79  3.80  3.85  3.84  3.77  4.00  4.02  4.21
#> 
#> Setting for 'lambda':
#> [1]  0.0237
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#>  [1]  0.1937  0.3664  0.2242  0.4461  0.3828  0.3462  0.0012  0.5606  0.1669
#> [10]  0.8419  0.7313  0.7920
#> 
#> Setting for 'weekly':
#>  [1]  0.00000  0.00000  0.13196  0.00746  0.00000  0.00000  0.00000  0.00000
#>  [9]  0.00000  0.05771  0.04823  0.00000
#> 
#> Setting for 'monthly':
#>  [1]  0.000  0.000  0.000  0.000  0.000  0.000  0.326  0.000  0.000  0.000
#> [11]  0.000  0.000
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
    \caption{\label{tab:empdgp2}Empirical Bayes Results for DGP2.}\\
    \toprule
     &    &     & y1 & y2 & y3 & y4 & y5 & y6 & y7 & y8 & y9 & y10 & y11 & y12\\
    \midrule
    \endfirsthead
    \caption[]{Empirical Bayes Results for DGP2. \textit{(continued)}}\\
    \toprule
      &    &     & y1 & y2 & y3 & y4 & y5 & y6 & y7 & y8 & y9 & y10 & y11 & y12\\
    \midrule
    \endhead

    \endfoot
    \bottomrule
    \endlastfoot
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{SMALL}}\\
    \hspace{1em} & BVAR & $\sigma$ & 2.556 & 1.540 & 2.866 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.126 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.829 & 0.223 & 0.959 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 2.558 & 1.545 & 2.853 &  &  &  &  &  &  &  &  & \\

    \hspace{1em}\hspace{1em} &  & $\lambda$ & 0.132 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.832 & 0.220 & 0.957 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 2.544 & 1.531 & 2.889 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.129 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.832 & 0.220 & 0.935 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.000 & 0.000 & 0.085 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.000 & 0.000 & 0.008 &  &  &  &  &  &  &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{MEDIUM}}\\
    \hspace{1em} & BVAR & $\sigma$ & 2.689 & 3.127 & 3.633 & 3.877 & 3.198 & 4.611 & 2.661 & 3.637 & 3.756 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.141 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.489 & 0.567 & 0.050 & 0.475 & 0.922 & 0.787 & 0.316 & 0.287 & 0.535 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 2.669 & 3.269 & 3.711 & 4.240 & 3.220 & 4.554 & 2.645 & 3.725 & 3.661 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.137 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.483 & 0.586 & 0.050 & 0.478 & 0.932 & 0.789 & 0.319 & 0.285 & 0.535 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 2.625 & 3.149 & 3.587 & 4.147 & 3.334 & 4.469 & 2.591 & 3.678 & 3.599 &  &  & \\

     &  & $\lambda$ & 0.132 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.453 & 0.565 & 0.050 & 0.478 & 0.867 & 0.789 & 0.316 & 0.264 & 0.533 &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.178 & 0.107 & 0.000 & 0.000 & 0.201 & 0.000 & 0.021 & 0.159 & 0.000 &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.079 &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{LARGE}}\\
    \hspace{1em} & BVAR & $\sigma$ & 4.173 & 3.920 & 4.139 & 4.219 & 3.939 & 4.029 & 4.364 & 3.932 & 3.779 & 3.311 & 4.196 & 3.531\\

    \hspace{1em} &  & $\lambda$ & 0.028 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.190 & 0.367 & 0.242 & 0.450 & 0.377 & 0.351 & 0.006 & 0.557 & 0.159 & 0.862 & 0.741 & 0.791\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 4.165 & 3.917 & 4.168 & 4.235 & 3.914 & 4.027 & 4.367 & 3.877 & 3.781 & 3.309 & 4.202 & 3.516\\

    \hspace{1em} &  & $\lambda$ & 0.026 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.194 & 0.366 & 0.243 & 0.447 & 0.383 & 0.346 & 0.007 & 0.560 & 0.166 & 0.864 & 0.745 & 0.792\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 3.811 & 3.782 & 3.818 & 3.892 & 3.792 & 3.803 & 3.848 & 3.836 & 3.772 & 4.001 & 4.021 & 4.213\\

    \hspace{1em} &  & $\lambda$ & 0.024 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.194 & 0.366 & 0.224 & 0.446 & 0.383 & 0.346 & 0.001 & 0.561 & 0.167 & 0.842 & 0.731 & 0.792\\

    \hspace{1em} &  & $w_i$ & 0.000 & 0.000 & 0.132 & 0.007 & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.058 & 0.048 & 0.000\\

    \hspace{1em} &  & $m_i$ & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.000 & 0.326 & 0.000 & 0.000 & 0.000 & 0.000 & 0.000\\*
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

## Relative Errors

Set VAR as the benchmark model.

    \begin{table}[H]

    \caption{\label{tab:dgp2result}Out-of-sample forecasting performance measures for DGP2 with VAR(5) model as benchmark.}
    \centering
    \resizebox{\linewidth}{!}{
    \begin{tabular}[t]{cc|cccc|cccc|cccc|}
    \toprule
    \multicolumn{2}{c}{ } & \multicolumn{4}{c}{RMAFE} & \multicolumn{4}{c}{RMSFE} & \multicolumn{4}{c}{RMASE} \\
    \cmidrule(l{3pt}r{3pt}){3-6} \cmidrule(l{3pt}r{3pt}){7-10} \cmidrule(l{3pt}r{3pt}){11-14}
    \rotatebox{0}{} & \rotatebox{0}{} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 10$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 10$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 10$} & \rotatebox{0}{$h = 20$}\\
    \midrule
     & VHAR & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{1.003}} & \textcolor{black}{\num{1.030}} & \textcolor{black}{\num{1.090}} & \textcolor{black}{\num{1.002}} & \textcolor{red}{\num{.972}} & \textcolor{black}{\num{1.026}} & \textcolor{black}{\num{1.199}} & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{1.002}} & \textcolor{black}{\num{1.030}} & \textcolor{black}{\num{1.089}}\\

     & BVAR & \textcolor{red}{\num{.988}} & \textcolor{red}{\num{.990}} & \textcolor{red}{\num{.997}} & \textcolor{red}{\num{.978}} & \textcolor{red}{\num{.987}} & \textcolor{black}{\num{.988}} & \textcolor{red}{\num{.985}} & \textcolor{red}{\num{.962}} & \textcolor{red}{\num{.988}} & \textcolor{red}{\num{.991}} & \textcolor{red}{\num{.996}} & \textcolor{red}{\num{.978}}\\

     & BVHAR-S & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{.993}} & \textcolor{black}{\num{1.003}} & \textcolor{black}{\num{.992}} & \textcolor{black}{\num{.995}} & \textcolor{black}{\num{.984}} & \textcolor{black}{\num{.994}} & \textcolor{black}{\num{.992}} & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{.993}} & \textcolor{black}{\num{1.003}} & \textcolor{black}{\num{.991}}\\

    \multirow{-4}{*}{\centering\arraybackslash SMALL} & BVHAR-L & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{.992}} & \textcolor{black}{\num{1.007}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.996}} & \textcolor{black}{\num{.981}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.010}} & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{.992}} & \textcolor{black}{\num{1.007}} & \textcolor{black}{\num{1.000}}\\
    \cmidrule{1-14}
     & VHAR & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.003}} & \textcolor{black}{\num{1.020}} & \textcolor{black}{\num{1.023}} & \textcolor{black}{\num{.992}} & \textcolor{black}{\num{.995}} & \textcolor{black}{\num{1.039}} & \textcolor{black}{\num{1.051}} & \textcolor{black}{\num{.998}} & \textcolor{black}{\num{1.003}} & \textcolor{black}{\num{1.020}} & \textcolor{black}{\num{1.024}}\\

     & BVAR & \textcolor{red}{\num{.984}} & \textcolor{red}{\num{.981}} & \textcolor{black}{\num{.988}} & \textcolor{red}{\num{.994}} & \textcolor{red}{\num{.967}} & \textcolor{red}{\num{.966}} & \textcolor{black}{\num{.974}} & \textcolor{red}{\num{.987}} & \textcolor{red}{\num{.984}} & \textcolor{red}{\num{.981}} & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{.993}}\\

     & BVHAR-S & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{.981}} & \textcolor{red}{\num{.985}} & \textcolor{black}{\num{.994}} & \textcolor{black}{\num{.971}} & \textcolor{black}{\num{.969}} & \textcolor{red}{\num{.972}} & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{.981}} & \textcolor{red}{\num{.985}} & \textcolor{red}{\num{.993}}\\

    \multirow{-4}{*}{\centering\arraybackslash MEDIUM} & BVHAR-L & \textcolor{black}{\num{.986}} & \textcolor{black}{\num{.984}} & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.971}} & \textcolor{black}{\num{.973}} & \textcolor{black}{\num{.984}} & \textcolor{black}{\num{1.002}} & \textcolor{black}{\num{.986}} & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{.999}}\\
    \cmidrule{1-14}
     & VHAR & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{1.019}} & \textcolor{black}{\num{1.020}} & \textcolor{black}{\num{1.016}} & \textcolor{black}{\num{.980}} & \textcolor{black}{\num{1.022}} & \textcolor{black}{\num{1.033}} & \textcolor{black}{\num{1.030}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{1.019}} & \textcolor{black}{\num{1.018}} & \textcolor{black}{\num{1.015}}\\

     & BVAR & \textcolor{black}{\num{.969}} & \textcolor{red}{\num{.984}} & \textcolor{red}{\num{.989}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{.938}} & \textcolor{red}{\num{.953}} & \textcolor{black}{\num{.983}} & \textcolor{black}{\num{1.002}} & \textcolor{black}{\num{.970}} & \textcolor{red}{\num{.983}} & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{.999}}\\

     & BVHAR-S & \textcolor{black}{\num{.969}} & \textcolor{black}{\num{.984}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{.937}} & \textcolor{black}{\num{.953}} & \textcolor{black}{\num{.983}} & \textcolor{black}{\num{1.002}} & \textcolor{red}{\num{.970}} & \textcolor{black}{\num{.983}} & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{.999}}\\

    \multirow{-4}{*}{\centering\arraybackslash LARGE} & BVHAR-L & \textcolor{red}{\num{.969}} & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{.989}} & \textcolor{red}{\num{.998}} & \textcolor{red}{\num{.937}} & \textcolor{black}{\num{.955}} & \textcolor{red}{\num{.983}} & \textcolor{red}{\num{1.001}} & \textcolor{black}{\num{.970}} & \textcolor{black}{\num{.985}} & \textcolor{red}{\num{.990}} & \textcolor{red}{\num{.999}}\\
    \bottomrule
    \end{tabular}}
    \end{table}

## Piecewise Errors

### SMALL

Plots

<img src="../output/figs/DGP-1-smallcvonefig-1.png" width="70%" style="display: block; margin: auto;" />

<img src="../output/figs/DGP-1-smallcvfivefig-1.png" width="70%" style="display: block; margin: auto;" />

<img src="../output/figs/DGP-1-smallcvtwentyfig-1.png" width="70%" style="display: block; margin: auto;" />

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
     & asset01 & \num{0.886} & \textcolor{red}{\num{0.843}} & \num{0.855} & \num{0.85} & \num{0.85}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.892} & \num{0.933} & \textcolor{red}{\num{0.883}} & \num{0.914} & \num{0.914}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1.05}} & \num{1.056} & \num{1.052} & \num{1.051} & \num{1.052}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.943} & \num{0.944} & \textcolor{red}{\num{0.93}} & \num{0.938} & \num{0.938}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.747} & \textcolor{red}{\num{0.732}} & \num{0.737} & \num{0.734} & \num{0.734}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.728} & \num{0.742} & \textcolor{red}{\num{0.719}} & \num{0.731} & \num{0.731}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.839} & \num{0.834} & \num{0.831} & \num{0.828} & \textcolor{red}{\num{0.826}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.771} & \num{0.769} & \textcolor{red}{\num{0.762}} & \num{0.764} & \num{0.764}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{81.14}} & \num{82.226} & \num{82.226} & \num{82.441} & \num{82.423}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1117.252} & \textcolor{red}{\num{956.486}} & \num{1125.953} & \num{1083.423} & \num{1084.842}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{200.675} & \textcolor{red}{\num{189.76}} & \num{201.743} & \num{198.361} & \num{193.36}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{466.356} & \textcolor{red}{\num{409.491}} & \num{469.974} & \num{454.742} & \num{453.542}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{80.28} & \textcolor{red}{\num{78.614}} & \num{79.175} & \num{78.872} & \num{78.868}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{76.633} & \num{78.12} & \textcolor{red}{\num{75.603}} & \num{76.916} & \num{76.919}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{88.591} & \num{87.952} & \num{87.755} & \num{87.436} & \textcolor{red}{\num{87.215}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{81.835} & \num{81.562} & \textcolor{red}{\num{80.844}} & \num{81.075} & \num{81.001}\\*
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
     & asset01 & \num{3.335} & \textcolor{red}{\num{3.087}} & \num{3.281} & \num{3.245} & \num{3.239}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.089} & \num{1.112} & \textcolor{red}{\num{1.075}} & \num{1.083} & \num{1.083}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{4.151} & \num{4.133} & \num{4.114} & \num{4.109} & \textcolor{red}{\num{4.093}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{2.859} & \textcolor{red}{\num{2.778}} & \num{2.823} & \num{2.812} & \num{2.805}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{1.412} & \textcolor{red}{\num{1.362}} & \num{1.403} & \num{1.402} & \num{1.4}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.776} & \num{0.795} & \textcolor{red}{\num{0.763}} & \num{0.766} & \num{0.767}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.647} & \num{1.688} & \textcolor{red}{\num{1.632}} & \num{1.639} & \num{1.639}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{1.278} & \num{1.282} & \textcolor{red}{\num{1.266}} & \num{1.269} & \num{1.269}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{111.308} & \textcolor{red}{\num{103.382}} & \num{111.281} & \num{113.488} & \num{113.566}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{787.616}} & \num{796.159} & \num{836.388} & \num{830.32} & \num{833.824}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{465.635} & \num{530.263} & \num{446.592} & \num{436.892} & \textcolor{red}{\num{436.863}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{454.853}} & \num{476.601} & \num{464.754} & \num{460.233} & \num{461.418}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{151.097} & \textcolor{red}{\num{145.399}} & \num{150.048} & \num{149.816} & \num{149.599}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{82.782} & \num{84.848} & \textcolor{red}{\num{81.466}} & \num{81.868} & \num{81.891}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{175.168} & \num{179.568} & \textcolor{red}{\num{173.676}} & \num{174.461} & \num{174.46}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{136.349} & \num{136.605} & \textcolor{red}{\num{135.063}} & \num{135.382} & \num{135.317}\\*
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
     & asset01 & \num{3.055} & \textcolor{red}{\num{2.843}} & \num{3.096} & \num{3.072} & \num{3.038}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.063} & \textcolor{red}{\num{1.048}} & \num{1.071} & \num{1.069} & \num{1.07}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{7.008} & \num{7.524} & \textcolor{red}{\num{6.793}} & \num{6.916} & \num{7.021}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{3.709} & \num{3.805} & \textcolor{red}{\num{3.653}} & \num{3.686} & \num{3.71}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{1.443} & \textcolor{red}{\num{1.398}} & \num{1.454} & \num{1.453} & \num{1.446}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.75}} & \num{0.75} & \num{0.753} & \num{0.75} & \num{0.751}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{2.139} & \num{2.312} & \textcolor{red}{\num{2.111}} & \num{2.144} & \num{2.167}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{1.444} & \num{1.487} & \textcolor{red}{\num{1.439}} & \num{1.449} & \num{1.455}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{123.434} & \textcolor{red}{\num{117.261}} & \num{125.809} & \num{126.702} & \num{126.035}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{976.333} & \num{955.617} & \num{966.822} & \textcolor{red}{\num{948.084}} & \num{972.971}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{425.101} & \num{592.396} & \textcolor{red}{\num{399.11}} & \num{403.4} & \num{409.823}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{508.289} & \num{555.092} & \num{497.247} & \textcolor{red}{\num{492.729}} & \num{502.943}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{153.88} & \textcolor{red}{\num{149.106}} & \num{155.026} & \num{154.919} & \num{154.166}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{79.083} & \textcolor{red}{\num{79.06}} & \num{79.399} & \num{79.095} & \num{79.178}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{227.075} & \num{245.472} & \textcolor{red}{\num{223.999}} & \num{227.562} & \num{230.027}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{153.346} & \num{157.879} & \textcolor{red}{\num{152.808}} & \num{153.859} & \num{154.457}\\*
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
     & asset01 & \num{0.975} & \num{0.964} & \textcolor{red}{\num{0.935}} & \num{0.95} & \num{0.95}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.996} & \num{0.979} & \textcolor{red}{\num{0.94}} & \num{0.948} & \num{0.955}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.479} & \num{1.477} & \textcolor{red}{\num{1.42}} & \num{1.421} & \num{1.421}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.64} & \num{1.65} & \textcolor{red}{\num{1.567}} & \num{1.573} & \num{1.573}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.802} & \textcolor{red}{\num{0.783}} & \num{0.792} & \num{0.801} & \num{0.788}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.497} & \num{1.408} & \textcolor{red}{\num{1.403}} & \num{1.407} & \num{1.407}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.718} & \num{0.708} & \textcolor{red}{\num{0.696}} & \num{0.698} & \num{0.699}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.273} & \num{1.287} & \num{1.249} & \textcolor{red}{\num{1.243}} & \num{1.246}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{1.305}} & \num{1.346} & \num{1.323} & \num{1.338} & \num{1.338}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.187} & \num{1.178} & \textcolor{red}{\num{1.147}} & \num{1.153} & \num{1.153}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.772} & \num{0.754} & \textcolor{red}{\num{0.746}} & \num{0.75} & \num{0.753}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.798} & \num{0.813} & \textcolor{red}{\num{0.783}} & \num{0.784} & \num{0.787}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1} & \num{0.995} & \textcolor{red}{\num{0.973}} & \num{0.974} & \num{0.974}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.02} & \num{1.02} & \textcolor{red}{\num{0.987}} & \num{0.992} & \num{0.992}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.714} & \textcolor{red}{\num{0.709}} & \num{0.719} & \num{0.724} & \num{0.72}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.968} & \num{0.959} & \num{0.941} & \textcolor{red}{\num{0.936}} & \num{0.936}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.688} & \textcolor{red}{\num{0.68}} & \num{0.685} & \num{0.685} & \num{0.685}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.924} & \num{0.935} & \num{0.92} & \textcolor{red}{\num{0.915}} & \num{0.917}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.938}} & \num{0.954} & \num{0.945} & \num{0.947} & \num{0.947}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.869} & \num{0.869} & \textcolor{red}{\num{0.855}} & \num{0.856} & \num{0.857}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{179.832} & \num{180.855} & \num{174.684} & \textcolor{red}{\num{164.141}} & \num{175.037}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{534.341} & \num{413.62} & \num{417.766} & \textcolor{red}{\num{391.499}} & \num{398.527}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{324.153} & \num{344.325} & \num{256.039} & \num{253.977} & \textcolor{red}{\num{252.786}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{269.56} & \textcolor{red}{\num{226.163}} & \num{236.399} & \num{231.628} & \num{231.906}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{209.075} & \num{212.792} & \num{208.323} & \num{210.219} & \textcolor{red}{\num{205.208}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{299.617} & \num{277.411} & \num{278.391} & \num{266.846} & \textcolor{red}{\num{266.699}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{224.395} & \num{198.464} & \textcolor{red}{\num{193.43}} & \num{194.348} & \num{194.27}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{197.98}} & \num{213.011} & \num{204.483} & \num{202.41} & \num{208.208}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{167.034} & \num{190.44} & \num{165.718} & \textcolor{red}{\num{164.244}} & \num{164.667}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{267.332} & \num{250.787} & \num{237.248} & \textcolor{red}{\num{231.035}} & \num{233.034}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{81.227} & \num{79.49} & \textcolor{red}{\num{78.556}} & \num{79.047} & \num{79.383}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{82.308} & \num{83.47} & \textcolor{red}{\num{80.226}} & \num{80.248} & \num{80.587}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{101.99} & \num{101.723} & \textcolor{red}{\num{99.567}} & \num{99.789} & \num{99.772}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{101.95} & \num{101.893} & \textcolor{red}{\num{98.863}} & \num{99.455} & \num{99.45}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{73.273} & \textcolor{red}{\num{72.912}} & \num{74.074} & \num{74.742} & \num{74.419}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{101.006} & \num{99.433} & \num{97.672} & \textcolor{red}{\num{97.093}} & \num{97.099}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{70.652} & \textcolor{red}{\num{69.564}} & \num{70.433} & \num{70.252} & \num{70.309}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{94.045} & \num{95.206} & \num{93.59} & \textcolor{red}{\num{93.127}} & \num{93.282}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{96.494}} & \num{97.797} & \num{97.079} & \num{97.268} & \num{97.303}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{89.216} & \num{89.054} & \textcolor{red}{\num{87.784}} & \num{87.891} & \num{87.956}\\*
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
     & asset01 & \num{1.223} & \num{1.289} & \num{1.187} & \textcolor{red}{\num{1.183}} & \num{1.195}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.714} & \num{1.861} & \num{1.659} & \textcolor{red}{\num{1.657}} & \num{1.665}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1.439}} & \num{1.468} & \num{1.459} & \num{1.466} & \num{1.464}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{2.288} & \num{2.405} & \textcolor{red}{\num{2.259}} & \num{2.268} & \num{2.266}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{2.28} & \num{2.52} & \textcolor{red}{\num{2.214}} & \num{2.284} & \num{2.339}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{5.143} & \textcolor{red}{\num{4.41}} & \num{4.778} & \num{4.77} & \num{4.773}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.789} & \num{0.765} & \num{0.759} & \num{0.758} & \textcolor{red}{\num{0.758}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.377} & \num{1.425} & \num{1.348} & \num{1.34} & \textcolor{red}{\num{1.339}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{2.017} & \num{2.041} & \num{1.982} & \textcolor{red}{\num{1.975}} & \num{1.975}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{2.03} & \num{2.02} & \textcolor{red}{\num{1.961}} & \num{1.967} & \num{1.975}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.893} & \num{0.917} & \num{0.886} & \textcolor{red}{\num{0.883}} & \num{0.89}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.056} & \num{1.099} & \num{1.032} & \textcolor{red}{\num{1.032}} & \num{1.04}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.978}} & \num{0.989} & \num{0.98} & \num{0.981} & \num{0.981}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.199} & \num{1.221} & \textcolor{red}{\num{1.184}} & \num{1.188} & \num{1.187}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{1.238} & \num{1.318} & \textcolor{red}{\num{1.199}} & \num{1.215} & \num{1.234}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.701} & \num{1.598} & \num{1.612} & \textcolor{red}{\num{1.596}} & \num{1.6}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.713} & \textcolor{red}{\num{0.686}} & \num{0.697} & \num{0.698} & \num{0.697}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.937} & \num{0.953} & \num{0.933} & \num{0.931} & \textcolor{red}{\num{0.929}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.153} & \textcolor{red}{\num{1.122}} & \num{1.155} & \num{1.155} & \num{1.154}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{1.097} & \num{1.1} & \textcolor{red}{\num{1.075}} & \num{1.075} & \num{1.079}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{122.175}} & \num{125.535} & \num{140.927} & \num{134.782} & \num{148.316}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{244.477} & \num{234.371} & \num{163.592} & \textcolor{red}{\num{158.617}} & \num{174.154}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{262.676} & \num{300.836} & \num{212.237} & \textcolor{red}{\num{196.309}} & \num{198.047}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{117.582}} & \num{137.085} & \num{119.616} & \num{118.944} & \num{117.613}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{275.137} & \num{355.619} & \textcolor{red}{\num{268.337}} & \num{274.108} & \num{288.664}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{249.404} & \num{236.801} & \num{189.519} & \textcolor{red}{\num{172.567}} & \num{174.957}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{127.696} & \num{143.15} & \textcolor{red}{\num{113.862}} & \num{117.274} & \num{117.621}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{142.638} & \textcolor{red}{\num{134.859}} & \num{135.866} & \num{136.528} & \num{142.386}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{102.873}} & \num{113.31} & \num{104.303} & \num{105.311} & \num{106.033}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{182.74} & \num{197.952} & \num{160.918} & \textcolor{red}{\num{157.16}} & \num{163.088}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{92.507} & \num{94.747} & \num{91.553} & \textcolor{red}{\num{91.293}} & \num{91.978}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{110.584} & \num{114.455} & \num{108.198} & \textcolor{red}{\num{108.156}} & \num{108.987}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{99.629}} & \num{100.766} & \num{99.674} & \num{99.736} & \num{99.671}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{125.749} & \num{128.454} & \textcolor{red}{\num{124.666}} & \num{125.111} & \num{125.035}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{127.349} & \num{135.148} & \textcolor{red}{\num{123.837}} & \num{125.756} & \num{127.511}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{173.682} & \num{163.551} & \num{164.35} & \textcolor{red}{\num{162.825}} & \num{163.37}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{74.118} & \textcolor{red}{\num{71.371}} & \num{72.321} & \num{72.336} & \num{72.302}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{94.57} & \num{96.26} & \num{94.199} & \num{93.953} & \textcolor{red}{\num{93.789}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{118.051} & \textcolor{red}{\num{114.187}} & \num{118.096} & \num{118.094} & \num{117.915}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{112.916} & \num{113.215} & \textcolor{red}{\num{110.766}} & \num{110.807} & \num{111.173}\\*
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
     & asset01 & \num{1.155} & \num{1.176} & \textcolor{red}{\num{1.142}} & \num{1.142} & \num{1.145}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.709} & \num{1.897} & \num{1.678} & \num{1.68} & \textcolor{red}{\num{1.675}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.419} & \num{1.424} & \num{1.409} & \num{1.408} & \textcolor{red}{\num{1.408}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{2.468}} & \num{2.723} & \num{2.497} & \num{2.514} & \num{2.511}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{3.895} & \num{4.771} & \textcolor{red}{\num{3.686}} & \num{3.708} & \num{3.95}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{6.482} & \textcolor{red}{\num{5.886}} & \num{6.191} & \num{6.119} & \num{6.123}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.726} & \num{0.72} & \num{0.718} & \textcolor{red}{\num{0.717}} & \num{0.719}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{1.347}} & \num{1.426} & \num{1.366} & \num{1.361} & \num{1.36}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{2.138} & \num{2.146} & \num{2.103} & \textcolor{red}{\num{2.099}} & \num{2.1}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{2.371} & \num{2.463} & \num{2.31} & \textcolor{red}{\num{2.306}} & \num{2.332}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.858} & \num{0.861} & \textcolor{red}{\num{0.85}} & \num{0.851} & \num{0.852}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.045} & \num{1.107} & \num{1.038} & \num{1.038} & \textcolor{red}{\num{1.036}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.958} & \textcolor{red}{\num{0.957}} & \num{0.958} & \num{0.958} & \num{0.957}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{1.234}} & \num{1.293} & \num{1.244} & \num{1.249} & \num{1.248}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{1.659} & \num{1.828} & \num{1.594} & \textcolor{red}{\num{1.582}} & \num{1.638}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.884} & \textcolor{red}{\num{1.804}} & \num{1.838} & \num{1.821} & \num{1.822}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.675} & \textcolor{red}{\num{0.659}} & \num{0.67} & \num{0.67} & \num{0.671}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{0.93}} & \num{0.96} & \num{0.938} & \num{0.935} & \num{0.935}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.203} & \num{1.189} & \num{1.192} & \num{1.191} & \textcolor{red}{\num{1.189}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{1.161} & \num{1.184} & \num{1.147} & \textcolor{red}{\num{1.144}} & \num{1.15}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{131.589} & \num{130.802} & \num{121.354} & \textcolor{red}{\num{115.695}} & \num{117.177}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{103.889}} & \num{139.562} & \num{105.338} & \num{104.155} & \num{106.154}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{237.931} & \num{290.089} & \num{219.753} & \textcolor{red}{\num{211.578}} & \num{217.455}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{113.602}} & \num{122.785} & \num{118.697} & \num{119.444} & \num{120.201}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{579.03} & \num{647.337} & \num{493.174} & \textcolor{red}{\num{480.46}} & \num{520.631}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{208.043} & \textcolor{red}{\num{176.491}} & \num{187.969} & \num{180.593} & \num{186.177}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{113.742} & \textcolor{red}{\num{105.875}} & \num{108.044} & \num{107.187} & \num{108.315}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{146.707} & \num{153.662} & \num{133.865} & \textcolor{red}{\num{131.535}} & \num{133.055}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{100.876}} & \num{121.427} & \num{100.934} & \num{101.838} & \num{102.593}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{192.823} & \num{209.781} & \num{176.57} & \textcolor{red}{\num{172.498}} & \num{179.084}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{90.01} & \num{90.202} & \textcolor{red}{\num{89.158}} & \num{89.217} & \num{89.318}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{107.396} & \num{113.461} & \num{106.697} & \num{106.716} & \textcolor{red}{\num{106.486}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{97.139} & \textcolor{red}{\num{96.825}} & \num{97.08} & \num{97.069} & \num{96.971}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{124.382}} & \num{129.906} & \num{125.485} & \num{126.046} & \num{125.936}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{169.777} & \num{188.069} & \num{163.12} & \textcolor{red}{\num{161.791}} & \num{167.461}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{194.362} & \textcolor{red}{\num{186.145}} & \num{189.293} & \num{187.501} & \num{187.607}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{69.787} & \textcolor{red}{\num{68.15}} & \num{69.278} & \num{69.256} & \num{69.338}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{94.246}} & \num{97.148} & \num{94.991} & \num{94.733} & \num{94.708}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{123.151} & \textcolor{red}{\num{121.349}} & \num{121.998} & \num{121.821} & \num{121.583}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{118.917} & \num{121.251} & \num{117.455} & \textcolor{red}{\num{117.128}} & \num{117.712}\\*
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
     & asset01 & \num{1.168} & \num{1.22} & \textcolor{red}{\num{1.114}} & \num{1.115} & \num{1.114}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.66} & \num{1.556} & \num{1.468} & \num{1.468} & \textcolor{red}{\num{1.468}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.571} & \textcolor{red}{\num{1.571}} & \num{1.591} & \num{1.592} & \num{1.58}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.992} & \textcolor{red}{\num{0.991}} & \num{0.991} & \num{0.992} & \num{0.992}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{1.057} & \num{0.981} & \textcolor{red}{\num{0.943}} & \num{0.943} & \num{0.943}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.352} & \num{1.328} & \num{1.311} & \num{1.31} & \textcolor{red}{\num{1.31}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1.262} & \num{1.265} & \num{1.152} & \textcolor{red}{\num{1.151}} & \num{1.152}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.424} & \num{1.439} & \num{1.326} & \textcolor{red}{\num{1.325}} & \num{1.326}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.058} & \num{1.044} & \num{1.037} & \num{1.035} & \textcolor{red}{\num{1.035}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{1.079} & \num{1.066} & \num{0.968} & \textcolor{red}{\num{0.968}} & \num{0.968}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{1.66} & \num{1.531} & \textcolor{red}{\num{1.462}} & \num{1.463} & \num{1.466}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.8} & \num{0.795} & \num{0.778} & \textcolor{red}{\num{0.777}} & \num{0.777}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.257} & \num{1.232} & \num{1.178} & \num{1.178} & \textcolor{red}{\num{1.178}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.877} & \num{0.884} & \num{0.857} & \num{0.857} & \textcolor{red}{\num{0.857}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.057} & \num{1.001} & \num{0.988} & \num{0.987} & \textcolor{red}{\num{0.987}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.96} & \num{0.951} & \num{0.948} & \num{0.948} & \textcolor{red}{\num{0.942}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.794}} & \num{0.809} & \num{0.808} & \num{0.808} & \num{0.807}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.812} & \num{0.792} & \textcolor{red}{\num{0.776}} & \num{0.776} & \num{0.777}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.928} & \textcolor{red}{\num{0.902}} & \num{0.904} & \num{0.903} & \num{0.903}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.896} & \num{0.917} & \num{0.875} & \textcolor{red}{\num{0.875}} & \num{0.875}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.957} & \num{0.957} & \num{0.918} & \textcolor{red}{\num{0.917}} & \num{0.917}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.809} & \textcolor{red}{\num{0.798}} & \num{0.799} & \num{0.798} & \num{0.798}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.819} & \num{0.821} & \textcolor{red}{\num{0.773}} & \num{0.773} & \num{0.775}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{1.012} & \num{0.975} & \textcolor{red}{\num{0.974}} & \num{0.975} & \num{0.978}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.737} & \num{0.722} & \num{0.71} & \textcolor{red}{\num{0.71}} & \num{0.71}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.888} & \num{0.878} & \num{0.861} & \num{0.861} & \textcolor{red}{\num{0.861}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{138.474} & \num{126.3} & \textcolor{red}{\num{123.178}} & \num{123.749} & \num{123.823}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{181.739} & \num{154.669} & \num{130.876} & \num{130.198} & \textcolor{red}{\num{129.995}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{297.482} & \num{278.464} & \num{191.243} & \textcolor{red}{\num{190.496}} & \num{191.427}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{210.329}} & \num{268.943} & \num{256.768} & \num{257.113} & \num{257.049}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{215.569}} & \num{271.246} & \num{278.129} & \num{282.547} & \num{282.571}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{213.693} & \num{194.877} & \num{179.367} & \num{178.073} & \textcolor{red}{\num{177.979}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{393.538} & \num{337.364} & \num{105.548} & \textcolor{red}{\num{104.857}} & \num{117.417}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{161.204} & \num{182.898} & \textcolor{red}{\num{154.241}} & \num{154.446} & \num{154.535}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{214.994} & \num{245.87} & \num{163.1} & \num{164.972} & \textcolor{red}{\num{162.554}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{159.454} & \num{160.02} & \num{150.416} & \textcolor{red}{\num{150.377}} & \num{150.895}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{152.337}} & \num{157.123} & \num{153.833} & \num{154.178} & \num{155.148}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{174.577}} & \num{188.23} & \num{184.102} & \num{184.206} & \num{183.731}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{209.449} & \num{213.834} & \textcolor{red}{\num{172.567}} & \num{172.934} & \num{173.927}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{86.824} & \num{87.614} & \num{85.121} & \num{85.111} & \textcolor{red}{\num{85.094}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{104.667} & \num{99.251} & \num{98.272} & \num{98.253} & \textcolor{red}{\num{98.246}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{96.211} & \num{95.497} & \num{94.62} & \num{94.623} & \textcolor{red}{\num{94.128}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{79.706}} & \num{81.355} & \num{81.35} & \num{81.365} & \num{81.324}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{81.87} & \num{80.26} & \textcolor{red}{\num{78.38}} & \num{78.4} & \num{78.426}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{92.389} & \textcolor{red}{\num{89.873}} & \num{90.066} & \num{90.02} & \num{90.008}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{89.105} & \num{90.597} & \num{86.775} & \textcolor{red}{\num{86.747}} & \num{86.868}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{95.741} & \num{95.568} & \num{91.662} & \textcolor{red}{\num{91.571}} & \num{91.604}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{82.024} & \num{81.517} & \num{81.408} & \textcolor{red}{\num{81.337}} & \num{81.351}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{82.03} & \num{81.944} & \textcolor{red}{\num{77.187}} & \num{77.206} & \num{77.371}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{101.75} & \textcolor{red}{\num{98.536}} & \num{98.556} & \num{98.616} & \num{98.833}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{72.949} & \num{71.285} & \textcolor{red}{\num{70.285}} & \num{70.291} & \num{70.3}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{88.772} & \num{87.775} & \num{86.14} & \textcolor{red}{\num{86.128}} & \num{86.129}\\*
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
     & asset01 & \num{1.1} & \num{1.234} & \textcolor{red}{\num{1.036}} & \num{1.037} & \num{1.037}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.635} & \num{1.683} & \num{1.6} & \num{1.6} & \textcolor{red}{\num{1.6}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1.758}} & \num{1.87} & \num{1.78} & \num{1.781} & \num{1.774}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.445} & \textcolor{red}{\num{1.343}} & \num{1.398} & \num{1.397} & \num{1.396}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{1.14} & \num{1.112} & \num{1.103} & \textcolor{red}{\num{1.103}} & \num{1.103}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1.403} & \num{1.474} & \num{1.388} & \textcolor{red}{\num{1.388}} & \num{1.388}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1.191} & \num{1.202} & \textcolor{red}{\num{1.144}} & \num{1.145} & \num{1.145}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{2.264} & \num{2.249} & \textcolor{red}{\num{2.147}} & \num{2.147} & \num{2.148}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.136} & \textcolor{red}{\num{1.123}} & \num{1.168} & \num{1.169} & \num{1.17}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{3.378} & \num{3.214} & \num{2.632} & \textcolor{red}{\num{2.625}} & \num{2.658}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{3.541}} & \num{3.972} & \num{3.542} & \num{3.556} & \num{3.564}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{2.095} & \textcolor{red}{\num{2.092}} & \num{2.111} & \num{2.11} & \num{2.113}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.841} & \num{1.881} & \textcolor{red}{\num{1.754}} & \num{1.755} & \num{1.758}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.877} & \num{0.923} & \textcolor{red}{\num{0.849}} & \num{0.85} & \num{0.85}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.054} & \num{1.061} & \num{1.053} & \num{1.053} & \textcolor{red}{\num{1.052}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1.006}} & \num{1.046} & \num{1.008} & \num{1.008} & \num{1.009}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.953} & \textcolor{red}{\num{0.923}} & \num{0.941} & \num{0.94} & \num{0.939}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.8} & \num{0.802} & \textcolor{red}{\num{0.792}} & \num{0.793} & \num{0.793}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.91} & \num{0.94} & \textcolor{red}{\num{0.907}} & \num{0.908} & \num{0.908}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.873} & \num{0.89} & \textcolor{red}{\num{0.869}} & \num{0.869} & \num{0.87}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.215} & \num{1.225} & \textcolor{red}{\num{1.2}} & \num{1.201} & \num{1.202}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.842} & \textcolor{red}{\num{0.828}} & \num{0.856} & \num{0.857} & \num{0.858}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{1.459} & \num{1.451} & \num{1.31} & \textcolor{red}{\num{1.308}} & \num{1.316}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{1.505}} & \num{1.603} & \num{1.513} & \num{1.516} & \num{1.522}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{1.157} & \num{1.192} & \textcolor{red}{\num{1.146}} & \num{1.146} & \num{1.146}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{1.054} & \num{1.074} & \textcolor{red}{\num{1.037}} & \num{1.037} & \num{1.039}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{120.39} & \num{131.294} & \num{99.649} & \textcolor{red}{\num{99.643}} & \num{99.741}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{104.195} & \num{105.571} & \num{104.012} & \num{103.334} & \textcolor{red}{\num{102.585}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{152.275} & \num{161.733} & \num{102.8} & \textcolor{red}{\num{102.237}} & \num{116.61}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{136.736} & \num{192.811} & \num{109.337} & \textcolor{red}{\num{108.598}} & \num{108.835}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{152.356} & \num{200.719} & \num{106.013} & \num{105.089} & \textcolor{red}{\num{104.977}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{123.393} & \num{122.355} & \textcolor{red}{\num{99.037}} & \num{99.366} & \num{99.836}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{335.673} & \num{290.669} & \num{105.115} & \textcolor{red}{\num{102.172}} & \num{109.137}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{112.865} & \num{122.013} & \textcolor{red}{\num{105.831}} & \num{106.097} & \num{106.41}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{128.958} & \num{152.572} & \num{101.066} & \num{100.757} & \textcolor{red}{\num{99.845}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{231.811} & \num{203.464} & \num{184.328} & \textcolor{red}{\num{184.127}} & \num{190.727}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{141.673} & \num{163.372} & \num{116.083} & \textcolor{red}{\num{115.745}} & \num{120.494}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{156.283} & \num{210.947} & \num{126.283} & \num{125.618} & \textcolor{red}{\num{124.578}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{158.051} & \num{171.46} & \num{113.296} & \textcolor{red}{\num{112.732}} & \num{115.314}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{88.88} & \num{93.44} & \textcolor{red}{\num{86.054}} & \num{86.062} & \num{86.064}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{106.538} & \num{107.327} & \num{106.212} & \num{106.217} & \textcolor{red}{\num{106.192}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{101.007} & \num{104.757} & \num{100.842} & \textcolor{red}{\num{100.836}} & \num{100.961}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{94.708} & \textcolor{red}{\num{92.101}} & \num{93.306} & \num{93.261} & \num{93.168}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{79.662} & \num{79.689} & \textcolor{red}{\num{78.626}} & \num{78.664} & \num{78.697}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{90.107} & \num{93.001} & \textcolor{red}{\num{89.868}} & \num{89.898} & \num{89.94}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{86.932} & \num{89.087} & \textcolor{red}{\num{86.326}} & \num{86.369} & \num{86.467}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{121.076} & \num{122.328} & \textcolor{red}{\num{119.703}} & \num{119.753} & \num{119.851}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{85.67} & \textcolor{red}{\num{84.732}} & \num{87.101} & \num{87.18} & \num{87.296}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{145.193} & \num{143.665} & \num{130.449} & \textcolor{red}{\num{130.321}} & \num{131.072}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{148.405}} & \num{158.616} & \num{149.342} & \num{149.648} & \num{150.265}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{115.017} & \num{119.079} & \textcolor{red}{\num{113.938}} & \num{113.961} & \num{114.038}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{105.266} & \num{107.318} & \textcolor{red}{\num{103.481}} & \num{103.514} & \num{103.668}\\*
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
     & asset01 & \num{1.086} & \num{1.215} & \textcolor{red}{\num{1.072}} & \num{1.073} & \num{1.072}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.609} & \num{1.647} & \textcolor{red}{\num{1.596}} & \num{1.597} & \num{1.597}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1.745}} & \num{1.842} & \num{1.766} & \num{1.766} & \num{1.766}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{1.37} & \textcolor{red}{\num{1.32}} & \num{1.406} & \num{1.405} & \num{1.405}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.901} & \num{0.894} & \num{0.887} & \num{0.887} & \textcolor{red}{\num{0.886}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{1.448}} & \num{1.584} & \num{1.448} & \num{1.448} & \num{1.448}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{1.061} & \num{1.075} & \num{1.034} & \textcolor{red}{\num{1.034}} & \num{1.035}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{2.224} & \num{2.218} & \textcolor{red}{\num{2.19}} & \num{2.19} & \num{2.191}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{1.226} & \num{1.249} & \num{1.206} & \num{1.205} & \textcolor{red}{\num{1.205}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{3.264} & \num{3.248} & \num{2.915} & \textcolor{red}{\num{2.913}} & \num{2.92}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{3.503}} & \num{3.968} & \num{3.557} & \num{3.564} & \num{3.552}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{2.538} & \textcolor{red}{\num{2.434}} & \num{2.522} & \num{2.521} & \num{2.52}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.831} & \num{1.891} & \num{1.8} & \num{1.8} & \textcolor{red}{\num{1.8}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.875} & \num{0.92} & \textcolor{red}{\num{0.87}} & \num{0.87} & \num{0.87}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.062} & \num{1.066} & \textcolor{red}{\num{1.05}} & \num{1.05} & \num{1.05}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{1}} & \num{1.023} & \num{1.003} & \num{1.003} & \num{1.002}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.932} & \textcolor{red}{\num{0.92}} & \num{0.944} & \num{0.944} & \num{0.944}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.733} & \num{0.736} & \num{0.729} & \num{0.729} & \textcolor{red}{\num{0.729}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.934}} & \num{0.972} & \num{0.934} & \num{0.934} & \num{0.934}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.841} & \num{0.857} & \num{0.835} & \num{0.835} & \textcolor{red}{\num{0.835}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1.226} & \num{1.233} & \textcolor{red}{\num{1.215}} & \num{1.215} & \num{1.215}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.876} & \num{0.873} & \num{0.87} & \num{0.869} & \textcolor{red}{\num{0.869}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{1.402} & \num{1.43} & \num{1.31} & \textcolor{red}{\num{1.309}} & \num{1.31}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{1.491}} & \num{1.598} & \num{1.498} & \num{1.501} & \num{1.5}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{1.259} & \num{1.256} & \num{1.239} & \num{1.238} & \textcolor{red}{\num{1.238}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{1.053} & \num{1.074} & \textcolor{red}{\num{1.041}} & \num{1.041} & \num{1.041}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{101.318} & \num{130.89} & \num{100.138} & \num{100.076} & \textcolor{red}{\num{100.039}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{115.273} & \num{121.84} & \num{102.14} & \num{101.609} & \textcolor{red}{\num{101.188}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{118.05} & \num{131.951} & \num{99.36} & \num{99.457} & \textcolor{red}{\num{98.77}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{104.326} & \num{201.039} & \num{98.981} & \num{98.92} & \textcolor{red}{\num{98.916}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{112.367} & \num{176.687} & \textcolor{red}{\num{98.848}} & \num{99.587} & \num{99.636}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{100.888} & \num{116.527} & \num{100.041} & \num{100.042} & \textcolor{red}{\num{100.022}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{102.178} & \num{304.051} & \textcolor{red}{\num{99.86}} & \num{100.743} & \num{114.972}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{110.754} & \num{122.479} & \num{99.237} & \textcolor{red}{\num{99.232}} & \num{99.278}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{115.031} & \num{121.153} & \num{98.476} & \textcolor{red}{\num{98.318}} & \num{98.768}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{156.82} & \num{157.851} & \textcolor{red}{\num{130.405}} & \num{130.696} & \num{135.779}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{118.386} & \num{146.17} & \textcolor{red}{\num{103.313}} & \num{103.697} & \num{106.024}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{107.191} & \num{142.403} & \num{106.437} & \num{106.362} & \textcolor{red}{\num{106.32}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{113.548} & \num{156.087} & \textcolor{red}{\num{103.103}} & \num{103.228} & \num{104.976}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{85.361} & \num{89.426} & \textcolor{red}{\num{84.821}} & \num{84.823} & \num{84.821}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{106.365} & \num{106.882} & \textcolor{red}{\num{105.137}} & \num{105.145} & \num{105.147}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{101.241}} & \num{103.475} & \num{101.448} & \num{101.448} & \num{101.434}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{94.306} & \textcolor{red}{\num{92.454}} & \num{95.71} & \num{95.683} & \num{95.668}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{72.601} & \num{72.84} & \num{72.149} & \num{72.151} & \textcolor{red}{\num{72.141}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{91.807} & \num{95.477} & \num{91.745} & \num{91.742} & \textcolor{red}{\num{91.741}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{84.074} & \num{86.444} & \num{83.551} & \num{83.547} & \textcolor{red}{\num{83.521}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{122.113} & \num{122.88} & \textcolor{red}{\num{120.933}} & \num{120.957} & \num{120.993}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{84.732} & \num{84.372} & \num{84.126} & \num{84.114} & \textcolor{red}{\num{84.114}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{141.362} & \num{143.218} & \num{132.414} & \textcolor{red}{\num{132.348}} & \num{132.406}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{152.765}} & \num{162.838} & \num{153.851} & \num{154.078} & \num{153.967}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{125.43} & \num{125.137} & \num{123.207} & \num{123.125} & \textcolor{red}{\num{123.061}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{105.18} & \num{107.12} & \num{104.091} & \num{104.097} & \textcolor{red}{\num{104.085}}\\*
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
    MSE & \num{0.943} & \num{0.944} & \textcolor{red}{\num{0.93}} & \num{0.938} & \num{0.938}\\
    MAE & \num{0.771} & \num{0.769} & \textcolor{red}{\num{0.762}} & \num{0.764} & \num{0.764}\\
    MAPE & \num{466.356} & \textcolor{red}{\num{409.491}} & \num{469.974} & \num{454.742} & \num{453.542}\\
    MASE & \num{81.835} & \num{81.562} & \textcolor{red}{\num{80.844}} & \num{81.075} & \num{81.001}\\
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
    MSE & \num{2.859} & \textcolor{red}{\num{2.778}} & \num{2.823} & \num{2.812} & \num{2.805}\\
    MAE & \num{1.278} & \num{1.282} & \textcolor{red}{\num{1.266}} & \num{1.269} & \num{1.269}\\
    MAPE & \textcolor{red}{\num{454.853}} & \num{476.601} & \num{464.754} & \num{460.233} & \num{461.418}\\
    MASE & \num{136.349} & \num{136.605} & \textcolor{red}{\num{135.063}} & \num{135.382} & \num{135.317}\\
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
    MSE & \num{3.709} & \num{3.805} & \textcolor{red}{\num{3.653}} & \num{3.686} & \num{3.71}\\
    MAE & \num{1.444} & \num{1.487} & \textcolor{red}{\num{1.439}} & \num{1.449} & \num{1.455}\\
    MAPE & \num{508.289} & \num{555.092} & \num{497.247} & \textcolor{red}{\num{492.729}} & \num{502.943}\\
    MASE & \num{153.346} & \num{157.879} & \textcolor{red}{\num{152.808}} & \num{153.859} & \num{154.457}\\
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
    MSE & \num{1.187} & \num{1.178} & \textcolor{red}{\num{1.147}} & \num{1.153} & \num{1.153}\\
    MAE & \num{0.869} & \num{0.869} & \textcolor{red}{\num{0.855}} & \num{0.856} & \num{0.857}\\
    MAPE & \num{267.332} & \num{250.787} & \num{237.248} & \textcolor{red}{\num{231.035}} & \num{233.034}\\
    MASE & \num{89.216} & \num{89.054} & \textcolor{red}{\num{87.784}} & \num{87.891} & \num{87.956}\\
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
    MSE & \num{2.03} & \num{2.02} & \textcolor{red}{\num{1.961}} & \num{1.967} & \num{1.975}\\
    MAE & \num{1.097} & \num{1.1} & \textcolor{red}{\num{1.075}} & \num{1.075} & \num{1.079}\\
    MAPE & \num{182.74} & \num{197.952} & \num{160.918} & \textcolor{red}{\num{157.16}} & \num{163.088}\\
    MASE & \num{112.916} & \num{113.215} & \textcolor{red}{\num{110.766}} & \num{110.807} & \num{111.173}\\
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
    MSE & \num{2.371} & \num{2.463} & \num{2.31} & \textcolor{red}{\num{2.306}} & \num{2.332}\\
    MAE & \num{1.161} & \num{1.184} & \num{1.147} & \textcolor{red}{\num{1.144}} & \num{1.15}\\
    MAPE & \num{192.823} & \num{209.781} & \num{176.57} & \textcolor{red}{\num{172.498}} & \num{179.084}\\
    MASE & \num{118.917} & \num{121.251} & \num{117.455} & \textcolor{red}{\num{117.128}} & \num{117.712}\\
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
    MSE & \num{1.257} & \num{1.232} & \num{1.178} & \num{1.178} & \textcolor{red}{\num{1.178}}\\
    MAE & \num{0.888} & \num{0.878} & \num{0.861} & \num{0.861} & \textcolor{red}{\num{0.861}}\\
    MAPE & \num{209.449} & \num{213.834} & \textcolor{red}{\num{172.567}} & \num{172.934} & \num{173.927}\\
    MASE & \num{88.772} & \num{87.775} & \num{86.14} & \textcolor{red}{\num{86.128}} & \num{86.129}\\
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
    MSE & \num{1.841} & \num{1.881} & \textcolor{red}{\num{1.754}} & \num{1.755} & \num{1.758}\\
    MAE & \num{1.054} & \num{1.074} & \textcolor{red}{\num{1.037}} & \num{1.037} & \num{1.039}\\
    MAPE & \num{158.051} & \num{171.46} & \num{113.296} & \textcolor{red}{\num{112.732}} & \num{115.314}\\
    MASE & \num{105.266} & \num{107.318} & \textcolor{red}{\num{103.481}} & \num{103.514} & \num{103.668}\\
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
    MSE & \num{1.831} & \num{1.891} & \num{1.8} & \num{1.8} & \textcolor{red}{\num{1.8}}\\
    MAE & \num{1.053} & \num{1.074} & \textcolor{red}{\num{1.041}} & \num{1.041} & \num{1.041}\\
    MAPE & \num{113.548} & \num{156.087} & \textcolor{red}{\num{103.103}} & \num{103.228} & \num{104.976}\\
    MASE & \num{105.18} & \num{107.12} & \num{104.091} & \num{104.097} & \textcolor{red}{\num{104.085}}\\
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
            "../output/dgp02-figs/small-coef/", 
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
            "../output/dgp02-figs/med-coef/", 
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
            "../output/dgp02-figs/large-coef/", 
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
