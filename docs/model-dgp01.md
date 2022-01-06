Simulating Minnesota VAR
================
Young Geun Kim
06 Jan, 2022

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
        -   [SMALL](#small-1)
        -   [MEDIUM](#medium-1)
        -   [LARGE](#large-1)
        -   [Lists](#lists)
    -   [Relative Errors](#relative-errors)
    -   [Piceswise Errors](#piceswise-errors)
        -   [SMALL](#small-2)
        -   [Tables](#tables)
        -   [MEDIUM](#medium-2)
        -   [LARGE](#large-2)
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
    rep(1e-2, n_small), # sigma
    1e-2, # lambda
    rep(1e-2, n_small) # delta
  ), 
  upper = c(
    rep(1, n_small), # sigma
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
#> [1]  0.0598  0.0496  0.0757
#> 
#> Setting for 'lambda':
#> [1]  0.203
#> 
#> Setting for 'delta':
#> [1]  0.0655  0.0540  0.0100
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
(bvar_medium_optim <- choose_bvar(
  bvar_medium_spec, 
  lower = c(
    rep(1e-2, n_medium), # sigma
    1e-2, # lambda
    rep(1e-2, n_medium) # delta
  ), 
  upper = c(
    rep(1, n_medium), # sigma
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
#> [1]  0.0406  0.0397  0.0521  0.0595  0.0633  0.0570  0.0657  0.0867  0.0848
#> 
#> Setting for 'lambda':
#> [1]  0.132
#> 
#> Setting for 'delta':
#> [1]  0.0647  0.0100  0.0106  0.0100  0.0100  0.0100  0.0436  0.0100  0.0293
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
(bvar_large_optim <- choose_bvar(
  bvar_large_spec, 
  lower = c(
    rep(1e-2, n_large), # sigma
    1e-2, # lambda
    rep(1e-2, n_large) # delta
  ), 
  upper = c(
    rep(1, n_large), # sigma
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
#>  [1]  0.0386  0.0416  0.0440  0.0577  0.0668  0.0439  0.0640  0.0582  0.0856
#> [10]  0.0785  0.0724  0.0906
#> 
#> Setting for 'lambda':
#> [1]  0.0484
#> 
#> Setting for 'delta':
#>  [1]  0.0100  0.0100  0.0100  0.0100  0.0100  0.0170  0.0100  0.0389  0.0100
#> [10]  0.0173  0.0100  0.0140
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
    rep(1e-2, n_small), # sigma
    1e-2, # lambda
    rep(1e-2, n_small) # delta
  ), 
  upper = c(
    rep(1, n_small), # sigma
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
#> [1]  0.0594  0.0492  0.0967
#> 
#> Setting for 'lambda':
#> [1]  0.305
#> 
#> Setting for 'delta':
#> [1]  0.0562  0.0100  0.0100
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
(bvhar_var_medium_optim <- choose_bvhar(
  bvhar_var_medium_spec, 
  lower = c(
    rep(1e-2, n_medium), # sigma
    1e-2, # lambda
    rep(1e-2, n_medium) # delta
  ), 
  upper = c(
    rep(1, n_medium), # sigma
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
#> [1]  0.0416  0.0359  0.0521  0.0577  0.0664  0.0559  0.0645  0.1248  0.0845
#> 
#> Setting for 'lambda':
#> [1]  0.146
#> 
#> Setting for 'delta':
#> [1]  0.0523  0.0100  0.0376  0.0100  0.0100  0.0100  0.0393  0.0100  0.0340
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
(bvhar_var_large_optim <- choose_bvhar(
  bvhar_var_large_spec, 
  lower = c(
    rep(1e-2, n_large), # sigma
    1e-2, # lambda
    rep(1e-2, n_large) # delta
  ), 
  upper = c(
    rep(1, n_large), # sigma
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
#>  [1]  0.0375  0.0411  0.0441  0.0563  0.0672  0.0429  0.0636  0.0618  0.0841
#> [10]  0.0785  0.0745  0.0950
#> 
#> Setting for 'lambda':
#> [1]  0.0345
#> 
#> Setting for 'delta':
#>  [1]  0.0100  0.0100  0.0100  0.0100  0.0100  0.0101  0.0100  0.0477  0.0100
#> [10]  0.0100  0.0100  0.0166
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
    rep(1e-2, n_small), # sigma
    1e-2, # lambda
    rep(1e-2, n_small), # daily
    rep(1e-2, n_small), # weekly
    rep(1e-2, n_small) # monthly
  ), 
  upper = c(
    rep(1, n_small), # sigma
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
#> [1]  0.0595  0.0495  0.0949
#> 
#> Setting for 'lambda':
#> [1]  0.291
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.0495  0.0100  0.0100
#> 
#> Setting for 'weekly':
#> [1]  0.101  0.010  0.160
#> 
#> Setting for 'monthly':
#> [1]  0.010  0.121  0.010
```

``` r
(bvhar_vhar_medium_optim <- choose_bvhar(
  bvhar_vhar_medium_spec, 
  lower = c(
    rep(1e-2, n_medium), # sigma
    1e-2, # lambda
    rep(1e-2, n_medium), # daily
    rep(1e-2, n_medium), # weekly
    rep(1e-2, n_medium) # monthly
  ), 
  upper = c(
    rep(1, n_medium), # sigma
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
#> [1]  0.0420  0.0359  0.0521  0.0579  0.0661  0.0554  0.0646  0.1276  0.0807
#> 
#> Setting for 'lambda':
#> [1]  0.146
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.0281  0.0100  0.0356  0.0100  0.0100  0.0100  0.0354  0.0100  0.0292
#> 
#> Setting for 'weekly':
#> [1]  0.2226  0.0265  0.0100  0.0100  0.0100  0.0100  0.0278  0.0365  0.0100
#> 
#> Setting for 'monthly':
#> [1]  0.010  0.010  0.010  0.010  0.010  0.010  0.010  0.010  0.126
```

``` r
(bvhar_vhar_large_optim <- choose_bvhar(
  bvhar_vhar_large_spec, 
  lower = c(
    rep(1e-2, n_large), # sigma
    1e-2, # lambda
    rep(1e-2, n_large), # daily
    rep(1e-2, n_large), # weekly
    rep(1e-2, n_large) # monthly
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
#>  [1]  0.0376  0.0411  0.0439  0.0563  0.0674  0.0429  0.0636  0.0621  0.0837
#> [10]  0.0782  0.0745  0.0941
#> 
#> Setting for 'lambda':
#> [1]  0.0342
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#>  [1]  0.0100  0.0100  0.0100  0.0100  0.0100  0.0112  0.0100  0.0490  0.0100
#> [10]  0.0100  0.0100  0.0214
#> 
#> Setting for 'weekly':
#>  [1]  0.0100  0.0100  0.0716  0.0466  0.0100  0.0100  0.0100  0.0100  0.0100
#> [10]  0.1116  0.0673  0.0100
#> 
#> Setting for 'monthly':
#>  [1]  0.1465  0.0100  0.0100  0.1316  0.0100  0.0100  0.0100  0.2151  0.1983
#> [10]  0.0946  0.2550  0.0100
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
    \hspace{1em} & BVAR & $\sigma$ & 0.060 & 0.050 & 0.076 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.203 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.066 & 0.054 & 0.010 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.059 & 0.049 & 0.097 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.305 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.056 & 0.010 & 0.010 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.059 & 0.049 & 0.095 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.291 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.050 & 0.010 & 0.010 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.101 & 0.010 & 0.160 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.010 & 0.121 & 0.010 &  &  &  &  &  &  &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{MEDIUM}}\\
    \hspace{1em} & BVAR & $\sigma$ & 0.041 & 0.040 & 0.052 & 0.060 & 0.063 & 0.057 & 0.066 & 0.087 & 0.085 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.132 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.065 & 0.010 & 0.011 & 0.010 & 0.010 & 0.010 & 0.044 & 0.010 & 0.029 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.042 & 0.036 & 0.052 & 0.058 & 0.066 & 0.056 & 0.065 & 0.125 & 0.085 &  &  & \\

    \hspace{1em}\hspace{1em} &  & $\lambda$ & 0.146 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.052 & 0.010 & 0.038 & 0.010 & 0.010 & 0.010 & 0.039 & 0.010 & 0.034 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.042 & 0.036 & 0.052 & 0.058 & 0.066 & 0.055 & 0.065 & 0.128 & 0.081 &  &  & \\

     &  & $\lambda$ & 0.146 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.028 & 0.010 & 0.036 & 0.010 & 0.010 & 0.010 & 0.035 & 0.010 & 0.029 &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.223 & 0.026 & 0.010 & 0.010 & 0.010 & 0.010 & 0.028 & 0.036 & 0.010 &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.126 &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{LARGE}}\\
    \hspace{1em} & BVAR & $\sigma$ & 0.039 & 0.042 & 0.044 & 0.058 & 0.067 & 0.044 & 0.064 & 0.058 & 0.086 & 0.079 & 0.072 & 0.091\\

    \hspace{1em} &  & $\lambda$ & 0.048 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.017 & 0.010 & 0.039 & 0.010 & 0.017 & 0.010 & 0.014\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.037 & 0.041 & 0.044 & 0.056 & 0.067 & 0.043 & 0.064 & 0.062 & 0.084 & 0.078 & 0.075 & 0.095\\

    \hspace{1em} &  & $\lambda$ & 0.035 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.048 & 0.010 & 0.010 & 0.010 & 0.017\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.038 & 0.041 & 0.044 & 0.056 & 0.067 & 0.043 & 0.064 & 0.062 & 0.084 & 0.078 & 0.075 & 0.094\\

    \hspace{1em} &  & $\lambda$ & 0.034 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.011 & 0.010 & 0.049 & 0.010 & 0.010 & 0.010 & 0.021\\

    \hspace{1em} &  & $w_i$ & 0.010 & 0.010 & 0.072 & 0.047 & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.112 & 0.067 & 0.010\\

    \hspace{1em} &  & $m_i$ & 0.146 & 0.010 & 0.010 & 0.132 & 0.010 & 0.010 & 0.010 & 0.215 & 0.198 & 0.095 & 0.255 & 0.010\\*
    \end{longtable}

# Errors

## Rolling Windows

### SMALL

``` r
mod_small_list <- list(
  fit_var_small,
  fit_vhar_small,
  fit_small_bvar,
  fit_bvhar_small_var,
  fit_bvhar_small_vhar
)
# 1-step-----------
cv_small_1 <- 
  mod_small_list %>% 
  lapply(
    function(mod) {
      forecast_roll(mod, 1, y_small_test)
    }
  )
# 5-step-----------
cv_small_5 <- 
  mod_small_list %>% 
  lapply(
    function(mod) {
      forecast_roll(mod, 5, y_small_test)
    }
  )
# 20-step----------
cv_small_20 <- 
  mod_small_list %>% 
  lapply(
    function(mod) {
      forecast_roll(mod, 20, y_small_test)
    }
  )
```

### MEDIUM

``` r
mod_medium_list <- list(
  fit_var_medium,
  fit_vhar_medium,
  fit_medium_bvar,
  fit_bvhar_medium_var,
  fit_bvhar_medium_vhar
)
# 1-step-----------
cv_medium_1 <- 
  mod_medium_list %>% 
  lapply(
    function(mod) {
      forecast_roll(mod, 1, y_medium_test)
    }
  )
# 5-step-----------
cv_medium_5 <- 
  mod_medium_list %>% 
  lapply(
    function(mod) {
      forecast_roll(mod, 5, y_medium_test)
    }
  )
# 20-step----------
cv_medium_20 <- 
  mod_medium_list %>% 
  lapply(
    function(mod) {
      forecast_roll(mod, 20, y_medium_test)
    }
  )
```

### LARGE

``` r
mod_large_list <- list(
  fit_var_large,
  fit_vhar_large,
  fit_large_bvar,
  fit_bvhar_large_var,
  fit_bvhar_large_vhar
)
# 1-step-----------
cv_large_1 <- 
  mod_large_list %>% 
  lapply(
    function(mod) {
      forecast_roll(mod, 1, y_large_test)
    }
  )
# 5-step-----------
cv_large_5 <- 
  mod_large_list %>% 
  lapply(
    function(mod) {
      forecast_roll(mod, 5, y_large_test)
    }
  )
# 20-step----------
cv_large_20 <- 
  mod_large_list %>% 
  lapply(
    function(mod) {
      forecast_roll(mod, 20, y_large_test)
    }
  )
```

### Lists

``` r
# SMALL-------------------------------
cv_small_list <- 
  lapply(
    c(1, 5, 20),
    function(h) {
      mod_small_list %>% 
        lapply(
          function(mod) {
            forecast_roll(mod, h, y_small_test)
          }
        )
    }
  )
# MEDIUM------------------------------
cv_medium_list <- 
  lapply(
    c(1, 5, 20),
    function(h) {
      mod_small_list %>% 
        lapply(
          function(mod) {
            forecast_roll(mod, h, y_medium_test)
          }
        )
    }
  )
# LARGE-------------------------------
cv_large_list <- 
  lapply(
    c(1, 5, 20),
    function(h) {
      mod_small_list %>% 
        lapply(
          function(mod) {
            forecast_roll(mod, h, y_large_test)
          }
        )
    }
  )
```

## Relative Errors

Set VAR as the benchmark model.

    \begin{table}[H]

    \caption{\label{tab:dgp1result}Out-of-sample forecasting performance measures for DGP1.}
    \centering
    \resizebox{\linewidth}{!}{
    \begin{tabular}[t]{cc|ccc|ccc|ccc|}
    \toprule
    \multicolumn{2}{c}{ } & \multicolumn{3}{c}{RMAFE} & \multicolumn{3}{c}{RMSFE} & \multicolumn{3}{c}{RMASE} \\
    \cmidrule(l{3pt}r{3pt}){3-5} \cmidrule(l{3pt}r{3pt}){6-8} \cmidrule(l{3pt}r{3pt}){9-11}
    \rotatebox{0}{} & \rotatebox{0}{} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$}\\
    \midrule
     & VHAR & \textcolor{black}{\num{.987}} & \textcolor{black}{\num{.973}} & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{.973}} & \textcolor{black}{\num{.941}} & \textcolor{black}{\num{1.018}} & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{.967}} & \textcolor{black}{\num{.996}}\\

     & BVAR & \textcolor{red}{\num{.976}} & \textcolor{black}{\num{.963}} & \textcolor{black}{\num{.979}} & \textcolor{black}{\num{.972}} & \textcolor{black}{\num{.939}} & \textcolor{black}{\num{.955}} & \textcolor{red}{\num{.975}} & \textcolor{black}{\num{.963}} & \textcolor{black}{\num{.979}}\\

     & BVHAR-S & \textcolor{black}{\num{.977}} & \textcolor{black}{\num{.949}} & \textcolor{red}{\num{.970}} & \textcolor{black}{\num{.963}} & \textcolor{black}{\num{.907}} & \textcolor{black}{\num{.931}} & \textcolor{black}{\num{.977}} & \textcolor{black}{\num{.949}} & \textcolor{red}{\num{.970}}\\

    \multirow{-4}{*}{\centering\arraybackslash SMALL} & BVHAR-L & \textcolor{black}{\num{.976}} & \textcolor{red}{\num{.947}} & \textcolor{black}{\num{.971}} & \textcolor{red}{\num{.962}} & \textcolor{red}{\num{.903}} & \textcolor{red}{\num{.931}} & \textcolor{black}{\num{.977}} & \textcolor{red}{\num{.947}} & \textcolor{black}{\num{.970}}\\
    \cmidrule{1-11}
     & VHAR & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{1.101}} & \textcolor{black}{\num{1.288}} & \textcolor{black}{\num{1.006}} & \textcolor{black}{\num{1.179}} & \textcolor{black}{\num{1.557}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{1.102}} & \textcolor{black}{\num{1.310}}\\

     & BVAR & \textcolor{black}{\num{.976}} & \textcolor{red}{\num{.940}} & \textcolor{black}{\num{.984}} & \textcolor{red}{\num{.995}} & \textcolor{red}{\num{.945}} & \textcolor{black}{\num{.969}} & \textcolor{black}{\num{.974}} & \textcolor{red}{\num{.944}} & \textcolor{black}{\num{.983}}\\

     & BVHAR-S & \textcolor{black}{\num{.974}} & \textcolor{black}{\num{.944}} & \textcolor{red}{\num{.984}} & \textcolor{black}{\num{1.014}} & \textcolor{black}{\num{.992}} & \textcolor{red}{\num{.966}} & \textcolor{black}{\num{.974}} & \textcolor{black}{\num{.949}} & \textcolor{red}{\num{.983}}\\

    \multirow{-4}{*}{\centering\arraybackslash MEDIUM} & BVHAR-L & \textcolor{red}{\num{.973}} & \textcolor{black}{\num{.944}} & \textcolor{black}{\num{.984}} & \textcolor{black}{\num{1.016}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{.968}} & \textcolor{red}{\num{.973}} & \textcolor{black}{\num{.949}} & \textcolor{black}{\num{.983}}\\
    \cmidrule{1-11}
     & VHAR & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{1.087}} & \textcolor{black}{\num{1.185}} & \textcolor{black}{\num{1.011}} & \textcolor{black}{\num{1.172}} & \textcolor{black}{\num{1.415}} & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{1.082}} & \textcolor{black}{\num{1.193}}\\

     & BVAR & \textcolor{black}{\num{.975}} & \textcolor{red}{\num{.948}} & \textcolor{red}{\num{.986}} & \textcolor{red}{\num{.988}} & \textcolor{red}{\num{.957}} & \textcolor{black}{\num{.968}} & \textcolor{black}{\num{.971}} & \textcolor{red}{\num{.957}} & \textcolor{red}{\num{.987}}\\

     & BVHAR-S & \textcolor{black}{\num{.971}} & \textcolor{black}{\num{.957}} & \textcolor{black}{\num{.987}} & \textcolor{black}{\num{1.006}} & \textcolor{black}{\num{1.013}} & \textcolor{red}{\num{.966}} & \textcolor{black}{\num{.966}} & \textcolor{black}{\num{.967}} & \textcolor{black}{\num{.989}}\\

    \multirow{-4}{*}{\centering\arraybackslash LARGE} & BVHAR-L & \textcolor{red}{\num{.971}} & \textcolor{black}{\num{.957}} & \textcolor{black}{\num{.987}} & \textcolor{black}{\num{1.008}} & \textcolor{black}{\num{1.021}} & \textcolor{black}{\num{.968}} & \textcolor{red}{\num{.965}} & \textcolor{black}{\num{.968}} & \textcolor{black}{\num{.990}}\\
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
     & asset01 & \num{0.000975} & \num{0.000943} & \num{0.00094} & \textcolor{red}{\num{0.00094}} & \num{0.00094}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000894} & \num{0.000954} & \textcolor{red}{\num{0.00086}} & \num{0.000944} & \num{0.000944}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0149} & \num{0.0144} & \num{0.0145} & \num{0.0142} & \textcolor{red}{\num{0.0142}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00558} & \num{0.00543} & \num{0.00542} & \num{0.00537} & \textcolor{red}{\num{0.00537}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0253} & \textcolor{red}{\num{0.0248}} & \num{0.0249} & \num{0.0248} & \num{0.0249}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0237} & \num{0.0249} & \textcolor{red}{\num{0.0234}} & \num{0.0247} & \num{0.0247}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.101} & \num{0.0984} & \num{0.0983} & \num{0.0971} & \textcolor{red}{\num{0.097}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.05} & \num{0.0494} & \textcolor{red}{\num{0.0488}} & \num{0.0489} & \num{0.0489}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{130.889} & \num{128.647} & \textcolor{red}{\num{128.351}} & \num{129.597} & \num{130.107}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{405.687} & \num{419.213} & \num{367.824} & \num{310.847} & \textcolor{red}{\num{302.557}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{92.648} & \textcolor{red}{\num{87.847}} & \num{90.147} & \num{88.837} & \num{88.718}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{209.741} & \num{211.902} & \num{195.441} & \num{176.427} & \textcolor{red}{\num{173.794}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{56.993} & \num{56.409} & \textcolor{red}{\num{55.988}} & \num{56.337} & \num{56.386}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{51.877} & \num{55.588} & \textcolor{red}{\num{51.184}} & \num{54.865} & \num{54.868}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{223.898} & \num{215.843} & \num{217.297} & \num{213.953} & \textcolor{red}{\num{213.846}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{110.923} & \num{109.28} & \textcolor{red}{\num{108.156}} & \num{108.385} & \num{108.366}\\*
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
     & asset01 & \num{0.00414} & \num{0.00388} & \num{0.00389} & \num{0.00375} & \textcolor{red}{\num{0.00374}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00321} & \num{0.00303} & \num{0.00305} & \num{0.003} & \textcolor{red}{\num{0.003}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0562} & \num{0.0528} & \num{0.0527} & \num{0.0509} & \textcolor{red}{\num{0.0506}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0212} & \num{0.0199} & \num{0.0199} & \num{0.0192} & \textcolor{red}{\num{0.0191}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0543} & \num{0.053} & \num{0.0526} & \num{0.0519} & \textcolor{red}{\num{0.0518}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.046} & \textcolor{red}{\num{0.0443}} & \num{0.0448} & \num{0.0444} & \num{0.0443}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.194} & \num{0.189} & \num{0.186} & \num{0.183} & \textcolor{red}{\num{0.183}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0981} & \num{0.0955} & \num{0.0945} & \num{0.0931} & \textcolor{red}{\num{0.093}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{188.029} & \num{181.528} & \textcolor{red}{\num{173.723}} & \num{179.855} & \num{179.378}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{393.718}} & \num{644.312} & \num{444.875} & \num{429.429} & \num{435.631}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{137.92} & \num{143.121} & \textcolor{red}{\num{127.949}} & \num{131.543} & \num{131.368}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{239.889}} & \num{322.987} & \num{248.849} & \num{246.943} & \num{248.793}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{121.33} & \num{117.705} & \num{117.5} & \num{115.688} & \textcolor{red}{\num{115.457}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{106.427} & \textcolor{red}{\num{100.487}} & \num{103.159} & \num{102.197} & \num{102.059}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{443.617} & \num{430.905} & \num{425.555} & \num{419.223} & \textcolor{red}{\num{418.473}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{223.791} & \num{216.365} & \num{215.405} & \num{212.37} & \textcolor{red}{\num{211.996}}\\*
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
     & asset01 & \num{0.00666} & \num{0.00667} & \num{0.0064} & \num{0.00624} & \textcolor{red}{\num{0.00624}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00396} & \num{0.00402} & \num{0.00381} & \textcolor{red}{\num{0.00378}} & \num{0.00378}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0857} & \num{0.0874} & \num{0.0818} & \num{0.0797} & \textcolor{red}{\num{0.0796}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0321} & \num{0.0327} & \num{0.0307} & \num{0.0299} & \textcolor{red}{\num{0.0299}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.067} & \num{0.0652} & \num{0.0656} & \textcolor{red}{\num{0.0649}} & \num{0.0649}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0506} & \num{0.05} & \num{0.0497} & \textcolor{red}{\num{0.0494}} & \num{0.0494}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.244} & \num{0.241} & \num{0.238} & \textcolor{red}{\num{0.236}} & \num{0.236}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.12} & \num{0.119} & \num{0.118} & \textcolor{red}{\num{0.117}} & \num{0.117}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{179.596} & \num{176.369} & \num{169.866} & \textcolor{red}{\num{168.968}} & \num{169.356}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{540.811} & \textcolor{red}{\num{232.673}} & \num{434.152} & \num{377.253} & \num{373.253}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{153.914} & \num{149.206} & \num{147.369} & \textcolor{red}{\num{143.222}} & \num{143.374}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{291.44} & \textcolor{red}{\num{186.083}} & \num{250.462} & \num{229.814} & \num{228.661}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{144.585} & \num{143.055} & \num{141.229} & \textcolor{red}{\num{139.435}} & \num{139.473}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{113.337} & \num{112.898} & \num{111.439} & \textcolor{red}{\num{111.029}} & \num{111.082}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{542.351} & \num{541.161} & \num{530.489} & \textcolor{red}{\num{525.694}} & \num{525.818}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{266.758} & \num{265.705} & \num{261.053} & \textcolor{red}{\num{258.72}} & \num{258.791}\\*
    \end{longtable}

### MEDIUM

Plots

``` r
cv_medium_1 %>% 
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
cv_medium_5 %>% 
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
cv_medium_20 %>% 
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
     & asset01 & \num{0.000379} & \num{0.000377} & \textcolor{red}{\num{0.000354}} & \num{0.000354} & \num{0.000355}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000234} & \num{0.00025} & \textcolor{red}{\num{0.00022}} & \num{0.000227} & \num{0.000227}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000346} & \num{0.000348} & \textcolor{red}{\num{0.000337}} & \num{0.000338} & \num{0.000338}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000509} & \num{0.000534} & \num{0.000504} & \num{0.000504} & \textcolor{red}{\num{0.000503}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.00097}} & \num{0.001026} & \num{0.000977} & \num{0.001031} & \num{0.001031}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000857} & \num{0.000881} & \textcolor{red}{\num{0.000844}} & \num{0.000911} & \num{0.000911}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000534} & \num{0.000551} & \textcolor{red}{\num{0.000517}} & \num{0.00055} & \num{0.000551}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00612} & \num{0.00613} & \textcolor{red}{\num{0.00583}} & \num{0.00603} & \num{0.00604}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.00414}} & \num{0.00443} & \num{0.00418} & \num{0.00457} & \num{0.00457}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00157} & \num{0.00161} & \textcolor{red}{\num{0.00153}} & \num{0.00161} & \num{0.00161}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0152} & \num{0.0152} & \textcolor{red}{\num{0.0147}} & \num{0.0147} & \num{0.0147}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0126} & \num{0.0126} & \num{0.0122} & \num{0.0121} & \textcolor{red}{\num{0.0121}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0151} & \num{0.0152} & \textcolor{red}{\num{0.0149}} & \num{0.0151} & \num{0.0151}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0175} & \num{0.0179} & \textcolor{red}{\num{0.0173}} & \num{0.0175} & \num{0.0175}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.0238}} & \num{0.025} & \num{0.0243} & \num{0.0256} & \num{0.0256}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0241} & \num{0.0244} & \textcolor{red}{\num{0.0238}} & \num{0.0244} & \num{0.0244}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0178} & \num{0.0179} & \textcolor{red}{\num{0.0171}} & \num{0.0176} & \num{0.0177}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0651} & \num{0.0652} & \textcolor{red}{\num{0.0633}} & \num{0.0641} & \num{0.0642}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.053}} & \num{0.0548} & \num{0.0539} & \num{0.056} & \num{0.056}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0271} & \num{0.0276} & \textcolor{red}{\num{0.0268}} & \num{0.0275} & \num{0.0275}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{153.786} & \num{145.495} & \num{137.828} & \textcolor{red}{\num{135.648}} & \num{136.098}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{374.249} & \num{232.446} & \num{243.453} & \num{120.058} & \textcolor{red}{\num{119.383}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{207.774} & \num{191.121} & \num{197.001} & \textcolor{red}{\num{179.972}} & \num{180.082}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{224.326} & \num{185.908} & \textcolor{red}{\num{133.864}} & \num{137.897} & \num{138.888}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{885.494} & \textcolor{red}{\num{217.43}} & \num{699.666} & \num{372.328} & \num{372.082}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{172.173} & \num{163.645} & \num{134.866} & \textcolor{red}{\num{108.752}} & \num{108.956}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{243.546} & \num{206.283} & \num{218.775} & \textcolor{red}{\num{153.595}} & \num{154.212}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{488.994} & \num{458.991} & \num{415.408} & \textcolor{red}{\num{384.017}} & \num{389.379}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{130.18} & \num{127.234} & \num{124.219} & \num{113.969} & \textcolor{red}{\num{113.668}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{320.058} & \num{214.284} & \num{256.12} & \textcolor{red}{\num{189.582}} & \num{190.305}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{51.138} & \num{52.113} & \textcolor{red}{\num{49.708}} & \num{50.054} & \num{49.955}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{41.467} & \num{40.585} & \num{39.702} & \num{38.948} & \textcolor{red}{\num{38.915}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{50.98} & \num{50.992} & \textcolor{red}{\num{50.138}} & \num{50.608} & \num{50.595}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{57.373} & \num{59.564} & \textcolor{red}{\num{57.198}} & \num{57.791} & \num{57.757}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{79.381}} & \num{83.434} & \num{81.013} & \num{85.689} & \num{85.724}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{82.016} & \num{82.637} & \textcolor{red}{\num{81.467}} & \num{83.815} & \num{83.803}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{59.954} & \num{59.873} & \textcolor{red}{\num{57.505}} & \num{59.46} & \num{59.531}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{213.237} & \num{213.884} & \textcolor{red}{\num{203.427}} & \num{206.177} & \num{206.268}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{179.352}} & \num{187.33} & \num{182.037} & \num{188.212} & \num{188.285}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{90.544} & \num{92.268} & \textcolor{red}{\num{89.133}} & \num{91.195} & \num{91.204}\\*
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
     & asset01 & \num{0.000365} & \num{0.000381} & \textcolor{red}{\num{0.000362}} & \num{0.000363} & \num{0.000364}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000219} & \num{0.000225} & \num{0.000216} & \textcolor{red}{\num{0.000216}} & \num{0.000216}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000379} & \num{0.000389} & \textcolor{red}{\num{0.000378}} & \num{0.000378} & \num{0.000378}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000503} & \num{0.000513} & \num{0.000498} & \num{0.000497} & \textcolor{red}{\num{0.000497}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.00112}} & \num{0.00113} & \num{0.00113} & \num{0.00113} & \num{0.00113}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000986} & \textcolor{red}{\num{0.000978}} & \num{0.000979} & \num{0.000982} & \num{0.000982}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{0.000565}} & \num{0.000584} & \num{0.000571} & \num{0.000574} & \num{0.000574}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00664} & \num{0.00664} & \num{0.00654} & \num{0.00652} & \textcolor{red}{\num{0.00652}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.00508}} & \num{0.00531} & \num{0.0051} & \num{0.00516} & \num{0.00516}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00176} & \num{0.0018} & \textcolor{red}{\num{0.00175}} & \num{0.00176} & \num{0.00176}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0151} & \num{0.0154} & \textcolor{red}{\num{0.015}} & \num{0.015} & \num{0.015}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0121} & \num{0.0122} & \num{0.012} & \textcolor{red}{\num{0.0119}} & \num{0.012}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.016} & \num{0.0163} & \textcolor{red}{\num{0.016}} & \num{0.016} & \num{0.016}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0176} & \num{0.0178} & \num{0.0174} & \num{0.0174} & \textcolor{red}{\num{0.0174}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.0264}} & \num{0.0267} & \num{0.0268} & \num{0.0267} & \num{0.0267}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0256} & \num{0.0257} & \textcolor{red}{\num{0.0254}} & \num{0.0255} & \num{0.0255}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0177} & \num{0.0184} & \textcolor{red}{\num{0.0177}} & \num{0.0179} & \num{0.0179}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.066} & \num{0.0654} & \num{0.0646} & \num{0.0644} & \textcolor{red}{\num{0.0644}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.0588}} & \num{0.0598} & \num{0.0591} & \num{0.0591} & \num{0.0591}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0284} & \num{0.0286} & \textcolor{red}{\num{0.0282}} & \num{0.0282} & \num{0.0282}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{105.465} & \num{123.285} & \textcolor{red}{\num{101.773}} & \num{102.911} & \num{104.01}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{137.319} & \num{187.819} & \textcolor{red}{\num{103.156}} & \num{113.688} & \num{114.671}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{114.843} & \num{119.472} & \num{102.796} & \num{101.974} & \textcolor{red}{\num{101.37}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{151.843} & \num{156.038} & \textcolor{red}{\num{105.87}} & \num{107.829} & \num{108.557}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{123.717} & \num{225.766} & \textcolor{red}{\num{117.033}} & \num{122.702} & \num{123.091}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{104.922} & \num{140.396} & \textcolor{red}{\num{100.087}} & \num{101.452} & \num{101.38}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{126.376} & \num{171.52} & \textcolor{red}{\num{98.04}} & \num{116.45} & \num{116.47}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{212.847} & \num{143.087} & \num{101.15} & \num{101.639} & \textcolor{red}{\num{99.866}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{100.208} & \num{116.023} & \num{100.588} & \num{96.217} & \textcolor{red}{\num{96.07}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{130.838} & \num{153.712} & \textcolor{red}{\num{103.388}} & \num{107.207} & \num{107.276}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{50.006} & \num{50.427} & \textcolor{red}{\num{49.585}} & \num{49.654} & \num{49.816}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{40.021} & \num{40.768} & \textcolor{red}{\num{39.544}} & \num{39.59} & \num{39.618}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{52.909} & \num{53.856} & \textcolor{red}{\num{52.715}} & \num{52.777} & \num{52.779}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{58.168} & \num{58.35} & \textcolor{red}{\num{57.341}} & \num{57.354} & \num{57.343}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{87.774}} & \num{88.333} & \num{89.029} & \num{88.785} & \num{88.786}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{80.149} & \num{81.661} & \textcolor{red}{\num{79.805}} & \num{79.943} & \num{79.932}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{59.978}} & \num{62.281} & \num{60.21} & \num{61.073} & \num{61.059}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{222.456} & \num{219.938} & \num{218.235} & \num{217.463} & \textcolor{red}{\num{217.386}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{195.36}} & \num{201.938} & \num{196.234} & \num{196.235} & \num{196.273}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{94.091} & \num{95.283} & \textcolor{red}{\num{93.633}} & \num{93.653} & \num{93.666}\\*
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
     & asset01 & \num{0.000393} & \num{0.000394} & \textcolor{red}{\num{0.000393}} & \num{0.000393} & \num{0.000393}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000223} & \textcolor{red}{\num{0.000223}} & \num{0.000223} & \num{0.000223} & \num{0.000223}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000371} & \textcolor{red}{\num{0.000369}} & \num{0.000371} & \num{0.000371} & \num{0.000371}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.00052} & \textcolor{red}{\num{0.000514}} & \num{0.00052} & \num{0.00052} & \num{0.00052}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.000987} & \num{0.00099} & \textcolor{red}{\num{0.000987}} & \num{0.000987} & \num{0.000987}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000905} & \num{0.000905} & \num{0.000905} & \textcolor{red}{\num{0.000905}} & \num{0.000905}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000519} & \textcolor{red}{\num{0.000508}} & \num{0.000519} & \num{0.000519} & \num{0.000519}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00658} & \num{0.00661} & \textcolor{red}{\num{0.00658}} & \num{0.00658} & \num{0.00658}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00487} & \num{0.00494} & \num{0.00487} & \textcolor{red}{\num{0.00487}} & \num{0.00488}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00171} & \num{0.00172} & \textcolor{red}{\num{0.00171}} & \num{0.00171} & \num{0.00171}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0156} & \num{0.0158} & \num{0.0156} & \num{0.0156} & \textcolor{red}{\num{0.0156}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0123} & \num{0.0123} & \num{0.0123} & \num{0.0123} & \textcolor{red}{\num{0.0123}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0156} & \textcolor{red}{\num{0.0156}} & \num{0.0156} & \num{0.0156} & \num{0.0156}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.018} & \textcolor{red}{\num{0.0179}} & \num{0.018} & \num{0.018} & \num{0.018}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0251} & \textcolor{red}{\num{0.0251}} & \num{0.0251} & \num{0.0251} & \num{0.0251}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0243} & \textcolor{red}{\num{0.0242}} & \num{0.0243} & \num{0.0243} & \num{0.0243}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0165} & \textcolor{red}{\num{0.0165}} & \num{0.0165} & \num{0.0165} & \num{0.0165}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0644} & \num{0.0645} & \textcolor{red}{\num{0.0644}} & \num{0.0644} & \num{0.0644}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0572} & \num{0.0574} & \num{0.0572} & \textcolor{red}{\num{0.0572}} & \num{0.0573}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0277} & \num{0.0277} & \num{0.0277} & \textcolor{red}{\num{0.0277}} & \num{0.0277}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{99.992} & \num{108.074} & \num{100} & \num{99.881} & \textcolor{red}{\num{99.845}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{99.955} & \num{117.357} & \num{100} & \textcolor{red}{\num{99.509}} & \num{99.528}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{100.007} & \num{120.435} & \textcolor{red}{\num{100}} & \num{100.336} & \num{100.449}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{99.935} & \num{125.088} & \num{100} & \num{98.778} & \textcolor{red}{\num{98.749}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{99.964}} & \num{142.365} & \num{100.001} & \num{101.559} & \num{102.277}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{100.018} & \num{106.801} & \textcolor{red}{\num{100}} & \num{100.065} & \num{100.097}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{100.02} & \num{107.269} & \textcolor{red}{\num{100}} & \num{100.005} & \num{100.017}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{100.112} & \num{200.667} & \textcolor{red}{\num{100}} & \num{100.175} & \num{100.298}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{100} & \textcolor{red}{\num{99.58}} & \num{100} & \num{100.025} & \num{99.833}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{100} & \num{125.293} & \textcolor{red}{\num{100}} & \num{100.037} & \num{100.122}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{52.762} & \num{53.088} & \num{52.762} & \num{52.758} & \textcolor{red}{\num{52.757}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{43.221} & \textcolor{red}{\num{43.116}} & \num{43.22} & \num{43.217} & \num{43.217}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{52.917} & \textcolor{red}{\num{52.843}} & \num{52.917} & \num{52.917} & \num{52.918}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{57.472} & \textcolor{red}{\num{57.163}} & \num{57.47} & \num{57.459} & \num{57.457}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{85.685} & \textcolor{red}{\num{85.467}} & \num{85.684} & \num{85.688} & \num{85.688}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{76.561} & \textcolor{red}{\num{76.34}} & \num{76.559} & \num{76.557} & \num{76.561}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{54.785} & \textcolor{red}{\num{54.253}} & \num{54.782} & \num{54.773} & \num{54.772}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{203.306} & \textcolor{red}{\num{202.754}} & \num{203.303} & \num{203.309} & \num{203.311}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{189.588} & \num{189.736} & \num{189.584} & \textcolor{red}{\num{189.569}} & \num{189.682}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{90.7} & \textcolor{red}{\num{90.529}} & \num{90.698} & \num{90.694} & \num{90.707}\\*
    \end{longtable}

### LARGE

Plots

``` r
cv_large_1 %>% 
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
cv_large_5 %>% 
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
cv_large_20 %>% 
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
     & asset01 & \num{0.000287} & \num{0.000297} & \textcolor{red}{\num{0.000276}} & \num{0.000277} & \num{0.000277}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00042} & \num{0.000404} & \num{0.000367} & \textcolor{red}{\num{0.000363}} & \num{0.000363}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000244} & \num{0.00026} & \textcolor{red}{\num{0.000237}} & \num{0.000239} & \num{0.000238}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.000438}} & \num{0.000445} & \num{0.000464} & \num{0.000465} & \num{0.000463}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.000815} & \num{0.000784} & \num{0.00077} & \num{0.000767} & \textcolor{red}{\num{0.000767}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000525} & \num{0.000517} & \num{0.000496} & \num{0.000493} & \textcolor{red}{\num{0.000493}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000712} & \num{0.000739} & \num{0.000679} & \num{0.000676} & \textcolor{red}{\num{0.000676}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00238} & \num{0.00226} & \num{0.00216} & \num{0.00215} & \textcolor{red}{\num{0.00214}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00378} & \num{0.00374} & \num{0.00368} & \num{0.00365} & \textcolor{red}{\num{0.00364}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.000921} & \num{0.000923} & \num{0.000873} & \textcolor{red}{\num{0.00087}} & \num{0.000872}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.00409} & \num{0.00378} & \num{0.00368} & \num{0.00365} & \textcolor{red}{\num{0.0036}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.00516} & \textcolor{red}{\num{0.00479}} & \num{0.00486} & \num{0.0048} & \num{0.0048}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00165} & \num{0.00158} & \num{0.00154} & \num{0.00153} & \textcolor{red}{\num{0.00153}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0138} & \num{0.0139} & \textcolor{red}{\num{0.0133}} & \num{0.0134} & \num{0.0134}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.016} & \num{0.0157} & \num{0.0152} & \num{0.0152} & \textcolor{red}{\num{0.0152}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0124} & \num{0.0128} & \num{0.012} & \num{0.012} & \textcolor{red}{\num{0.012}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0173} & \textcolor{red}{\num{0.0172}} & \num{0.0174} & \num{0.0175} & \num{0.0174}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.0222}} & \num{0.0223} & \num{0.0224} & \num{0.0224} & \num{0.0224}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0184} & \num{0.0183} & \num{0.0181} & \textcolor{red}{\num{0.018}} & \num{0.018}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0221} & \num{0.0228} & \num{0.0216} & \num{0.0215} & \textcolor{red}{\num{0.0215}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0393} & \num{0.0383} & \num{0.037} & \num{0.037} & \textcolor{red}{\num{0.0369}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0494} & \num{0.0494} & \num{0.0483} & \num{0.048} & \textcolor{red}{\num{0.0479}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.0238} & \num{0.0241} & \num{0.0236} & \num{0.0235} & \textcolor{red}{\num{0.0235}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.0492} & \num{0.0484} & \num{0.0479} & \num{0.0478} & \textcolor{red}{\num{0.0471}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.0585} & \num{0.0558} & \num{0.0561} & \num{0.0556} & \textcolor{red}{\num{0.0555}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0285} & \num{0.0283} & \num{0.0277} & \num{0.0277} & \textcolor{red}{\num{0.0276}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{121.37} & \num{120.782} & \num{102.738} & \num{100.105} & \textcolor{red}{\num{99.819}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{127.865} & \num{205.831} & \num{131.223} & \num{117.591} & \textcolor{red}{\num{116.827}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{137.865} & \num{147.204} & \num{107.973} & \num{102.075} & \textcolor{red}{\num{100.611}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{671.665} & \num{439.037} & \num{162.208} & \textcolor{red}{\num{121.232}} & \num{149.037}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{109.256} & \num{107.392} & \textcolor{red}{\num{99.695}} & \num{100.126} & \num{100.112}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{146.785} & \num{131.437} & \num{105.907} & \num{103.017} & \textcolor{red}{\num{103.012}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{162.815} & \num{149.28} & \num{106.525} & \textcolor{red}{\num{101.145}} & \num{101.182}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{676.997} & \num{324.518} & \textcolor{red}{\num{205.816}} & \num{229.016} & \num{234.679}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{174.718} & \num{238.354} & \num{128.015} & \textcolor{red}{\num{114.804}} & \num{116.337}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{139.415} & \num{139.177} & \num{101.388} & \textcolor{red}{\num{98.987}} & \num{103.587}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{143.523} & \num{136.191} & \num{108.107} & \num{104.896} & \textcolor{red}{\num{98.911}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{184.849} & \num{161.885} & \num{120.477} & \num{110.62} & \textcolor{red}{\num{109.808}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{233.094} & \num{191.757} & \num{123.339} & \textcolor{red}{\num{116.968}} & \num{119.494}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{49.857} & \num{51.31} & \textcolor{red}{\num{49.312}} & \num{49.425} & \num{49.371}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{56.318} & \num{55.329} & \num{53.854} & \num{53.734} & \textcolor{red}{\num{53.727}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{40.879} & \num{41.573} & \num{38.911} & \textcolor{red}{\num{38.677}} & \num{38.695}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{56.365} & \textcolor{red}{\num{56.073}} & \num{56.357} & \num{56.309} & \num{56.168}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{77.099} & \textcolor{red}{\num{75.332}} & \num{77.186} & \num{77.381} & \num{77.346}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{60.902} & \num{60.648} & \num{59.694} & \textcolor{red}{\num{59.437}} & \num{59.449}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{76.507} & \num{78.75} & \num{73.622} & \num{73.037} & \textcolor{red}{\num{73.01}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{135.402} & \num{128.247} & \num{126.577} & \num{126.68} & \textcolor{red}{\num{126.188}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{167.733} & \num{164.172} & \num{161.875} & \num{160.737} & \textcolor{red}{\num{160.64}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{85.668} & \num{84.112} & \num{83.781} & \num{83.58} & \textcolor{red}{\num{83.404}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{172.274} & \num{165.678} & \num{166.727} & \num{166.061} & \textcolor{red}{\num{163.777}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{201.944} & \num{192.419} & \num{191.551} & \num{189.446} & \textcolor{red}{\num{189.2}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{98.412} & \num{96.137} & \num{94.954} & \num{94.542} & \textcolor{red}{\num{94.248}}\\*
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
     & asset01 & \num{0.000277} & \num{0.000289} & \num{0.000273} & \textcolor{red}{\num{0.000273}} & \num{0.000273}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000376} & \num{0.00038} & \num{0.000363} & \num{0.000363} & \textcolor{red}{\num{0.000363}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.000227}} & \num{0.000241} & \num{0.000228} & \num{0.000228} & \num{0.000228}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000477} & \textcolor{red}{\num{0.000461}} & \num{0.000471} & \num{0.000472} & \num{0.000471}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.000757}} & \num{0.000758} & \num{0.000771} & \num{0.000772} & \num{0.000772}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000492} & \num{5e-04} & \num{0.000488} & \num{0.000488} & \textcolor{red}{\num{0.000488}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000689} & \num{0.000693} & \num{0.000668} & \textcolor{red}{\num{0.000668}} & \num{0.000668}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00212} & \num{0.00213} & \num{0.0021} & \num{0.0021} & \textcolor{red}{\num{0.00209}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00374} & \textcolor{red}{\num{0.00366}} & \num{0.00369} & \num{0.00369} & \num{0.00368}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.000858} & \num{0.000888} & \textcolor{red}{\num{0.000853}} & \num{0.000853} & \num{0.000857}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.00345} & \num{0.00344} & \num{0.00337} & \num{0.00337} & \textcolor{red}{\num{0.00335}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.00465} & \num{0.00454} & \num{0.00453} & \num{0.00453} & \textcolor{red}{\num{0.00452}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00151} & \num{0.0015} & \num{0.00148} & \num{0.00148} & \textcolor{red}{\num{0.00148}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0134} & \num{0.0137} & \num{0.0133} & \textcolor{red}{\num{0.0133}} & \num{0.0133}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0154} & \num{0.0156} & \textcolor{red}{\num{0.0153}} & \num{0.0153} & \num{0.0153}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0117} & \num{0.0121} & \num{0.0117} & \textcolor{red}{\num{0.0117}} & \num{0.0117}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0178} & \textcolor{red}{\num{0.0173}} & \num{0.0176} & \num{0.0176} & \num{0.0176}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.0219}} & \num{0.0222} & \num{0.0223} & \num{0.0223} & \num{0.0223}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0178} & \num{0.018} & \num{0.0177} & \num{0.0177} & \textcolor{red}{\num{0.0177}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0216} & \num{0.022} & \num{0.0213} & \textcolor{red}{\num{0.0213}} & \num{0.0213}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0368} & \num{0.0375} & \num{0.0367} & \num{0.0366} & \textcolor{red}{\num{0.0365}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0486} & \textcolor{red}{\num{0.0475}} & \num{0.0478} & \num{0.0478} & \num{0.0477}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.0232} & \num{0.0238} & \textcolor{red}{\num{0.0232}} & \num{0.0232} & \num{0.0232}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.0461} & \num{0.047} & \num{0.0463} & \num{0.0462} & \textcolor{red}{\num{0.0458}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.0542} & \num{0.0538} & \num{0.0533} & \num{0.0533} & \textcolor{red}{\num{0.0533}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0274} & \num{0.0275} & \num{0.0272} & \num{0.0272} & \textcolor{red}{\num{0.0272}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{113.751} & \num{120.044} & \textcolor{red}{\num{99.867}} & \num{100.036} & \num{99.972}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{131.003} & \num{127.58} & \num{100.204} & \num{100.049} & \textcolor{red}{\num{99.369}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{105.424} & \num{127.781} & \num{100.129} & \textcolor{red}{\num{100.046}} & \num{100.626}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{231.784} & \num{277.084} & \num{103.914} & \textcolor{red}{\num{101.047}} & \num{104.365}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{97.704}} & \num{103.314} & \num{99.757} & \num{99.928} & \num{99.866}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{111.47} & \num{110.967} & \textcolor{red}{\num{99.451}} & \num{99.882} & \num{99.877}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{116.286} & \num{139.62} & \textcolor{red}{\num{99.896}} & \num{99.971} & \num{99.975}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{229.39} & \num{298.314} & \num{103.609} & \textcolor{red}{\num{100.714}} & \num{116.672}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{127.073} & \num{157.666} & \num{99.554} & \textcolor{red}{\num{98.628}} & \num{104.51}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{121.781} & \num{137.431} & \textcolor{red}{\num{99.641}} & \num{99.817} & \num{101.588}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{98.421} & \num{119.471} & \num{100.054} & \num{99.996} & \textcolor{red}{\num{95.441}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{144.812} & \num{144.97} & \textcolor{red}{\num{100.051}} & \num{100.342} & \num{100.249}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{135.742} & \num{155.353} & \num{100.511} & \textcolor{red}{\num{100.038}} & \num{101.876}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{42.719} & \num{43.488} & \num{42.209} & \num{42.212} & \textcolor{red}{\num{42.185}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{53.892} & \num{53.686} & \num{52.988} & \textcolor{red}{\num{52.981}} & \num{52.986}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{38.741} & \num{40.272} & \textcolor{red}{\num{38.738}} & \num{38.744} & \num{38.764}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{60.58} & \textcolor{red}{\num{58.314}} & \num{59.403} & \num{59.39} & \num{59.286}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{74.87}} & \num{75.985} & \num{76.403} & \num{76.485} & \num{76.463}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{60.142} & \num{60.686} & \num{60.088} & \num{60.088} & \textcolor{red}{\num{60.079}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{75.296} & \num{77.182} & \num{75.135} & \textcolor{red}{\num{75.113}} & \num{75.118}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{123.397}} & \num{125.035} & \num{124.645} & \num{124.614} & \num{124.398}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{165.515} & \textcolor{red}{\num{161.012}} & \num{162.612} & \num{162.54} & \num{162.238}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{79.814} & \num{80.672} & \textcolor{red}{\num{79.204}} & \num{79.237} & \num{79.257}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{154.52} & \num{157.664} & \num{155.481} & \num{155.452} & \textcolor{red}{\num{153.754}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{188.743} & \num{187.242} & \num{185.999} & \num{185.995} & \textcolor{red}{\num{185.956}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{93.186} & \num{93.436} & \num{92.742} & \num{92.738} & \textcolor{red}{\num{92.54}}\\*
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
     & asset01 & \num{0.000284} & \textcolor{red}{\num{0.00028}} & \num{0.000284} & \num{0.000284} & \num{0.000285}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000382} & \num{0.000388} & \num{0.000382} & \num{0.000382} & \textcolor{red}{\num{0.000382}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000242} & \num{0.000249} & \textcolor{red}{\num{0.000242}} & \num{0.000242} & \num{0.000242}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000458} & \textcolor{red}{\num{0.000451}} & \num{0.000458} & \num{0.000458} & \num{0.000458}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.000753} & \num{0.000754} & \num{0.000753} & \num{0.000753} & \textcolor{red}{\num{0.000753}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.000519}} & \num{0.000527} & \num{0.000519} & \num{0.000519} & \num{0.000519}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000651} & \num{0.000665} & \num{0.000651} & \num{0.000651} & \textcolor{red}{\num{0.000651}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00192} & \num{0.00196} & \num{0.00192} & \num{0.00192} & \textcolor{red}{\num{0.00191}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0039} & \num{0.00392} & \num{0.0039} & \num{0.0039} & \textcolor{red}{\num{0.00389}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.000844} & \num{0.00086} & \num{0.000844} & \num{0.000844} & \textcolor{red}{\num{0.000844}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.00312} & \num{0.00318} & \num{0.00312} & \num{0.00312} & \textcolor{red}{\num{0.00311}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.00424} & \num{0.00436} & \num{0.00424} & \num{0.00424} & \textcolor{red}{\num{0.00424}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00144} & \num{0.00147} & \num{0.00144} & \num{0.00144} & \textcolor{red}{\num{0.00144}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0137} & \textcolor{red}{\num{0.0135}} & \num{0.0137} & \num{0.0137} & \num{0.0137}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0156} & \num{0.0157} & \num{0.0156} & \num{0.0156} & \textcolor{red}{\num{0.0156}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0122} & \num{0.0123} & \textcolor{red}{\num{0.0122}} & \num{0.0122} & \num{0.0122}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0173} & \textcolor{red}{\num{0.017}} & \num{0.0173} & \num{0.0173} & \num{0.0173}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0222} & \num{0.0223} & \num{0.0222} & \num{0.0222} & \textcolor{red}{\num{0.0222}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.0188}} & \num{0.0189} & \num{0.0188} & \num{0.0188} & \num{0.0188}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0213} & \num{0.0216} & \num{0.0213} & \num{0.0213} & \textcolor{red}{\num{0.0213}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0344} & \num{0.0352} & \num{0.0344} & \num{0.0344} & \textcolor{red}{\num{0.0343}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.049} & \num{0.0491} & \num{0.049} & \num{0.049} & \textcolor{red}{\num{0.049}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.0229} & \num{0.0234} & \num{0.0229} & \num{0.0229} & \textcolor{red}{\num{0.0229}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.045} & \num{0.0454} & \num{0.045} & \num{0.045} & \textcolor{red}{\num{0.0448}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.0514} & \num{0.052} & \num{0.0514} & \num{0.0514} & \textcolor{red}{\num{0.0514}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.027} & \num{0.0272} & \num{0.027} & \num{0.027} & \textcolor{red}{\num{0.027}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{99.985} & \textcolor{red}{\num{99.355}} & \num{100} & \num{100.008} & \num{100.311}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{100.082} & \num{141.706} & \num{100} & \num{99.995} & \textcolor{red}{\num{99.991}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{100.009} & \textcolor{red}{\num{99.088}} & \num{100} & \num{100.008} & \num{100.058}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{100.161} & \num{142.903} & \textcolor{red}{\num{100}} & \num{100.045} & \num{103.347}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{100.03} & \num{102.994} & \num{100} & \textcolor{red}{\num{99.996}} & \num{100.006}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{99.999} & \num{102.574} & \num{100} & \num{100.001} & \textcolor{red}{\num{99.993}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{100.059} & \num{106.21} & \textcolor{red}{\num{100}} & \num{100.003} & \num{100.011}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{99.74}} & \num{241.74} & \num{100} & \num{99.962} & \num{113.373}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{100.035} & \num{130.966} & \num{100} & \textcolor{red}{\num{99.931}} & \num{105.545}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{99.936} & \num{112.543} & \num{100} & \num{100.007} & \textcolor{red}{\num{99.686}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{99.881} & \num{103.592} & \num{100} & \num{99.964} & \textcolor{red}{\num{98.16}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{100.031} & \num{114.848} & \num{100} & \num{99.976} & \textcolor{red}{\num{99.93}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{99.996} & \num{124.877} & \num{100} & \textcolor{red}{\num{99.991}} & \num{101.701}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{48.288} & \textcolor{red}{\num{47.607}} & \num{48.29} & \num{48.291} & \num{48.321}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{49.659} & \num{50.385} & \num{49.66} & \num{49.66} & \textcolor{red}{\num{49.659}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{42.241} & \num{42.525} & \textcolor{red}{\num{42.238}} & \num{42.24} & \num{42.243}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{57.957} & \textcolor{red}{\num{57.303}} & \num{57.953} & \num{57.954} & \num{57.91}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{81.037} & \num{81.671} & \num{81.033} & \textcolor{red}{\num{81.032}} & \num{81.034}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{64.631}} & \num{64.957} & \num{64.636} & \num{64.637} & \num{64.631}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{70.895} & \num{71.443} & \num{70.888} & \num{70.889} & \textcolor{red}{\num{70.884}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{111.923} & \num{114.469} & \num{111.928} & \num{111.927} & \textcolor{red}{\num{111.712}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{169.32} & \num{170.112} & \num{169.311} & \num{169.313} & \textcolor{red}{\num{169.259}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{76.488} & \num{78.106} & \num{76.499} & \num{76.501} & \textcolor{red}{\num{76.462}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{145.773} & \num{146.061} & \num{145.803} & \num{145.792} & \textcolor{red}{\num{145.078}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{172.659}} & \num{175.452} & \num{172.675} & \num{172.676} & \num{172.662}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{90.906} & \num{91.674} & \num{90.91} & \num{90.909} & \textcolor{red}{\num{90.821}}\\*
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
    MSE & \num{0.00558} & \num{0.00543} & \num{0.00542} & \num{0.00537} & \textcolor{red}{\num{0.00537}}\\
    MAE & \num{0.05} & \num{0.0494} & \textcolor{red}{\num{0.0488}} & \num{0.0489} & \num{0.0489}\\
    MAPE & \num{209.741} & \num{211.902} & \num{195.441} & \num{176.427} & \textcolor{red}{\num{173.794}}\\
    MASE & \num{110.923} & \num{109.28} & \textcolor{red}{\num{108.156}} & \num{108.385} & \num{108.366}\\
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
    MSE & \num{0.0212} & \num{0.0199} & \num{0.0199} & \num{0.0192} & \textcolor{red}{\num{0.0191}}\\
    MAE & \num{0.0981} & \num{0.0955} & \num{0.0945} & \num{0.0931} & \textcolor{red}{\num{0.093}}\\
    MAPE & \textcolor{red}{\num{239.889}} & \num{322.987} & \num{248.849} & \num{246.943} & \num{248.793}\\
    MASE & \num{223.791} & \num{216.365} & \num{215.405} & \num{212.37} & \textcolor{red}{\num{211.996}}\\
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
    MSE & \num{0.0321} & \num{0.0327} & \num{0.0307} & \num{0.0299} & \textcolor{red}{\num{0.0299}}\\
    MAE & \num{0.12} & \num{0.119} & \num{0.118} & \textcolor{red}{\num{0.117}} & \num{0.117}\\
    MAPE & \num{291.44} & \textcolor{red}{\num{186.083}} & \num{250.462} & \num{229.814} & \num{228.661}\\
    MASE & \num{266.758} & \num{265.705} & \num{261.053} & \textcolor{red}{\num{258.72}} & \num{258.791}\\
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
    MSE & \num{0.00157} & \num{0.00161} & \textcolor{red}{\num{0.00153}} & \num{0.00161} & \num{0.00161}\\
    MAE & \num{0.0271} & \num{0.0276} & \textcolor{red}{\num{0.0268}} & \num{0.0275} & \num{0.0275}\\
    MAPE & \num{320.058} & \num{214.284} & \num{256.12} & \textcolor{red}{\num{189.582}} & \num{190.305}\\
    MASE & \num{90.544} & \num{92.268} & \textcolor{red}{\num{89.133}} & \num{91.195} & \num{91.204}\\
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
    MSE & \num{0.00176} & \num{0.0018} & \textcolor{red}{\num{0.00175}} & \num{0.00176} & \num{0.00176}\\
    MAE & \num{0.0284} & \num{0.0286} & \textcolor{red}{\num{0.0282}} & \num{0.0282} & \num{0.0282}\\
    MAPE & \num{130.838} & \num{153.712} & \textcolor{red}{\num{103.388}} & \num{107.207} & \num{107.276}\\
    MASE & \num{94.091} & \num{95.283} & \textcolor{red}{\num{93.633}} & \num{93.653} & \num{93.666}\\
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
    MSE & \num{0.00171} & \num{0.00172} & \textcolor{red}{\num{0.00171}} & \num{0.00171} & \num{0.00171}\\
    MAE & \num{0.0277} & \num{0.0277} & \num{0.0277} & \textcolor{red}{\num{0.0277}} & \num{0.0277}\\
    MAPE & \num{100} & \num{125.293} & \textcolor{red}{\num{100}} & \num{100.037} & \num{100.122}\\
    MASE & \num{90.7} & \textcolor{red}{\num{90.529}} & \num{90.698} & \num{90.694} & \num{90.707}\\
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
    MSE & \num{0.00165} & \num{0.00158} & \num{0.00154} & \num{0.00153} & \textcolor{red}{\num{0.00153}}\\
    MAE & \num{0.0285} & \num{0.0283} & \num{0.0277} & \num{0.0277} & \textcolor{red}{\num{0.0276}}\\
    MAPE & \num{233.094} & \num{191.757} & \num{123.339} & \textcolor{red}{\num{116.968}} & \num{119.494}\\
    MASE & \num{98.412} & \num{96.137} & \num{94.954} & \num{94.542} & \textcolor{red}{\num{94.248}}\\
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
    MSE & \num{0.00151} & \num{0.0015} & \num{0.00148} & \num{0.00148} & \textcolor{red}{\num{0.00148}}\\
    MAE & \num{0.0274} & \num{0.0275} & \num{0.0272} & \num{0.0272} & \textcolor{red}{\num{0.0272}}\\
    MAPE & \num{135.742} & \num{155.353} & \num{100.511} & \textcolor{red}{\num{100.038}} & \num{101.876}\\
    MASE & \num{93.186} & \num{93.436} & \num{92.742} & \num{92.738} & \textcolor{red}{\num{92.54}}\\
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
    MSE & \num{0.00144} & \num{0.00147} & \num{0.00144} & \num{0.00144} & \textcolor{red}{\num{0.00144}}\\
    MAE & \num{0.027} & \num{0.0272} & \num{0.027} & \num{0.027} & \textcolor{red}{\num{0.027}}\\
    MAPE & \num{99.996} & \num{124.877} & \num{100} & \textcolor{red}{\num{99.991}} & \num{101.701}\\
    MASE & \num{90.906} & \num{91.674} & \num{90.91} & \num{90.909} & \textcolor{red}{\num{90.821}}\\
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
