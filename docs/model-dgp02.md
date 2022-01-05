Simulating Minnesota VAR
================
Young Geun Kim
05 Jan, 2022

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
-   [Errors](#errors)
    -   [Hyperparameters](#hyperparameters)
    -   [SMALL](#small-1)
        -   [Plots](#plots)
        -   [Tables](#tables)
    -   [MEDIUM](#medium-1)
        -   [Plots](#plots-1)
        -   [Tables](#tables-1)
    -   [LARGE](#large-1)
        -   [Plots](#plots-2)
        -   [Tables](#tables-2)
    -   [Average](#average)
        -   [SMALL](#small-2)
        -   [MEDIUM](#medium-2)
        -   [LARGE](#large-2)
        -   [RMSFE or RMAFE](#rmsfe-or-rmafe)
-   [Coefficients](#coefficients)

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
dgp <- readRDS("../data/processed/bvarsim_dgp_rw.rds")
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
    1e-4, # lambda
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
#> [1]  0.0829  0.1078  0.5340
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'delta':
#> [1]  0.544  0.130  0.544
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
(bvar_medium_optim <- choose_bvar(
  bvar_medium_spec, 
  lower = c(
    rep(1e-2, n_medium), # sigma
    1e-4, # lambda
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
#> [1]  0.0223  0.1085  0.2431  0.2549  0.0173  0.3091  0.0315  0.6988  0.3833
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'delta':
#> [1]  0.293  0.284  0.010  0.234  0.509  0.482  0.123  0.166  0.359
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
(bvar_large_optim <- choose_bvar(
  bvar_large_spec, 
  lower = c(
    rep(1e-2, n_large), # sigma
    1e-4, # lambda
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
#>  [1]  0.1623  0.1561  0.1821  0.2344  0.2281  0.1886  0.3839  0.2231  0.1646
#> [10]  0.1250  0.3542  0.0872
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'delta':
#>  [1]  0.123  0.231  0.147  0.264  0.211  0.205  0.010  0.346  0.119  0.514
#> [11]  0.459  0.480
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
    1e-4, # lambda
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
#> [1]  0.0829  0.1082  0.5360
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'delta':
#> [1]  0.545  0.129  0.545
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
(bvhar_var_medium_optim <- choose_bvhar(
  bvhar_var_medium_spec, 
  lower = c(
    rep(1e-2, n_medium), # sigma
    1e-4, # lambda
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
#> [1]  0.0222  0.1083  0.2430  0.2547  0.0174  0.3097  0.0315  0.6947  0.3826
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'delta':
#> [1]  0.282  0.289  0.010  0.235  0.512  0.480  0.122  0.165  0.356
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
(bvhar_var_large_optim <- choose_bvhar(
  bvhar_var_large_spec, 
  lower = c(
    rep(1e-2, n_large), # sigma
    1e-4, # lambda
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
#>  [1]  0.1624  0.1564  0.1821  0.2351  0.2271  0.1882  0.3820  0.2208  0.1645
#> [10]  0.1241  0.3541  0.0877
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'delta':
#>  [1]  0.127  0.232  0.148  0.265  0.217  0.202  0.010  0.350  0.123  0.511
#> [11]  0.462  0.482
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
    1e-4, # lambda
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
#> [1]  0.0829  0.1081  0.5359
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.534  0.127  0.543
#> 
#> Setting for 'weekly':
#> [1]  0.0386  0.0100  0.0100
#> 
#> Setting for 'monthly':
#> [1]  0.0686  0.0100  0.0100
```

``` r
(bvhar_vhar_medium_optim <- choose_bvhar(
  bvhar_vhar_medium_spec, 
  lower = c(
    rep(1e-2, n_medium), # sigma
    1e-4, # lambda
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
#> [1]  0.0221  0.1110  0.2390  0.2565  0.0174  0.3094  0.0316  0.6959  0.3783
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.262  0.277  0.010  0.235  0.508  0.479  0.121  0.161  0.351
#> 
#> Setting for 'weekly':
#> [1]  0.1258  0.0710  0.0100  0.0100  0.0165  0.0100  0.0100  0.0342  0.0100
#> 
#> Setting for 'monthly':
#> [1]  0.0701  0.0221  0.0849  0.0100  0.0856  0.0100  0.0178  0.0100  0.1712
```

``` r
(bvhar_vhar_large_optim <- choose_bvhar(
  bvhar_vhar_large_spec, 
  lower = c(
    rep(1e-2, n_large), # sigma
    1e-4, # lambda
    rep(1e-2, n_large), # daily
    rep(1e-2, n_large), # weekly
    rep(1e-2, n_large) # monthly
  ), 
  upper = c(
    rep(1, n_large), # sigma
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
#>  [1]  0.1625  0.1562  0.1820  0.2350  0.2275  0.1883  0.3815  0.2208  0.1644
#> [10]  0.1238  0.3536  0.0875
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#>  [1]  0.123  0.228  0.131  0.250  0.215  0.200  0.010  0.347  0.119  0.480
#> [11]  0.458  0.482
#> 
#> Setting for 'weekly':
#>  [1]  0.0100  0.0169  0.1354  0.0952  0.0100  0.0100  0.0100  0.0100  0.0100
#> [10]  0.1369  0.0100  0.0100
#> 
#> Setting for 'monthly':
#>  [1]  0.1303  0.0100  0.0100  0.0784  0.0100  0.0100  0.0100  0.0754  0.1492
#> [10]  0.0553  0.0981  0.0100
```

``` r
fit_bvhar_small_vhar <- bvhar_vhar_small_optim$fit
fit_bvhar_medium_vhar <- bvhar_vhar_medium_optim$fit
fit_bvhar_large_vhar <- bvhar_vhar_large_optim$fit
```

``` r
parallel::stopCluster(cl)
```

# Errors

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
    \hspace{1em} & BVAR & $\sigma$ & 0.083 & 0.108 & 0.534 &  &  &  &  &  &  &  &  & \\

    \hspace{1em}\hspace{1em}\hspace{1em}\hspace{1em}\hspace{1em}\hspace{1em}\hspace{1em}\hspace{1em}\hspace{1em} &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.544 & 0.130 & 0.544 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.083 & 0.108 & 0.536 &  &  &  &  &  &  &  &  & \\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.545 & 0.129 & 0.545 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.083 & 0.108 & 0.536 &  &  &  &  &  &  &  &  & \\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.534 & 0.127 & 0.543 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.039 & 0.010 & 0.010 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.069 & 0.010 & 0.010 &  &  &  &  &  &  &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{MEDIUM}}\\
    \hspace{1em} & BVAR & $\sigma$ & 0.022 & 0.109 & 0.243 & 0.255 & 0.017 & 0.309 & 0.031 & 0.699 & 0.383 &  &  & \\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.293 & 0.284 & 0.010 & 0.234 & 0.509 & 0.482 & 0.123 & 0.166 & 0.359 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.022 & 0.108 & 0.243 & 0.255 & 0.017 & 0.310 & 0.031 & 0.695 & 0.383 &  &  & \\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.282 & 0.289 & 0.010 & 0.235 & 0.512 & 0.480 & 0.122 & 0.165 & 0.356 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.022 & 0.111 & 0.239 & 0.257 & 0.017 & 0.309 & 0.032 & 0.696 & 0.378 &  &  & \\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.262 & 0.277 & 0.010 & 0.235 & 0.508 & 0.479 & 0.121 & 0.161 & 0.351 &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.126 & 0.071 & 0.010 & 0.010 & 0.016 & 0.010 & 0.010 & 0.034 & 0.010 &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.070 & 0.022 & 0.085 & 0.010 & 0.086 & 0.010 & 0.018 & 0.010 & 0.171 &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{LARGE}}\\
    \hspace{1em} & BVAR & $\sigma$ & 0.162 & 0.156 & 0.182 & 0.234 & 0.228 & 0.189 & 0.384 & 0.223 & 0.165 & 0.125 & 0.354 & 0.087\\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.123 & 0.231 & 0.147 & 0.264 & 0.211 & 0.205 & 0.010 & 0.346 & 0.119 & 0.514 & 0.459 & 0.480\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.162 & 0.156 & 0.182 & 0.235 & 0.227 & 0.188 & 0.382 & 0.221 & 0.165 & 0.124 & 0.354 & 0.088\\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.127 & 0.232 & 0.148 & 0.265 & 0.217 & 0.202 & 0.010 & 0.350 & 0.123 & 0.511 & 0.462 & 0.482\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.162 & 0.156 & 0.182 & 0.235 & 0.228 & 0.188 & 0.381 & 0.221 & 0.164 & 0.124 & 0.354 & 0.088\\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.123 & 0.228 & 0.131 & 0.250 & 0.215 & 0.200 & 0.010 & 0.347 & 0.119 & 0.480 & 0.458 & 0.482\\

    \hspace{1em} &  & $w_i$ & 0.010 & 0.017 & 0.135 & 0.095 & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.137 & 0.010 & 0.010\\

    \hspace{1em} &  & $m_i$ & 0.130 & 0.010 & 0.010 & 0.078 & 0.010 & 0.010 & 0.010 & 0.075 & 0.149 & 0.055 & 0.098 & 0.010\\*
    \end{longtable}

## SMALL

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

### Plots

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
     & asset01 & \num{0.00286} & \num{0.00287} & \num{0.00277} & \textcolor{red}{\num{0.00277}} & \num{0.00279}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00489} & \num{0.00488} & \textcolor{red}{\num{0.00479}} & \num{0.00479} & \num{0.0048}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.18}} & \num{0.183} & \num{0.18} & \num{0.18} & \num{0.18}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{0.0626}} & \num{0.0636} & \num{0.0626} & \num{0.0626} & \num{0.0627}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0436} & \num{0.0437} & \num{0.0433} & \textcolor{red}{\num{0.0433}} & \num{0.0435}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0558} & \num{0.0566} & \num{0.0557} & \textcolor{red}{\num{0.0557}} & \num{0.0557}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.349} & \num{0.349} & \textcolor{red}{\num{0.346}} & \num{0.346} & \num{0.346}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.15} & \num{0.15} & \textcolor{red}{\num{0.148}} & \num{0.148} & \num{0.148}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{155.532}} & \num{169.354} & \num{165.334} & \num{165.475} & \num{164.631}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{178.96} & \num{218.598} & \num{172.756} & \num{172.251} & \textcolor{red}{\num{172.113}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{204.036} & \textcolor{red}{\num{165.643}} & \num{181.515} & \num{181.661} & \num{181.878}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{179.509} & \num{184.532} & \num{173.202} & \num{173.129} & \textcolor{red}{\num{172.874}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{48.822}} & \num{49.815} & \num{49.025} & \num{49.017} & \num{49.26}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{57.625} & \num{58.418} & \num{56.871} & \textcolor{red}{\num{56.866}} & \num{56.883}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{372.978} & \num{367.795} & \textcolor{red}{\num{366.49}} & \num{366.505} & \num{366.706}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{159.808} & \num{158.676} & \textcolor{red}{\num{157.462}} & \num{157.463} & \num{157.616}\\*
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
     & asset01 & \textcolor{red}{\num{0.0044}} & \num{0.00476} & \num{0.00442} & \num{0.00442} & \num{0.00445}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00525} & \num{0.00525} & \num{0.0051} & \num{0.0051} & \textcolor{red}{\num{0.0051}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.256} & \num{0.267} & \num{0.255} & \num{0.255} & \textcolor{red}{\num{0.255}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0885} & \num{0.0924} & \num{0.0882} & \num{0.0882} & \textcolor{red}{\num{0.0882}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0524} & \num{0.0541} & \textcolor{red}{\num{0.0521}} & \num{0.0521} & \num{0.0524}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0575} & \num{0.0576} & \num{0.0567} & \num{0.0567} & \textcolor{red}{\num{0.0567}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.4}} & \num{0.412} & \num{0.402} & \num{0.402} & \num{0.402}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{0.17}} & \num{0.174} & \num{0.17} & \num{0.17} & \num{0.171}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{129.681} & \num{133.605} & \textcolor{red}{\num{101.82}} & \num{101.863} & \num{107.97}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{125.709} & \num{119.401} & \num{99.997} & \num{99.997} & \textcolor{red}{\num{99.73}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{127.508} & \num{109.238} & \textcolor{red}{\num{98.876}} & \num{98.894} & \num{100.343}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{127.633} & \num{120.748} & \textcolor{red}{\num{100.231}} & \num{100.251} & \num{102.681}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{55.246} & \num{57.712} & \textcolor{red}{\num{55.097}} & \num{55.101} & \num{55.678}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{63.878} & \num{63.108} & \textcolor{red}{\num{63.068}} & \num{63.068} & \num{63.069}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{445.662} & \num{452.709} & \textcolor{red}{\num{445.168}} & \num{445.193} & \num{445.492}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{188.262} & \num{191.176} & \textcolor{red}{\num{187.777}} & \num{187.787} & \num{188.079}\\*
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
     & asset01 & \num{0.00467} & \num{0.00474} & \textcolor{red}{\num{0.00466}} & \num{0.00466} & \num{0.00469}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.00484}} & \num{0.00489} & \num{0.00484} & \num{0.00484} & \num{0.00484}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.245} & \num{0.249} & \textcolor{red}{\num{0.244}} & \num{0.244} & \num{0.245}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0847} & \num{0.0862} & \textcolor{red}{\num{0.0847}} & \num{0.0847} & \num{0.0847}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0531} & \num{0.0536} & \textcolor{red}{\num{0.0531}} & \num{0.0531} & \num{0.0532}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.0551}} & \num{0.0558} & \num{0.0551} & \num{0.0551} & \num{0.0551}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.388} & \num{0.394} & \textcolor{red}{\num{0.388}} & \num{0.388} & \num{0.389}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.166} & \num{0.168} & \textcolor{red}{\num{0.166}} & \num{0.166} & \num{0.166}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{100.193} & \num{117.779} & \textcolor{red}{\num{100.001}} & \num{100.001} & \num{101.495}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{100.004} & \num{113.909} & \textcolor{red}{\num{100}} & \num{100} & \num{100.045}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{99.903}} & \num{117.835} & \num{99.999} & \num{99.999} & \num{100.212}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{100.033} & \num{116.508} & \num{100} & \textcolor{red}{\num{100}} & \num{100.584}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{55.724} & \num{56.256} & \textcolor{red}{\num{55.713}} & \num{55.713} & \num{55.854}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{61.979} & \num{62.245} & \textcolor{red}{\num{61.978}} & \num{61.978} & \num{61.981}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{429.591} & \num{435.901} & \textcolor{red}{\num{429.474}} & \num{429.474} & \num{429.689}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{182.431} & \num{184.8} & \textcolor{red}{\num{182.388}} & \num{182.388} & \num{182.508}\\*
    \end{longtable}

## MEDIUM

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

### Plots

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

### Tables

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
     & asset01 & \num{0.000132} & \num{0.000133} & \textcolor{red}{\num{0.000122}} & \num{0.000123} & \num{0.000123}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00152} & \num{0.00155} & \num{0.00143} & \textcolor{red}{\num{0.00143}} & \num{0.00143}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00877} & \num{0.00874} & \textcolor{red}{\num{0.00824}} & \num{0.00824} & \num{0.00824}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0133} & \num{0.0139} & \num{0.0131} & \num{0.0131} & \textcolor{red}{\num{0.0131}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{7.58e-05} & \num{7.72e-05} & \num{7.38e-05} & \num{7.38e-05} & \textcolor{red}{\num{7.35e-05}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0258} & \textcolor{red}{\num{0.0244}} & \num{0.0245} & \num{0.0245} & \num{0.0245}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000134} & \num{0.000131} & \textcolor{red}{\num{0.000131}} & \num{0.000131} & \num{0.000131}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.144} & \num{0.141} & \num{0.135} & \num{0.135} & \textcolor{red}{\num{0.135}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0681} & \num{0.0668} & \textcolor{red}{\num{0.0661}} & \num{0.0661} & \num{0.0661}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0291} & \num{0.0286} & \num{0.0276} & \num{0.0276} & \textcolor{red}{\num{0.0276}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.00917} & \num{0.00915} & \textcolor{red}{\num{0.00875}} & \num{0.00879} & \num{0.00885}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0326} & \num{0.0325} & \num{0.0308} & \textcolor{red}{\num{0.0308}} & \num{0.0308}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.077} & \num{0.0756} & \num{0.0734} & \num{0.0734} & \textcolor{red}{\num{0.0734}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0902} & \num{0.092} & \num{0.0899} & \num{0.0899} & \textcolor{red}{\num{0.0899}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00685} & \num{0.00691} & \textcolor{red}{\num{0.00665}} & \num{0.00665} & \num{0.00665}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.13} & \textcolor{red}{\num{0.127}} & \num{0.127} & \num{0.127} & \num{0.127}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.00888} & \num{0.00859} & \textcolor{red}{\num{0.00859}} & \num{0.00859} & \num{0.00859}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.313} & \num{0.313} & \textcolor{red}{\num{0.299}} & \num{0.299} & \num{0.299}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.218} & \textcolor{red}{\num{0.213}} & \num{0.214} & \num{0.214} & \num{0.214}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0985} & \num{0.0975} & \textcolor{red}{\num{0.0953}} & \num{0.0953} & \num{0.0953}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{132.135} & \num{123.14} & \num{103.51} & \textcolor{red}{\num{102.86}} & \num{108.254}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{157.874} & \num{150.796} & \textcolor{red}{\num{108.147}} & \num{108.694} & \num{109.962}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{138.942} & \num{120.47} & \num{99.259} & \num{99.259} & \textcolor{red}{\num{99.092}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{139.823} & \num{130.306} & \textcolor{red}{\num{124.092}} & \num{124.338} & \num{124.371}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{406.529} & \num{446.743} & \textcolor{red}{\num{375.593}} & \num{377.515} & \num{381.342}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{440.843}} & \num{466.037} & \num{485.22} & \num{483.833} & \num{484.869}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{130.038} & \num{131.958} & \num{107.875} & \textcolor{red}{\num{107.731}} & \num{107.888}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{224.71} & \num{235.485} & \num{150.447} & \textcolor{red}{\num{149.877}} & \num{151.888}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{228.095} & \num{269.648} & \num{177.207} & \textcolor{red}{\num{176.298}} & \num{179.157}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{222.11} & \num{230.509} & \num{192.372} & \textcolor{red}{\num{192.267}} & \num{194.091}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{38.878} & \num{38.542} & \textcolor{red}{\num{36.823}} & \num{36.937} & \num{37.056}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{127.725} & \num{122.448} & \num{112.565} & \textcolor{red}{\num{112.518}} & \num{112.567}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{281.985} & \num{267.436} & \textcolor{red}{\num{260.014}} & \num{260.014} & \num{260.076}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{306.201}} & \num{319.113} & \num{313.489} & \num{313.425} & \num{313.136}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{28.443}} & \num{28.595} & \num{28.715} & \num{28.735} & \num{28.633}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{555.429} & \num{532.846} & \textcolor{red}{\num{525.1}} & \num{525.24} & \num{525.192}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{34.417} & \textcolor{red}{\num{33.726}} & \num{34.353} & \num{34.372} & \num{34.397}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1083.591} & \num{1083.517} & \num{1026.146} & \num{1025.802} & \textcolor{red}{\num{1025.455}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{928.299} & \num{896.394} & \textcolor{red}{\num{894.404}} & \num{894.809} & \num{896.386}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{376.107} & \num{369.18} & \textcolor{red}{\num{359.068}} & \num{359.095} & \num{359.211}\\*
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
     & asset01 & \num{0.000149} & \num{0.000158} & \num{0.000146} & \textcolor{red}{\num{0.000146}} & \num{0.000147}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00172} & \num{0.00187} & \textcolor{red}{\num{0.00166}} & \num{0.00166} & \num{0.00167}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00838} & \num{0.00853} & \textcolor{red}{\num{0.00838}} & \num{0.00838} & \num{0.00839}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0142} & \num{0.0148} & \num{0.014} & \num{0.014} & \textcolor{red}{\num{0.0139}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{9.98e-05} & \num{0.000104} & \num{9.44e-05} & \num{9.44e-05} & \textcolor{red}{\num{9.38e-05}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0368} & \textcolor{red}{\num{0.0329}} & \num{0.0364} & \num{0.0364} & \num{0.0363}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{0.000133}} & \num{0.000135} & \num{0.000134} & \num{0.000134} & \num{0.000134}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.141} & \num{0.143} & \num{0.138} & \num{0.138} & \textcolor{red}{\num{0.138}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.078}} & \num{0.0791} & \num{0.0786} & \num{0.0787} & \num{0.0786}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0312} & \num{0.0312} & \num{0.0309} & \num{0.0309} & \textcolor{red}{\num{0.0308}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0101} & \num{0.0103} & \num{0.00986} & \textcolor{red}{\num{0.00986}} & \num{0.00995}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0341} & \num{0.0355} & \textcolor{red}{\num{0.0335}} & \num{0.0335} & \num{0.0336}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0743} & \num{0.0743} & \textcolor{red}{\num{0.0739}} & \num{0.0739} & \num{0.074}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0924} & \num{0.0936} & \num{0.0919} & \num{0.0919} & \textcolor{red}{\num{0.0918}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00807} & \num{0.00817} & \textcolor{red}{\num{0.00772}} & \num{0.00772} & \num{0.00772}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.154} & \textcolor{red}{\num{0.148}} & \num{0.152} & \num{0.152} & \num{0.152}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{0.00862}} & \num{0.00884} & \num{0.0087} & \num{0.0087} & \num{0.0087}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.307} & \num{0.312} & \num{0.301} & \num{0.301} & \textcolor{red}{\num{0.3}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.227}} & \num{0.23} & \num{0.229} & \num{0.229} & \num{0.229}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.102} & \num{0.102} & \textcolor{red}{\num{0.101}} & \num{0.101} & \num{0.101}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{118.226} & \num{113.926} & \textcolor{red}{\num{99.968}} & \num{99.974} & \num{101.433}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{121.351} & \num{138.365} & \textcolor{red}{\num{100.054}} & \num{100.059} & \num{103.443}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{107.743} & \num{104.233} & \num{100} & \textcolor{red}{\num{100}} & \num{100.206}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{101.685} & \num{114.443} & \textcolor{red}{\num{100.037}} & \num{100.038} & \num{100.272}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{167.516} & \num{218.566} & \textcolor{red}{\num{119.628}} & \num{120.356} & \num{123.421}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{145.48} & \num{217.883} & \num{122.902} & \textcolor{red}{\num{122.446}} & \num{128.549}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{110.704} & \num{127.226} & \num{100.001} & \textcolor{red}{\num{100.001}} & \num{100.259}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{130.374} & \num{162.794} & \num{99.993} & \num{99.994} & \textcolor{red}{\num{99.892}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{105.752} & \num{228.92} & \num{100.47} & \textcolor{red}{\num{100.448}} & \num{100.879}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{123.203} & \num{158.484} & \textcolor{red}{\num{104.784}} & \num{104.813} & \num{106.484}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{37.445} & \num{38.178} & \num{36.679} & \textcolor{red}{\num{36.669}} & \num{37.261}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{158.117} & \num{164.812} & \textcolor{red}{\num{157.933}} & \num{157.935} & \num{158.191}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{282.273} & \num{281.986} & \textcolor{red}{\num{279.63}} & \num{279.63} & \num{279.894}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{422.425} & \num{437.071} & \num{420.922} & \num{420.921} & \textcolor{red}{\num{420.785}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{30.377} & \num{30.996} & \textcolor{red}{\num{28}} & \num{28.006} & \num{28.033}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{604.804} & \textcolor{red}{\num{582.926}} & \num{600.316} & \num{600.361} & \num{599.782}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{36.6} & \num{36.893} & \num{36.159} & \num{36.159} & \textcolor{red}{\num{36.142}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1101.838} & \num{1110.755} & \num{1065.658} & \num{1065.659} & \textcolor{red}{\num{1064.777}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{984.341} & \textcolor{red}{\num{971.694}} & \num{985.779} & \num{985.822} & \num{982.62}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{406.469} & \num{406.146} & \num{401.231} & \num{401.24} & \textcolor{red}{\num{400.832}}\\*
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
     & asset01 & \num{0.000155} & \textcolor{red}{\num{0.000155}} & \num{0.000155} & \num{0.000155} & \num{0.000155}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0017} & \textcolor{red}{\num{0.00169}} & \num{0.0017} & \num{0.0017} & \num{0.0017}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00734} & \textcolor{red}{\num{0.00725}} & \num{0.00734} & \num{0.00734} & \num{0.00733}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0148} & \textcolor{red}{\num{0.0145}} & \num{0.0148} & \num{0.0148} & \num{0.0148}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{9.45e-05} & \textcolor{red}{\num{8.7e-05}} & \num{9.45e-05} & \num{9.45e-05} & \num{9.39e-05}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0354} & \textcolor{red}{\num{0.0334}} & \num{0.0354} & \num{0.0354} & \num{0.0354}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000122} & \textcolor{red}{\num{0.000121}} & \num{0.000122} & \num{0.000122} & \num{0.000122}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.135} & \textcolor{red}{\num{0.134}} & \num{0.135} & \num{0.135} & \num{0.135}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0763} & \num{0.078} & \num{0.0763} & \textcolor{red}{\num{0.0763}} & \num{0.0766}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0301} & \textcolor{red}{\num{0.0299}} & \num{0.0301} & \num{0.0301} & \num{0.0301}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0101} & \num{0.0103} & \num{0.0101} & \num{0.0101} & \textcolor{red}{\num{0.0101}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0342} & \textcolor{red}{\num{0.0337}} & \num{0.0342} & \num{0.0342} & \num{0.0342}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.069} & \textcolor{red}{\num{0.0685}} & \num{0.069} & \num{0.069} & \num{0.069}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0946} & \textcolor{red}{\num{0.0928}} & \num{0.0946} & \num{0.0946} & \num{0.0946}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00762} & \textcolor{red}{\num{0.00732}} & \num{0.00762} & \num{0.00762} & \num{0.00761}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.149} & \textcolor{red}{\num{0.144}} & \num{0.149} & \num{0.149} & \num{0.149}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0081} & \textcolor{red}{\num{0.00805}} & \num{0.0081} & \num{0.0081} & \num{0.0081}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.292} & \num{0.293} & \num{0.292} & \num{0.292} & \textcolor{red}{\num{0.292}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.223} & \num{0.225} & \num{0.223} & \textcolor{red}{\num{0.223}} & \num{0.223}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0986} & \textcolor{red}{\num{0.0981}} & \num{0.0986} & \num{0.0986} & \num{0.0986}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{100.066} & \num{118.134} & \num{100} & \num{100} & \textcolor{red}{\num{99.411}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{100.012} & \num{100.425} & \num{100} & \num{100} & \textcolor{red}{\num{99.987}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{100.002} & \num{102.425} & \num{100} & \num{100} & \textcolor{red}{\num{99.714}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{99.945} & \textcolor{red}{\num{98.439}} & \num{100} & \num{100} & \num{100.045}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{100.265} & \num{185.241} & \num{99.999} & \textcolor{red}{\num{99.999}} & \num{100.445}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{100.306} & \num{197.611} & \textcolor{red}{\num{100}} & \num{100} & \num{100.073}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{100.005} & \num{104.726} & \num{100} & \textcolor{red}{\num{100}} & \num{100.129}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{99.989}} & \num{123.751} & \num{100} & \num{100} & \num{100.086}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{99.98}} & \num{126.461} & \num{100} & \num{100} & \num{100.998}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{100.063} & \num{128.579} & \num{100} & \textcolor{red}{\num{100}} & \num{100.099}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{40.946} & \num{41.528} & \num{40.945} & \num{40.945} & \textcolor{red}{\num{40.945}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{136.048} & \textcolor{red}{\num{134.951}} & \num{136.036} & \num{136.036} & \num{136.048}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{268.711} & \num{269.271} & \num{268.709} & \num{268.709} & \textcolor{red}{\num{268.645}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{410.346} & \textcolor{red}{\num{401.022}} & \num{410.351} & \num{410.351} & \num{410.38}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{33.783} & \textcolor{red}{\num{32.269}} & \num{33.782} & \num{33.782} & \num{33.743}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{611.602} & \textcolor{red}{\num{599.692}} & \num{611.525} & \num{611.525} & \num{611.556}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{31.723}} & \num{32.383} & \num{31.726} & \num{31.726} & \num{31.732}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1319.543} & \num{1337.58} & \num{1319.529} & \num{1319.529} & \textcolor{red}{\num{1319.522}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{923.619} & \textcolor{red}{\num{921.71}} & \num{923.593} & \num{923.593} & \num{927.159}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{419.591} & \textcolor{red}{\num{418.934}} & \num{419.577} & \num{419.577} & \num{419.97}\\*
    \end{longtable}

## LARGE

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

### Plots

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

### Tables

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
     & asset01 & \num{0.0054} & \num{0.00541} & \textcolor{red}{\num{0.00512}} & \num{0.00512} & \num{0.00512}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00617} & \num{0.0059} & \textcolor{red}{\num{0.0054}} & \num{0.0054} & \num{0.0054}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00424} & \num{0.00446} & \num{0.00417} & \num{0.00417} & \textcolor{red}{\num{0.00416}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.0078}} & \num{0.00801} & \num{0.0081} & \num{0.0081} & \num{0.00805}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00937} & \num{0.00881} & \textcolor{red}{\num{0.00858}} & \num{0.00859} & \num{0.00858}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0105} & \num{0.0104} & \num{0.0102} & \num{0.0102} & \textcolor{red}{\num{0.0102}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.024} & \num{0.0251} & \num{0.0224} & \num{0.0224} & \textcolor{red}{\num{0.0224}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0339} & \num{0.033} & \num{0.0312} & \num{0.0312} & \textcolor{red}{\num{0.0311}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0128} & \num{0.013} & \num{0.0126} & \num{0.0126} & \textcolor{red}{\num{0.0125}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.0025} & \num{0.00247} & \num{0.00228} & \textcolor{red}{\num{0.00228}} & \num{0.00229}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.1078} & \num{0.1021} & \num{0.0988} & \num{0.0988} & \textcolor{red}{\num{0.0983}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.00426} & \textcolor{red}{\num{0.00395}} & \num{0.00405} & \num{0.00405} & \num{0.00404}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0191} & \num{0.0185} & \num{0.0177} & \num{0.0177} & \textcolor{red}{\num{0.0177}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0588} & \num{0.0581} & \textcolor{red}{\num{0.0571}} & \num{0.0571} & \num{0.0572}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0624} & \num{0.0609} & \textcolor{red}{\num{0.0592}} & \num{0.0592} & \num{0.0592}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0518} & \num{0.0533} & \num{0.0503} & \textcolor{red}{\num{0.0503}} & \num{0.0505}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.0719}} & \num{0.0724} & \num{0.0725} & \num{0.0725} & \num{0.0722}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0753} & \num{0.0755} & \textcolor{red}{\num{0.0739}} & \num{0.0739} & \num{0.0739}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0825} & \num{0.0811} & \num{0.0804} & \num{0.0803} & \textcolor{red}{\num{0.0803}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.128} & \num{0.132} & \num{0.124} & \num{0.124} & \textcolor{red}{\num{0.124}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.147} & \num{0.145} & \num{0.139} & \num{0.139} & \textcolor{red}{\num{0.139}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0894} & \num{0.0894} & \num{0.0883} & \num{0.0882} & \textcolor{red}{\num{0.088}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.0396} & \num{0.0399} & \textcolor{red}{\num{0.0382}} & \num{0.0382} & \num{0.0383}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.258} & \num{0.248} & \num{0.247} & \num{0.246} & \textcolor{red}{\num{0.245}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.0535} & \textcolor{red}{\num{0.0513}} & \num{0.0516} & \num{0.0516} & \num{0.0516}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0931} & \num{0.0922} & \num{0.0901} & \num{0.0901} & \textcolor{red}{\num{0.0899}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{285.491}} & \num{465.404} & \num{307.628} & \num{313.962} & \num{305.665}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{225.301} & \num{195.379} & \num{181.259} & \num{181.657} & \textcolor{red}{\num{180.942}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{191.147} & \num{212.591} & \num{141.191} & \num{141.641} & \textcolor{red}{\num{133.837}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{151.41} & \num{134.444} & \textcolor{red}{\num{130.558}} & \num{130.577} & \num{131.02}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{189.632} & \num{206.211} & \textcolor{red}{\num{157.676}} & \num{159.833} & \num{159.391}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{287.408} & \num{214.1} & \num{170.237} & \textcolor{red}{\num{168.825}} & \num{169.061}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{171.16} & \num{179.307} & \textcolor{red}{\num{98.947}} & \num{98.947} & \num{98.98}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{377.372} & \num{342.892} & \num{243.058} & \num{245.111} & \textcolor{red}{\num{242.799}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{116.87} & \num{167.411} & \num{108.638} & \num{108.986} & \textcolor{red}{\num{104.488}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{186.301} & \textcolor{red}{\num{170.352}} & \num{180.356} & \num{179.738} & \num{175.976}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{227.274} & \num{197.454} & \textcolor{red}{\num{196.814}} & \num{197.535} & \num{198.479}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{201.982}} & \num{233.245} & \num{220.873} & \num{221.643} & \num{221.517}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{217.612} & \num{226.566} & \num{178.103} & \num{179.038} & \textcolor{red}{\num{176.846}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{73.047} & \textcolor{red}{\num{72.679}} & \num{72.787} & \num{72.794} & \num{72.762}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{74.937} & \num{73.278} & \num{71.863} & \num{71.863} & \textcolor{red}{\num{71.852}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{60.525} & \num{61.942} & \num{56.688} & \textcolor{red}{\num{56.668}} & \num{57.043}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{82.873}} & \num{84.106} & \num{84.505} & \num{84.504} & \num{84.102}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{88.35} & \num{88.557} & \textcolor{red}{\num{87.492}} & \num{87.579} & \num{87.506}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{98.62} & \num{96.101} & \num{95.142} & \num{95.056} & \textcolor{red}{\num{95.031}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{153.954} & \num{158.492} & \num{147.174} & \num{147.174} & \textcolor{red}{\num{147.089}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{171.105} & \num{166.41} & \num{159.936} & \num{160.024} & \textcolor{red}{\num{159.632}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{110.02} & \num{109.824} & \num{108.764} & \num{108.708} & \textcolor{red}{\num{108.599}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{50.041} & \num{50.107} & \textcolor{red}{\num{48.503}} & \num{48.513} & \num{48.688}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{319.357} & \num{309.981} & \num{309.8} & \num{309.67} & \textcolor{red}{\num{307.506}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{65.27} & \textcolor{red}{\num{61.788}} & \num{62.607} & \num{62.57} & \num{62.538}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{112.342} & \num{111.105} & \num{108.772} & \num{108.76} & \textcolor{red}{\num{108.529}}\\*
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
     & asset01 & \num{0.00542} & \num{0.00571} & \textcolor{red}{\num{0.00524}} & \num{0.00524} & \num{0.00524}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00578} & \num{0.00601} & \textcolor{red}{\num{0.00564}} & \num{0.00564} & \num{0.00564}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00424} & \num{0.00457} & \num{0.00422} & \num{0.00422} & \textcolor{red}{\num{0.00422}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0093} & \textcolor{red}{\num{0.00871}} & \num{0.00906} & \num{0.00906} & \num{0.00901}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00914} & \textcolor{red}{\num{0.009}} & \num{0.00907} & \num{0.00907} & \num{0.00907}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.01}} & \num{0.0104} & \num{0.0102} & \num{0.0102} & \num{0.0102}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0233} & \num{0.0235} & \textcolor{red}{\num{0.0222}} & \num{0.0222} & \num{0.0222}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.033} & \num{0.0338} & \num{0.033} & \num{0.033} & \textcolor{red}{\num{0.0328}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0135} & \num{0.0134} & \num{0.0134} & \num{0.0134} & \textcolor{red}{\num{0.0134}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.00339} & \num{0.00336} & \num{0.00291} & \textcolor{red}{\num{0.00291}} & \num{0.00296}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{0.114}} & \num{0.119} & \num{0.116} & \num{0.116} & \num{0.115}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.00534} & \textcolor{red}{\num{0.00527}} & \num{0.00568} & \num{0.00569} & \num{0.00569}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0197} & \num{0.0202} & \num{0.0198} & \num{0.0198} & \textcolor{red}{\num{0.0197}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0594} & \num{0.0604} & \num{0.0584} & \num{0.0584} & \textcolor{red}{\num{0.0583}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0609} & \num{0.0626} & \textcolor{red}{\num{0.0605}} & \num{0.0605} & \num{0.0605}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.0503}} & \num{0.0523} & \num{0.0504} & \num{0.0504} & \num{0.0504}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0786} & \textcolor{red}{\num{0.0745}} & \num{0.0763} & \num{0.0763} & \num{0.0758}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0758} & \num{0.0759} & \num{0.0746} & \num{0.0746} & \textcolor{red}{\num{0.0746}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.0774}} & \num{0.0789} & \num{0.0784} & \num{0.0784} & \num{0.0784}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.126} & \num{0.128} & \textcolor{red}{\num{0.123}} & \num{0.123} & \num{0.123}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.145} & \num{0.149} & \num{0.144} & \num{0.144} & \textcolor{red}{\num{0.143}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0928} & \textcolor{red}{\num{0.0905}} & \num{0.0921} & \num{0.0921} & \num{0.092}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.0476} & \num{0.0484} & \num{0.0449} & \textcolor{red}{\num{0.0449}} & \num{0.0452}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{0.27}} & \num{0.275} & \num{0.276} & \num{0.276} & \num{0.275}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{0.0595}} & \num{0.0599} & \num{0.0613} & \num{0.0613} & \num{0.0613}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0952} & \num{0.0963} & \num{0.095} & \num{0.095} & \textcolor{red}{\num{0.0948}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{111.636} & \num{127.695} & \textcolor{red}{\num{100.002}} & \num{100.002} & \num{100.103}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{121.183} & \num{138.474} & \num{99.97} & \num{99.969} & \textcolor{red}{\num{99.645}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{134.257} & \num{188.765} & \num{99.997} & \textcolor{red}{\num{99.997}} & \num{100.944}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{127.877} & \num{115.797} & \num{99.917} & \num{99.916} & \textcolor{red}{\num{98.182}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{126.862} & \num{153.464} & \num{99.958} & \num{99.952} & \textcolor{red}{\num{99.461}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{132.78} & \num{124.021} & \num{100.055} & \textcolor{red}{\num{100.051}} & \num{100.647}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{114.896} & \num{140.914} & \textcolor{red}{\num{100}} & \num{100} & \num{100.144}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{122.179} & \num{154.831} & \num{98.968} & \textcolor{red}{\num{98.905}} & \num{99.111}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{115.045} & \num{134.357} & \textcolor{red}{\num{100.017}} & \num{100.019} & \num{101.611}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{146.634} & \num{190.978} & \num{106.751} & \textcolor{red}{\num{106.508}} & \num{120.524}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{117.201} & \num{105.526} & \textcolor{red}{\num{100.546}} & \num{100.626} & \num{103.078}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{124.013} & \num{169.067} & \textcolor{red}{\num{101.128}} & \num{101.159} & \num{101.324}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{124.547} & \num{145.324} & \num{100.609} & \textcolor{red}{\num{100.592}} & \num{102.065}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{66.995} & \num{67.7} & \num{66.178} & \num{66.178} & \textcolor{red}{\num{66.138}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{71.876} & \num{73.59} & \num{71.476} & \num{71.476} & \textcolor{red}{\num{71.462}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{57.782} & \num{60.102} & \num{57.245} & \num{57.245} & \textcolor{red}{\num{57.216}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{89.445} & \textcolor{red}{\num{82.879}} & \num{86.29} & \num{86.29} & \num{85.912}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{96.698} & \num{96.178} & \num{94.944} & \num{94.942} & \textcolor{red}{\num{94.884}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{90.115}} & \num{92.162} & \num{91.99} & \num{91.99} & \num{91.98}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{147.794} & \num{150.847} & \textcolor{red}{\num{145.73}} & \num{145.73} & \num{145.743}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{172.783} & \num{179.754} & \num{173.191} & \num{173.183} & \textcolor{red}{\num{172.391}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{115.295} & \textcolor{red}{\num{113.352}} & \num{113.654} & \num{113.654} & \num{113.473}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{55.12} & \num{55.305} & \num{51.531} & \textcolor{red}{\num{51.524}} & \num{52.125}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{310.3}} & \num{319.033} & \num{317.915} & \num{317.895} & \num{315.842}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{72.439}} & \num{73.305} & \num{74.103} & \num{74.104} & \num{74.129}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{112.22} & \num{113.684} & \num{112.021} & \num{112.018} & \textcolor{red}{\num{111.775}}\\*
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
     & asset01 & \textcolor{red}{\num{0.00542}} & \num{0.00545} & \num{0.00542} & \num{0.00542} & \num{0.00542}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.00596}} & \num{0.00616} & \num{0.00596} & \num{0.00596} & \num{0.00596}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.00437}} & \num{0.00455} & \num{0.00437} & \num{0.00437} & \num{0.00438}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.00901} & \textcolor{red}{\num{0.00879}} & \num{0.00901} & \num{0.00901} & \num{0.00899}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0083} & \textcolor{red}{\num{0.00815}} & \num{0.0083} & \num{0.0083} & \num{0.0083}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.0108}} & \num{0.011} & \num{0.0108} & \num{0.0108} & \num{0.0108}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0214} & \num{0.0221} & \num{0.0214} & \num{0.0214} & \textcolor{red}{\num{0.0214}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0297} & \num{0.0302} & \num{0.0298} & \num{0.0298} & \textcolor{red}{\num{0.0297}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0135} & \num{0.0136} & \num{0.0135} & \num{0.0135} & \textcolor{red}{\num{0.0135}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.0026}} & \num{0.00279} & \num{0.00262} & \num{0.00262} & \num{0.00261}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{0.11}} & \num{0.113} & \num{0.11} & \num{0.11} & \num{0.11}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{0.0048}} & \num{0.00482} & \num{0.00483} & \num{0.00483} & \num{0.00483}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0188} & \num{0.0192} & \num{0.0189} & \num{0.0189} & \textcolor{red}{\num{0.0188}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{0.0597}} & \num{0.06} & \num{0.0598} & \num{0.0598} & \num{0.0598}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.0624}} & \num{0.0634} & \num{0.0624} & \num{0.0624} & \num{0.0624}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0518} & \num{0.053} & \textcolor{red}{\num{0.0518}} & \num{0.0518} & \num{0.0518}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0768} & \textcolor{red}{\num{0.0751}} & \num{0.0768} & \num{0.0768} & \num{0.0767}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0708} & \textcolor{red}{\num{0.0707}} & \num{0.0708} & \num{0.0708} & \num{0.0708}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.0824}} & \num{0.0832} & \num{0.0824} & \num{0.0824} & \num{0.0824}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.123} & \num{0.124} & \num{0.123} & \num{0.123} & \textcolor{red}{\num{0.123}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.134} & \num{0.137} & \num{0.134} & \num{0.134} & \textcolor{red}{\num{0.134}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0919} & \num{0.0922} & \num{0.0919} & \num{0.0919} & \textcolor{red}{\num{0.0918}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.0428}} & \num{0.0444} & \num{0.0429} & \num{0.0429} & \num{0.0429}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{0.27}} & \num{0.274} & \num{0.271} & \num{0.271} & \num{0.27}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{0.0572}} & \num{0.058} & \num{0.0574} & \num{0.0574} & \num{0.0574}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0936} & \num{0.0946} & \num{0.0937} & \num{0.0937} & \textcolor{red}{\num{0.0936}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{99.991} & \num{121.511} & \num{100} & \num{100} & \textcolor{red}{\num{99.806}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{99.891}} & \num{111.685} & \num{100} & \num{100} & \num{99.956}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{99.769}} & \num{128.085} & \num{100} & \num{100} & \num{100.224}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{99.954} & \num{100.273} & \num{100} & \num{100} & \textcolor{red}{\num{99.456}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{99.976} & \num{126.247} & \num{100} & \num{100} & \textcolor{red}{\num{99.843}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{100.003} & \textcolor{red}{\num{99.271}} & \num{100} & \num{100} & \num{99.993}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{100.069} & \num{110.295} & \textcolor{red}{\num{100}} & \num{100} & \num{100.013}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{99.124}} & \num{123.716} & \num{100} & \num{100} & \num{99.68}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{99.835} & \num{115.307} & \num{100} & \num{100} & \textcolor{red}{\num{99.566}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{99.871} & \num{109.831} & \num{100} & \num{100} & \textcolor{red}{\num{99.032}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{100.077} & \num{103.209} & \num{100} & \num{100} & \textcolor{red}{\num{99.141}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{98.664}} & \num{139.038} & \num{100} & \num{100} & \num{99.567}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{99.769} & \num{115.706} & \num{100} & \num{100} & \textcolor{red}{\num{99.69}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{70.243}} & \num{70.671} & \num{70.244} & \num{70.244} & \num{70.249}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{73.663}} & \num{74.832} & \num{73.676} & \num{73.676} & \num{73.671}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{61.991} & \num{63.866} & \textcolor{red}{\num{61.987}} & \num{61.987} & \num{61.992}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{87.968} & \textcolor{red}{\num{86.304}} & \num{87.984} & \num{87.984} & \num{87.868}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{84.491} & \textcolor{red}{\num{84.34}} & \num{84.504} & \num{84.504} & \num{84.496}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{102.237}} & \num{103.077} & \num{102.316} & \num{102.316} & \num{102.302}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{149.097} & \num{150.713} & \num{149.077} & \num{149.077} & \textcolor{red}{\num{149.073}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{154.979} & \num{159.87} & \num{155.092} & \num{155.092} & \textcolor{red}{\num{154.871}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{110.135} & \num{110.271} & \num{110.137} & \num{110.137} & \textcolor{red}{\num{109.985}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{50}} & \num{52.25} & \num{50.214} & \num{50.214} & \num{50.102}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{317.105} & \num{322.742} & \num{317.866} & \num{317.866} & \textcolor{red}{\num{316.959}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{66.08}} & \num{67.314} & \num{66.264} & \num{66.264} & \num{66.243}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{110.666} & \num{112.187} & \num{110.78} & \num{110.78} & \textcolor{red}{\num{110.651}}\\*
    \end{longtable}

## Average

### SMALL

1-step:

    \begin{table}

    \caption{\label{tab:smallonemean}SMALL Simulation - 1-step ahead Rolling Window Forecasting Average Loss}
    \centering
    \begin{tabular}[t]{llllll}
    \toprule
      & VAR & VHAR & BVAR-Minnesota & BVHAR-MN-VAR & BVHAR-MN-VHAR\\
    \midrule
    MSE & \textcolor{red}{\num{0.0626}} & \num{0.0636} & \num{0.0626} & \num{0.0626} & \num{0.0627}\\
    MAE & \num{0.15} & \num{0.15} & \textcolor{red}{\num{0.148}} & \num{0.148} & \num{0.148}\\
    MAPE & \num{179.509} & \num{184.532} & \num{173.202} & \num{173.129} & \textcolor{red}{\num{172.874}}\\
    MASE & \num{159.808} & \num{158.676} & \textcolor{red}{\num{157.462}} & \num{157.463} & \num{157.616}\\
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
    MSE & \num{0.0885} & \num{0.0924} & \num{0.0882} & \num{0.0882} & \textcolor{red}{\num{0.0882}}\\
    MAE & \textcolor{red}{\num{0.17}} & \num{0.174} & \num{0.17} & \num{0.17} & \num{0.171}\\
    MAPE & \num{127.633} & \num{120.748} & \textcolor{red}{\num{100.231}} & \num{100.251} & \num{102.681}\\
    MASE & \num{188.262} & \num{191.176} & \textcolor{red}{\num{187.777}} & \num{187.787} & \num{188.079}\\
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
    MSE & \num{0.0847} & \num{0.0862} & \textcolor{red}{\num{0.0847}} & \num{0.0847} & \num{0.0847}\\
    MAE & \num{0.166} & \num{0.168} & \textcolor{red}{\num{0.166}} & \num{0.166} & \num{0.166}\\
    MAPE & \num{100.033} & \num{116.508} & \num{100} & \textcolor{red}{\num{100}} & \num{100.584}\\
    MASE & \num{182.431} & \num{184.8} & \textcolor{red}{\num{182.388}} & \num{182.388} & \num{182.508}\\
    \bottomrule
    \end{tabular}
    \end{table}

### MEDIUM

1-step:

    \begin{table}

    \caption{\label{tab:medonemean}MEDIUM Simulation - 1-step ahead Rolling Window Forecasting Average Loss}
    \centering
    \begin{tabular}[t]{llllll}
    \toprule
      & VAR & VHAR & BVAR-Minnesota & BVHAR-MN-VAR & BVHAR-MN-VHAR\\
    \midrule
    MSE & \num{0.0291} & \num{0.0286} & \num{0.0276} & \num{0.0276} & \textcolor{red}{\num{0.0276}}\\
    MAE & \num{0.0985} & \num{0.0975} & \textcolor{red}{\num{0.0953}} & \num{0.0953} & \num{0.0953}\\
    MAPE & \num{222.11} & \num{230.509} & \num{192.372} & \textcolor{red}{\num{192.267}} & \num{194.091}\\
    MASE & \num{376.107} & \num{369.18} & \textcolor{red}{\num{359.068}} & \num{359.095} & \num{359.211}\\
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
    MSE & \num{0.0312} & \num{0.0312} & \num{0.0309} & \num{0.0309} & \textcolor{red}{\num{0.0308}}\\
    MAE & \num{0.102} & \num{0.102} & \textcolor{red}{\num{0.101}} & \num{0.101} & \num{0.101}\\
    MAPE & \num{123.203} & \num{158.484} & \textcolor{red}{\num{104.784}} & \num{104.813} & \num{106.484}\\
    MASE & \num{406.469} & \num{406.146} & \num{401.231} & \num{401.24} & \textcolor{red}{\num{400.832}}\\
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
    MSE & \num{0.0301} & \textcolor{red}{\num{0.0299}} & \num{0.0301} & \num{0.0301} & \num{0.0301}\\
    MAE & \num{0.0986} & \textcolor{red}{\num{0.0981}} & \num{0.0986} & \num{0.0986} & \num{0.0986}\\
    MAPE & \num{100.063} & \num{128.579} & \num{100} & \textcolor{red}{\num{100}} & \num{100.099}\\
    MASE & \num{419.591} & \textcolor{red}{\num{418.934}} & \num{419.577} & \num{419.577} & \num{419.97}\\
    \bottomrule
    \end{tabular}
    \end{table}

### LARGE

1-step:

    \begin{table}

    \caption{\label{tab:largeonemean}LARGE Simulation - 1-step ahead Rolling Window Forecasting Average Loss}
    \centering
    \begin{tabular}[t]{llllll}
    \toprule
      & VAR & VHAR & BVAR-Minnesota & BVHAR-MN-VAR & BVHAR-MN-VHAR\\
    \midrule
    MSE & \num{0.0191} & \num{0.0185} & \num{0.0177} & \num{0.0177} & \textcolor{red}{\num{0.0177}}\\
    MAE & \num{0.0931} & \num{0.0922} & \num{0.0901} & \num{0.0901} & \textcolor{red}{\num{0.0899}}\\
    MAPE & \num{217.612} & \num{226.566} & \num{178.103} & \num{179.038} & \textcolor{red}{\num{176.846}}\\
    MASE & \num{112.342} & \num{111.105} & \num{108.772} & \num{108.76} & \textcolor{red}{\num{108.529}}\\
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
    MSE & \num{0.0197} & \num{0.0202} & \num{0.0198} & \num{0.0198} & \textcolor{red}{\num{0.0197}}\\
    MAE & \num{0.0952} & \num{0.0963} & \num{0.095} & \num{0.095} & \textcolor{red}{\num{0.0948}}\\
    MAPE & \num{124.547} & \num{145.324} & \num{100.609} & \textcolor{red}{\num{100.592}} & \num{102.065}\\
    MASE & \num{112.22} & \num{113.684} & \num{112.021} & \num{112.018} & \textcolor{red}{\num{111.775}}\\
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
    MSE & \num{0.0188} & \num{0.0192} & \num{0.0189} & \num{0.0189} & \textcolor{red}{\num{0.0188}}\\
    MAE & \num{0.0936} & \num{0.0946} & \num{0.0937} & \num{0.0937} & \textcolor{red}{\num{0.0936}}\\
    MAPE & \num{99.769} & \num{115.706} & \num{100} & \num{100} & \textcolor{red}{\num{99.69}}\\
    MASE & \num{110.666} & \num{112.187} & \num{110.78} & \num{110.78} & \textcolor{red}{\num{110.651}}\\
    \bottomrule
    \end{tabular}
    \end{table}

### RMSFE or RMAFE

Set VAR as the benchmark model.

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

    \begin{table}[H]

    \caption{\label{tab:dgp2result}Out-of-sample forecasting performance measures for DGP2.}
    \centering
    \resizebox{\linewidth}{!}{
    \begin{tabular}[t]{cc|ccc|ccc|ccc|}
    \toprule
    \multicolumn{2}{c}{ } & \multicolumn{3}{c}{RMAFE} & \multicolumn{3}{c}{RMSFE} & \multicolumn{3}{c}{RMASE} \\
    \cmidrule(l{3pt}r{3pt}){3-5} \cmidrule(l{3pt}r{3pt}){6-8} \cmidrule(l{3pt}r{3pt}){9-11}
    \rotatebox{0}{} & \rotatebox{0}{} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$}\\
    \midrule
     & VHAR & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.026}} & \textcolor{black}{\num{1.013}} & \textcolor{black}{\num{1.017}} & \textcolor{black}{\num{1.044}} & \textcolor{black}{\num{1.018}} & \textcolor{black}{\num{.993}} & \textcolor{black}{\num{1.015}} & \textcolor{black}{\num{1.013}}\\

     & BVAR & \textcolor{red}{\num{.991}} & \textcolor{red}{\num{1.002}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{1.000}} & \textcolor{black}{\num{.997}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{.985}} & \textcolor{red}{\num{.997}} & \textcolor{red}{\num{1.000}}\\

     & BVHAR-S & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{1.002}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{1.000}}\\

    \multirow{-4}{*}{\centering\arraybackslash SMALL} & BVHAR-L & \textcolor{black}{\num{.992}} & \textcolor{black}{\num{1.003}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{1.001}} & \textcolor{red}{\num{.997}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.986}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.000}}\\
    \cmidrule{1-11}
     & VHAR & \textcolor{black}{\num{1.004}} & \textcolor{black}{\num{1.067}} & \textcolor{black}{\num{1.016}} & \textcolor{black}{\num{.998}} & \textcolor{black}{\num{1.075}} & \textcolor{black}{\num{1.014}} & \textcolor{black}{\num{1.007}} & \textcolor{black}{\num{1.064}} & \textcolor{black}{\num{1.021}}\\

     & BVAR & \textcolor{black}{\num{.983}} & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.987}} & \textcolor{black}{\num{.996}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{1.000}}\\

     & BVHAR-S & \textcolor{black}{\num{.983}} & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.987}} & \textcolor{black}{\num{.996}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{1.000}}\\

    \multirow{-4}{*}{\centering\arraybackslash MEDIUM} & BVHAR-L & \textcolor{red}{\num{.983}} & \textcolor{red}{\num{.991}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{.987}} & \textcolor{red}{\num{.996}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{.989}} & \textcolor{red}{\num{.988}} & \textcolor{red}{\num{1.000}}\\
    \cmidrule{1-11}
     & VHAR & \textcolor{black}{\num{.940}} & \textcolor{black}{\num{1.090}} & \textcolor{black}{\num{1.031}} & \textcolor{black}{\num{.916}} & \textcolor{black}{\num{1.114}} & \textcolor{black}{\num{1.037}} & \textcolor{black}{\num{.931}} & \textcolor{black}{\num{1.082}} & \textcolor{black}{\num{1.037}}\\

     & BVAR & \textcolor{black}{\num{.899}} & \textcolor{red}{\num{.965}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.875}} & \textcolor{black}{\num{.957}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.904}} & \textcolor{black}{\num{.965}} & \textcolor{black}{\num{1.000}}\\

     & BVHAR-S & \textcolor{black}{\num{.899}} & \textcolor{black}{\num{.965}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.875}} & \textcolor{black}{\num{.957}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.904}} & \textcolor{black}{\num{.965}} & \textcolor{black}{\num{1.000}}\\

    \multirow{-4}{*}{\centering\arraybackslash LARGE} & BVHAR-L & \textcolor{red}{\num{.898}} & \textcolor{black}{\num{.965}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{.875}} & \textcolor{red}{\num{.957}} & \textcolor{red}{\num{1.000}} & \textcolor{red}{\num{.903}} & \textcolor{red}{\num{.965}} & \textcolor{red}{\num{1.000}}\\
    \bottomrule
    \end{tabular}}
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
