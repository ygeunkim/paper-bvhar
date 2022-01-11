Simulating VHAR-type Minnesota BVHAR
================
Young Geun Kim
10 Jan, 2022

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
        -   [SMALL](#small-1)
        -   [MEDIUM](#medium-1)
        -   [LARGE](#large-1)
        -   [Lists](#lists)
    -   [Relative Error](#relative-error)
    -   [Piecewise Errors](#piecewise-errors)
        -   [SMALL](#small-2)
        -   [MEDIUM](#medium-2)
        -   [LARGE](#large-2)
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
#> [1]  0.0423  0.0494  0.1564
#> 
#> Setting for 'lambda':
#> [1]  0.178
#> 
#> Setting for 'delta':
#> [1]  0.588  0.572  0.579
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
#> [1]  0.0379  0.0315  0.0472  0.0548  0.0703  0.0490  0.0644  0.1364  0.1181
#> 
#> Setting for 'lambda':
#> [1]  0.123
#> 
#> Setting for 'delta':
#> [1]  0.637  0.549  0.578  0.555  0.552  0.588  0.635  0.602  0.626
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
#>  [1]  0.0394  0.0414  0.0343  0.0579  0.0697  0.0448  0.0558  0.0663  0.1029
#> [10]  0.0630  0.0746  0.1101
#> 
#> Setting for 'lambda':
#> [1]  0.0685
#> 
#> Setting for 'delta':
#>  [1]  0.599  0.600  0.599  0.637  0.584  0.622  0.581  0.605  0.589  0.613
#> [11]  0.596  0.599
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
#> [1]  0.0423  0.0556  0.1372
#> 
#> Setting for 'lambda':
#> [1]  0.134
#> 
#> Setting for 'delta':
#> [1]  0.643  0.608  0.613
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
#> [1]  0.0395  0.0409  0.0492  0.0531  0.0617  0.0588  0.0680  0.1281  0.1020
#> 
#> Setting for 'lambda':
#> [1]  0.0446
#> 
#> Setting for 'delta':
#> [1]  0.675  0.613  0.626  0.596  0.570  0.616  0.665  0.623  0.639
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
#>  [1]  0.0391  0.0410  0.0442  0.0576  0.0681  0.0431  0.0644  0.0613  0.0921
#> [10]  0.0774  0.0724  0.1021
#> 
#> Setting for 'lambda':
#> [1]  0.02
#> 
#> Setting for 'delta':
#>  [1]  0.628  0.625  0.628  0.648  0.616  0.620  0.586  0.640  0.616  0.645
#> [11]  0.630  0.622
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
#> [1]  0.0546  0.0527  0.0993
#> 
#> Setting for 'lambda':
#> [1]  0.0602
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.573  0.571  0.562
#> 
#> Setting for 'weekly':
#> [1]  0.377  0.189  0.314
#> 
#> Setting for 'monthly':
#> [1]  0.0100  0.0829  0.0223
```

``` r
(bvhar_vhar_medium_optim <- choose_bvhar(
  bvhar_medium_spec, 
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
#> [1]  0.0390  0.0415  0.0499  0.0519  0.0632  0.0575  0.0664  0.1329  0.0959
#> 
#> Setting for 'lambda':
#> [1]  0.0308
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.554  0.499  0.512  0.510  0.477  0.528  0.592  0.505  0.554
#> 
#> Setting for 'weekly':
#> [1]  0.406  0.422  0.409  0.344  0.299  0.305  0.259  0.415  0.254
#> 
#> Setting for 'monthly':
#> [1]  0.010  0.010  0.010  0.010  0.149  0.010  0.010  0.010  0.159
```

``` r
(bvhar_vhar_large_optim <- choose_bvhar(
  bvhar_large_spec, 
  lower = c(
    rep(1e-2, n_large), # sigma
    1e-2, # lambda
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
#>  [1]  0.0386  0.0405  0.0445  0.0566  0.0673  0.0424  0.0647  0.0608  0.0893
#> [10]  0.0787  0.0726  0.1001
#> 
#> Setting for 'lambda':
#> [1]  0.01
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#>  [1]  0.524  0.512  0.524  0.536  0.513  0.506  0.474  0.555  0.531  0.513
#> [11]  0.520  0.536
#> 
#> Setting for 'weekly':
#>  [1]  0.383  0.402  0.372  0.395  0.377  0.386  0.373  0.306  0.288  0.454
#> [11]  0.401  0.320
#> 
#> Setting for 'monthly':
#>  [1]  0.0150  0.0100  0.0100  0.0100  0.0100  0.0100  0.0100  0.1135  0.1825
#> [10]  0.0100  0.0306  0.0100
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
    \hspace{1em} & BVAR & $\sigma$ & 0.042 & 0.049 & 0.156 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.178 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.588 & 0.572 & 0.579 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.042 & 0.056 & 0.137 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.134 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.643 & 0.608 & 0.613 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.055 & 0.053 & 0.099 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.060 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.573 & 0.571 & 0.562 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.377 & 0.189 & 0.314 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.010 & 0.083 & 0.022 &  &  &  &  &  &  &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{MEDIUM}}\\
    \hspace{1em} & BVAR & $\sigma$ & 0.038 & 0.032 & 0.047 & 0.055 & 0.070 & 0.049 & 0.064 & 0.136 & 0.118 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.123 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.637 & 0.549 & 0.578 & 0.555 & 0.552 & 0.588 & 0.635 & 0.602 & 0.626 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.039 & 0.041 & 0.049 & 0.053 & 0.062 & 0.059 & 0.068 & 0.128 & 0.102 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.045 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.675 & 0.613 & 0.626 & 0.596 & 0.570 & 0.616 & 0.665 & 0.623 & 0.639 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.039 & 0.041 & 0.050 & 0.052 & 0.063 & 0.058 & 0.066 & 0.133 & 0.096 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.031 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.554 & 0.499 & 0.512 & 0.510 & 0.477 & 0.528 & 0.592 & 0.505 & 0.554 &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.406 & 0.422 & 0.409 & 0.344 & 0.299 & 0.305 & 0.259 & 0.415 & 0.254 &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.010 & 0.010 & 0.010 & 0.010 & 0.149 & 0.010 & 0.010 & 0.010 & 0.159 &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{LARGE}}\\
    \hspace{1em} & BVAR & $\sigma$ & 0.039 & 0.041 & 0.034 & 0.058 & 0.070 & 0.045 & 0.056 & 0.066 & 0.103 & 0.063 & 0.075 & 0.110\\

    \hspace{1em} &  & $\lambda$ & 0.068 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.599 & 0.600 & 0.599 & 0.637 & 0.584 & 0.622 & 0.581 & 0.605 & 0.589 & 0.613 & 0.596 & 0.599\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.039 & 0.041 & 0.044 & 0.058 & 0.068 & 0.043 & 0.064 & 0.061 & 0.092 & 0.077 & 0.072 & 0.102\\

    \hspace{1em} &  & $\lambda$ & 0.020 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.628 & 0.625 & 0.628 & 0.648 & 0.616 & 0.620 & 0.586 & 0.640 & 0.616 & 0.645 & 0.630 & 0.622\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.039 & 0.041 & 0.044 & 0.057 & 0.067 & 0.042 & 0.065 & 0.061 & 0.089 & 0.079 & 0.073 & 0.100\\

    \hspace{1em} &  & $\lambda$ & 0.010 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.524 & 0.512 & 0.524 & 0.536 & 0.513 & 0.506 & 0.474 & 0.555 & 0.531 & 0.513 & 0.520 & 0.536\\

    \hspace{1em} &  & $w_i$ & 0.383 & 0.402 & 0.372 & 0.395 & 0.377 & 0.386 & 0.373 & 0.306 & 0.288 & 0.454 & 0.401 & 0.320\\

    \hspace{1em} &  & $m_i$ & 0.015 & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.114 & 0.182 & 0.010 & 0.031 & 0.010\\*
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

## Relative Error

Set VAR as the benchmark model.

    \begin{table}[H]

    \caption{\label{tab:dgp4result}Out-of-sample forecasting performance measures for DGP4.}
    \centering
    \resizebox{\linewidth}{!}{
    \begin{tabular}[t]{cc|ccc|ccc|ccc|}
    \toprule
    \multicolumn{2}{c}{ } & \multicolumn{3}{c}{RMAFE} & \multicolumn{3}{c}{RMSFE} & \multicolumn{3}{c}{RMASE} \\
    \cmidrule(l{3pt}r{3pt}){3-5} \cmidrule(l{3pt}r{3pt}){6-8} \cmidrule(l{3pt}r{3pt}){9-11}
    \rotatebox{0}{} & \rotatebox{0}{} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$}\\
    \midrule
     & VHAR & \textcolor{black}{\num{.983}} & \textcolor{black}{\num{.981}} & \textcolor{black}{\num{1.075}} & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{.972}} & \textcolor{black}{\num{1.184}} & \textcolor{black}{\num{.986}} & \textcolor{black}{\num{.972}} & \textcolor{black}{\num{1.074}}\\

     & BVAR & \textcolor{black}{\num{.994}} & \textcolor{black}{\num{.989}} & \textcolor{red}{\num{.988}} & \textcolor{black}{\num{.998}} & \textcolor{black}{\num{1.006}} & \textcolor{red}{\num{.978}} & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{.987}} & \textcolor{red}{\num{.986}}\\

     & BVHAR-S & \textcolor{black}{\num{.981}} & \textcolor{black}{\num{.960}} & \textcolor{black}{\num{1.000}} & \textcolor{black}{\num{.982}} & \textcolor{black}{\num{.965}} & \textcolor{black}{\num{.993}} & \textcolor{black}{\num{.983}} & \textcolor{black}{\num{.957}} & \textcolor{black}{\num{.997}}\\

    \multirow{-4}{*}{\centering\arraybackslash SMALL} & BVHAR-L & \textcolor{red}{\num{.981}} & \textcolor{red}{\num{.945}} & \textcolor{black}{\num{1.007}} & \textcolor{red}{\num{.978}} & \textcolor{red}{\num{.943}} & \textcolor{black}{\num{1.009}} & \textcolor{red}{\num{.982}} & \textcolor{red}{\num{.941}} & \textcolor{black}{\num{1.004}}\\
    \cmidrule{1-11}
     & VHAR & \textcolor{black}{\num{.998}} & \textcolor{black}{\num{1.047}} & \textcolor{black}{\num{1.338}} & \textcolor{black}{\num{1.017}} & \textcolor{black}{\num{1.184}} & \textcolor{black}{\num{1.752}} & \textcolor{black}{\num{.998}} & \textcolor{black}{\num{1.054}} & \textcolor{black}{\num{1.357}}\\

     & BVAR & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{.983}} & \textcolor{black}{\num{.982}} & \textcolor{black}{\num{.986}} & \textcolor{red}{\num{.948}} & \textcolor{red}{\num{.947}} & \textcolor{black}{\num{.993}} & \textcolor{black}{\num{.979}} & \textcolor{black}{\num{.982}}\\

     & BVHAR-S & \textcolor{red}{\num{.988}} & \textcolor{black}{\num{.974}} & \textcolor{black}{\num{.981}} & \textcolor{red}{\num{.981}} & \textcolor{black}{\num{.986}} & \textcolor{black}{\num{.963}} & \textcolor{red}{\num{.988}} & \textcolor{black}{\num{.974}} & \textcolor{black}{\num{.981}}\\

    \multirow{-4}{*}{\centering\arraybackslash MEDIUM} & BVHAR-L & \textcolor{black}{\num{.994}} & \textcolor{red}{\num{.962}} & \textcolor{red}{\num{.974}} & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{.986}} & \textcolor{black}{\num{.970}} & \textcolor{black}{\num{.993}} & \textcolor{red}{\num{.961}} & \textcolor{red}{\num{.973}}\\
    \cmidrule{1-11}
     & VHAR & \textcolor{black}{\num{1.001}} & \textcolor{black}{\num{1.061}} & \textcolor{black}{\num{1.184}} & \textcolor{black}{\num{1.014}} & \textcolor{black}{\num{1.179}} & \textcolor{black}{\num{1.511}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.066}} & \textcolor{black}{\num{1.184}}\\

     & BVAR & \textcolor{black}{\num{.993}} & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{.989}} & \textcolor{red}{\num{.959}} & \textcolor{red}{\num{.952}} & \textcolor{black}{\num{.994}} & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{.984}}\\

     & BVHAR-S & \textcolor{red}{\num{.988}} & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{.991}} & \textcolor{red}{\num{.983}} & \textcolor{black}{\num{.987}} & \textcolor{black}{\num{.976}} & \textcolor{red}{\num{.987}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{.988}}\\

    \multirow{-4}{*}{\centering\arraybackslash LARGE} & BVHAR-L & \textcolor{black}{\num{.990}} & \textcolor{red}{\num{.982}} & \textcolor{red}{\num{.984}} & \textcolor{black}{\num{.987}} & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{.981}} & \textcolor{black}{\num{.989}} & \textcolor{red}{\num{.987}} & \textcolor{red}{\num{.983}}\\
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
     & asset01 & \num{0.000976} & \num{0.000943} & \num{0.00095} & \textcolor{red}{\num{0.000907}} & \num{0.000924}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000886} & \num{0.000853} & \num{0.000867} & \num{0.000844} & \textcolor{red}{\num{0.000835}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0148} & \num{0.0147} & \num{0.0148} & \num{0.0146} & \textcolor{red}{\num{0.0146}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00556} & \num{0.00551} & \num{0.00555} & \num{0.00546} & \textcolor{red}{\num{0.00544}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0253} & \num{0.025} & \num{0.0251} & \textcolor{red}{\num{0.0247}} & \num{0.0249}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0235} & \num{0.0237} & \num{0.0235} & \num{0.0235} & \textcolor{red}{\num{0.0233}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.1006} & \textcolor{red}{\num{0.0983}} & \num{0.1} & \num{0.0985} & \num{0.0985}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0498} & \num{0.049} & \num{0.0495} & \num{0.0489} & \textcolor{red}{\num{0.0489}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{1939.443}} & \num{3137.715} & \num{3296.872} & \num{2599.985} & \num{2277.541}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{162.125} & \num{146.006} & \num{155.57} & \num{144.75} & \textcolor{red}{\num{144.184}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{395.904} & \textcolor{red}{\num{310.18}} & \num{404.668} & \num{353.503} & \num{364.018}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{832.491}} & \num{1197.967} & \num{1285.703} & \num{1032.746} & \num{928.581}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{65.538} & \num{65.015} & \num{65.109} & \textcolor{red}{\num{63.968}} & \num{64.578}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{58.781} & \num{59.252} & \num{58.896} & \num{58.231} & \textcolor{red}{\num{57.573}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{257.818} & \textcolor{red}{\num{252.611}} & \num{254.685} & \num{253.48} & \num{253.221}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{127.379} & \num{125.626} & \num{126.23} & \num{125.226} & \textcolor{red}{\num{125.124}}\\*
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
     & asset01 & \num{0.00334} & \num{0.00331} & \num{0.00326} & \textcolor{red}{\num{0.00305}} & \num{0.00313}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00248} & \num{0.00246} & \num{0.00245} & \num{0.00242} & \textcolor{red}{\num{0.00238}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0426} & \num{0.0413} & \num{0.043} & \num{0.0412} & \textcolor{red}{\num{0.0401}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0161} & \num{0.0157} & \num{0.0162} & \num{0.0156} & \textcolor{red}{\num{0.0152}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0481} & \num{0.0475} & \num{0.0475} & \textcolor{red}{\num{0.0456}} & \num{0.046}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0408} & \num{0.0401} & \num{0.0406} & \num{0.0401} & \textcolor{red}{\num{0.0397}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.169} & \num{0.165} & \num{0.167} & \num{0.162} & \textcolor{red}{\num{0.158}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0858} & \num{0.0842} & \num{0.0849} & \num{0.0824} & \textcolor{red}{\num{0.0812}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{2243.901} & \textcolor{red}{\num{1291.904}} & \num{4290.288} & \num{4963.996} & \num{5090.721}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{258.372}} & \num{268.71} & \num{261.855} & \num{264.274} & \num{262.072}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{397.922} & \textcolor{red}{\num{336.297}} & \num{448.115} & \num{464.041} & \num{418.95}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{966.732} & \textcolor{red}{\num{632.304}} & \num{1666.753} & \num{1897.437} & \num{1923.914}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{121.618} & \num{119.858} & \num{120.186} & \textcolor{red}{\num{115.611}} & \num{116.407}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{107.932} & \num{104.726} & \num{107.207} & \num{105.509} & \textcolor{red}{\num{104.221}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{444.528} & \num{430.819} & \num{438.214} & \num{423.991} & \textcolor{red}{\num{413.912}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{224.693} & \num{218.468} & \num{221.869} & \num{215.037} & \textcolor{red}{\num{211.513}}\\*
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
     & asset01 & \num{0.00692} & \num{0.00781} & \textcolor{red}{\num{0.00682}} & \num{0.00689} & \num{0.00704}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00279} & \num{0.00331} & \textcolor{red}{\num{0.00275}} & \num{0.00287} & \num{0.00293}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0728} & \num{0.0866} & \textcolor{red}{\num{0.0712}} & \num{0.0722} & \num{0.0733}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0275} & \num{0.0326} & \textcolor{red}{\num{0.0269}} & \num{0.0273} & \num{0.0278}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0703} & \num{0.074} & \textcolor{red}{\num{0.0696}} & \num{0.07} & \num{0.0706}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0438} & \num{0.0461} & \textcolor{red}{\num{0.0436}} & \num{0.0448} & \num{0.0451}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.221} & \num{0.241} & \textcolor{red}{\num{0.218}} & \num{0.221} & \num{0.222}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.112} & \num{0.12} & \textcolor{red}{\num{0.11}} & \num{0.112} & \num{0.113}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{24036.616} & \num{27584.038} & \num{18654.079} & \num{16133.821} & \textcolor{red}{\num{15598.295}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{316.99} & \num{341.94} & \num{308.284} & \textcolor{red}{\num{303.924}} & \num{308.648}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{475.771} & \num{540.906} & \num{385.005} & \num{359.886} & \textcolor{red}{\num{355.277}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{8276.459} & \num{9488.962} & \num{6449.123} & \num{5599.211} & \textcolor{red}{\num{5420.74}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{177.742} & \num{187.144} & \textcolor{red}{\num{175.462}} & \num{175.882} & \num{177.505}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{111.448} & \num{118.304} & \textcolor{red}{\num{110.963}} & \num{114.386} & \num{114.68}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{572.408} & \num{619.849} & \textcolor{red}{\num{563.26}} & \num{569.073} & \num{572.869}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{287.199} & \num{308.432} & \textcolor{red}{\num{283.229}} & \num{286.447} & \num{288.351}\\*
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

<img src="../output/figs/DGP-4-medcvonefig-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-4-medcvfivefig-1.png" width="70%" style="display: block; margin: auto;" />

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
     & asset01 & \num{0.000382} & \num{0.000388} & \textcolor{red}{\num{0.000353}} & \num{0.000355} & \num{0.000358}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000238} & \num{0.000248} & \num{0.000219} & \textcolor{red}{\num{0.000218}} & \num{0.000219}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000349} & \num{0.00038} & \textcolor{red}{\num{0.000339}} & \num{0.000342} & \num{0.000353}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.000511}} & \num{0.000565} & \num{0.000522} & \num{0.00054} & \num{0.000517}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.000961}} & \num{0.001005} & \num{0.000976} & \num{0.000992} & \num{0.00098}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.00086} & \textcolor{red}{\num{0.00078}} & \num{0.000825} & \num{0.00086} & \num{0.000821}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000536} & \num{0.000497} & \num{0.000497} & \num{0.000492} & \textcolor{red}{\num{0.000486}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00596} & \num{0.00603} & \textcolor{red}{\num{0.00572}} & \num{0.00584} & \num{0.00577}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00411} & \textcolor{red}{\num{0.00386}} & \num{0.0039} & \num{0.00388} & \num{0.00387}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00155} & \num{0.00153} & \textcolor{red}{\num{0.00148}} & \num{0.0015} & \num{0.00149}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0152} & \num{0.0153} & \num{0.0147} & \textcolor{red}{\num{0.0145}} & \num{0.0147}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0128} & \num{0.0128} & \num{0.0121} & \textcolor{red}{\num{0.0118}} & \num{0.012}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0152} & \num{0.0159} & \textcolor{red}{\num{0.015}} & \num{0.0151} & \num{0.0154}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.0173}} & \num{0.0186} & \num{0.0179} & \num{0.0185} & \num{0.0178}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.0239}} & \num{0.0246} & \num{0.024} & \num{0.0241} & \num{0.0241}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0239} & \textcolor{red}{\num{0.0227}} & \num{0.0234} & \num{0.0239} & \num{0.0235}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0176} & \num{0.0172} & \num{0.017} & \num{0.0171} & \textcolor{red}{\num{0.0169}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0637} & \num{0.0642} & \num{0.0623} & \num{0.0624} & \textcolor{red}{\num{0.0621}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.053} & \textcolor{red}{\num{0.0506}} & \num{0.0517} & \num{0.0516} & \num{0.0514}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0269} & \num{0.0269} & \num{0.0264} & \num{0.0266} & \textcolor{red}{\num{0.0264}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{189.566}} & \num{202.452} & \num{196.96} & \num{229.557} & \num{194.298}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{113.815} & \num{107.239} & \num{111.763} & \num{113.492} & \textcolor{red}{\num{101.141}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{134.225} & \num{136.503} & \num{132.371} & \textcolor{red}{\num{131.624}} & \num{139.049}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{214.933}} & \num{1456.685} & \num{525.237} & \num{925.242} & \num{703.288}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{463.591}} & \num{604.548} & \num{494.795} & \num{511.066} & \num{494.497}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{181.231} & \num{164.941} & \textcolor{red}{\num{149.816}} & \num{167.907} & \num{153.122}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{165.941} & \num{140.873} & \num{145.801} & \num{142.672} & \textcolor{red}{\num{133.647}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{185.454} & \num{194.084} & \num{168.408} & \textcolor{red}{\num{168.143}} & \num{182.978}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{334.697} & \num{366.456} & \num{288.608} & \num{306.943} & \textcolor{red}{\num{275.786}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{220.384}} & \num{374.865} & \num{245.973} & \num{299.627} & \num{264.201}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{66.091} & \num{69.339} & \num{65.051} & \textcolor{red}{\num{64.833}} & \num{65.38}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{55.729} & \num{55.79} & \num{52.375} & \textcolor{red}{\num{51.496}} & \num{51.919}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{67.097} & \num{70.062} & \textcolor{red}{\num{65.661}} & \num{66.121} & \num{67.717}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{74.697}} & \num{80.05} & \num{76.912} & \num{78.908} & \num{76.36}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{104.065}} & \num{106.339} & \num{104.696} & \num{105.931} & \num{105.781}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{106.605} & \textcolor{red}{\num{100.179}} & \num{104.432} & \num{105.382} & \num{104.413}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{76.666} & \num{76.21} & \textcolor{red}{\num{75.081}} & \num{76.266} & \num{75.86}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{270.227} & \num{270.321} & \num{262.1} & \num{261.439} & \textcolor{red}{\num{259.458}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{234.353} & \num{228.692} & \textcolor{red}{\num{228.405}} & \num{228.935} & \num{229.971}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{117.281} & \num{117.443} & \textcolor{red}{\num{114.968}} & \num{115.479} & \num{115.207}\\*
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
     & asset01 & \num{0.000696} & \num{0.000777} & \num{0.000667} & \textcolor{red}{\num{0.000652}} & \num{0.000686}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000434} & \num{0.000479} & \textcolor{red}{\num{0.000407}} & \num{0.000409} & \num{0.000411}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000614} & \num{0.000704} & \num{0.000592} & \textcolor{red}{\num{0.000577}} & \num{0.00066}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000945} & \num{0.001092} & \num{0.00097} & \num{0.000984} & \textcolor{red}{\num{0.000892}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00113} & \num{0.00117} & \num{0.00112} & \textcolor{red}{\num{0.00109}} & \num{0.00111}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.00162} & \textcolor{red}{\num{0.0014}} & \num{0.00166} & \num{0.0018} & \num{0.0016}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000749} & \num{0.000787} & \num{0.000733} & \num{0.000771} & \textcolor{red}{\num{0.000728}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00781} & \num{0.00778} & \num{0.00773} & \num{0.00784} & \textcolor{red}{\num{0.00752}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.00597}} & \num{0.00655} & \num{0.006} & \num{0.00637} & \num{0.00626}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00222} & \num{0.0023} & \num{0.00221} & \num{0.00228} & \textcolor{red}{\num{0.00221}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0211} & \num{0.0226} & \num{0.0205} & \textcolor{red}{\num{0.0203}} & \num{0.0209}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0173} & \num{0.0182} & \textcolor{red}{\num{0.0165}} & \num{0.0165} & \num{0.0168}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0197} & \num{0.0214} & \num{0.0196} & \textcolor{red}{\num{0.0195}} & \num{0.0205}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0255} & \num{0.0274} & \num{0.0257} & \num{0.0259} & \textcolor{red}{\num{0.0247}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0271} & \num{0.0275} & \num{0.0267} & \textcolor{red}{\num{0.0263}} & \num{0.0266}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0313} & \textcolor{red}{\num{0.0296}} & \num{0.032} & \num{0.0331} & \num{0.031}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0209} & \num{0.0213} & \num{0.0209} & \num{0.0216} & \textcolor{red}{\num{0.0205}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.071} & \num{0.0713} & \num{0.0713} & \num{0.0729} & \textcolor{red}{\num{0.0707}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0599} & \num{0.0619} & \textcolor{red}{\num{0.0597}} & \num{0.0615} & \num{0.0611}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0327} & \num{0.0335} & \num{0.0326} & \num{0.0331} & \textcolor{red}{\num{0.0325}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{395.13} & \num{279.931} & \num{255.865} & \textcolor{red}{\num{169.997}} & \num{303.091}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{169.522} & \num{163.193} & \num{148.179} & \textcolor{red}{\num{119.708}} & \num{170.018}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{111.635} & \num{136.321} & \num{104.35} & \textcolor{red}{\num{101.805}} & \num{116.729}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{131.421}} & \num{1179.24} & \num{163.387} & \num{201.27} & \num{343.118}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{205.743} & \num{296.149} & \num{150.233} & \textcolor{red}{\num{133.944}} & \num{239.831}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{140.719} & \num{199.817} & \num{141.244} & \textcolor{red}{\num{110.17}} & \num{129.18}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{108.057} & \num{108.93} & \num{94.217} & \num{91.353} & \textcolor{red}{\num{91.186}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{116.132} & \num{120.653} & \num{105.777} & \textcolor{red}{\num{105.231}} & \num{136.021}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{292.375} & \num{383.142} & \num{260.118} & \textcolor{red}{\num{235.987}} & \num{372.054}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{185.637} & \num{318.597} & \num{158.152} & \textcolor{red}{\num{141.052}} & \num{211.248}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{92.622} & \num{99.089} & \num{90.279} & \textcolor{red}{\num{90.156}} & \num{91.992}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{73.43} & \num{77.61} & \num{68.821} & \textcolor{red}{\num{68.367}} & \num{70.979}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{88.453}} & \num{97.216} & \num{88.616} & \num{88.878} & \num{91.503}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{111.051} & \num{117.886} & \num{111.853} & \num{112.504} & \textcolor{red}{\num{106.936}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{110.676} & \num{113.204} & \num{109.068} & \textcolor{red}{\num{107.569}} & \num{108.7}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{124.105} & \textcolor{red}{\num{122.119}} & \num{127.871} & \num{133.352} & \num{123.545}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{87.715} & \num{89.898} & \num{88.89} & \num{92.398} & \textcolor{red}{\num{87.275}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{318.303} & \num{320.319} & \num{320.94} & \num{330.201} & \textcolor{red}{\num{315.629}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{253.905} & \num{271.533} & \textcolor{red}{\num{252.828}} & \num{262.081} & \num{260.97}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{140.029} & \num{145.43} & \num{139.907} & \num{142.834} & \textcolor{red}{\num{139.725}}\\*
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
     & asset01 & \num{0.000709} & \num{0.00082} & \num{0.000706} & \num{0.000709} & \textcolor{red}{\num{7e-04}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000441} & \num{0.000442} & \num{0.000437} & \num{0.000437} & \textcolor{red}{\num{0.000435}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000484} & \num{0.000574} & \num{0.000486} & \num{0.000487} & \textcolor{red}{\num{0.000482}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.00117} & \num{0.00124} & \num{0.00117} & \num{0.00117} & \textcolor{red}{\num{0.00116}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00109} & \textcolor{red}{\num{0.00106}} & \num{0.00109} & \num{0.00109} & \num{0.00108}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.00209} & \textcolor{red}{\num{0.00193}} & \num{0.00209} & \num{0.00209} & \num{0.00209}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000904} & \num{0.001012} & \num{0.000902} & \num{0.000901} & \textcolor{red}{\num{0.000897}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00729} & \num{0.00728} & \num{0.0073} & \num{0.0073} & \textcolor{red}{\num{0.00726}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00717} & \num{0.0082} & \num{0.00714} & \textcolor{red}{\num{0.00713}} & \num{0.00715}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00237} & \num{0.00251} & \num{0.00237} & \num{0.00237} & \textcolor{red}{\num{0.00236}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0212} & \num{0.0233} & \num{0.0211} & \num{0.0212} & \textcolor{red}{\num{0.021}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0176} & \textcolor{red}{\num{0.0173}} & \num{0.0175} & \num{0.0175} & \num{0.0174}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.0178}} & \num{0.0199} & \num{0.0179} & \num{0.0179} & \num{0.0179}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0287} & \num{0.0292} & \num{0.0288} & \num{0.0288} & \textcolor{red}{\num{0.0287}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0257} & \textcolor{red}{\num{0.0253}} & \num{0.0258} & \num{0.0258} & \num{0.0257}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.036} & \textcolor{red}{\num{0.0341}} & \num{0.036} & \num{0.036} & \num{0.036}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0245} & \num{0.0266} & \num{0.0244} & \num{0.0244} & \textcolor{red}{\num{0.0243}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0701} & \textcolor{red}{\num{0.0696}} & \num{0.0702} & \num{0.0702} & \num{0.0701}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.066} & \num{0.0716} & \num{0.0658} & \textcolor{red}{\num{0.0658}} & \num{0.066}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0342} & \num{0.0352} & \num{0.0342} & \num{0.0342} & \textcolor{red}{\num{0.0341}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{99.482}} & \num{155.717} & \num{105.612} & \num{101.984} & \num{115.096}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{102.195} & \num{115.846} & \textcolor{red}{\num{99.228}} & \num{99.691} & \num{100.54}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{99.596}} & \num{132.22} & \num{99.86} & \num{100.037} & \num{101.779}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{127.331} & \num{1134.46} & \num{105.436} & \textcolor{red}{\num{100.301}} & \num{186.201}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{104.031} & \num{203.602} & \textcolor{red}{\num{99.652}} & \num{102.129} & \num{114.312}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{103.584} & \num{180.569} & \num{101.629} & \textcolor{red}{\num{98.956}} & \num{101.637}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{101.585} & \num{124.386} & \num{99.97} & \num{99.965} & \textcolor{red}{\num{99.034}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{98.785}} & \num{107.145} & \num{100.581} & \num{100.06} & \num{101.698}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{105.952}} & \num{626.48} & \num{108.008} & \num{105.992} & \num{194.846}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{104.727} & \num{308.936} & \num{102.22} & \textcolor{red}{\num{101.013}} & \num{123.905}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{95.556} & \num{105.53} & \num{95.409} & \num{95.502} & \textcolor{red}{\num{95.013}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{81.937} & \textcolor{red}{\num{81.238}} & \num{81.602} & \num{81.65} & \num{81.326}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{84.571}} & \num{94.704} & \num{84.9} & \num{85.032} & \num{84.709}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{125.215} & \num{128.556} & \num{125.493} & \num{125.391} & \textcolor{red}{\num{124.993}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{120.265} & \textcolor{red}{\num{118.716}} & \num{120.616} & \num{120.573} & \num{120.019}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{147.924} & \textcolor{red}{\num{142.653}} & \num{147.965} & \num{147.757} & \num{147.724}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{108.56} & \num{115.844} & \num{108.092} & \num{108.012} & \textcolor{red}{\num{107.542}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{299.066} & \textcolor{red}{\num{298.451}} & \num{299.578} & \num{299.629} & \num{298.947}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{273.902} & \num{294.678} & \num{273.432} & \textcolor{red}{\num{273.36}} & \num{273.547}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{148.555} & \num{153.374} & \num{148.565} & \num{148.545} & \textcolor{red}{\num{148.202}}\\*
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

<img src="../output/figs/DGP-4-largecvonefig-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-4-largecvfivefig-1.png" width="70%" style="display: block; margin: auto;" />

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
     & asset01 & \num{0.000295} & \num{0.000308} & \num{0.00029} & \num{0.000292} & \textcolor{red}{\num{0.000289}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000422} & \num{0.000416} & \num{0.000386} & \num{0.000383} & \textcolor{red}{\num{0.00038}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.000242}} & \num{0.000262} & \num{0.000249} & \num{0.000254} & \num{0.000248}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.000429}} & \num{0.000461} & \num{0.000473} & \num{0.000491} & \num{0.000473}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.000838} & \num{8e-04} & \num{0.000806} & \num{0.000813} & \textcolor{red}{\num{0.000793}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000519} & \num{0.000511} & \textcolor{red}{\num{0.000504}} & \num{0.000519} & \num{0.000506}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000721} & \num{0.00081} & \num{0.000727} & \num{0.000736} & \textcolor{red}{\num{0.000718}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00235} & \num{0.00222} & \num{0.00213} & \num{0.00217} & \textcolor{red}{\num{0.00209}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00375} & \num{0.00375} & \num{0.00356} & \textcolor{red}{\num{0.00353}} & \num{0.00357}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.000987} & \num{0.000996} & \num{0.000962} & \num{0.000963} & \textcolor{red}{\num{0.000945}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.004} & \num{0.0039} & \num{0.00379} & \num{0.00397} & \textcolor{red}{\num{0.00378}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.00497} & \num{0.00492} & \textcolor{red}{\num{0.00481}} & \num{0.00482} & \num{0.00488}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00163} & \num{0.00161} & \num{0.00156} & \num{0.00158} & \textcolor{red}{\num{0.00156}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0138} & \num{0.0141} & \num{0.0136} & \textcolor{red}{\num{0.0136}} & \num{0.0137}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0161} & \num{0.0155} & \num{0.0153} & \num{0.0153} & \textcolor{red}{\num{0.0153}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0123} & \num{0.0128} & \num{0.0123} & \textcolor{red}{\num{0.0123}} & \num{0.0123}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.0169}} & \num{0.0176} & \num{0.0177} & \num{0.0182} & \num{0.0177}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0228} & \num{0.0227} & \num{0.0231} & \num{0.0232} & \textcolor{red}{\num{0.0225}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0186} & \num{0.0183} & \textcolor{red}{\num{0.0182}} & \num{0.0184} & \num{0.0182}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0224} & \num{0.0239} & \num{0.0224} & \num{0.0225} & \textcolor{red}{\num{0.0223}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0393} & \num{0.0385} & \num{0.0369} & \num{0.0372} & \textcolor{red}{\num{0.0364}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.049} & \num{0.0497} & \num{0.0482} & \num{0.0478} & \textcolor{red}{\num{0.0477}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.0248} & \num{0.0248} & \num{0.0241} & \textcolor{red}{\num{0.024}} & \num{0.0241}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.0494} & \textcolor{red}{\num{0.0481}} & \num{0.0482} & \num{0.0495} & \num{0.0484}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.0576} & \num{0.0571} & \textcolor{red}{\num{0.0561}} & \num{0.0565} & \num{0.0567}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0286} & \num{0.0286} & \num{0.028} & \num{0.0282} & \textcolor{red}{\num{0.0279}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{131.666} & \num{130.368} & \num{119.211} & \textcolor{red}{\num{114.463}} & \num{123.493}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{171.921} & \num{164.632} & \textcolor{red}{\num{158.022}} & \num{165.782} & \num{167.04}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{150.165} & \num{163.323} & \num{147.994} & \textcolor{red}{\num{145.667}} & \num{155.108}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{148.182} & \num{164.166} & \num{148.051} & \textcolor{red}{\num{147.094}} & \num{149.175}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{227.397} & \num{227.093} & \num{211.392} & \num{201.304} & \textcolor{red}{\num{193.544}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{163.056} & \textcolor{red}{\num{154.682}} & \num{164.741} & \num{170.201} & \num{157.417}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{550.335} & \textcolor{red}{\num{474.95}} & \num{563.062} & \num{573.817} & \num{573.935}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{103.125} & \num{103.231} & \num{91.835} & \textcolor{red}{\num{90.159}} & \num{91.922}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{249.477} & \num{226.788} & \num{209.059} & \num{207.097} & \textcolor{red}{\num{202.317}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{338.759}} & \num{488.291} & \num{506.585} & \num{499.155} & \num{541.045}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{297.711} & \num{339.262} & \textcolor{red}{\num{288.207}} & \num{291.309} & \num{305.702}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{254.713}} & \num{369.067} & \num{332.198} & \num{307.628} & \num{334.162}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{232.209}} & \num{250.488} & \num{245.03} & \num{242.806} & \num{249.572}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{64.37} & \num{66.172} & \num{64.103} & \num{63.506} & \textcolor{red}{\num{63.48}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{71.322} & \num{69.453} & \num{67.653} & \textcolor{red}{\num{67.444}} & \num{68.222}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{50.866} & \num{52.439} & \textcolor{red}{\num{50.163}} & \num{50.166} & \num{50.196}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{70.312}} & \num{73.277} & \num{73.382} & \num{75.089} & \num{72.463}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{100.146} & \textcolor{red}{\num{97.838}} & \num{102.249} & \num{103.136} & \num{99.44}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{78.242} & \num{76.912} & \textcolor{red}{\num{76.191}} & \num{77.946} & \num{76.63}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{97.555} & \num{104.364} & \num{97.167} & \num{98.143} & \textcolor{red}{\num{96.582}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{170.77} & \num{162.283} & \num{158.08} & \num{160.256} & \textcolor{red}{\num{156.202}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{210.293} & \num{206.993} & \num{203.26} & \num{199.067} & \textcolor{red}{\num{198.721}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{112.071} & \num{110.389} & \num{108.28} & \textcolor{red}{\num{107.663}} & \num{108.833}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{216.736} & \textcolor{red}{\num{205.567}} & \num{211.695} & \num{217.399} & \num{212.896}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{250.187} & \num{246.395} & \textcolor{red}{\num{241.926}} & \num{243.611} & \num{244.153}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{124.406} & \num{122.674} & \num{121.179} & \num{121.952} & \textcolor{red}{\num{120.652}}\\*
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
     & asset01 & \num{0.000554} & \num{0.000638} & \textcolor{red}{\num{0.000515}} & \num{0.000516} & \num{0.000531}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000604} & \num{0.000641} & \num{0.000534} & \textcolor{red}{\num{0.000523}} & \num{0.000566}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000497} & \num{0.000559} & \num{0.000507} & \num{0.000521} & \textcolor{red}{\num{0.000475}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.00078} & \textcolor{red}{\num{0.000736}} & \num{0.000861} & \num{0.000911} & \num{0.00084}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00143} & \textcolor{red}{\num{0.00129}} & \num{0.00133} & \num{0.00134} & \num{0.00133}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000748} & \textcolor{red}{\num{0.00074}} & \num{0.000761} & \num{0.000788} & \num{0.000784}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.00122} & \num{0.00135} & \num{0.00111} & \num{0.00111} & \textcolor{red}{\num{0.00108}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00349} & \num{0.00358} & \num{0.00336} & \num{0.00365} & \textcolor{red}{\num{0.00325}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00596} & \num{0.00626} & \textcolor{red}{\num{0.00594}} & \num{0.00607} & \num{0.00598}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.00191} & \num{0.00166} & \num{0.00149} & \textcolor{red}{\num{0.00138}} & \num{0.00147}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{0.00651}} & \num{0.00669} & \num{0.0066} & \num{0.00731} & \num{0.00659}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{0.00793}} & \num{0.00823} & \num{0.00831} & \num{0.00891} & \num{0.00896}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00264} & \num{0.0027} & \textcolor{red}{\num{0.00261}} & \num{0.00275} & \num{0.00265}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0196} & \num{0.0205} & \textcolor{red}{\num{0.019}} & \num{0.0192} & \num{0.0192}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0198} & \num{0.0201} & \num{0.019} & \textcolor{red}{\num{0.0189}} & \num{0.0195}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0165} & \num{0.0175} & \num{0.017} & \num{0.0174} & \textcolor{red}{\num{0.0165}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0227} & \textcolor{red}{\num{0.0218}} & \num{0.0241} & \num{0.0248} & \num{0.0237}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.03} & \num{0.0291} & \num{0.0283} & \num{0.0283} & \textcolor{red}{\num{0.0283}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0218} & \textcolor{red}{\num{0.0214}} & \num{0.0223} & \num{0.023} & \num{0.0226}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0291} & \num{0.0299} & \num{0.0276} & \num{0.0276} & \textcolor{red}{\num{0.0271}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0477} & \num{0.0494} & \num{0.0468} & \num{0.0502} & \textcolor{red}{\num{0.0455}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.0631}} & \num{0.0638} & \num{0.0636} & \num{0.0641} & \num{0.0648}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.035} & \num{0.0337} & \num{0.0313} & \textcolor{red}{\num{0.0304}} & \num{0.031}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{0.0642}} & \num{0.0653} & \num{0.0657} & \num{0.0697} & \num{0.0654}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{0.0717}} & \num{0.0752} & \num{0.0733} & \num{0.0766} & \num{0.0763}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0368} & \num{0.0373} & \textcolor{red}{\num{0.0365}} & \num{0.0375} & \num{0.0367}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{117.606} & \num{138.274} & \num{107.735} & \textcolor{red}{\num{101.288}} & \num{121.426}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{128.533} & \num{122.645} & \num{110.287} & \textcolor{red}{\num{106.843}} & \num{132.463}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{129.947} & \num{157.262} & \num{111.497} & \textcolor{red}{\num{103.191}} & \num{147.373}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{121.416} & \num{138.074} & \num{108.265} & \textcolor{red}{\num{107.806}} & \num{125.891}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{193.042} & \num{236.396} & \num{120.926} & \textcolor{red}{\num{105.527}} & \num{139.644}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{140.268} & \num{164.483} & \num{123.57} & \textcolor{red}{\num{107.417}} & \num{134.323}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{254.019} & \num{364.346} & \num{114.877} & \textcolor{red}{\num{113.927}} & \num{156.215}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{108.182} & \num{129.173} & \num{93.861} & \textcolor{red}{\num{90.408}} & \num{94.532}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{156.715} & \num{197.485} & \num{129.825} & \textcolor{red}{\num{107.554}} & \num{135.671}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{641.347} & \textcolor{red}{\num{216.807}} & \num{329.265} & \num{272.357} & \num{530.437}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{321.957} & \num{403.741} & \num{238.898} & \textcolor{red}{\num{156.791}} & \num{259.837}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{326.398} & \num{510.463} & \num{251.333} & \textcolor{red}{\num{147.342}} & \num{230.991}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{219.953} & \num{231.596} & \num{153.362} & \textcolor{red}{\num{126.704}} & \num{184.067}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{85.396} & \num{87.497} & \textcolor{red}{\num{82.719}} & \num{84.076} & \num{83.024}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{82.936} & \num{83.049} & \num{78.613} & \textcolor{red}{\num{77.899}} & \num{80.992}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{71.1}} & \num{75.367} & \num{73.846} & \num{76.017} & \num{71.167}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{97.346} & \textcolor{red}{\num{94.771}} & \num{103.03} & \num{105.366} & \num{103.225}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{131.992} & \num{129.918} & \num{122.974} & \textcolor{red}{\num{121.765}} & \num{124.719}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{95.936} & \textcolor{red}{\num{92.746}} & \num{98.014} & \num{102.224} & \num{99.092}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{122.145} & \num{125.192} & \num{114.498} & \num{114.306} & \textcolor{red}{\num{113.077}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{212.436} & \num{220.18} & \num{209.268} & \num{226.166} & \textcolor{red}{\num{205.698}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{267.215} & \textcolor{red}{\num{264.952}} & \num{269.018} & \num{270.071} & \num{276.412}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{155.557} & \num{145.322} & \num{139.312} & \textcolor{red}{\num{135.575}} & \num{137.003}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{302.87}} & \num{307.196} & \num{310.24} & \num{327.142} & \num{307.809}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{347.886}} & \num{355.025} & \num{356.236} & \num{369.714} & \num{365.851}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{164.401} & \num{165.101} & \textcolor{red}{\num{163.147}} & \num{167.527} & \num{164.006}\\*
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
     & asset01 & \textcolor{red}{\num{0.000542}} & \num{0.00063} & \num{0.000546} & \num{0.000547} & \num{0.000551}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000571} & \num{0.000632} & \num{0.000571} & \num{0.000572} & \textcolor{red}{\num{0.000567}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.000498}} & \num{0.000611} & \num{0.000506} & \num{0.000506} & \num{0.000507}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000983} & \textcolor{red}{\num{0.00093}} & \num{0.000999} & \num{0.000999} & \num{0.000989}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00112} & \textcolor{red}{\num{0.00105}} & \num{0.00114} & \num{0.00114} & \num{0.00113}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.000817}} & \num{0.00089} & \num{0.000834} & \num{0.000839} & \num{0.000831}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.00124} & \num{0.00148} & \num{0.00123} & \num{0.00123} & \textcolor{red}{\num{0.00122}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00379} & \num{0.00417} & \num{0.00382} & \num{0.00383} & \textcolor{red}{\num{0.00367}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00658} & \num{0.0067} & \num{0.00661} & \num{0.00661} & \textcolor{red}{\num{0.00636}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.00127} & \num{0.00154} & \num{0.00126} & \num{0.00126} & \textcolor{red}{\num{0.00125}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{0.00677}} & \num{0.00769} & \num{0.00698} & \num{0.00705} & \num{0.00689}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{0.00721}} & \num{0.00802} & \num{0.00738} & \num{0.00743} & \num{0.00742}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00262} & \num{0.00286} & \num{0.00265} & \num{0.00267} & \textcolor{red}{\num{0.00261}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{0.02}} & \num{0.0213} & \num{0.02} & \num{0.02} & \num{0.0201}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0207} & \num{0.0217} & \num{0.0207} & \num{0.0207} & \textcolor{red}{\num{0.0206}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.0171}} & \num{0.0194} & \num{0.0173} & \num{0.0173} & \num{0.0174}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0253} & \textcolor{red}{\num{0.0236}} & \num{0.0256} & \num{0.0256} & \num{0.0255}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0262} & \textcolor{red}{\num{0.025}} & \num{0.0264} & \num{0.0264} & \num{0.0264}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.0238}} & \num{0.025} & \num{0.0241} & \num{0.0241} & \num{0.024}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0292} & \num{0.0314} & \num{0.0292} & \num{0.0292} & \textcolor{red}{\num{0.0291}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0529} & \num{0.0547} & \num{0.053} & \num{0.0532} & \textcolor{red}{\num{0.0514}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0665} & \num{0.0666} & \num{0.0666} & \num{0.0666} & \textcolor{red}{\num{0.0656}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.0284}} & \num{0.0329} & \num{0.0286} & \num{0.0286} & \num{0.0285}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{0.0679}} & \num{0.0708} & \num{0.0692} & \num{0.0695} & \num{0.0689}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{0.0702}} & \num{0.0747} & \num{0.071} & \num{0.0712} & \num{0.0711}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{0.0374}} & \num{0.0389} & \num{0.0376} & \num{0.0377} & \num{0.0374}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{100.377} & \num{113.547} & \textcolor{red}{\num{100.084}} & \num{100.213} & \num{101.563}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{100.503} & \num{105.622} & \num{99.901} & \num{99.985} & \textcolor{red}{\num{98.63}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{99.471}} & \num{153.156} & \num{100.342} & \num{100.143} & \num{104.963}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{97.627} & \textcolor{red}{\num{96.528}} & \num{100.097} & \num{99.949} & \num{99.375}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{99.53}} & \num{122.585} & \num{100.131} & \num{100.062} & \num{99.907}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{100.517} & \num{118.888} & \num{99.464} & \num{99.901} & \textcolor{red}{\num{99.081}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{107.484} & \num{264.334} & \textcolor{red}{\num{99.856}} & \num{101.034} & \num{106.161}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{103.375} & \num{103.006} & \num{99.849} & \num{99.916} & \textcolor{red}{\num{96.503}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{103.091} & \num{146.323} & \num{100.493} & \textcolor{red}{\num{100.069}} & \num{125.448}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{124.51} & \num{462.293} & \num{103.503} & \textcolor{red}{\num{98.831}} & \num{112.696}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{112.106} & \num{181.684} & \num{104.034} & \textcolor{red}{\num{100.85}} & \num{113.282}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{108.672} & \num{175.839} & \num{100.508} & \textcolor{red}{\num{99.825}} & \num{100.034}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{104.772} & \num{170.317} & \num{100.689} & \textcolor{red}{\num{100.065}} & \num{104.804}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{86.805}} & \num{93.732} & \num{87.216} & \num{87.333} & \num{87.557}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{91.09} & \num{96.13} & \num{91.223} & \num{91.291} & \textcolor{red}{\num{90.887}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{79.768}} & \num{89.418} & \num{80.48} & \num{80.456} & \num{80.655}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{104.844} & \textcolor{red}{\num{100.304}} & \num{106.304} & \num{106.304} & \num{105.988}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{116.136} & \textcolor{red}{\num{115.204}} & \num{117.091} & \num{117.137} & \num{116.779}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{102.928}} & \num{108.219} & \num{104.417} & \num{104.756} & \num{104.242}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{120.298} & \num{129.117} & \num{120.144} & \num{120.193} & \textcolor{red}{\num{119.98}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{222.789} & \num{231.696} & \num{224.072} & \num{224.911} & \textcolor{red}{\num{215.923}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{283.591} & \num{282.493} & \num{283.459} & \num{283.326} & \textcolor{red}{\num{280.118}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{117.095}} & \num{138.501} & \num{117.873} & \num{118.008} & \num{117.325}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{281.835}} & \num{294.044} & \num{286.426} & \num{287.974} & \num{285.142}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{318.413}} & \num{338.037} & \num{321.325} & \num{322.319} & \num{321.875}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{160.466}} & \num{168.075} & \num{161.669} & \num{162.001} & \num{160.539}\\*
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
    MSE & \num{0.00556} & \num{0.00551} & \num{0.00555} & \num{0.00546} & \textcolor{red}{\num{0.00544}}\\
    MAE & \num{0.0498} & \num{0.049} & \num{0.0495} & \num{0.0489} & \textcolor{red}{\num{0.0489}}\\
    MAPE & \textcolor{red}{\num{832.491}} & \num{1197.967} & \num{1285.703} & \num{1032.746} & \num{928.581}\\
    MASE & \num{127.379} & \num{125.626} & \num{126.23} & \num{125.226} & \textcolor{red}{\num{125.124}}\\
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
    MSE & \num{0.0161} & \num{0.0157} & \num{0.0162} & \num{0.0156} & \textcolor{red}{\num{0.0152}}\\
    MAE & \num{0.0858} & \num{0.0842} & \num{0.0849} & \num{0.0824} & \textcolor{red}{\num{0.0812}}\\
    MAPE & \num{966.732} & \textcolor{red}{\num{632.304}} & \num{1666.753} & \num{1897.437} & \num{1923.914}\\
    MASE & \num{224.693} & \num{218.468} & \num{221.869} & \num{215.037} & \textcolor{red}{\num{211.513}}\\
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
    MSE & \num{0.0275} & \num{0.0326} & \textcolor{red}{\num{0.0269}} & \num{0.0273} & \num{0.0278}\\
    MAE & \num{0.112} & \num{0.12} & \textcolor{red}{\num{0.11}} & \num{0.112} & \num{0.113}\\
    MAPE & \num{8276.459} & \num{9488.962} & \num{6449.123} & \num{5599.211} & \textcolor{red}{\num{5420.74}}\\
    MASE & \num{287.199} & \num{308.432} & \textcolor{red}{\num{283.229}} & \num{286.447} & \num{288.351}\\
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
    MSE & \num{0.00155} & \num{0.00153} & \textcolor{red}{\num{0.00148}} & \num{0.0015} & \num{0.00149}\\
    MAE & \num{0.0269} & \num{0.0269} & \num{0.0264} & \num{0.0266} & \textcolor{red}{\num{0.0264}}\\
    MAPE & \textcolor{red}{\num{220.384}} & \num{374.865} & \num{245.973} & \num{299.627} & \num{264.201}\\
    MASE & \num{117.281} & \num{117.443} & \textcolor{red}{\num{114.968}} & \num{115.479} & \num{115.207}\\
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
    MSE & \num{0.00222} & \num{0.0023} & \num{0.00221} & \num{0.00228} & \textcolor{red}{\num{0.00221}}\\
    MAE & \num{0.0327} & \num{0.0335} & \num{0.0326} & \num{0.0331} & \textcolor{red}{\num{0.0325}}\\
    MAPE & \num{185.637} & \num{318.597} & \num{158.152} & \textcolor{red}{\num{141.052}} & \num{211.248}\\
    MASE & \num{140.029} & \num{145.43} & \num{139.907} & \num{142.834} & \textcolor{red}{\num{139.725}}\\
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
    MSE & \num{0.00237} & \num{0.00251} & \num{0.00237} & \num{0.00237} & \textcolor{red}{\num{0.00236}}\\
    MAE & \num{0.0342} & \num{0.0352} & \num{0.0342} & \num{0.0342} & \textcolor{red}{\num{0.0341}}\\
    MAPE & \num{104.727} & \num{308.936} & \num{102.22} & \textcolor{red}{\num{101.013}} & \num{123.905}\\
    MASE & \num{148.555} & \num{153.374} & \num{148.565} & \num{148.545} & \textcolor{red}{\num{148.202}}\\
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
    MSE & \num{0.00163} & \num{0.00161} & \num{0.00156} & \num{0.00158} & \textcolor{red}{\num{0.00156}}\\
    MAE & \num{0.0286} & \num{0.0286} & \num{0.028} & \num{0.0282} & \textcolor{red}{\num{0.0279}}\\
    MAPE & \textcolor{red}{\num{232.209}} & \num{250.488} & \num{245.03} & \num{242.806} & \num{249.572}\\
    MASE & \num{124.406} & \num{122.674} & \num{121.179} & \num{121.952} & \textcolor{red}{\num{120.652}}\\
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
    MSE & \num{0.00264} & \num{0.0027} & \textcolor{red}{\num{0.00261}} & \num{0.00275} & \num{0.00265}\\
    MAE & \num{0.0368} & \num{0.0373} & \textcolor{red}{\num{0.0365}} & \num{0.0375} & \num{0.0367}\\
    MAPE & \num{219.953} & \num{231.596} & \num{153.362} & \textcolor{red}{\num{126.704}} & \num{184.067}\\
    MASE & \num{164.401} & \num{165.101} & \textcolor{red}{\num{163.147}} & \num{167.527} & \num{164.006}\\
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
    MSE & \num{0.00262} & \num{0.00286} & \num{0.00265} & \num{0.00267} & \textcolor{red}{\num{0.00261}}\\
    MAE & \textcolor{red}{\num{0.0374}} & \num{0.0389} & \num{0.0376} & \num{0.0377} & \num{0.0374}\\
    MAPE & \num{104.772} & \num{170.317} & \num{100.689} & \textcolor{red}{\num{100.065}} & \num{104.804}\\
    MASE & \textcolor{red}{\num{160.466}} & \num{168.075} & \num{161.669} & \num{162.001} & \num{160.539}\\
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
