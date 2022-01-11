Simulating Minnesota VAR
================
Young Geun Kim
10 Jan, 2022

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
    -   [Piecewise Errors](#piecewise-errors)
        -   [SMALL](#small-2)
        -   [MEDIUM](#medium-2)
        -   [LARGE](#large-2)
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
#> [1]  0.0553  0.0523  0.0886
#> 
#> Setting for 'lambda':
#> [1]  0.117
#> 
#> Setting for 'delta':
#> [1]  0.206  0.258  0.356
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
#> [1]  0.0392  0.0406  0.0511  0.0558  0.0652  0.0585  0.0638  0.1026  0.0889
#> 
#> Setting for 'lambda':
#> [1]  0.0743
#> 
#> Setting for 'delta':
#> [1]  0.0795  0.2079  0.5019  0.5022  0.0100  0.5174  0.0810  0.4651  0.3969
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
#>  [1]  0.0379  0.0405  0.0437  0.0557  0.0669  0.0423  0.0645  0.0607  0.0892
#> [10]  0.0788  0.0727  0.0993
#> 
#> Setting for 'lambda':
#> [1]  0.01
#> 
#> Setting for 'delta':
#>  [1]  0.4159  0.3647  0.4001  0.4197  0.3058  0.4240  0.5448  0.3797  0.1709
#> [10]  0.1409  0.4928  0.0796
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
#> [1]  0.0560  0.0511  0.0964
#> 
#> Setting for 'lambda':
#> [1]  0.145
#> 
#> Setting for 'delta':
#> [1]  0.204  0.253  0.367
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
#> [1]  0.0392  0.0401  0.0506  0.0541  0.0667  0.0581  0.0608  0.1102  0.0976
#> 
#> Setting for 'lambda':
#> [1]  0.0684
#> 
#> Setting for 'delta':
#> [1]  0.0673  0.2157  0.5064  0.5055  0.0282  0.5180  0.0842  0.4657  0.3963
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
#>  [1]  0.0380  0.0405  0.0437  0.0558  0.0667  0.0422  0.0643  0.0602  0.0891
#> [10]  0.0784  0.0726  0.0997
#> 
#> Setting for 'lambda':
#> [1]  0.01
#> 
#> Setting for 'delta':
#>  [1]  0.4185  0.3646  0.4002  0.4195  0.3104  0.4200  0.5420  0.3839  0.1743
#> [10]  0.1342  0.4958  0.0816
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
#> [1]  0.0565  0.0514  0.0918
#> 
#> Setting for 'lambda':
#> [1]  0.13
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.195  0.248  0.357
#> 
#> Setting for 'weekly':
#> [1]  0.0844  0.0100  0.1311
#> 
#> Setting for 'monthly':
#> [1]  0.0157  0.0623  0.0100
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
#> [1]  0.0393  0.0403  0.0509  0.0543  0.0665  0.0576  0.0609  0.1118  0.0957
#> 
#> Setting for 'lambda':
#> [1]  0.0658
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.0442  0.2077  0.5028  0.5052  0.0220  0.5164  0.0836  0.4542  0.3934
#> 
#> Setting for 'weekly':
#> [1]  0.2390  0.0636  0.0203  0.0100  0.0100  0.0100  0.0100  0.0804  0.0100
#> 
#> Setting for 'monthly':
#> [1]  0.0100  0.0100  0.0227  0.0100  0.0100  0.0100  0.0100  0.0100  0.0896
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
#>  [1]  0.0381  0.0403  0.0437  0.0558  0.0667  0.0423  0.0641  0.0601  0.0890
#> [10]  0.0782  0.0725  0.0996
#> 
#> Setting for 'lambda':
#> [1]  0.01
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#>  [1]  0.4145  0.3590  0.3880  0.4094  0.3085  0.4158  0.5386  0.3753  0.1658
#> [10]  0.1113  0.4785  0.0742
#> 
#> Setting for 'weekly':
#>  [1]  0.0100  0.0219  0.0626  0.0415  0.0100  0.0100  0.0100  0.0100  0.0100
#> [10]  0.1422  0.0577  0.0100
#> 
#> Setting for 'monthly':
#>  [1]  0.0749  0.0100  0.0100  0.0872  0.0100  0.0100  0.0100  0.1292  0.1784
#> [10]  0.1740  0.0919  0.0100
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
    \hspace{1em} & BVAR & $\sigma$ & 0.055 & 0.052 & 0.089 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.117 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.206 & 0.258 & 0.356 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.056 & 0.051 & 0.096 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.145 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.204 & 0.253 & 0.367 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.056 & 0.051 & 0.092 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.130 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.195 & 0.248 & 0.357 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.084 & 0.010 & 0.131 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.016 & 0.062 & 0.010 &  &  &  &  &  &  &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{MEDIUM}}\\
    \hspace{1em} & BVAR & $\sigma$ & 0.039 & 0.041 & 0.051 & 0.056 & 0.065 & 0.059 & 0.064 & 0.103 & 0.089 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.074 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.079 & 0.208 & 0.502 & 0.502 & 0.010 & 0.517 & 0.081 & 0.465 & 0.397 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.039 & 0.040 & 0.051 & 0.054 & 0.067 & 0.058 & 0.061 & 0.110 & 0.098 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.068 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.067 & 0.216 & 0.506 & 0.506 & 0.028 & 0.518 & 0.084 & 0.466 & 0.396 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.039 & 0.040 & 0.051 & 0.054 & 0.067 & 0.058 & 0.061 & 0.112 & 0.096 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.066 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.044 & 0.208 & 0.503 & 0.505 & 0.022 & 0.516 & 0.084 & 0.454 & 0.393 &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.239 & 0.064 & 0.020 & 0.010 & 0.010 & 0.010 & 0.010 & 0.080 & 0.010 &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.010 & 0.010 & 0.023 & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.090 &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{LARGE}}\\
    \hspace{1em} & BVAR & $\sigma$ & 0.038 & 0.040 & 0.044 & 0.056 & 0.067 & 0.042 & 0.065 & 0.061 & 0.089 & 0.079 & 0.073 & 0.099\\

    \hspace{1em}\hspace{1em}\hspace{1em} &  & $\lambda$ & 0.010 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.416 & 0.365 & 0.400 & 0.420 & 0.306 & 0.424 & 0.545 & 0.380 & 0.171 & 0.141 & 0.493 & 0.080\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.038 & 0.041 & 0.044 & 0.056 & 0.067 & 0.042 & 0.064 & 0.060 & 0.089 & 0.078 & 0.073 & 0.100\\

     &  & $\lambda$ & 0.010 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.419 & 0.365 & 0.400 & 0.419 & 0.310 & 0.420 & 0.542 & 0.384 & 0.174 & 0.134 & 0.496 & 0.082\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.038 & 0.040 & 0.044 & 0.056 & 0.067 & 0.042 & 0.064 & 0.060 & 0.089 & 0.078 & 0.073 & 0.100\\

     &  & $\lambda$ & 0.010 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.415 & 0.359 & 0.388 & 0.409 & 0.308 & 0.416 & 0.539 & 0.375 & 0.166 & 0.111 & 0.479 & 0.074\\

    \hspace{1em} &  & $w_i$ & 0.010 & 0.022 & 0.063 & 0.042 & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.142 & 0.058 & 0.010\\

    \hspace{1em} &  & $m_i$ & 0.075 & 0.010 & 0.010 & 0.087 & 0.010 & 0.010 & 0.010 & 0.129 & 0.178 & 0.174 & 0.092 & 0.010\\*
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

    \caption{\label{tab:dgp2result}Out-of-sample forecasting performance measures for DGP2 with VAR(5) model as benchmark.}
    \centering
    \resizebox{\linewidth}{!}{
    \begin{tabular}[t]{cc|ccc|ccc|ccc|}
    \toprule
    \multicolumn{2}{c}{ } & \multicolumn{3}{c}{RMAFE} & \multicolumn{3}{c}{RMSFE} & \multicolumn{3}{c}{RMASE} \\
    \cmidrule(l{3pt}r{3pt}){3-5} \cmidrule(l{3pt}r{3pt}){6-8} \cmidrule(l{3pt}r{3pt}){9-11}
    \rotatebox{0}{} & \rotatebox{0}{} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$}\\
    \midrule
     & VHAR & \textcolor{black}{\num{.994}} & \textcolor{black}{\num{1.018}} & \textcolor{black}{\num{1.022}} & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{1.035}} & \textcolor{black}{\num{1.063}} & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{1.014}} & \textcolor{black}{\num{1.026}}\\

     & BVAR & \textcolor{red}{\num{.975}} & \textcolor{black}{\num{.961}} & \textcolor{red}{\num{.992}} & \textcolor{black}{\num{.975}} & \textcolor{black}{\num{.958}} & \textcolor{red}{\num{.987}} & \textcolor{red}{\num{.972}} & \textcolor{black}{\num{.960}} & \textcolor{red}{\num{.992}}\\

     & BVHAR-S & \textcolor{black}{\num{.975}} & \textcolor{black}{\num{.961}} & \textcolor{black}{\num{.994}} & \textcolor{black}{\num{.973}} & \textcolor{black}{\num{.953}} & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{.974}} & \textcolor{black}{\num{.960}} & \textcolor{black}{\num{.994}}\\

    \multirow{-4}{*}{\centering\arraybackslash SMALL} & BVHAR-L & \textcolor{black}{\num{.975}} & \textcolor{red}{\num{.957}} & \textcolor{black}{\num{.992}} & \textcolor{red}{\num{.973}} & \textcolor{red}{\num{.945}} & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{.974}} & \textcolor{red}{\num{.956}} & \textcolor{black}{\num{.993}}\\
    \cmidrule{1-11}
     & VHAR & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{1.102}} & \textcolor{black}{\num{1.080}} & \textcolor{black}{\num{.993}} & \textcolor{black}{\num{1.187}} & \textcolor{black}{\num{1.132}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{1.108}} & \textcolor{black}{\num{1.087}}\\

     & BVAR & \textcolor{black}{\num{.973}} & \textcolor{black}{\num{.974}} & \textcolor{black}{\num{1.000}} & \textcolor{red}{\num{.983}} & \textcolor{red}{\num{.963}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{.976}} & \textcolor{black}{\num{.976}} & \textcolor{black}{\num{1.000}}\\

     & BVHAR-S & \textcolor{red}{\num{.972}} & \textcolor{red}{\num{.970}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{.992}} & \textcolor{black}{\num{.963}} & \textcolor{black}{\num{.999}} & \textcolor{red}{\num{.975}} & \textcolor{red}{\num{.973}} & \textcolor{black}{\num{.999}}\\

    \multirow{-4}{*}{\centering\arraybackslash MEDIUM} & BVHAR-L & \textcolor{black}{\num{.973}} & \textcolor{black}{\num{.971}} & \textcolor{red}{\num{.999}} & \textcolor{black}{\num{.995}} & \textcolor{black}{\num{.967}} & \textcolor{red}{\num{.999}} & \textcolor{black}{\num{.975}} & \textcolor{black}{\num{.975}} & \textcolor{red}{\num{.999}}\\
    \cmidrule{1-11}
     & VHAR & \textcolor{black}{\num{1.002}} & \textcolor{black}{\num{1.125}} & \textcolor{black}{\num{1.070}} & \textcolor{black}{\num{1.009}} & \textcolor{black}{\num{1.230}} & \textcolor{black}{\num{1.135}} & \textcolor{black}{\num{.998}} & \textcolor{black}{\num{1.122}} & \textcolor{black}{\num{1.074}}\\

     & BVAR & \textcolor{black}{\num{.977}} & \textcolor{red}{\num{.980}} & \textcolor{black}{\num{1.000}} & \textcolor{red}{\num{.977}} & \textcolor{red}{\num{.964}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{.975}} & \textcolor{red}{\num{.983}} & \textcolor{black}{\num{1.000}}\\

     & BVHAR-S & \textcolor{black}{\num{.976}} & \textcolor{black}{\num{.984}} & \textcolor{red}{\num{.999}} & \textcolor{black}{\num{.983}} & \textcolor{black}{\num{.970}} & \textcolor{red}{\num{.999}} & \textcolor{black}{\num{.972}} & \textcolor{black}{\num{.988}} & \textcolor{red}{\num{.999}}\\

    \multirow{-4}{*}{\centering\arraybackslash LARGE} & BVHAR-L & \textcolor{red}{\num{.976}} & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{.972}} & \textcolor{black}{\num{.999}} & \textcolor{red}{\num{.971}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{.999}}\\
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
     & asset01 & \num{0.000966} & \num{0.000946} & \num{0.000929} & \textcolor{red}{\num{0.000924}} & \num{0.000925}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000894} & \num{0.000915} & \textcolor{red}{\num{0.000857}} & \num{0.000889} & \num{0.000889}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0148} & \num{0.0148} & \num{0.0145} & \num{0.0144} & \textcolor{red}{\num{0.0144}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00556} & \num{0.00555} & \num{0.00542} & \num{0.00541} & \textcolor{red}{\num{0.00541}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0252} & \num{0.0248} & \num{0.0248} & \textcolor{red}{\num{0.0246}} & \num{0.0247}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0237} & \num{0.0244} & \textcolor{red}{\num{0.0234}} & \num{0.024} & \num{0.024}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.1012} & \num{0.1} & \num{0.0981} & \num{0.0978} & \textcolor{red}{\num{0.0977}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.05} & \num{0.0497} & \textcolor{red}{\num{0.0488}} & \num{0.0488} & \num{0.0488}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{754.746} & \num{704.204} & \num{686.248} & \num{687.941} & \textcolor{red}{\num{685.501}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{127.242} & \num{132.074} & \textcolor{red}{\num{124.312}} & \num{129.768} & \num{129.874}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{356.112} & \num{357.666} & \textcolor{red}{\num{347.871}} & \num{354.675} & \num{357.83}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{412.7} & \num{397.981} & \textcolor{red}{\num{386.144}} & \num{390.795} & \num{391.068}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{60.38} & \num{60.165} & \num{59.552} & \textcolor{red}{\num{59.457}} & \num{59.622}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{55.499} & \num{57.854} & \textcolor{red}{\num{54.702}} & \num{56.55} & \num{56.564}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{240.241} & \num{234.671} & \num{231.979} & \num{230.861} & \textcolor{red}{\num{230.756}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{118.707} & \num{117.563} & \textcolor{red}{\num{115.411}} & \num{115.623} & \num{115.647}\\*
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
     & asset01 & \num{0.00196} & \num{0.00202} & \num{0.00187} & \num{0.00187} & \textcolor{red}{\num{0.00186}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00181} & \num{0.00183} & \textcolor{red}{\num{0.00173}} & \num{0.00174} & \num{0.00174}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0364} & \num{0.0377} & \num{0.0349} & \num{0.0346} & \textcolor{red}{\num{0.0343}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0134} & \num{0.0138} & \num{0.0128} & \num{0.0128} & \textcolor{red}{\num{0.0126}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0365} & \num{0.0368} & \num{0.0357} & \num{0.0356} & \textcolor{red}{\num{0.0356}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0345} & \num{0.0345} & \textcolor{red}{\num{0.0335}} & \num{0.0336} & \num{0.0336}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.155} & \num{0.159} & \num{0.148} & \num{0.148} & \textcolor{red}{\num{0.147}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0753} & \num{0.0766} & \num{0.0724} & \num{0.0723} & \textcolor{red}{\num{0.072}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{204.394} & \num{166.659} & \textcolor{red}{\num{143.548}} & \num{151.965} & \num{154.762}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{130.905} & \num{137.642} & \textcolor{red}{\num{119.506}} & \num{120.878} & \num{121.142}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{235.678} & \num{230.253} & \textcolor{red}{\num{203.394}} & \num{209.49} & \num{211.412}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{190.326} & \num{178.185} & \textcolor{red}{\num{155.483}} & \num{160.778} & \num{162.439}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{85.824} & \num{86.256} & \num{83.459} & \num{83.404} & \textcolor{red}{\num{83.191}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{84.389} & \num{83.554} & \textcolor{red}{\num{81.435}} & \num{81.828} & \num{81.621}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{378.975} & \num{387.167} & \num{362.471} & \num{362.203} & \textcolor{red}{\num{360.187}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{183.063} & \num{185.659} & \num{175.788} & \num{175.812} & \textcolor{red}{\num{175}}\\*
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
     & asset01 & \num{0.00235} & \num{0.00243} & \textcolor{red}{\num{0.00233}} & \num{0.00234} & \num{0.00234}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00172} & \num{0.00179} & \textcolor{red}{\num{0.00172}} & \num{0.00173} & \num{0.00173}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0392} & \num{0.0417} & \textcolor{red}{\num{0.0387}} & \num{0.0388} & \num{0.0387}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0144} & \num{0.0153} & \textcolor{red}{\num{0.0142}} & \num{0.0143} & \num{0.0142}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0388} & \num{0.0398} & \textcolor{red}{\num{0.0386}} & \num{0.0387} & \num{0.0386}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0337} & \textcolor{red}{\num{0.0335}} & \num{0.0336} & \num{0.0337} & \num{0.0337}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.159} & \num{0.163} & \num{0.157} & \num{0.157} & \textcolor{red}{\num{0.157}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.077} & \num{0.0787} & \textcolor{red}{\num{0.0764}} & \num{0.0766} & \num{0.0764}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{105.132} & \num{201.901} & \textcolor{red}{\num{100.165}} & \num{101.14} & \num{100.667}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{102.368} & \num{107.859} & \textcolor{red}{\num{101.106}} & \num{102.193} & \num{102.154}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{107.607} & \num{139.898} & \num{100.942} & \num{100.852} & \textcolor{red}{\num{100.04}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{105.036} & \num{149.886} & \textcolor{red}{\num{100.738}} & \num{101.395} & \num{100.954}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{88.379} & \num{91.079} & \textcolor{red}{\num{87.951}} & \num{88.161} & \num{88.074}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{82.105} & \textcolor{red}{\num{81.128}} & \num{81.842} & \num{82.18} & \num{82.192}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{378.665} & \num{391.093} & \num{375.119} & \num{375.586} & \textcolor{red}{\num{374.782}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{183.049} & \num{187.767} & \textcolor{red}{\num{181.638}} & \num{181.976} & \num{181.683}\\*
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
     & asset01 & \num{0.000385} & \num{0.000375} & \num{0.000351} & \textcolor{red}{\num{0.000351}} & \num{0.000352}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000237} & \num{0.000239} & \num{0.000217} & \num{0.000216} & \textcolor{red}{\num{0.000215}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000349} & \num{0.000347} & \num{0.000331} & \textcolor{red}{\num{0.000327}} & \num{0.000328}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000513} & \num{0.000539} & \num{0.000506} & \num{0.000506} & \textcolor{red}{\num{0.000505}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.00097}} & \num{0.001015} & \num{0.000979} & \num{0.000999} & \num{0.000998}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000853} & \num{0.000843} & \textcolor{red}{\num{0.000842}} & \num{0.000863} & \num{0.000862}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000526} & \num{0.000526} & \textcolor{red}{\num{0.000515}} & \num{0.000522} & \num{0.000523}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00603} & \num{0.006} & \textcolor{red}{\num{0.00582}} & \num{0.00588} & \num{0.0059}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00414} & \num{0.00419} & \textcolor{red}{\num{0.00413}} & \num{0.00427} & \num{0.00427}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00155} & \num{0.00156} & \textcolor{red}{\num{0.00152}} & \num{0.00155} & \num{0.00155}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0153} & \num{0.0151} & \num{0.0146} & \textcolor{red}{\num{0.0146}} & \num{0.0146}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0127} & \num{0.0125} & \num{0.012} & \num{0.0119} & \textcolor{red}{\num{0.0119}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0152} & \num{0.0152} & \num{0.0148} & \textcolor{red}{\num{0.0148}} & \num{0.0148}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.0175}} & \num{0.018} & \num{0.0175} & \num{0.0176} & \num{0.0175}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.024}} & \num{0.0246} & \num{0.0242} & \num{0.0246} & \num{0.0246}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0239} & \textcolor{red}{\num{0.0238}} & \num{0.0238} & \num{0.0241} & \num{0.024}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0177} & \num{0.0174} & \textcolor{red}{\num{0.017}} & \num{0.017} & \num{0.017}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0641} & \num{0.064} & \textcolor{red}{\num{0.0629}} & \num{0.0631} & \num{0.0632}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0531} & \textcolor{red}{\num{0.0527}} & \num{0.0532} & \num{0.0537} & \num{0.0537}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.027} & \num{0.027} & \textcolor{red}{\num{0.0267}} & \num{0.0268} & \num{0.0268}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{123.46} & \num{118.783} & \num{108.984} & \num{108.283} & \textcolor{red}{\num{107.963}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{222.604} & \num{198.332} & \num{188.309} & \textcolor{red}{\num{187.58}} & \num{190.791}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{450.391} & \num{460.003} & \num{369.23} & \num{360.303} & \textcolor{red}{\num{360.102}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{415.742} & \num{460.443} & \num{335.898} & \num{333.275} & \textcolor{red}{\num{331.852}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{131.076} & \num{114.208} & \num{106.99} & \num{100.263} & \textcolor{red}{\num{99.922}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{187.677} & \textcolor{red}{\num{174.319}} & \num{174.852} & \num{175.316} & \num{175.625}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{242.926} & \num{170.739} & \num{152.812} & \num{110.79} & \textcolor{red}{\num{109.965}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{1264.6} & \num{1791.341} & \num{1095.905} & \num{1081.057} & \textcolor{red}{\num{1078.78}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{4501.105} & \num{5326.287} & \num{4534.384} & \num{4469.972} & \textcolor{red}{\num{4453.796}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{837.731} & \num{979.384} & \num{785.263} & \num{769.649} & \textcolor{red}{\num{767.644}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{60.215} & \num{60.907} & \num{57.951} & \num{58.058} & \textcolor{red}{\num{57.886}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{49.977} & \num{48.893} & \num{46.623} & \num{46.294} & \textcolor{red}{\num{46.228}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{60.866} & \num{61.107} & \num{59.077} & \textcolor{red}{\num{59.019}} & \num{59.053}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{68.194}} & \num{70.922} & \num{68.537} & \num{68.892} & \num{68.819}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{94.161}} & \num{95.189} & \num{94.808} & \num{96.346} & \num{96.375}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{94.844}} & \num{95.228} & \num{95.774} & \num{97.308} & \num{97.29}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{69.926} & \num{68.551} & \textcolor{red}{\num{67.715}} & \num{68.075} & \num{68.135}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{248.855} & \num{247.082} & \textcolor{red}{\num{240.009}} & \num{240.84} & \num{241.307}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{210.14}} & \num{212.358} & \num{210.983} & \num{212.942} & \num{213.096}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{106.353} & \num{106.693} & \textcolor{red}{\num{104.609}} & \num{105.308} & \num{105.354}\\*
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
     & asset01 & \num{0.000368} & \num{0.000375} & \num{0.000361} & \textcolor{red}{\num{0.00036}} & \num{0.000361}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000233} & \num{0.000247} & \num{0.000229} & \textcolor{red}{\num{0.000228}} & \num{0.000229}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.00048}} & \num{0.000513} & \num{0.000482} & \num{0.000481} & \num{0.000484}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000746} & \num{0.000805} & \num{0.000717} & \num{0.000716} & \textcolor{red}{\num{0.000714}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.00101}} & \num{0.00101} & \num{0.00101} & \num{0.00101} & \num{0.00101}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.00135} & \textcolor{red}{\num{0.00126}} & \num{0.00136} & \num{0.00136} & \num{0.00135}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{0.000524}} & \num{0.000541} & \num{0.000535} & \num{0.000534} & \num{0.000534}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00666} & \num{0.00679} & \num{0.00666} & \num{0.00666} & \textcolor{red}{\num{0.00665}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.0054}} & \num{0.00559} & \num{0.00546} & \num{0.00544} & \num{0.00544}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{0.00186}} & \num{0.0019} & \num{0.00187} & \num{0.00187} & \num{0.00186}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0151} & \num{0.0152} & \num{0.0149} & \textcolor{red}{\num{0.0149}} & \num{0.015}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0126} & \num{0.0129} & \num{0.0125} & \textcolor{red}{\num{0.0125}} & \num{0.0125}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0177} & \num{0.0184} & \num{0.0178} & \textcolor{red}{\num{0.0177}} & \num{0.0178}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0222} & \num{0.0229} & \num{0.0218} & \num{0.0218} & \textcolor{red}{\num{0.0218}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0251} & \num{0.0249} & \num{0.0249} & \num{0.0249} & \textcolor{red}{\num{0.0249}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0292} & \textcolor{red}{\num{0.0287}} & \num{0.0292} & \num{0.0292} & \num{0.0292}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0172} & \num{0.0172} & \textcolor{red}{\num{0.017}} & \num{0.017} & \num{0.017}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{0.0665}} & \num{0.0673} & \num{0.0667} & \num{0.0667} & \num{0.0666}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.0581}} & \num{0.06} & \num{0.0585} & \num{0.0585} & \num{0.0585}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0293} & \num{0.0297} & \num{0.0293} & \num{0.0292} & \textcolor{red}{\num{0.0292}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{107.293} & \num{111.705} & \textcolor{red}{\num{99.751}} & \num{100.165} & \num{102.108}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{113.448} & \num{113.99} & \textcolor{red}{\num{100.041}} & \num{101.196} & \num{102.458}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{148.774} & \num{164.707} & \num{135.078} & \textcolor{red}{\num{133.577}} & \num{138.998}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{138.943} & \num{149.953} & \textcolor{red}{\num{100.263}} & \num{108.216} & \num{108.433}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{112.233} & \num{102.073} & \num{100.588} & \num{99.324} & \textcolor{red}{\num{99.281}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{104.482} & \num{113.695} & \num{100.089} & \textcolor{red}{\num{99.736}} & \num{100.325}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{135.72} & \num{125.567} & \textcolor{red}{\num{97.196}} & \num{101.632} & \num{103.428}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{158.848} & \num{1076.263} & \num{141.17} & \textcolor{red}{\num{116}} & \num{177.309}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{561.789} & \num{520.318} & \textcolor{red}{\num{138.447}} & \num{143.972} & \num{158.347}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{175.726} & \num{275.364} & \num{112.514} & \textcolor{red}{\num{111.535}} & \num{121.188}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{59.058} & \num{58.875} & \num{58.485} & \textcolor{red}{\num{58.33}} & \num{58.536}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{48.68} & \num{50.157} & \num{47.861} & \textcolor{red}{\num{47.844}} & \num{48.013}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{70.398}} & \num{74.022} & \num{71.152} & \num{71.127} & \num{71.223}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{87.018} & \num{87.66} & \num{84.765} & \num{84.516} & \textcolor{red}{\num{84.348}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{99.314} & \textcolor{red}{\num{97.77}} & \num{98.629} & \num{98.471} & \num{98.475}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{104.223}} & \num{105.269} & \num{105.103} & \num{105.207} & \num{105.079}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{68.69} & \textcolor{red}{\num{68.523}} & \num{68.701} & \num{68.821} & \num{68.847}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{269.872}} & \num{275.194} & \num{271.999} & \num{272.248} & \num{271.353}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{226.08}} & \num{239.785} & \num{228.464} & \num{228.536} & \num{228.824}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{114.815}} & \num{117.473} & \num{115.018} & \num{115.011} & \num{114.967}\\*
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
     & asset01 & \num{0.000387} & \textcolor{red}{\num{0.000386}} & \num{0.000387} & \num{0.000387} & \num{0.000387}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000232} & \textcolor{red}{\num{0.00023}} & \num{0.000232} & \num{0.000232} & \num{0.000232}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00042} & \num{0.000427} & \num{0.00042} & \num{0.00042} & \textcolor{red}{\num{0.000419}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000792} & \textcolor{red}{\num{0.000775}} & \num{0.000792} & \num{0.000792} & \num{0.000791}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.000894} & \textcolor{red}{\num{0.000891}} & \num{0.000894} & \num{0.000894} & \num{0.000894}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.00136} & \textcolor{red}{\num{0.00131}} & \num{0.00136} & \num{0.00136} & \num{0.00136}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000491} & \textcolor{red}{\num{0.000482}} & \num{0.000491} & \num{0.000491} & \num{0.000491}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00622} & \textcolor{red}{\num{0.00617}} & \num{0.00622} & \num{0.00622} & \num{0.00622}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00544} & \num{0.00557} & \num{0.00544} & \textcolor{red}{\num{0.00544}} & \num{0.00544}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0018} & \num{0.0018} & \num{0.0018} & \textcolor{red}{\num{0.0018}} & \num{0.0018}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0155} & \num{0.0156} & \num{0.0155} & \num{0.0155} & \textcolor{red}{\num{0.0155}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0126} & \textcolor{red}{\num{0.0125}} & \num{0.0126} & \num{0.0126} & \num{0.0126}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0163} & \num{0.0166} & \num{0.0163} & \num{0.0163} & \textcolor{red}{\num{0.0163}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0233} & \textcolor{red}{\num{0.0228}} & \num{0.0233} & \num{0.0233} & \num{0.0233}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0234} & \textcolor{red}{\num{0.0233}} & \num{0.0234} & \num{0.0234} & \num{0.0234}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0292} & \textcolor{red}{\num{0.0285}} & \num{0.0292} & \num{0.0292} & \num{0.0292}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0159} & \textcolor{red}{\num{0.0158}} & \num{0.0159} & \num{0.0159} & \num{0.0159}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0631} & \textcolor{red}{\num{0.063}} & \num{0.0632} & \num{0.0631} & \num{0.0631}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.0586}} & \num{0.0594} & \num{0.0586} & \num{0.0586} & \num{0.0587}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0287} & \textcolor{red}{\num{0.0286}} & \num{0.0287} & \num{0.0287} & \num{0.0287}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{99.999} & \num{111.586} & \num{100} & \num{100.001} & \textcolor{red}{\num{99.993}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{100.014} & \num{102.676} & \textcolor{red}{\num{100}} & \num{100.021} & \num{100.122}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{99.98} & \num{133.445} & \num{100.001} & \textcolor{red}{\num{99.675}} & \num{100.377}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{100.001} & \textcolor{red}{\num{98.619}} & \num{100} & \num{99.968} & \num{99.921}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{100.002} & \textcolor{red}{\num{98.632}} & \num{100} & \num{99.988} & \num{99.988}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{100.019} & \num{102.245} & \textcolor{red}{\num{100}} & \num{100.014} & \num{100.043}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{99.98}} & \num{104.997} & \num{100} & \num{100.242} & \num{100.268}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{99.002}} & \num{250.439} & \num{100.009} & \num{99.062} & \num{101.351}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{99.062}} & \num{170.72} & \num{99.997} & \num{99.793} & \num{138.383}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{99.784}} & \num{130.373} & \num{100.001} & \num{99.863} & \num{104.494}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{62.439} & \num{62.618} & \num{62.44} & \num{62.437} & \textcolor{red}{\num{62.436}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{52.717} & \textcolor{red}{\num{51.93}} & \num{52.721} & \num{52.707} & \num{52.706}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{68.964} & \num{70.183} & \num{68.962} & \num{68.977} & \textcolor{red}{\num{68.935}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{89.327} & \textcolor{red}{\num{87.374}} & \num{89.325} & \num{89.322} & \num{89.29}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{91.864} & \textcolor{red}{\num{91.458}} & \num{91.865} & \num{91.867} & \num{91.867}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{106.234} & \textcolor{red}{\num{104.096}} & \num{106.225} & \num{106.222} & \num{106.233}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{62.126} & \textcolor{red}{\num{61.352}} & \num{62.129} & \num{62.125} & \num{62.127}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{233.958} & \textcolor{red}{\num{232.022}} & \num{233.977} & \num{233.939} & \num{233.91}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{226.33}} & \num{229.917} & \num{226.34} & \num{226.338} & \num{226.635}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{110.44} & \textcolor{red}{\num{110.105}} & \num{110.443} & \num{110.437} & \num{110.46}\\*
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
     & asset01 & \num{0.000288} & \num{0.000289} & \textcolor{red}{\num{0.000273}} & \num{0.000273} & \num{0.000273}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000415} & \num{0.000399} & \textcolor{red}{\num{0.000362}} & \num{0.000362} & \num{0.000362}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000246} & \num{0.00026} & \num{0.000238} & \textcolor{red}{\num{0.000238}} & \num{0.000239}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.00043}} & \num{0.000454} & \num{0.000466} & \num{0.000466} & \num{0.000464}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.000835} & \num{0.000774} & \num{0.000762} & \num{0.000762} & \textcolor{red}{\num{0.000762}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000521} & \num{0.000515} & \num{0.000496} & \num{0.000496} & \textcolor{red}{\num{0.000496}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000712} & \num{0.000766} & \textcolor{red}{\num{0.000677}} & \num{0.000677} & \num{0.000677}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00237} & \num{0.00227} & \num{0.00216} & \num{0.00216} & \textcolor{red}{\num{0.00215}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00374} & \num{0.00375} & \num{0.00367} & \num{0.00367} & \textcolor{red}{\num{0.00367}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.000966} & \num{0.000956} & \num{0.000873} & \textcolor{red}{\num{0.000872}} & \num{0.000874}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.00407} & \num{0.0038} & \num{0.00367} & \num{0.00367} & \textcolor{red}{\num{0.00364}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.0051} & \num{0.0049} & \num{0.00487} & \textcolor{red}{\num{0.00487}} & \num{0.00488}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00164} & \num{0.00159} & \num{0.00154} & \num{0.00154} & \textcolor{red}{\num{0.00154}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0137} & \num{0.0137} & \num{0.0133} & \textcolor{red}{\num{0.0133}} & \num{0.0133}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0159} & \num{0.0157} & \textcolor{red}{\num{0.0152}} & \num{0.0152} & \num{0.0152}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0125} & \num{0.0129} & \num{0.012} & \textcolor{red}{\num{0.012}} & \num{0.0121}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.0171}} & \num{0.0172} & \num{0.0175} & \num{0.0175} & \num{0.0175}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0227} & \num{0.0223} & \num{0.0221} & \num{0.0221} & \textcolor{red}{\num{0.0221}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0186} & \num{0.0184} & \num{0.018} & \num{0.018} & \textcolor{red}{\num{0.018}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0222} & \num{0.0231} & \textcolor{red}{\num{0.0217}} & \num{0.0217} & \num{0.0217}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0393} & \num{0.0386} & \num{0.0371} & \num{0.0371} & \textcolor{red}{\num{0.037}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0489} & \num{0.0495} & \num{0.048} & \num{0.048} & \textcolor{red}{\num{0.048}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.0244} & \num{0.0247} & \num{0.0236} & \num{0.0237} & \textcolor{red}{\num{0.0236}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.0495} & \num{0.0485} & \num{0.0482} & \num{0.0482} & \textcolor{red}{\num{0.0478}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.0581} & \num{0.0569} & \num{0.0565} & \textcolor{red}{\num{0.0565}} & \num{0.0566}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0286} & \num{0.0285} & \num{0.0278} & \num{0.0278} & \textcolor{red}{\num{0.0277}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{177.556} & \num{179.519} & \textcolor{red}{\num{134.738}} & \num{135.075} & \num{137.131}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{199.962} & \num{181.833} & \num{162.015} & \num{161.911} & \textcolor{red}{\num{161.072}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{282.422}} & \num{476.707} & \num{432.258} & \num{432.041} & \num{431.253}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{362.563} & \textcolor{red}{\num{252.17}} & \num{289.181} & \num{289.785} & \num{290.292}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{184.243} & \num{164.984} & \textcolor{red}{\num{139.701}} & \num{140.577} & \num{139.992}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{519.735} & \num{406.793} & \num{360.065} & \textcolor{red}{\num{357.638}} & \num{361.093}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{144.955} & \num{142.03} & \num{137.738} & \num{137.485} & \textcolor{red}{\num{137.4}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{247.666} & \num{255.835} & \textcolor{red}{\num{210.635}} & \num{211.964} & \num{214.239}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{156.481} & \num{213.792} & \textcolor{red}{\num{124.604}} & \num{125.75} & \num{124.63}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{109.196} & \num{115.628} & \num{101.395} & \textcolor{red}{\num{101.297}} & \num{104.613}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{351.991} & \num{263.194} & \num{184.826} & \num{185.492} & \textcolor{red}{\num{180.198}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{133.118} & \num{128.173} & \num{106.416} & \num{106.804} & \textcolor{red}{\num{106.17}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{239.157} & \num{231.722} & \textcolor{red}{\num{198.631}} & \num{198.818} & \num{199.007}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{59.405} & \num{60.233} & \textcolor{red}{\num{57.911}} & \num{57.912} & \num{57.925}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{66.179} & \num{65.504} & \textcolor{red}{\num{63.639}} & \num{63.651} & \num{63.69}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{48.276} & \num{49.441} & \num{45.876} & \textcolor{red}{\num{45.867}} & \num{46.091}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{66.396} & \num{66.271} & \num{66.481} & \num{66.484} & \textcolor{red}{\num{66.207}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{93.502} & \textcolor{red}{\num{88.941}} & \num{90.278} & \num{90.296} & \num{90.21}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{72.926} & \num{70.969} & \num{69.714} & \num{69.646} & \textcolor{red}{\num{69.614}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{90.675} & \num{94.425} & \num{87.496} & \num{87.498} & \textcolor{red}{\num{87.47}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{160.476} & \num{152.905} & \num{150.855} & \num{150.925} & \textcolor{red}{\num{150.304}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{196.573} & \num{195.053} & \num{191.184} & \textcolor{red}{\num{191.097}} & \num{191.137}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{103.199} & \num{101.419} & \num{99.007} & \num{99.107} & \textcolor{red}{\num{98.984}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{203.706} & \textcolor{red}{\num{194.536}} & \num{198.758} & \num{198.682} & \num{197.173}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{236.89} & \num{231.175} & \num{226.908} & \textcolor{red}{\num{226.79}} & \num{227.082}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{116.517} & \num{114.239} & \num{112.342} & \num{112.33} & \textcolor{red}{\num{112.157}}\\*
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
     & asset01 & \num{0.000363} & \num{0.000393} & \textcolor{red}{\num{0.000358}} & \num{0.000358} & \num{0.000359}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000414} & \num{0.00044} & \num{0.000404} & \textcolor{red}{\num{0.000404}} & \num{0.000405}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000311} & \num{0.000342} & \num{0.000304} & \num{0.000304} & \textcolor{red}{\num{0.000303}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000575} & \textcolor{red}{\num{0.000556}} & \num{0.000586} & \num{0.000586} & \num{0.000581}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.000869} & \textcolor{red}{\num{0.000833}} & \num{0.000859} & \num{0.000858} & \num{0.000858}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.000567}} & \num{0.000588} & \num{0.00058} & \num{0.00058} & \num{0.00058}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.001063} & \num{0.00116} & \num{0.000988} & \num{0.000988} & \textcolor{red}{\num{0.000988}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00243} & \num{0.00247} & \num{0.00237} & \num{0.00237} & \textcolor{red}{\num{0.00235}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00398} & \textcolor{red}{\num{0.00396}} & \num{0.00402} & \num{0.00402} & \num{0.004}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.000926} & \num{0.000922} & \num{0.000863} & \textcolor{red}{\num{0.000863}} & \num{0.00087}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.00477} & \num{0.00477} & \num{0.00476} & \num{0.00476} & \textcolor{red}{\num{0.00471}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.00482} & \textcolor{red}{\num{0.00477}} & \num{0.0048} & \num{0.0048} & \num{0.0048}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00176} & \num{0.00177} & \num{0.00174} & \num{0.00174} & \textcolor{red}{\num{0.00173}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0157} & \num{0.0164} & \num{0.0156} & \textcolor{red}{\num{0.0156}} & \num{0.0156}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0161} & \num{0.0167} & \num{0.0161} & \textcolor{red}{\num{0.0161}} & \num{0.0161}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0134} & \num{0.0141} & \textcolor{red}{\num{0.0134}} & \num{0.0134} & \num{0.0134}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0195} & \textcolor{red}{\num{0.019}} & \num{0.0194} & \num{0.0194} & \num{0.0193}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0232} & \textcolor{red}{\num{0.0226}} & \num{0.0227} & \num{0.0227} & \num{0.0227}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.0191}} & \num{0.0195} & \num{0.0195} & \num{0.0195} & \num{0.0195}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.027} & \num{0.0279} & \num{0.0261} & \textcolor{red}{\num{0.0261}} & \num{0.0261}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0399} & \num{0.0411} & \num{0.0393} & \num{0.0393} & \textcolor{red}{\num{0.0391}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0505} & \textcolor{red}{\num{0.0492}} & \num{0.0501} & \num{0.0501} & \num{0.0501}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.0244} & \num{0.0248} & \num{0.024} & \num{0.024} & \textcolor{red}{\num{0.024}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.0565} & \num{0.0569} & \num{0.0568} & \num{0.0568} & \textcolor{red}{\num{0.0561}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.0566} & \num{0.0568} & \num{0.0565} & \num{0.0565} & \textcolor{red}{\num{0.0565}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0302} & \num{0.0304} & \num{0.03} & \num{0.03} & \textcolor{red}{\num{0.0299}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{117.21} & \num{162.152} & \textcolor{red}{\num{103.016}} & \num{103.065} & \num{105.513}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{104.865} & \num{132.573} & \textcolor{red}{\num{100.708}} & \num{100.711} & \num{101.42}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{198.712} & \num{234.218} & \num{102.996} & \textcolor{red}{\num{102.938}} & \num{111.364}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{219.502} & \num{256.358} & \textcolor{red}{\num{97.743}} & \num{97.943} & \num{110.522}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{128.334} & \num{133.438} & \num{99.953} & \num{99.967} & \textcolor{red}{\num{99.699}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{227.711} & \num{179.955} & \num{105.179} & \textcolor{red}{\num{104.502}} & \num{109.03}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{116.84} & \num{130.209} & \num{98.918} & \textcolor{red}{\num{98.822}} & \num{99.535}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{132.188} & \num{146.507} & \textcolor{red}{\num{100.839}} & \num{100.937} & \num{103.279}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{135.974} & \num{164.059} & \num{100.091} & \num{100.079} & \textcolor{red}{\num{98.615}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{112.661} & \num{113.329} & \num{99.94} & \num{99.963} & \textcolor{red}{\num{98.083}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{112.78} & \num{238.903} & \textcolor{red}{\num{101.304}} & \num{101.385} & \num{118.589}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{121.989} & \num{163.305} & \num{100.219} & \textcolor{red}{\num{100.14}} & \num{100.41}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{144.064} & \num{171.25} & \num{100.909} & \textcolor{red}{\num{100.871}} & \num{104.672}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{60.999} & \num{63.943} & \num{60.992} & \textcolor{red}{\num{60.99}} & \num{61.044}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{63.392} & \num{65.105} & \num{63.064} & \textcolor{red}{\num{63.062}} & \num{63.122}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{53.607}} & \num{56.776} & \num{53.663} & \num{53.662} & \num{53.726}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{78.734} & \textcolor{red}{\num{75.516}} & \num{77.946} & \num{77.949} & \num{77.84}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{92.778} & \num{91.631} & \num{91.607} & \num{91.598} & \textcolor{red}{\num{91.554}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{77.301}} & \num{77.876} & \num{78.882} & \num{78.878} & \num{78.874}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{106.925} & \num{111.883} & \num{104.453} & \textcolor{red}{\num{104.44}} & \num{104.483}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{160.262} & \num{164.107} & \num{159.33} & \num{159.329} & \textcolor{red}{\num{158.677}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{205.572} & \textcolor{red}{\num{196.905}} & \num{203.827} & \num{203.826} & \num{203.81}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{100.313} & \num{100.567} & \num{97.899} & \num{97.9} & \textcolor{red}{\num{97.866}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{244.064} & \num{244.761} & \num{244.365} & \num{244.333} & \textcolor{red}{\num{241.577}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{234.783} & \num{234.705} & \num{234.538} & \num{234.529} & \textcolor{red}{\num{234.5}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{123.227} & \num{123.648} & \num{122.547} & \num{122.541} & \textcolor{red}{\num{122.256}}\\*
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
     & asset01 & \textcolor{red}{\num{0.000367}} & \num{0.000367} & \num{0.000367} & \num{0.000367} & \num{0.000367}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000429} & \num{0.000448} & \num{0.000429} & \num{0.000429} & \textcolor{red}{\num{0.000429}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.000298}} & \num{0.000314} & \num{0.000298} & \num{0.000298} & \num{0.000298}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000599} & \textcolor{red}{\num{0.000582}} & \num{0.000599} & \num{0.000599} & \num{0.000596}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.000775} & \textcolor{red}{\num{0.00076}} & \num{0.000775} & \num{0.000775} & \num{0.000775}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.000614}} & \num{0.000629} & \num{0.000615} & \num{0.000615} & \num{0.000615}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.00105} & \num{0.00115} & \num{0.00105} & \num{0.00105} & \textcolor{red}{\num{0.00105}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00214} & \num{0.00224} & \num{0.00214} & \num{0.00214} & \textcolor{red}{\num{0.00213}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00419} & \num{0.00425} & \num{0.00418} & \num{0.00418} & \textcolor{red}{\num{0.00417}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.000836}} & \num{0.000865} & \num{0.000837} & \num{0.000837} & \num{0.000836}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.00424} & \num{0.00446} & \num{0.00426} & \num{0.00426} & \textcolor{red}{\num{0.00423}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{0.00442}} & \num{0.00448} & \num{0.00443} & \num{0.00443} & \num{0.00443}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00166} & \num{0.00171} & \num{0.00167} & \num{0.00167} & \textcolor{red}{\num{0.00166}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{0.0159}} & \num{0.016} & \num{0.0159} & \num{0.0159} & \num{0.0159}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0168} & \num{0.0171} & \num{0.0168} & \num{0.0168} & \textcolor{red}{\num{0.0167}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.0133}} & \num{0.0137} & \num{0.0133} & \num{0.0133} & \num{0.0133}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0197} & \textcolor{red}{\num{0.0193}} & \num{0.0197} & \num{0.0197} & \num{0.0196}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0216} & \textcolor{red}{\num{0.0214}} & \num{0.0216} & \num{0.0216} & \num{0.0216}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.0203}} & \num{0.0206} & \num{0.0204} & \num{0.0204} & \num{0.0204}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0269} & \num{0.0275} & \num{0.0269} & \num{0.0269} & \textcolor{red}{\num{0.0269}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.037} & \num{0.0383} & \num{0.037} & \num{0.037} & \textcolor{red}{\num{0.0369}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0509} & \num{0.0512} & \num{0.0509} & \num{0.0509} & \textcolor{red}{\num{0.0508}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.0236}} & \num{0.0242} & \num{0.0237} & \num{0.0237} & \num{0.0236}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.0542} & \num{0.0547} & \num{0.0543} & \num{0.0543} & \textcolor{red}{\num{0.0541}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{0.0543}} & \num{0.0548} & \num{0.0543} & \num{0.0543} & \num{0.0543}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0295} & \num{0.0299} & \num{0.0296} & \num{0.0296} & \textcolor{red}{\num{0.0295}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{100.004} & \num{122.449} & \num{100} & \textcolor{red}{\num{99.994}} & \num{100.537}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{100.012} & \num{108.979} & \num{100} & \num{100} & \textcolor{red}{\num{99.985}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{100.342} & \num{206.584} & \textcolor{red}{\num{100}} & \num{100.003} & \num{100.296}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{100.007} & \num{169.496} & \num{100} & \num{100.086} & \textcolor{red}{\num{99.171}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{100.487} & \num{105.304} & \num{100} & \num{99.997} & \textcolor{red}{\num{99.919}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{99.594}} & \num{325.33} & \num{100} & \num{100.042} & \num{99.97}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{100.018} & \num{101.88} & \num{100} & \textcolor{red}{\num{100}} & \num{100.032}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{99.991} & \num{117.199} & \num{100} & \num{99.999} & \textcolor{red}{\num{99.337}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{99.926}} & \num{111.929} & \num{100} & \num{100.002} & \num{100.222}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{99.581} & \num{114.056} & \num{100} & \num{100} & \textcolor{red}{\num{99.397}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{100.332} & \num{130.401} & \num{99.999} & \textcolor{red}{\num{99.983}} & \num{114.012}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{99.803}} & \num{106.549} & \num{100} & \num{100.002} & \num{99.941}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{100.008} & \num{143.346} & \textcolor{red}{\num{100}} & \num{100.009} & \num{101.068}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{64.801}} & \num{64.96} & \num{64.815} & \num{64.815} & \num{64.893}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{65.61} & \num{67.092} & \num{65.614} & \num{65.614} & \textcolor{red}{\num{65.602}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{56.561}} & \num{58.336} & \num{56.57} & \num{56.571} & \num{56.575}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{79.176} & \textcolor{red}{\num{78.067}} & \num{79.177} & \num{79.177} & \num{79.118}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{91.423} & \textcolor{red}{\num{90.916}} & \num{91.396} & \num{91.395} & \num{91.387}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{84.877}} & \num{85.122} & \num{84.961} & \num{84.961} & \num{84.934}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{103.234} & \num{104.903} & \num{103.243} & \num{103.244} & \textcolor{red}{\num{103.22}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{140.867} & \num{146.992} & \num{140.93} & \num{140.93} & \textcolor{red}{\num{140.525}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{205.037} & \num{206.543} & \num{205.03} & \num{205.031} & \textcolor{red}{\num{204.658}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{92.22} & \num{94.284} & \num{92.278} & \num{92.278} & \textcolor{red}{\num{92.186}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{207.529} & \num{207.855} & \num{207.965} & \num{207.962} & \textcolor{red}{\num{207.256}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{217.611}} & \num{220.451} & \num{217.716} & \num{217.716} & \num{217.686}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{117.412} & \num{118.793} & \num{117.475} & \num{117.475} & \textcolor{red}{\num{117.337}}\\*
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
    MSE & \num{0.00556} & \num{0.00555} & \num{0.00542} & \num{0.00541} & \textcolor{red}{\num{0.00541}}\\
    MAE & \num{0.05} & \num{0.0497} & \textcolor{red}{\num{0.0488}} & \num{0.0488} & \num{0.0488}\\
    MAPE & \num{412.7} & \num{397.981} & \textcolor{red}{\num{386.144}} & \num{390.795} & \num{391.068}\\
    MASE & \num{118.707} & \num{117.563} & \textcolor{red}{\num{115.411}} & \num{115.623} & \num{115.647}\\
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
    MSE & \num{0.0134} & \num{0.0138} & \num{0.0128} & \num{0.0128} & \textcolor{red}{\num{0.0126}}\\
    MAE & \num{0.0753} & \num{0.0766} & \num{0.0724} & \num{0.0723} & \textcolor{red}{\num{0.072}}\\
    MAPE & \num{190.326} & \num{178.185} & \textcolor{red}{\num{155.483}} & \num{160.778} & \num{162.439}\\
    MASE & \num{183.063} & \num{185.659} & \num{175.788} & \num{175.812} & \textcolor{red}{\num{175}}\\
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
    MSE & \num{0.0144} & \num{0.0153} & \textcolor{red}{\num{0.0142}} & \num{0.0143} & \num{0.0142}\\
    MAE & \num{0.077} & \num{0.0787} & \textcolor{red}{\num{0.0764}} & \num{0.0766} & \num{0.0764}\\
    MAPE & \num{105.036} & \num{149.886} & \textcolor{red}{\num{100.738}} & \num{101.395} & \num{100.954}\\
    MASE & \num{183.049} & \num{187.767} & \textcolor{red}{\num{181.638}} & \num{181.976} & \num{181.683}\\
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
    MSE & \num{0.00155} & \num{0.00156} & \textcolor{red}{\num{0.00152}} & \num{0.00155} & \num{0.00155}\\
    MAE & \num{0.027} & \num{0.027} & \textcolor{red}{\num{0.0267}} & \num{0.0268} & \num{0.0268}\\
    MAPE & \num{837.731} & \num{979.384} & \num{785.263} & \num{769.649} & \textcolor{red}{\num{767.644}}\\
    MASE & \num{106.353} & \num{106.693} & \textcolor{red}{\num{104.609}} & \num{105.308} & \num{105.354}\\
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
    MSE & \textcolor{red}{\num{0.00186}} & \num{0.0019} & \num{0.00187} & \num{0.00187} & \num{0.00186}\\
    MAE & \num{0.0293} & \num{0.0297} & \num{0.0293} & \num{0.0292} & \textcolor{red}{\num{0.0292}}\\
    MAPE & \num{175.726} & \num{275.364} & \num{112.514} & \textcolor{red}{\num{111.535}} & \num{121.188}\\
    MASE & \textcolor{red}{\num{114.815}} & \num{117.473} & \num{115.018} & \num{115.011} & \num{114.967}\\
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
    MSE & \num{0.0018} & \num{0.0018} & \num{0.0018} & \textcolor{red}{\num{0.0018}} & \num{0.0018}\\
    MAE & \num{0.0287} & \textcolor{red}{\num{0.0286}} & \num{0.0287} & \num{0.0287} & \num{0.0287}\\
    MAPE & \textcolor{red}{\num{99.784}} & \num{130.373} & \num{100.001} & \num{99.863} & \num{104.494}\\
    MASE & \num{110.44} & \textcolor{red}{\num{110.105}} & \num{110.443} & \num{110.437} & \num{110.46}\\
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
    MSE & \num{0.00164} & \num{0.00159} & \num{0.00154} & \num{0.00154} & \textcolor{red}{\num{0.00154}}\\
    MAE & \num{0.0286} & \num{0.0285} & \num{0.0278} & \num{0.0278} & \textcolor{red}{\num{0.0277}}\\
    MAPE & \num{239.157} & \num{231.722} & \textcolor{red}{\num{198.631}} & \num{198.818} & \num{199.007}\\
    MASE & \num{116.517} & \num{114.239} & \num{112.342} & \num{112.33} & \textcolor{red}{\num{112.157}}\\
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
    MSE & \num{0.00176} & \num{0.00177} & \num{0.00174} & \num{0.00174} & \textcolor{red}{\num{0.00173}}\\
    MAE & \num{0.0302} & \num{0.0304} & \num{0.03} & \num{0.03} & \textcolor{red}{\num{0.0299}}\\
    MAPE & \num{144.064} & \num{171.25} & \num{100.909} & \textcolor{red}{\num{100.871}} & \num{104.672}\\
    MASE & \num{123.227} & \num{123.648} & \num{122.547} & \num{122.541} & \textcolor{red}{\num{122.256}}\\
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
    MSE & \num{0.00166} & \num{0.00171} & \num{0.00167} & \num{0.00167} & \textcolor{red}{\num{0.00166}}\\
    MAE & \num{0.0295} & \num{0.0299} & \num{0.0296} & \num{0.0296} & \textcolor{red}{\num{0.0295}}\\
    MAPE & \num{100.008} & \num{143.346} & \textcolor{red}{\num{100}} & \num{100.009} & \num{101.068}\\
    MASE & \num{117.412} & \num{118.793} & \num{117.475} & \num{117.475} & \textcolor{red}{\num{117.337}}\\
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
