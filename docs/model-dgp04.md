Simulating VAR-type Minnesota BVHAR
================
Young Geun Kim
06 Jan, 2022

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
dgp <- readRDS("../data/processed/bvharsim_dgp_l.rds")
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
#> [1]  0.0399  0.0503  0.1586
#> 
#> Setting for 'lambda':
#> [1]  0.0415
#> 
#> Setting for 'delta':
#> [1]  0.800  0.777  0.790
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
#> [1]  0.0370  0.0415  0.0484  0.0493  0.0640  0.0574  0.0643  0.1407  0.0990
#> 
#> Setting for 'lambda':
#> [1]  0.00904
#> 
#> Setting for 'delta':
#> [1]  0.823  0.801  0.802  0.774  0.772  0.807  0.815  0.802  0.804
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
#>  [1]  0.0383  0.0405  0.0443  0.0560  0.0668  0.0421  0.0661  0.0606  0.0892
#> [10]  0.0795  0.0717  0.0990
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'delta':
#>  [1]  0.799  0.801  0.801  0.814  0.793  0.791  0.771  0.798  0.791  0.817
#> [11]  0.799  0.791
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
#> [1]  0.0479  0.0460  0.1525
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'delta':
#> [1]  0.822  0.801  0.813
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
#> [1]  0.0371  0.0415  0.0479  0.0488  0.0647  0.0571  0.0639  0.1416  0.0994
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'delta':
#> [1]  0.822  0.804  0.801  0.779  0.776  0.807  0.813  0.804  0.803
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
#>  [1]  0.0383  0.0406  0.0443  0.0562  0.0665  0.0420  0.0658  0.0599  0.0890
#> [10]  0.0791  0.0716  0.0995
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'delta':
#>  [1]  0.801  0.803  0.800  0.815  0.796  0.790  0.767  0.800  0.793  0.817
#> [11]  0.801  0.792
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
#> [1]  0.0478  0.0459  0.1523
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.746  0.760  0.757
#> 
#> Setting for 'weekly':
#> [1]  0.202  0.103  0.150
#> 
#> Setting for 'monthly':
#> [1]  0.010  0.075  0.010
```

``` r
(bvhar_vhar_medium_optim <- choose_bvhar(
  bvhar_medium_spec, 
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
#> [1]  0.0370  0.0413  0.0476  0.0487  0.0642  0.0570  0.0639  0.1412  0.0990
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.751  0.733  0.721  0.722  0.685  0.757  0.785  0.734  0.758
#> 
#> Setting for 'weekly':
#> [1]  0.1888  0.1940  0.2234  0.1650  0.2452  0.1356  0.0798  0.1904  0.0779
#> 
#> Setting for 'monthly':
#> [1]  0.0100  0.0100  0.0100  0.0100  0.0851  0.0100  0.0100  0.0100  0.1415
```

``` r
(bvhar_vhar_large_optim <- choose_bvhar(
  bvhar_large_spec, 
  lower = c(
    rep(1e-2, n_large), # sigma
    1e-4, # lambda
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
#>  [1]  0.0381  0.0403  0.0442  0.0560  0.0664  0.0418  0.0650  0.0595  0.0884
#> [10]  0.0786  0.0711  0.0989
#> 
#> Setting for 'lambda':
#> [1]  1e-04
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#>  [1]  0.732  0.723  0.745  0.745  0.727  0.709  0.684  0.751  0.739  0.730
#> [11]  0.730  0.742
#> 
#> Setting for 'weekly':
#>  [1]  0.189  0.223  0.152  0.193  0.193  0.223  0.220  0.116  0.118  0.235
#> [11]  0.197  0.135
#> 
#> Setting for 'monthly':
#>  [1]  0.0190  0.0100  0.0100  0.0100  0.0274  0.0100  0.0100  0.0936  0.1423
#> [10]  0.0100  0.0100  0.0100
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
    \hspace{1em} & BVAR & $\sigma$ & 0.040 & 0.050 & 0.159 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.042 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.800 & 0.777 & 0.790 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.048 & 0.046 & 0.153 &  &  &  &  &  &  &  &  & \\

    \hspace{1em}\hspace{1em}\hspace{1em}\hspace{1em}\hspace{1em}\hspace{1em}\hspace{1em} &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.822 & 0.801 & 0.813 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.048 & 0.046 & 0.152 &  &  &  &  &  &  &  &  & \\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.746 & 0.760 & 0.757 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.202 & 0.103 & 0.150 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.010 & 0.075 & 0.010 &  &  &  &  &  &  &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{MEDIUM}}\\
    \hspace{1em} & BVAR & $\sigma$ & 0.037 & 0.041 & 0.048 & 0.049 & 0.064 & 0.057 & 0.064 & 0.141 & 0.099 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.009 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.823 & 0.801 & 0.802 & 0.774 & 0.772 & 0.807 & 0.815 & 0.802 & 0.804 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.037 & 0.041 & 0.048 & 0.049 & 0.065 & 0.057 & 0.064 & 0.142 & 0.099 &  &  & \\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.822 & 0.804 & 0.801 & 0.779 & 0.776 & 0.807 & 0.813 & 0.804 & 0.803 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.037 & 0.041 & 0.048 & 0.049 & 0.064 & 0.057 & 0.064 & 0.141 & 0.099 &  &  & \\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.751 & 0.733 & 0.721 & 0.722 & 0.685 & 0.757 & 0.785 & 0.734 & 0.758 &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.189 & 0.194 & 0.223 & 0.165 & 0.245 & 0.136 & 0.080 & 0.190 & 0.078 &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.010 & 0.010 & 0.010 & 0.010 & 0.085 & 0.010 & 0.010 & 0.010 & 0.141 &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{LARGE}}\\
    \hspace{1em} & BVAR & $\sigma$ & 0.038 & 0.041 & 0.044 & 0.056 & 0.067 & 0.042 & 0.066 & 0.061 & 0.089 & 0.080 & 0.072 & 0.099\\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.799 & 0.801 & 0.801 & 0.814 & 0.793 & 0.791 & 0.771 & 0.798 & 0.791 & 0.817 & 0.799 & 0.791\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.038 & 0.041 & 0.044 & 0.056 & 0.066 & 0.042 & 0.066 & 0.060 & 0.089 & 0.079 & 0.072 & 0.099\\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.801 & 0.803 & 0.800 & 0.815 & 0.796 & 0.790 & 0.767 & 0.800 & 0.793 & 0.817 & 0.801 & 0.792\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.038 & 0.040 & 0.044 & 0.056 & 0.066 & 0.042 & 0.065 & 0.059 & 0.088 & 0.079 & 0.071 & 0.099\\

     &  & $\lambda$ & 0.000 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.732 & 0.723 & 0.745 & 0.745 & 0.727 & 0.709 & 0.684 & 0.751 & 0.739 & 0.730 & 0.730 & 0.742\\

    \hspace{1em} &  & $w_i$ & 0.189 & 0.223 & 0.152 & 0.193 & 0.193 & 0.223 & 0.220 & 0.116 & 0.118 & 0.235 & 0.197 & 0.135\\

    \hspace{1em} &  & $m_i$ & 0.019 & 0.010 & 0.010 & 0.010 & 0.027 & 0.010 & 0.010 & 0.094 & 0.142 & 0.010 & 0.010 & 0.010\\*
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

<img src="../output/figs/DGP-4-smallcvonefig-1.png" width="70%" style="display: block; margin: auto;" />

<img src="../output/figs/DGP-4-smallcvfivefig-1.png" width="70%" style="display: block; margin: auto;" />

<img src="../output/figs/DGP-4-smallcvtwentyfig-1.png" width="70%" style="display: block; margin: auto;" />

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
     & asset01 & \num{0.00097} & \num{0.000933} & \num{0.000907} & \textcolor{red}{\num{0.000905}} & \num{0.000916}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000876} & \num{0.000865} & \num{0.000845} & \textcolor{red}{\num{0.000828}} & \num{0.000837}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0147} & \num{0.0146} & \num{0.0146} & \num{0.0144} & \textcolor{red}{\num{0.0144}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00553} & \num{0.00548} & \num{0.00543} & \num{0.00538} & \textcolor{red}{\num{0.00537}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0252} & \num{0.0249} & \num{0.0245} & \textcolor{red}{\num{0.0244}} & \num{0.0247}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0234} & \num{0.0237} & \num{0.0233} & \textcolor{red}{\num{0.023}} & \num{0.0231}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.1002} & \num{0.0983} & \num{0.0989} & \num{0.0986} & \textcolor{red}{\num{0.098}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0496} & \num{0.0489} & \num{0.0489} & \num{0.0487} & \textcolor{red}{\num{0.0486}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{126.439} & \num{128.022} & \num{122.252} & \textcolor{red}{\num{119.485}} & \num{126.261}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{145.238} & \num{146.08} & \num{139.793} & \textcolor{red}{\num{131.345}} & \num{139.289}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{472.518} & \textcolor{red}{\num{457.737}} & \num{496.871} & \num{530.933} & \num{462.237}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{248.065} & \num{243.946} & \num{252.972} & \num{260.588} & \textcolor{red}{\num{242.595}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{65.553} & \num{65.244} & \num{63.764} & \textcolor{red}{\num{63.077}} & \num{64.42}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{58.997} & \num{59.62} & \num{58.462} & \textcolor{red}{\num{57.392}} & \num{57.411}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{258.29} & \num{253.33} & \num{253.916} & \num{254.043} & \textcolor{red}{\num{252.06}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{127.613} & \num{126.064} & \num{125.38} & \num{124.837} & \textcolor{red}{\num{124.63}}\\*
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
     & asset01 & \num{0.00282} & \num{0.00278} & \textcolor{red}{\num{0.00258}} & \num{0.00264} & \num{0.00268}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0023} & \num{0.00239} & \num{0.00225} & \textcolor{red}{\num{0.0022}} & \num{0.00227}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0346} & \num{0.034} & \num{0.0347} & \num{0.0344} & \textcolor{red}{\num{0.0335}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0132} & \num{0.0131} & \num{0.0132} & \num{0.0131} & \textcolor{red}{\num{0.0128}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0426} & \num{0.042} & \textcolor{red}{\num{0.0406}} & \num{0.0407} & \num{0.041}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0384} & \num{0.0389} & \num{0.0381} & \textcolor{red}{\num{0.0379}} & \num{0.0386}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.153} & \num{0.149} & \num{0.147} & \num{0.144} & \textcolor{red}{\num{0.143}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.078} & \num{0.0768} & \num{0.0753} & \textcolor{red}{\num{0.0742}} & \num{0.0743}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{182.311} & \num{190.589} & \num{178.413} & \textcolor{red}{\num{156.019}} & \num{180.091}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{185.072} & \num{207.463} & \num{193.281} & \textcolor{red}{\num{172.305}} & \num{201.555}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{629.566} & \num{727.741} & \num{635.49} & \textcolor{red}{\num{537.857}} & \num{644.425}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{332.316} & \num{375.264} & \num{335.728} & \textcolor{red}{\num{288.727}} & \num{342.024}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{109.787} & \num{108.018} & \num{104.887} & \textcolor{red}{\num{104.314}} & \num{105.534}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{101.065} & \num{101.733} & \num{100.629} & \textcolor{red}{\num{100.316}} & \num{101.654}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{406.333} & \num{395.806} & \num{390.678} & \num{381.937} & \textcolor{red}{\num{379.919}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{205.728} & \num{201.852} & \num{198.731} & \textcolor{red}{\num{195.523}} & \num{195.702}\\*
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
     & asset01 & \num{0.0037} & \num{0.0037} & \num{0.00355} & \textcolor{red}{\num{0.00338}} & \num{0.00362}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00265} & \num{0.00276} & \num{0.00265} & \textcolor{red}{\num{0.00259}} & \num{0.00275}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0414} & \num{0.0421} & \num{0.0399} & \textcolor{red}{\num{0.0384}} & \num{0.0403}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0159} & \num{0.0162} & \num{0.0154} & \textcolor{red}{\num{0.0148}} & \num{0.0156}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0508} & \num{0.0502} & \num{0.0492} & \textcolor{red}{\num{0.0477}} & \num{0.0498}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0431} & \num{0.0438} & \num{0.0434} & \textcolor{red}{\num{0.0428}} & \num{0.0447}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.166} & \num{0.163} & \num{0.164} & \textcolor{red}{\num{0.159}} & \num{0.164}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0867} & \num{0.0858} & \num{0.0854} & \textcolor{red}{\num{0.0833}} & \num{0.0862}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{126.355} & \num{126.583} & \num{106.459} & \textcolor{red}{\num{100.359}} & \num{110.922}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{107.864} & \num{109.565} & \textcolor{red}{\num{101.813}} & \num{102.23} & \num{114.57}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{114.435} & \num{108.328} & \num{112.595} & \textcolor{red}{\num{103.571}} & \num{112.917}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{116.218} & \num{114.825} & \num{106.956} & \textcolor{red}{\num{102.053}} & \num{112.803}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{126.342} & \num{125.049} & \num{122.101} & \textcolor{red}{\num{118.249}} & \num{123.659}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{114.028} & \num{115.837} & \num{114.758} & \textcolor{red}{\num{113.329}} & \num{118.222}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{426.863} & \num{422.603} & \num{419.765} & \textcolor{red}{\num{409.156}} & \num{421.018}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{222.411} & \num{221.163} & \num{218.875} & \textcolor{red}{\num{213.578}} & \num{220.967}\\*
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
     & asset01 & \num{0.000381} & \num{0.000384} & \textcolor{red}{\num{0.000349}} & \num{0.000349} & \num{0.000352}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000238} & \num{0.000243} & \textcolor{red}{\num{0.000215}} & \num{0.000217} & \num{0.000219}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000352} & \num{0.000368} & \textcolor{red}{\num{0.000326}} & \num{0.000326} & \num{0.000337}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.000503}} & \num{0.000548} & \num{0.000528} & \num{0.000529} & \num{0.000516}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.000962} & \num{0.000994} & \num{0.000974} & \num{0.000976} & \textcolor{red}{\num{0.000948}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000853} & \textcolor{red}{\num{0.000775}} & \num{0.000821} & \num{0.000822} & \num{0.000802}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.00054} & \num{0.000505} & \num{5e-04} & \num{0.000501} & \textcolor{red}{\num{0.000496}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00593} & \num{0.00594} & \num{0.00577} & \num{0.00578} & \textcolor{red}{\num{0.00563}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00411} & \textcolor{red}{\num{0.00388}} & \num{0.00397} & \num{0.00398} & \num{0.00395}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00154} & \num{0.00152} & \num{0.0015} & \num{0.0015} & \textcolor{red}{\num{0.00147}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0152} & \num{0.0151} & \textcolor{red}{\num{0.0143}} & \num{0.0143} & \num{0.0146}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0128} & \num{0.0127} & \textcolor{red}{\num{0.0117}} & \num{0.0117} & \num{0.0119}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0152} & \num{0.0156} & \num{0.0146} & \textcolor{red}{\num{0.0146}} & \num{0.0148}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.0172}} & \num{0.0181} & \num{0.0182} & \num{0.0182} & \num{0.0179}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0239} & \num{0.0244} & \num{0.0239} & \num{0.0239} & \textcolor{red}{\num{0.0235}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.024} & \textcolor{red}{\num{0.0224}} & \num{0.0233} & \num{0.0234} & \num{0.0232}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0175} & \num{0.0173} & \num{0.0169} & \num{0.017} & \textcolor{red}{\num{0.0169}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0634} & \num{0.0635} & \num{0.0621} & \num{0.0621} & \textcolor{red}{\num{0.0612}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.053} & \textcolor{red}{\num{0.0508}} & \num{0.0525} & \num{0.0525} & \num{0.0523}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0269} & \num{0.0267} & \num{0.0264} & \num{0.0264} & \textcolor{red}{\num{0.0263}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{115.696} & \num{139.289} & \num{114.301} & \textcolor{red}{\num{113.846}} & \num{122.367}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{90.747} & \num{88.052} & \num{81.721} & \textcolor{red}{\num{81.607}} & \num{86.872}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1112.82} & \num{1536.473} & \num{680.768} & \textcolor{red}{\num{672.753}} & \num{695.36}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{83.039} & \num{81.226} & \num{83.525} & \num{82.861} & \textcolor{red}{\num{79.226}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{246.203} & \num{237.89} & \num{239.442} & \num{239.208} & \textcolor{red}{\num{234.616}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{1933.089} & \textcolor{red}{\num{445.853}} & \num{3339.407} & \num{3478.664} & \num{2890.231}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{148.834} & \num{125.277} & \num{120.053} & \num{119.963} & \textcolor{red}{\num{117.574}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{989.557} & \num{1001.11} & \num{1053.695} & \num{1065.39} & \textcolor{red}{\num{983.636}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{154.962}} & \num{166.697} & \num{171.032} & \num{169.479} & \num{162.022}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{541.661} & \textcolor{red}{\num{424.652}} & \num{653.772} & \num{669.308} & \num{596.878}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{70.575} & \num{73.364} & \textcolor{red}{\num{68.183}} & \num{68.334} & \num{69.488}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{59.434} & \num{59.557} & \textcolor{red}{\num{54.148}} & \num{54.262} & \num{55.159}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{71.807} & \num{73.307} & \textcolor{red}{\num{68.343}} & \num{68.352} & \num{70.158}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{79.453}} & \num{83.643} & \num{83.793} & \num{83.934} & \num{82.174}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{111.309} & \num{112.446} & \num{111.762} & \num{111.815} & \textcolor{red}{\num{109.97}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{114.436} & \textcolor{red}{\num{105.846}} & \num{111.425} & \num{111.516} & \num{111.077}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{82.117} & \num{81.178} & \num{80.459} & \num{80.431} & \textcolor{red}{\num{80.088}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{287.503} & \num{285.976} & \num{276.923} & \num{276.914} & \textcolor{red}{\num{273.455}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{252.562} & \textcolor{red}{\num{244.48}} & \num{248.118} & \num{248.304} & \num{248.334}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{125.466} & \num{124.422} & \num{122.573} & \num{122.651} & \textcolor{red}{\num{122.212}}\\*
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
     & asset01 & \num{0.0011} & \num{0.00125} & \textcolor{red}{\num{0.00102}} & \num{0.00102} & \num{0.00103}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00071} & \num{0.000805} & \num{0.000676} & \num{0.000683} & \textcolor{red}{\num{0.000672}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000971} & \num{0.001093} & \num{0.000875} & \textcolor{red}{\num{0.000871}} & \num{0.00096}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.00149} & \num{0.00175} & \num{0.00159} & \num{0.00158} & \textcolor{red}{\num{0.00145}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00179} & \num{0.00187} & \num{0.00168} & \num{0.00168} & \textcolor{red}{\num{0.00164}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0023} & \textcolor{red}{\num{0.00203}} & \num{0.00256} & \num{0.00257} & \num{0.00235}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000971} & \num{0.001071} & \num{0.000936} & \num{0.000942} & \textcolor{red}{\num{0.000924}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0126} & \num{0.0125} & \num{0.0133} & \num{0.0134} & \textcolor{red}{\num{0.0121}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.00764}} & \num{0.00871} & \num{0.00844} & \num{0.0085} & \num{0.00845}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{0.00329}} & \num{0.00345} & \num{0.00345} & \num{0.00347} & \num{0.00329}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.027} & \num{0.029} & \textcolor{red}{\num{0.0254}} & \num{0.0255} & \num{0.0255}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0226} & \num{0.0238} & \textcolor{red}{\num{0.0218}} & \num{0.0219} & \num{0.0219}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0249} & \num{0.0269} & \num{0.0237} & \textcolor{red}{\num{0.0237}} & \num{0.0246}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0325} & \num{0.0353} & \num{0.0332} & \num{0.0332} & \textcolor{red}{\num{0.0318}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0338} & \num{0.0339} & \num{0.0314} & \textcolor{red}{\num{0.0313}} & \num{0.0319}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0369} & \textcolor{red}{\num{0.0355}} & \num{0.0385} & \num{0.0385} & \num{0.0363}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0242} & \num{0.0252} & \num{0.0241} & \num{0.0242} & \textcolor{red}{\num{0.0239}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{0.0888}} & \num{0.089} & \num{0.0922} & \num{0.0927} & \num{0.0889}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.066}} & \num{0.0718} & \num{0.0708} & \num{0.0711} & \num{0.0709}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0396} & \num{0.0412} & \num{0.0401} & \num{0.0402} & \textcolor{red}{\num{0.0395}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{226.252} & \num{273.126} & \num{188.392} & \textcolor{red}{\num{185.862}} & \num{222.277}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{135.742} & \num{128.922} & \textcolor{red}{\num{112.944}} & \num{112.994} & \num{125.625}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1936.207} & \textcolor{red}{\num{494.505}} & \num{810.871} & \num{690.49} & \num{1256.036}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{144.341} & \num{157.164} & \num{114.529} & \textcolor{red}{\num{112.391}} & \num{127.25}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{207.72} & \num{209.586} & \num{137.262} & \textcolor{red}{\num{132.503}} & \num{178.214}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{9683.207} & \num{8940.775} & \num{5888.054} & \textcolor{red}{\num{5662.428}} & \num{6577.744}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{122.952} & \num{119.95} & \textcolor{red}{\num{107.49}} & \num{107.654} & \num{111.005}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{843.581} & \textcolor{red}{\num{431.628}} & \num{481.66} & \num{453.087} & \num{514.87}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{107.714}} & \num{154.002} & \num{112.311} & \num{113.724} & \num{114.33}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{1489.746} & \num{1212.184} & \num{883.724} & \textcolor{red}{\num{841.237}} & \num{1025.261}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{127.418} & \num{138.393} & \num{121.198} & \num{121.572} & \textcolor{red}{\num{120.981}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{101.93} & \num{106.297} & \textcolor{red}{\num{96.809}} & \num{97.499} & \num{98.197}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{122.293} & \num{134.446} & \num{117.79} & \textcolor{red}{\num{117.668}} & \num{121.433}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{152.436} & \num{165.701} & \num{155.158} & \num{155.052} & \textcolor{red}{\num{148.557}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{147.983} & \num{149.953} & \textcolor{red}{\num{139.456}} & \num{139.523} & \num{140.453}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{156.8} & \num{156.824} & \num{165.073} & \num{165.165} & \textcolor{red}{\num{154.906}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{108.977} & \num{112.58} & \num{109.719} & \num{109.961} & \textcolor{red}{\num{108.458}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{425.625}} & \num{426.214} & \num{446.966} & \num{449.12} & \num{428.197}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{298.453}} & \num{334.966} & \num{316.458} & \num{318.03} & \num{320.45}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{182.435} & \num{191.708} & \num{185.403} & \num{185.955} & \textcolor{red}{\num{182.404}}\\*
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
     & asset01 & \textcolor{red}{\num{0.00111}} & \num{0.00155} & \num{0.00116} & \num{0.00117} & \num{0.00112}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.000859}} & \num{0.000897} & \num{0.000873} & \num{0.000876} & \num{0.00086}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.00077}} & \num{0.001102} & \num{0.000817} & \num{0.00082} & \num{0.000777}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.00234}} & \num{0.00272} & \num{0.00243} & \num{0.00243} & \num{0.00237}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00183} & \num{0.00198} & \num{0.00198} & \num{0.00198} & \textcolor{red}{\num{0.00183}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.00416} & \textcolor{red}{\num{0.00354}} & \num{0.00423} & \num{0.00423} & \num{0.00416}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{0.00127}} & \num{0.00161} & \num{0.00128} & \num{0.00128} & \num{0.00127}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0144} & \textcolor{red}{\num{0.0141}} & \num{0.0154} & \num{0.0155} & \num{0.0145}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.01015} & \num{0.01193} & \textcolor{red}{\num{0.00999}} & \num{0.00999} & \num{0.01003}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{0.0041}} & \num{0.00438} & \num{0.00424} & \num{0.00425} & \num{0.0041}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0273} & \num{0.0322} & \num{0.0277} & \num{0.0278} & \textcolor{red}{\num{0.0271}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.025} & \textcolor{red}{\num{0.025}} & \num{0.0253} & \num{0.0254} & \num{0.0251}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.0225}} & \num{0.0273} & \num{0.0233} & \num{0.0233} & \num{0.0228}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.0413}} & \num{0.0433} & \num{0.0422} & \num{0.0422} & \num{0.0418}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.0334}} & \num{0.0343} & \num{0.0353} & \num{0.0353} & \num{0.0339}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0524} & \textcolor{red}{\num{0.046}} & \num{0.0527} & \num{0.0526} & \num{0.0518}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.03} & \num{0.0345} & \num{0.0299} & \num{0.0299} & \textcolor{red}{\num{0.0298}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0958} & \textcolor{red}{\num{0.0924}} & \num{0.0997} & \num{0.0998} & \num{0.0967}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0789} & \num{0.0863} & \num{0.0786} & \num{0.0786} & \textcolor{red}{\num{0.0782}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{0.0452}} & \num{0.0468} & \num{0.0461} & \num{0.0461} & \num{0.0452}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{114.161} & \num{173.861} & \num{98.269} & \num{98.606} & \textcolor{red}{\num{93.735}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{96.878}} & \num{105.226} & \num{99.25} & \num{99.54} & \num{97.457}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{342.952} & \num{589.26} & \textcolor{red}{\num{139.585}} & \num{161.694} & \num{431.245}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{101.888} & \num{102.824} & \num{100.444} & \textcolor{red}{\num{100.32}} & \num{102.043}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{95.248}} & \num{105.692} & \num{98.344} & \num{98.805} & \num{103.848}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{483.358} & \num{1249.311} & \num{212.737} & \textcolor{red}{\num{153.214}} & \num{324.905}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{102.582} & \num{137.613} & \num{99.36} & \num{99.48} & \textcolor{red}{\num{98.784}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{257.578} & \num{411.859} & \textcolor{red}{\num{117.512}} & \num{120.175} & \num{220.233}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{100.32} & \num{180.987} & \textcolor{red}{\num{99.032}} & \num{99.12} & \num{100.097}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{188.329} & \num{339.626} & \num{118.282} & \textcolor{red}{\num{114.55}} & \num{174.705}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{130.018} & \num{152.796} & \num{131.759} & \num{132.116} & \textcolor{red}{\num{128.241}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{122.69}} & \num{123.452} & \num{124.777} & \num{125.047} & \num{123.106}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{114.999}} & \num{139.843} & \num{119.546} & \num{119.687} & \num{116.879}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{194.957}} & \num{205.834} & \num{199.206} & \num{199.237} & \num{196.803}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{165.672}} & \num{170.286} & \num{173.919} & \num{174.043} & \num{167.623}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{230.422} & \textcolor{red}{\num{205.915}} & \num{231.721} & \num{231.461} & \num{227.639}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{142.989} & \num{163.099} & \num{142.385} & \num{142.471} & \textcolor{red}{\num{141.921}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{452.899} & \textcolor{red}{\num{442.116}} & \num{468.93} & \num{469.651} & \num{454.039}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{339.854} & \num{369.599} & \num{338.046} & \num{338.082} & \textcolor{red}{\num{336.147}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{210.5} & \num{219.215} & \num{214.477} & \num{214.644} & \textcolor{red}{\num{210.266}}\\*
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
     & asset01 & \num{0.000298} & \num{0.000299} & \num{0.000279} & \textcolor{red}{\num{0.000279}} & \num{0.000281}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000421} & \num{0.000401} & \textcolor{red}{\num{0.000373}} & \num{0.000373} & \num{0.000373}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000245} & \num{0.000253} & \num{0.000243} & \num{0.000243} & \textcolor{red}{\num{0.000241}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.000427}} & \num{0.00046} & \num{0.000476} & \num{0.000476} & \num{0.000466}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.000839} & \num{0.000796} & \num{0.000778} & \num{0.000778} & \textcolor{red}{\num{0.000776}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000518} & \num{0.000504} & \num{0.000508} & \num{0.000508} & \textcolor{red}{\num{0.000502}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000722} & \num{0.000792} & \num{0.000699} & \num{0.000699} & \textcolor{red}{\num{0.000693}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00234} & \num{0.00222} & \num{0.00217} & \num{0.00217} & \textcolor{red}{\num{0.00211}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00374} & \num{0.00378} & \num{0.00358} & \num{0.00358} & \textcolor{red}{\num{0.00355}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.000998} & \num{0.000984} & \num{0.000914} & \num{0.000914} & \textcolor{red}{\num{0.000907}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.00395} & \num{0.0038} & \num{0.00382} & \num{0.00382} & \textcolor{red}{\num{0.00374}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.0049} & \num{0.00497} & \num{0.00482} & \textcolor{red}{\num{0.00482}} & \num{0.00486}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00162} & \num{0.00161} & \num{0.00155} & \num{0.00155} & \textcolor{red}{\num{0.00154}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0139} & \num{0.0138} & \textcolor{red}{\num{0.0133}} & \num{0.0133} & \num{0.0135}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0161} & \num{0.0155} & \textcolor{red}{\num{0.0152}} & \num{0.0152} & \num{0.0152}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0124} & \num{0.0126} & \textcolor{red}{\num{0.0121}} & \num{0.0121} & \num{0.0121}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.0167}} & \num{0.0175} & \num{0.0177} & \num{0.0177} & \num{0.0175}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0229} & \num{0.0228} & \num{0.0225} & \num{0.0225} & \textcolor{red}{\num{0.0223}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0185} & \num{0.0183} & \num{0.0182} & \num{0.0182} & \textcolor{red}{\num{0.018}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0225} & \num{0.0235} & \num{0.022} & \num{0.022} & \textcolor{red}{\num{0.0219}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0391} & \num{0.0384} & \num{0.0372} & \num{0.0372} & \textcolor{red}{\num{0.0366}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0489} & \num{0.0499} & \num{0.0481} & \num{0.0481} & \textcolor{red}{\num{0.0476}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.025} & \num{0.0249} & \num{0.0237} & \textcolor{red}{\num{0.0237}} & \num{0.0238}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.0491} & \textcolor{red}{\num{0.0477}} & \num{0.0488} & \num{0.0487} & \num{0.0483}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.057} & \num{0.0579} & \num{0.0565} & \textcolor{red}{\num{0.0565}} & \num{0.0567}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0285} & \num{0.0286} & \num{0.0279} & \num{0.0279} & \textcolor{red}{\num{0.0278}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{124.502} & \num{124.889} & \textcolor{red}{\num{115.323}} & \num{115.488} & \num{117.68}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{152.991} & \num{145.469} & \num{142.713} & \num{142.947} & \textcolor{red}{\num{139.21}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{118.669} & \num{114.814} & \num{89.319} & \num{89.264} & \textcolor{red}{\num{89.073}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{259.587} & \num{230.066} & \textcolor{red}{\num{193.244}} & \num{193.3} & \num{195.452}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{207.238} & \num{240.911} & \textcolor{red}{\num{192.477}} & \num{193.145} & \num{200.076}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{363.126} & \textcolor{red}{\num{230.306}} & \num{399.226} & \num{398.968} & \num{288.619}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{287.118} & \num{338.432} & \num{254.218} & \num{253.291} & \textcolor{red}{\num{240.989}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{101.937} & \num{104.813} & \textcolor{red}{\num{94.674}} & \num{94.845} & \num{95.139}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{155.879} & \num{161.458} & \num{144.501} & \num{144.778} & \textcolor{red}{\num{141.504}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{1640.692} & \num{1531.017} & \textcolor{red}{\num{1289.554}} & \num{1290.955} & \num{1309.688}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{2771.684} & \num{3519.376} & \textcolor{red}{\num{2620.411}} & \num{2625.606} & \num{2767.224}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{209.335} & \num{208.44} & \textcolor{red}{\num{199.883}} & \num{200.108} & \num{204.43}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{532.73} & \num{579.166} & \textcolor{red}{\num{477.962}} & \num{478.558} & \num{482.424}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{68.894} & \num{69.201} & \textcolor{red}{\num{66.012}} & \num{66.015} & \num{66.489}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{75.435} & \num{73.375} & \num{71.012} & \textcolor{red}{\num{71.01}} & \num{71.971}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{54.393} & \num{54.313} & \textcolor{red}{\num{52.21}} & \num{52.23} & \num{52.256}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{73.742}} & \num{76.854} & \num{77.47} & \num{77.466} & \num{76.017}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{106.013} & \num{104.504} & \num{105.504} & \num{105.434} & \textcolor{red}{\num{104.164}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{82.469} & \num{80.589} & \num{81.012} & \num{81.003} & \textcolor{red}{\num{80.02}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{103.687} & \num{108.753} & \num{101.467} & \num{101.556} & \textcolor{red}{\num{100.744}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{179.567} & \num{172.67} & \num{170.69} & \num{170.647} & \textcolor{red}{\num{166.953}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{221.79} & \num{224.076} & \num{215.654} & \num{215.683} & \textcolor{red}{\num{213.55}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{119.148} & \num{117.556} & \num{112.401} & \textcolor{red}{\num{112.394}} & \num{113.482}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{228.051} & \textcolor{red}{\num{217.831}} & \num{228.426} & \num{228.282} & \num{225.863}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{261.903} & \num{263.04} & \num{258.528} & \textcolor{red}{\num{258.374}} & \num{259.748}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{131.258} & \num{130.23} & \num{128.365} & \num{128.341} & \textcolor{red}{\num{127.605}}\\*
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
     & asset01 & \num{0.000837} & \num{0.000976} & \textcolor{red}{\num{0.000735}} & \num{0.000735} & \num{0.000764}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000916} & \num{0.000928} & \textcolor{red}{\num{0.000749}} & \num{0.000751} & \num{0.000799}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000777} & \num{0.000868} & \num{0.000787} & \num{0.000788} & \textcolor{red}{\num{0.000737}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0011} & \textcolor{red}{\num{0.00107}} & \num{0.00129} & \num{0.00129} & \num{0.00122}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00203} & \num{0.00193} & \num{0.00182} & \textcolor{red}{\num{0.00182}} & \num{0.00189}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.00108} & \textcolor{red}{\num{0.00104}} & \num{0.00111} & \num{0.00111} & \num{0.00111}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.00179} & \num{0.00211} & \num{0.00158} & \num{0.00159} & \textcolor{red}{\num{0.00154}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00458} & \num{0.00474} & \num{0.00468} & \num{0.00466} & \textcolor{red}{\num{0.00422}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00857} & \num{0.00934} & \num{0.00915} & \num{0.00915} & \textcolor{red}{\num{0.00855}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.00289} & \num{0.00256} & \textcolor{red}{\num{0.00191}} & \num{0.00191} & \num{0.00198}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{0.00922}} & \num{0.00948} & \num{0.01014} & \num{0.01012} & \num{0.00943}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{0.0116}} & \num{0.013} & \num{0.0133} & \num{0.0133} & \num{0.0133}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{0.00378}} & \num{0.004} & \num{0.00394} & \num{0.00393} & \num{0.00379}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0243} & \num{0.0252} & \num{0.0229} & \textcolor{red}{\num{0.0229}} & \num{0.0229}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0241} & \num{0.0244} & \textcolor{red}{\num{0.0223}} & \num{0.0223} & \num{0.0232}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0206} & \num{0.0216} & \num{0.0216} & \num{0.0216} & \textcolor{red}{\num{0.0206}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.0264}} & \num{0.0265} & \num{0.0294} & \num{0.0294} & \num{0.0286}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0357} & \num{0.035} & \num{0.0335} & \textcolor{red}{\num{0.0335}} & \num{0.0339}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0263} & \textcolor{red}{\num{0.0254}} & \num{0.0274} & \num{0.0274} & \num{0.027}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0349} & \num{0.0373} & \num{0.0327} & \num{0.0327} & \textcolor{red}{\num{0.0321}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0548} & \num{0.0553} & \num{0.0567} & \num{0.0565} & \textcolor{red}{\num{0.0525}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.076}} & \num{0.0781} & \num{0.0787} & \num{0.0787} & \num{0.0775}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.0419} & \num{0.042} & \textcolor{red}{\num{0.0349}} & \num{0.0349} & \num{0.0355}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{0.0758}} & \num{0.078} & \num{0.0814} & \num{0.0813} & \num{0.0776}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{0.0852}} & \num{0.0935} & \num{0.093} & \num{0.093} & \num{0.0928}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0438} & \num{0.0452} & \num{0.0445} & \num{0.0445} & \textcolor{red}{\num{0.0437}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{142.694} & \num{152.743} & \textcolor{red}{\num{118.005}} & \num{118.355} & \num{127.942}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{143.761} & \num{129.669} & \textcolor{red}{\num{114.551}} & \num{115.069} & \num{138.167}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{97.956}} & \num{121.326} & \num{98.626} & \num{98.504} & \num{110.961}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{166.528} & \num{273.063} & \num{134.045} & \num{134.141} & \textcolor{red}{\num{126.738}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{242.186} & \num{327.029} & \textcolor{red}{\num{171.06}} & \num{173.366} & \num{214.971}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{825.252} & \num{1033.727} & \num{522.341} & \textcolor{red}{\num{520.768}} & \num{603.109}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{218.696} & \num{389.742} & \num{163.159} & \textcolor{red}{\num{161.583}} & \num{176.513}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{106.355} & \num{117.161} & \textcolor{red}{\num{86.546}} & \num{86.646} & \num{90.364}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{160.841} & \num{168.852} & \textcolor{red}{\num{139.56}} & \num{140.216} & \num{162.219}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{871.854} & \num{1173.598} & \textcolor{red}{\num{755.113}} & \num{758.976} & \num{884.194}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{481.802}} & \num{1884.262} & \num{1759.44} & \num{1776.476} & \num{1926.479}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{159.488} & \num{236.423} & \textcolor{red}{\num{152.823}} & \num{153.361} & \num{175.635}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{301.451}} & \num{500.633} & \num{351.272} & \num{353.122} & \num{394.774}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{114.052} & \num{113.847} & \num{108.108} & \num{108.066} & \textcolor{red}{\num{106.82}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{105.734} & \num{107.152} & \textcolor{red}{\num{96.658}} & \num{96.766} & \num{101.13}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{96.3} & \num{99.544} & \num{101.289} & \num{101.391} & \textcolor{red}{\num{95.882}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{118.454}} & \num{122.268} & \num{132.888} & \num{132.876} & \num{131.645}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{168.192} & \num{168.195} & \textcolor{red}{\num{151.929}} & \num{152.097} & \num{157.691}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{123.346} & \textcolor{red}{\num{116.553}} & \num{130.145} & \num{130.178} & \num{127.157}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{152.77} & \num{162.801} & \num{140.105} & \num{140.289} & \textcolor{red}{\num{137.69}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{262.143} & \num{265.702} & \num{275.349} & \num{274.578} & \textcolor{red}{\num{254.957}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{344.811}} & \num{344.963} & \num{354.785} & \num{354.785} & \num{353.842}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{201.001} & \num{194.83} & \textcolor{red}{\num{164.903}} & \num{164.915} & \num{167.95}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{390.817}} & \num{394.222} & \num{412.077} & \num{411.518} & \num{394.123}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{445.064}} & \num{464.444} & \num{480.756} & \num{480.601} & \num{477.175}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{210.224} & \num{212.877} & \num{212.416} & \num{212.338} & \textcolor{red}{\num{208.839}}\\*
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
     & asset01 & \num{0.000838} & \num{0.001218} & \textcolor{red}{\num{0.000826}} & \num{0.000826} & \num{0.000849}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000828} & \num{0.000893} & \num{0.000809} & \num{0.000809} & \textcolor{red}{\num{0.000786}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.000861}} & \num{0.001231} & \num{0.000891} & \num{0.000891} & \num{0.000896}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.00167} & \textcolor{red}{\num{0.00157}} & \num{0.00182} & \num{0.00182} & \num{0.00179}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.00166}} & \num{0.00169} & \num{0.00175} & \num{0.00175} & \num{0.00174}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.00121}} & \num{0.0014} & \num{0.00132} & \num{0.00132} & \num{0.00128}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.00208} & \num{0.00297} & \num{0.00205} & \num{0.00205} & \textcolor{red}{\num{0.00201}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00604} & \num{0.00626} & \num{0.00614} & \num{0.00614} & \textcolor{red}{\num{0.0057}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0111} & \num{0.012} & \num{0.0115} & \num{0.0115} & \textcolor{red}{\num{0.0106}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.00236} & \num{0.0031} & \num{0.00218} & \num{0.00218} & \textcolor{red}{\num{0.00213}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{0.0105}} & \num{0.0121} & \num{0.0116} & \num{0.0116} & \num{0.0112}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{0.0112}} & \num{0.0145} & \num{0.0123} & \num{0.0123} & \num{0.0124}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{0.00419}} & \num{0.00491} & \num{0.00444} & \num{0.00444} & \num{0.00428}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0247} & \num{0.0292} & \textcolor{red}{\num{0.0246}} & \num{0.0246} & \num{0.025}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0249} & \num{0.026} & \num{0.0247} & \num{0.0247} & \textcolor{red}{\num{0.0244}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.0242}} & \num{0.0291} & \num{0.0248} & \num{0.0248} & \num{0.0247}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0328} & \textcolor{red}{\num{0.0302}} & \num{0.0344} & \num{0.0344} & \num{0.0344}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.0317}} & \num{0.0328} & \num{0.0328} & \num{0.0328} & \num{0.0329}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.0291}} & \num{0.0309} & \num{0.0303} & \num{0.0303} & \num{0.0299}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.038} & \num{0.0452} & \num{0.038} & \num{0.038} & \textcolor{red}{\num{0.0377}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0691} & \num{0.0687} & \num{0.0696} & \num{0.0696} & \textcolor{red}{\num{0.0664}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.0861}} & \num{0.0892} & \num{0.0878} & \num{0.0877} & \num{0.0861}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.0384} & \num{0.0465} & \num{0.0368} & \num{0.0368} & \textcolor{red}{\num{0.0362}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{0.0836}} & \num{0.0874} & \num{0.0886} & \num{0.0885} & \num{0.0878}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{0.0876}} & \num{0.1011} & \num{0.0918} & \num{0.0918} & \num{0.0921}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{0.0475}} & \num{0.0514} & \num{0.0487} & \num{0.0487} & \num{0.0481}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{111.03} & \num{138.163} & \num{99.806} & \textcolor{red}{\num{99.797}} & \num{102.337}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{99.61}} & \num{114.978} & \num{100.505} & \num{100.53} & \num{102.221}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{98.638} & \num{133.906} & \textcolor{red}{\num{98.53}} & \num{98.557} & \num{100.999}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{146.127} & \num{115.565} & \textcolor{red}{\num{106.44}} & \num{106.505} & \num{134.829}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{108.43} & \num{259.699} & \textcolor{red}{\num{102.46}} & \num{102.684} & \num{118.374}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{146.65} & \num{152.854} & \num{103.503} & \num{103.418} & \textcolor{red}{\num{99.858}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{100.068}} & \num{179.131} & \num{100.198} & \num{100.181} & \num{100.172}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{106.587} & \num{100.467} & \num{99.508} & \num{99.48} & \textcolor{red}{\num{96.437}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{98.338}} & \num{140.4} & \num{100.564} & \num{100.593} & \num{129.211}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{1188.674} & \num{1011.668} & \textcolor{red}{\num{142.391}} & \num{143.528} & \num{401.73}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{1188.616} & \num{1230.908} & \textcolor{red}{\num{196.346}} & \num{200.416} & \num{565.409}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{144.921} & \num{182.558} & \textcolor{red}{\num{103.024}} & \num{103.114} & \num{111.429}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{294.808} & \num{313.358} & \textcolor{red}{\num{112.773}} & \num{113.234} & \num{171.917}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{113.309} & \num{131.405} & \textcolor{red}{\num{112.182}} & \num{112.189} & \num{113.133}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{118.928} & \num{124.203} & \num{117.813} & \num{117.802} & \textcolor{red}{\num{116.506}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{117.102}} & \num{139.676} & \num{118.97} & \num{118.971} & \num{119.182}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{143.927} & \textcolor{red}{\num{133.409}} & \num{151.107} & \num{151.108} & \num{151.813}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{147.073}} & \num{160.788} & \num{151.544} & \num{151.531} & \num{151.001}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{131.898}} & \num{141.773} & \num{138.944} & \num{138.948} & \num{137.504}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{167.517} & \num{197.725} & \num{167.386} & \num{167.4} & \textcolor{red}{\num{166.4}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{309.785} & \num{308.37} & \num{316.757} & \num{316.642} & \textcolor{red}{\num{298.891}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{388.15}} & \num{399.823} & \num{395.861} & \num{395.821} & \num{391.594}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{173.005} & \num{213.052} & \num{164.64} & \num{164.613} & \textcolor{red}{\num{161.499}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{369.566}} & \num{386.677} & \num{392.298} & \num{392.25} & \num{388.436}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{428.23}} & \num{484.104} & \num{445.841} & \num{445.855} & \num{447.531}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{217.374}} & \num{235.084} & \num{222.779} & \num{222.761} & \num{220.291}\\*
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
    MSE & \num{0.00553} & \num{0.00548} & \num{0.00543} & \num{0.00538} & \textcolor{red}{\num{0.00537}}\\
    MAE & \num{0.0496} & \num{0.0489} & \num{0.0489} & \num{0.0487} & \textcolor{red}{\num{0.0486}}\\
    MAPE & \num{248.065} & \num{243.946} & \num{252.972} & \num{260.588} & \textcolor{red}{\num{242.595}}\\
    MASE & \num{127.613} & \num{126.064} & \num{125.38} & \num{124.837} & \textcolor{red}{\num{124.63}}\\
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
    MSE & \num{0.0132} & \num{0.0131} & \num{0.0132} & \num{0.0131} & \textcolor{red}{\num{0.0128}}\\
    MAE & \num{0.078} & \num{0.0768} & \num{0.0753} & \textcolor{red}{\num{0.0742}} & \num{0.0743}\\
    MAPE & \num{332.316} & \num{375.264} & \num{335.728} & \textcolor{red}{\num{288.727}} & \num{342.024}\\
    MASE & \num{205.728} & \num{201.852} & \num{198.731} & \textcolor{red}{\num{195.523}} & \num{195.702}\\
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
    MSE & \num{0.0159} & \num{0.0162} & \num{0.0154} & \textcolor{red}{\num{0.0148}} & \num{0.0156}\\
    MAE & \num{0.0867} & \num{0.0858} & \num{0.0854} & \textcolor{red}{\num{0.0833}} & \num{0.0862}\\
    MAPE & \num{116.218} & \num{114.825} & \num{106.956} & \textcolor{red}{\num{102.053}} & \num{112.803}\\
    MASE & \num{222.411} & \num{221.163} & \num{218.875} & \textcolor{red}{\num{213.578}} & \num{220.967}\\
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
    MSE & \num{0.00154} & \num{0.00152} & \num{0.0015} & \num{0.0015} & \textcolor{red}{\num{0.00147}}\\
    MAE & \num{0.0269} & \num{0.0267} & \num{0.0264} & \num{0.0264} & \textcolor{red}{\num{0.0263}}\\
    MAPE & \num{541.661} & \textcolor{red}{\num{424.652}} & \num{653.772} & \num{669.308} & \num{596.878}\\
    MASE & \num{125.466} & \num{124.422} & \num{122.573} & \num{122.651} & \textcolor{red}{\num{122.212}}\\
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
    MSE & \textcolor{red}{\num{0.00329}} & \num{0.00345} & \num{0.00345} & \num{0.00347} & \num{0.00329}\\
    MAE & \num{0.0396} & \num{0.0412} & \num{0.0401} & \num{0.0402} & \textcolor{red}{\num{0.0395}}\\
    MAPE & \num{1489.746} & \num{1212.184} & \num{883.724} & \textcolor{red}{\num{841.237}} & \num{1025.261}\\
    MASE & \num{182.435} & \num{191.708} & \num{185.403} & \num{185.955} & \textcolor{red}{\num{182.404}}\\
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
    MSE & \textcolor{red}{\num{0.0041}} & \num{0.00438} & \num{0.00424} & \num{0.00425} & \num{0.0041}\\
    MAE & \textcolor{red}{\num{0.0452}} & \num{0.0468} & \num{0.0461} & \num{0.0461} & \num{0.0452}\\
    MAPE & \num{188.329} & \num{339.626} & \num{118.282} & \textcolor{red}{\num{114.55}} & \num{174.705}\\
    MASE & \num{210.5} & \num{219.215} & \num{214.477} & \num{214.644} & \textcolor{red}{\num{210.266}}\\
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
    MSE & \num{0.00162} & \num{0.00161} & \num{0.00155} & \num{0.00155} & \textcolor{red}{\num{0.00154}}\\
    MAE & \num{0.0285} & \num{0.0286} & \num{0.0279} & \num{0.0279} & \textcolor{red}{\num{0.0278}}\\
    MAPE & \num{532.73} & \num{579.166} & \textcolor{red}{\num{477.962}} & \num{478.558} & \num{482.424}\\
    MASE & \num{131.258} & \num{130.23} & \num{128.365} & \num{128.341} & \textcolor{red}{\num{127.605}}\\
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
    MSE & \textcolor{red}{\num{0.00378}} & \num{0.004} & \num{0.00394} & \num{0.00393} & \num{0.00379}\\
    MAE & \num{0.0438} & \num{0.0452} & \num{0.0445} & \num{0.0445} & \textcolor{red}{\num{0.0437}}\\
    MAPE & \textcolor{red}{\num{301.451}} & \num{500.633} & \num{351.272} & \num{353.122} & \num{394.774}\\
    MASE & \num{210.224} & \num{212.877} & \num{212.416} & \num{212.338} & \textcolor{red}{\num{208.839}}\\
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
    MSE & \textcolor{red}{\num{0.00419}} & \num{0.00491} & \num{0.00444} & \num{0.00444} & \num{0.00428}\\
    MAE & \textcolor{red}{\num{0.0475}} & \num{0.0514} & \num{0.0487} & \num{0.0487} & \num{0.0481}\\
    MAPE & \num{294.808} & \num{313.358} & \textcolor{red}{\num{112.773}} & \num{113.234} & \num{171.917}\\
    MASE & \textcolor{red}{\num{217.374}} & \num{235.084} & \num{222.779} & \num{222.761} & \num{220.291}\\
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

    \caption{\label{tab:dgp4result}Out-of-sample forecasting performance measures for DGP4.}
    \centering
    \resizebox{\linewidth}{!}{
    \begin{tabular}[t]{cc|ccc|ccc|ccc|}
    \toprule
    \multicolumn{2}{c}{ } & \multicolumn{3}{c}{RMAFE} & \multicolumn{3}{c}{RMSFE} & \multicolumn{3}{c}{RMASE} \\
    \cmidrule(l{3pt}r{3pt}){3-5} \cmidrule(l{3pt}r{3pt}){6-8} \cmidrule(l{3pt}r{3pt}){9-11}
    \rotatebox{0}{} & \rotatebox{0}{} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$}\\
    \midrule
     & VHAR & \textcolor{black}{\num{.987}} & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{.987}} & \textcolor{black}{\num{1.016}} & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{.981}} & \textcolor{black}{\num{.994}}\\

     & BVAR & \textcolor{black}{\num{.987}} & \textcolor{black}{\num{.966}} & \textcolor{black}{\num{.985}} & \textcolor{black}{\num{.983}} & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{.964}} & \textcolor{black}{\num{.983}} & \textcolor{black}{\num{.966}} & \textcolor{black}{\num{.984}}\\

     & BVHAR-S & \textcolor{black}{\num{.982}} & \textcolor{red}{\num{.952}} & \textcolor{red}{\num{.961}} & \textcolor{black}{\num{.972}} & \textcolor{black}{\num{.988}} & \textcolor{red}{\num{.928}} & \textcolor{black}{\num{.978}} & \textcolor{red}{\num{.950}} & \textcolor{red}{\num{.960}}\\

    \multirow{-4}{*}{\centering\arraybackslash SMALL} & BVHAR-L & \textcolor{red}{\num{.980}} & \textcolor{black}{\num{.953}} & \textcolor{black}{\num{.994}} & \textcolor{red}{\num{.972}} & \textcolor{red}{\num{.969}} & \textcolor{black}{\num{.976}} & \textcolor{red}{\num{.977}} & \textcolor{black}{\num{.951}} & \textcolor{black}{\num{.994}}\\
    \cmidrule{1-11}
     & VHAR & \textcolor{black}{\num{.986}} & \textcolor{black}{\num{.994}} & \textcolor{black}{\num{1.039}} & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{.995}} & \textcolor{black}{\num{1.052}} & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{.996}} & \textcolor{black}{\num{1.042}}\\

     & BVAR & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{.983}} & \textcolor{black}{\num{.999}} & \textcolor{black}{\num{.992}} & \textcolor{black}{\num{.963}} & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{.989}} & \textcolor{black}{\num{.979}} & \textcolor{black}{\num{.999}}\\

     & BVHAR-S & \textcolor{red}{\num{.981}} & \textcolor{red}{\num{.957}} & \textcolor{black}{\num{.999}} & \textcolor{red}{\num{.980}} & \textcolor{red}{\num{.926}} & \textcolor{red}{\num{.997}} & \textcolor{red}{\num{.983}} & \textcolor{red}{\num{.952}} & \textcolor{black}{\num{.998}}\\

    \multirow{-4}{*}{\centering\arraybackslash MEDIUM} & BVHAR-L & \textcolor{black}{\num{.987}} & \textcolor{black}{\num{.975}} & \textcolor{red}{\num{.998}} & \textcolor{black}{\num{.991}} & \textcolor{black}{\num{.959}} & \textcolor{black}{\num{.997}} & \textcolor{black}{\num{.988}} & \textcolor{black}{\num{.973}} & \textcolor{red}{\num{.998}}\\
    \cmidrule{1-11}
     & VHAR & \textcolor{black}{\num{.996}} & \textcolor{black}{\num{1.013}} & \textcolor{black}{\num{1.023}} & \textcolor{black}{\num{.998}} & \textcolor{black}{\num{1.025}} & \textcolor{black}{\num{1.038}} & \textcolor{black}{\num{.995}} & \textcolor{black}{\num{1.015}} & \textcolor{black}{\num{1.022}}\\

     & BVAR & \textcolor{black}{\num{.990}} & \textcolor{black}{\num{.980}} & \textcolor{black}{\num{.994}} & \textcolor{black}{\num{.986}} & \textcolor{black}{\num{.976}} & \textcolor{black}{\num{.992}} & \textcolor{black}{\num{.992}} & \textcolor{black}{\num{.981}} & \textcolor{black}{\num{.992}}\\

     & BVHAR-S & \textcolor{red}{\num{.985}} & \textcolor{red}{\num{.971}} & \textcolor{black}{\num{.994}} & \textcolor{red}{\num{.970}} & \textcolor{red}{\num{.954}} & \textcolor{red}{\num{.990}} & \textcolor{black}{\num{.989}} & \textcolor{red}{\num{.973}} & \textcolor{black}{\num{.993}}\\

    \multirow{-4}{*}{\centering\arraybackslash LARGE} & BVHAR-L & \textcolor{black}{\num{.986}} & \textcolor{black}{\num{.976}} & \textcolor{red}{\num{.993}} & \textcolor{black}{\num{.980}} & \textcolor{black}{\num{.967}} & \textcolor{black}{\num{.993}} & \textcolor{red}{\num{.989}} & \textcolor{black}{\num{.978}} & \textcolor{red}{\num{.992}}\\
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
