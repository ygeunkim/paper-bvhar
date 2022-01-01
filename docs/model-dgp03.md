Simulating VAR-type Minnesota BVHAR
================
Young Geun Kim
02 Jan, 2022

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
# set seed for reproducible result-----
set.seed(1)
```

``` r
# result table-------------------------
source("report-fns.R")
# hyperparameter setting table---------
source("param-fns.R")
# Simulation---------------------------
dgp <- readRDS("../data/processed/bvharsim_dgp_s.rds")
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
    rep(1e-2, n_small), # sigma
    1e-2, # lambda
    rep(1e-2, n_small) # delta
  ), 
  upper = c(
    rep(1, n_small), # sigma
    1, # lambda
    rep(1, n_small) # delta
  ), 
  y = y_small_train, 
  p = bvar_lag, 
  include_mean = FALSE
))
#> Model Specification for BVAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: Minnesota
#> # Type '?bvar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#> [1]  0.2112  0.2112  0.0432
#> 
#> Setting for 'lambda':
#> [1]  0.336
#> 
#> Setting for 'delta':
#> [1]  0.178  0.178  0.010
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
    1, # lambda
    rep(1, n_medium) # delta
  ), 
  y = y_medium_train, 
  p = bvar_lag, 
  include_mean = FALSE
))
#> Model Specification for BVAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: Minnesota
#> # Type '?bvar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#> [1]  0.0403  0.0403  0.0349  0.0583  0.0505  0.0690  0.0716  0.0880  0.1100
#> 
#> Setting for 'lambda':
#> [1]  0.198
#> 
#> Setting for 'delta':
#> [1]  0.0857  0.0100  0.0410  0.0100  0.0175  0.0100  0.0574  0.0100  0.0254
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
    1, # lambda
    rep(1, n_large) # delta
  ), 
  y = y_large_train, 
  p = bvar_lag, 
  include_mean = FALSE
))
#> Model Specification for BVAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: Minnesota
#> # Type '?bvar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#>  [1]  0.0465  0.0454  0.0190  0.0341  0.0637  0.0369  0.0688  0.0770  0.1011
#> [10]  0.0817  0.0803  0.0785
#> 
#> Setting for 'lambda':
#> [1]  0.142
#> 
#> Setting for 'delta':
#>  [1]  0.0100  0.0100  0.0291  0.0568  0.0100  0.1248  0.0100  0.0152  0.0100
#> [10]  0.0275  0.0186  0.0286
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
    rep(1e-2, n_small), # sigma
    1e-2, # lambda
    rep(1e-2, n_small) # delta
  ), 
  upper = c(
    rep(1, n_small), # sigma
    1, # lambda
    rep(1, n_small) # delta
  ), 
  y = y_small_train, 
  include_mean = FALSE
))
#> Model Specification for BVHAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: MN_VAR
#> # Type '?bvhar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#> [1]  0.141  0.105  0.122
#> 
#> Setting for 'lambda':
#> [1]  0.271
#> 
#> Setting for 'delta':
#> [1]  0.0881  0.0430  0.0100
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
(bvhar_var_medium_optim <- choose_bvhar(
  bvhar_medium_spec, 
  lower = c(
    rep(1e-2, n_medium), # sigma
    1e-2, # lambda
    rep(1e-2, n_medium) # delta
  ), 
  upper = c(
    rep(1, n_medium), # sigma
    1, # lambda
    rep(1, n_medium) # delta
  ), 
  y = y_medium_train, 
  include_mean = FALSE
))
#> Model Specification for BVHAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: MN_VAR
#> # Type '?bvhar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#> [1]  0.0410  0.0391  0.0478  0.0586  0.0660  0.0620  0.0698  0.1403  0.0172
#> 
#> Setting for 'lambda':
#> [1]  0.195
#> 
#> Setting for 'delta':
#> [1]  0.0518  0.0100  0.0492  0.0343  0.0100  0.0100  0.0478  0.0100  0.0101
#> 
#> Setting for 'eps':
#> [1]  1e-04
```

``` r
(bvhar_var_large_optim <- choose_bvhar(
  bvhar_large_spec, 
  lower = c(
    rep(1e-2, n_large), # sigma
    1e-2, # lambda
    rep(1e-2, n_large) # delta
  ), 
  upper = c(
    rep(1, n_large), # sigma
    1, # lambda
    rep(1, n_large) # delta
  ), 
  y = y_large_train, 
  include_mean = FALSE
))
#> Model Specification for BVHAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: MN_VAR
#> # Type '?bvhar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#>  [1]  0.0441  0.0472  0.0100  0.0425  0.0721  0.0469  0.0711  0.0734  0.1036
#> [10]  0.0842  0.0822  0.0447
#> 
#> Setting for 'lambda':
#> [1]  0.181
#> 
#> Setting for 'delta':
#>  [1]  0.0100  0.0100  0.0529  0.0307  0.0100  0.0860  0.0100  0.0100  0.0100
#> [10]  0.0100  0.0100  0.0457
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
    rep(1e-2, n_small), # sigma
    1e-2, # lambda
    rep(1e-2, n_small), # daily
    rep(1e-2, n_small), # weekly
    rep(1e-2, n_small) # monthly
  ), 
  upper = c(
    rep(1, n_small), # sigma
    1, # lambda
    rep(1, n_small), # daily
    rep(1, n_small), # weekly
    rep(1, n_small) # monthly
  ), 
  y = y_small_train, 
  include_mean = FALSE
))
#> Model Specification for BVHAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: MN_VHAR
#> # Type '?bvhar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#> [1]  0.216  0.157  0.154
#> 
#> Setting for 'lambda':
#> [1]  0.279
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.1349  0.0495  0.0100
#> 
#> Setting for 'weekly':
#> [1]  0.135  0.010  0.135
#> 
#> Setting for 'monthly':
#> [1]  0.135  0.010  0.010
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
    1, # lambda
    rep(1, n_medium), # daily
    rep(1, n_medium), # weekly
    rep(1, n_medium) # monthly
  ), 
  y = y_medium_train, 
  include_mean = FALSE
))
#> Model Specification for BVHAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: MN_VHAR
#> # Type '?bvhar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#> [1]  0.0434  0.0393  0.0491  0.0597  0.0612  0.0624  0.0701  0.1367  0.0178
#> 
#> Setting for 'lambda':
#> [1]  0.191
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.0270  0.0100  0.0290  0.0100  0.0100  0.0100  0.0229  0.0100  0.0126
#> 
#> Setting for 'weekly':
#> [1]  0.2913  0.0883  0.0100  0.0101  0.0100  0.0111  0.0618  0.1527  0.0100
#> 
#> Setting for 'monthly':
#> [1]  0.1277  0.0867  0.0939  0.0526  0.2076  0.0168  0.0387  0.1525  0.0101
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
    1, # lambda
    rep(1, n_large), # daily
    rep(1, n_large), # weekly
    rep(1, n_large) # monthly
  ), 
  y = y_large_train, 
  include_mean = FALSE
))
#> Model Specification for BVHAR
#> 
#> Parameters: Coefficent matrice and Covariance matrix
#> Prior: MN_VHAR
#> # Type '?bvhar_minnesota' in the console for some help.
#> ========================================================
#> 
#> Setting for 'sigma':
#>  [1]  0.0418  0.0459  0.0100  0.0510  0.0698  0.0456  0.0673  0.0724  0.0932
#> [10]  0.0851  0.0740  0.0805
#> 
#> Setting for 'lambda':
#> [1]  0.15
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#>  [1]  0.0100  0.0100  0.0100  0.0100  0.0100  0.1187  0.0100  0.0101  0.0100
#> [10]  0.0100  0.0100  0.0152
#> 
#> Setting for 'weekly':
#>  [1]  0.0100  0.0100  0.3753  0.2162  0.1081  0.1626  0.0278  0.0100  0.0100
#> [10]  0.1926  0.0759  0.0465
#> 
#> Setting for 'monthly':
#>  [1]  0.010  0.396  0.902  1.000  0.627  0.492  0.581  0.415  0.500  0.379
#> [11]  0.621  0.486
```

``` r
fit_bvhar_small_vhar <- bvhar_vhar_small_optim$fit
fit_bvhar_medium_vhar <- bvhar_vhar_medium_optim$fit
fit_bvhar_large_vhar <- bvhar_vhar_large_optim$fit
```

# Errors

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
    \hspace{1em} & BVAR & $\sigma$ & 0.211 & 0.211 & 0.043 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.336 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.178 & 0.178 & 0.010 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.141 & 0.105 & 0.122 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.271 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.088 & 0.043 & 0.010 &  &  &  &  &  &  &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.216 & 0.157 & 0.154 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.279 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.135 & 0.049 & 0.010 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.135 & 0.010 & 0.135 &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.135 & 0.010 & 0.010 &  &  &  &  &  &  &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{MEDIUM}}\\
    \hspace{1em} & BVAR & $\sigma$ & 0.040 & 0.040 & 0.035 & 0.058 & 0.051 & 0.069 & 0.072 & 0.088 & 0.110 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.198 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.086 & 0.010 & 0.041 & 0.010 & 0.017 & 0.010 & 0.057 & 0.010 & 0.025 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.041 & 0.039 & 0.048 & 0.059 & 0.066 & 0.062 & 0.070 & 0.140 & 0.017 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.195 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.052 & 0.010 & 0.049 & 0.034 & 0.010 & 0.010 & 0.048 & 0.010 & 0.010 &  &  & \\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.043 & 0.039 & 0.049 & 0.060 & 0.061 & 0.062 & 0.070 & 0.137 & 0.018 &  &  & \\

    \hspace{1em} &  & $\lambda$ & 0.191 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.027 & 0.010 & 0.029 & 0.010 & 0.010 & 0.010 & 0.023 & 0.010 & 0.013 &  &  & \\

    \hspace{1em} &  & $w_i$ & 0.291 & 0.088 & 0.010 & 0.010 & 0.010 & 0.011 & 0.062 & 0.153 & 0.010 &  &  & \\

    \hspace{1em} &  & $m_i$ & 0.128 & 0.087 & 0.094 & 0.053 & 0.208 & 0.017 & 0.039 & 0.152 & 0.010 &  &  & \\
    \cmidrule{1-15}
    \addlinespace[0.3em]
    \multicolumn{15}{l}{\textbf{LARGE}}\\
    \hspace{1em} & BVAR & $\sigma$ & 0.046 & 0.045 & 0.019 & 0.034 & 0.064 & 0.037 & 0.069 & 0.077 & 0.101 & 0.082 & 0.080 & 0.078\\

    \hspace{1em} &  & $\lambda$ & 0.142 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.010 & 0.010 & 0.029 & 0.057 & 0.010 & 0.125 & 0.010 & 0.015 & 0.010 & 0.028 & 0.019 & 0.029\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-S & $\sigma$ & 0.044 & 0.047 & 0.010 & 0.042 & 0.072 & 0.047 & 0.071 & 0.073 & 0.104 & 0.084 & 0.082 & 0.045\\

    \hspace{1em} &  & $\lambda$ & 0.181 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $\delta$ & 0.010 & 0.010 & 0.053 & 0.031 & 0.010 & 0.086 & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.046\\
    \cmidrule{2-15}
    \hspace{1em} & BVHAR-L & $\sigma$ & 0.042 & 0.046 & 0.010 & 0.051 & 0.070 & 0.046 & 0.067 & 0.072 & 0.093 & 0.085 & 0.074 & 0.080\\

    \hspace{1em} &  & $\lambda$ & 0.150 &  &  &  &  &  &  &  &  &  &  & \\

    \hspace{1em} &  & $d_i$ & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.119 & 0.010 & 0.010 & 0.010 & 0.010 & 0.010 & 0.015\\

    \hspace{1em} &  & $w_i$ & 0.010 & 0.010 & 0.375 & 0.216 & 0.108 & 0.163 & 0.028 & 0.010 & 0.010 & 0.193 & 0.076 & 0.047\\

    \hspace{1em} &  & $m_i$ & 0.010 & 0.396 & 0.902 & 1.000 & 0.627 & 0.492 & 0.581 & 0.415 & 0.500 & 0.379 & 0.621 & 0.486\\*
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

<img src="../output/figs/DGP-3-smallcvonefig-1.png" width="70%" style="display: block; margin: auto;" />

<img src="../output/figs/DGP-3-smallcvfivefig-1.png" width="70%" style="display: block; margin: auto;" />

<img src="../output/figs/DGP-3-smallcvtwentyfig-1.png" width="70%" style="display: block; margin: auto;" />

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
     & asset01 & \num{0.01252} & \num{2.02976} & \num{0.00246} & \num{0.00319} & \textcolor{red}{\num{0.00194}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00671} & \num{0.96958} & \textcolor{red}{\num{0.00139}} & \num{0.00211} & \num{0.00145}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.1509} & \num{23.4826} & \num{0.0302} & \num{0.0393} & \textcolor{red}{\num{0.0263}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0567} & \num{8.8273} & \num{0.0114} & \num{0.0149} & \textcolor{red}{\num{0.0099}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0857} & \num{1.1756} & \num{0.0384} & \num{0.0448} & \textcolor{red}{\num{0.0354}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0608} & \num{0.8114} & \textcolor{red}{\num{0.0293}} & \num{0.0372} & \num{0.0304}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.287} & \num{3.995} & \num{0.139} & \num{0.159} & \textcolor{red}{\num{0.127}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.1444} & \num{1.9941} & \num{0.0689} & \num{0.0802} & \textcolor{red}{\num{0.0643}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.001748} & \num{0.023982} & \num{0.000783} & \num{0.000913} & \textcolor{red}{\num{0.000722}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.001794} & \num{0.023922} & \textcolor{red}{\num{0.000864}} & \num{0.001097} & \num{0.000895}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.001722} & \num{0.023993} & \num{0.000834} & \num{0.000953} & \textcolor{red}{\num{0.000763}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.001755} & \num{0.023966} & \num{0.000827} & \num{0.000988} & \textcolor{red}{\num{0.000793}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{195.739} & \num{2621.46} & \num{90.881} & \num{107.679} & \textcolor{red}{\num{82.242}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{139.773} & \num{1853.089} & \textcolor{red}{\num{65.59}} & \num{80.198} & \num{67.333}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{653.174} & \num{9330.211} & \num{311.953} & \num{372.484} & \textcolor{red}{\num{286.584}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{329.562} & \num{4601.587} & \num{156.141} & \num{186.787} & \textcolor{red}{\num{145.386}}\\*
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
     & asset01 & \num{0.2294} & \num{40.1395} & \num{0.0267} & \num{0.0424} & \textcolor{red}{\num{0.0247}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.1274} & \num{23.8079} & \num{0.0156} & \num{0.0259} & \textcolor{red}{\num{0.0144}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{2.775} & \num{491.704} & \num{0.323} & \num{0.525} & \textcolor{red}{\num{0.304}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{1.044} & \num{185.217} & \num{0.122} & \num{0.198} & \textcolor{red}{\num{0.114}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.366} & \num{5.236} & \num{0.129} & \num{0.166} & \textcolor{red}{\num{0.121}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.272} & \num{4.0296} & \num{0.0983} & \num{0.1309} & \textcolor{red}{\num{0.0922}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{1.276} & \num{18.319} & \num{0.45} & \num{0.586} & \textcolor{red}{\num{0.422}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.638} & \num{9.195} & \num{0.226} & \num{0.294} & \textcolor{red}{\num{0.212}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.00746} & \num{0.10682} & \num{0.00262} & \num{0.00339} & \textcolor{red}{\num{0.00246}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00802} & \num{0.11879} & \num{0.0029} & \num{0.00386} & \textcolor{red}{\num{0.00272}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00766} & \num{0.11001} & \num{0.0027} & \num{0.00352} & \textcolor{red}{\num{0.00253}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.00771} & \num{0.11188} & \num{0.00274} & \num{0.00359} & \textcolor{red}{\num{0.00257}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{815.761} & \num{11684.827} & \num{305.17} & \num{398.004} & \textcolor{red}{\num{280.646}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{608.396} & \num{8987.717} & \num{233.095} & \num{316.007} & \textcolor{red}{\num{214.276}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{2851.604} & \num{40877.618} & \num{1067.162} & \num{1414.731} & \textcolor{red}{\num{976.448}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{1425.254} & \num{20516.721} & \num{535.142} & \num{709.581} & \textcolor{red}{\num{490.457}}\\*
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
     & asset01 & \num{3.616} & \num{463.485} & \num{0.346} & \num{0.456} & \textcolor{red}{\num{0.202}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.7868} & \num{233.9074} & \num{0.1735} & \num{0.2284} & \textcolor{red}{\num{0.0999}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{41.99} & \num{5502.171} & \num{3.941} & \num{5.35} & \textcolor{red}{\num{2.364}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{15.798} & \num{2066.521} & \num{1.487} & \num{2.011} & \textcolor{red}{\num{0.889}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{1.459} & \num{17.767} & \num{0.445} & \num{0.549} & \textcolor{red}{\num{0.342}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{1.028} & \num{12.622} & \num{0.315} & \num{0.391} & \textcolor{red}{\num{0.241}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{4.971} & \num{61.215} & \num{1.503} & \num{1.882} & \textcolor{red}{\num{1.17}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{2.486} & \num{30.535} & \num{0.754} & \num{0.941} & \textcolor{red}{\num{0.585}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.02975} & \num{0.36244} & \num{0.00909} & \num{0.0112} & \textcolor{red}{\num{0.00699}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0303} & \num{0.3721} & \num{0.00927} & \num{0.01152} & \textcolor{red}{\num{0.00712}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.02985} & \num{0.36763} & \num{0.00903} & \num{0.0113} & \textcolor{red}{\num{0.00703}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.02997} & \num{0.36739} & \num{0.00913} & \num{0.01134} & \textcolor{red}{\num{0.00704}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{3247.797} & \num{39067.462} & \num{1052.379} & \num{1279.649} & \textcolor{red}{\num{759.397}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{2288.116} & \num{27758.512} & \num{749.655} & \num{916.329} & \textcolor{red}{\num{533.53}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{11074.86} & \num{134626.959} & \num{3547.97} & \num{4401.129} & \textcolor{red}{\num{2598.633}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{5536.925} & \num{67150.978} & \num{1783.335} & \num{2199.036} & \textcolor{red}{\num{1297.187}}\\*
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

<img src="../output/figs/DGP-3-medcvonefig-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-3-medcvfivefig-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-3-medcvtwentyfig-1.png" width="70%" style="display: block; margin: auto;" />

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
     & asset01 & \num{0.000384} & \num{0.000382} & \num{0.000361} & \num{0.000354} & \textcolor{red}{\num{0.000354}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00024} & \num{0.000248} & \num{0.00023} & \num{0.000222} & \textcolor{red}{\num{0.000222}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000355} & \num{0.000355} & \num{0.000343} & \num{0.000338} & \textcolor{red}{\num{0.000337}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000501} & \num{0.000512} & \num{0.000495} & \num{0.000492} & \textcolor{red}{\num{0.000491}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00106} & \textcolor{red}{\num{0.00101}} & \num{0.00108} & \num{0.00104} & \num{0.00104}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000887} & \num{0.000867} & \textcolor{red}{\num{0.000855}} & \num{0.000885} & \num{0.000883}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000572} & \num{0.000562} & \num{0.000554} & \textcolor{red}{\num{0.00054}} & \num{0.000543}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00668} & \textcolor{red}{\num{0.00602}} & \num{0.00638} & \num{0.00614} & \num{0.00612}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00494} & \num{0.0047} & \num{0.00486} & \num{0.00468} & \textcolor{red}{\num{0.00468}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00174} & \textcolor{red}{\num{0.00163}} & \num{0.00168} & \num{0.00163} & \num{0.00163}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0153} & \num{0.0153} & \num{0.0147} & \textcolor{red}{\num{0.0146}} & \num{0.0146}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0127} & \num{0.0127} & \num{0.0123} & \num{0.012} & \textcolor{red}{\num{0.012}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0152} & \num{0.0153} & \textcolor{red}{\num{0.0149}} & \num{0.0149} & \num{0.0149}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0174} & \num{0.0175} & \num{0.0172} & \num{0.0171} & \textcolor{red}{\num{0.0171}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0253} & \textcolor{red}{\num{0.0252}} & \num{0.0258} & \num{0.0257} & \num{0.0257}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0242} & \num{0.0243} & \textcolor{red}{\num{0.024}} & \num{0.0241} & \num{0.0241}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0184} & \num{0.018} & \num{0.0175} & \textcolor{red}{\num{0.0173}} & \num{0.0174}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0685} & \textcolor{red}{\num{0.0643}} & \num{0.0666} & \num{0.0647} & \num{0.0647}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0588} & \textcolor{red}{\num{0.0562}} & \num{0.0578} & \num{0.0562} & \num{0.0562}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0284} & \num{0.0276} & \num{0.0279} & \num{0.0274} & \textcolor{red}{\num{0.0274}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0123} & \num{0.0122} & \num{0.0118} & \textcolor{red}{\num{0.0117}} & \num{0.0117}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0112} & \num{0.0112} & \num{0.0109} & \num{0.0106} & \textcolor{red}{\num{0.0106}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00647} & \num{0.00653} & \textcolor{red}{\num{0.00633}} & \num{0.00635} & \num{0.00635}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0126} & \num{0.0127} & \num{0.0125} & \num{0.0124} & \textcolor{red}{\num{0.0124}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00709} & \textcolor{red}{\num{0.00706}} & \num{0.00724} & \num{0.00721} & \num{0.00721}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0263} & \num{0.0264} & \textcolor{red}{\num{0.0261}} & \num{0.0262} & \num{0.0262}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0114} & \num{0.0112} & \num{0.0109} & \textcolor{red}{\num{0.0108}} & \num{0.0108}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00738} & \textcolor{red}{\num{0.00693}} & \num{0.00718} & \num{0.00698} & \num{0.00697}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00654} & \textcolor{red}{\num{0.00625}} & \num{0.00643} & \num{0.00625} & \num{0.00625}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.0113} & \num{0.0112} & \num{0.011} & \num{0.0109} & \textcolor{red}{\num{0.0109}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{51.577} & \num{52.291} & \num{50.036} & \num{49.919} & \textcolor{red}{\num{49.791}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{41.697} & \num{41.137} & \num{40.196} & \num{38.765} & \textcolor{red}{\num{38.609}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{51.676} & \num{51.753} & \num{50.409} & \num{50.15} & \textcolor{red}{\num{50.134}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{57.095} & \num{58.262} & \num{56.838} & \num{56.729} & \textcolor{red}{\num{56.63}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{82.95}} & \num{84.615} & \num{86.106} & \num{86.213} & \num{86.293}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{83.219} & \textcolor{red}{\num{82.135}} & \num{82.83} & \num{82.315} & \num{82.288}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{63.018} & \num{60.348} & \num{59.082} & \textcolor{red}{\num{58.819}} & \num{58.98}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{224.896} & \textcolor{red}{\num{209.041}} & \num{218.496} & \num{210.515} & \num{210.429}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{200.552} & \num{195.135} & \num{199.458} & \num{192.941} & \textcolor{red}{\num{192.941}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{95.187} & \num{92.746} & \num{93.717} & \num{91.818} & \textcolor{red}{\num{91.788}}\\*
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
     & asset01 & \num{0.000365} & \num{0.000384} & \num{0.000363} & \textcolor{red}{\num{0.000361}} & \num{0.000361}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000214} & \num{0.000222} & \num{0.000209} & \textcolor{red}{\num{0.000206}} & \num{0.000206}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000413} & \num{0.000419} & \num{4e-04} & \textcolor{red}{\num{0.000387}} & \num{0.000388}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.000483}} & \num{0.000488} & \num{0.000486} & \num{0.000484} & \num{0.000484}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00114} & \textcolor{red}{\num{0.00111}} & \num{0.00115} & \num{0.00111} & \num{0.00111}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000957} & \textcolor{red}{\num{0.000936}} & \num{0.00095} & \num{0.000947} & \num{0.000946}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000563} & \num{0.000565} & \num{0.000556} & \num{0.00055} & \textcolor{red}{\num{0.00055}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00672} & \num{0.00657} & \num{0.00659} & \num{0.0065} & \textcolor{red}{\num{0.00648}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0052} & \num{0.00528} & \textcolor{red}{\num{0.00504}} & \num{0.00507} & \num{0.00506}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00178} & \num{0.00177} & \num{0.00175} & \num{0.00174} & \textcolor{red}{\num{0.00173}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0152} & \num{0.0154} & \num{0.015} & \textcolor{red}{\num{0.015}} & \num{0.015}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0121} & \num{0.0121} & \num{0.0118} & \textcolor{red}{\num{0.0116}} & \num{0.0117}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0164} & \num{0.0169} & \textcolor{red}{\num{0.0161}} & \num{0.0161} & \num{0.0161}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0172} & \num{0.0174} & \num{0.0172} & \num{0.0172} & \textcolor{red}{\num{0.0172}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.0264}} & \num{0.0268} & \num{0.0265} & \num{0.0265} & \num{0.0265}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0254} & \num{0.0253} & \num{0.0252} & \num{0.025} & \textcolor{red}{\num{0.025}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0179} & \num{0.0179} & \num{0.0176} & \num{0.0173} & \textcolor{red}{\num{0.0173}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0675} & \num{0.0655} & \num{0.066} & \num{0.0648} & \textcolor{red}{\num{0.0646}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0597} & \num{0.0597} & \num{0.059} & \num{0.0589} & \textcolor{red}{\num{0.0589}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0287} & \num{0.0286} & \num{0.0283} & \num{0.028} & \textcolor{red}{\num{0.028}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0122} & \num{0.0123} & \num{0.012} & \textcolor{red}{\num{0.012}} & \num{0.012}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0107} & \num{0.0107} & \num{0.0104} & \textcolor{red}{\num{0.0103}} & \num{0.0103}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00697} & \num{0.00721} & \textcolor{red}{\num{0.00684}} & \num{0.00685} & \num{0.00686}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0125} & \num{0.0126} & \num{0.0125} & \num{0.0125} & \textcolor{red}{\num{0.0125}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.00741}} & \num{0.00751} & \num{0.00745} & \num{0.00744} & \num{0.00743}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0276} & \num{0.0275} & \num{0.0274} & \num{0.0272} & \textcolor{red}{\num{0.0272}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0111} & \num{0.0111} & \num{0.011} & \num{0.0108} & \textcolor{red}{\num{0.0108}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00728} & \num{0.00706} & \num{0.00712} & \num{0.00698} & \textcolor{red}{\num{0.00696}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00665} & \num{0.00664} & \num{0.00657} & \num{0.00655} & \textcolor{red}{\num{0.00655}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.0114} & \num{0.0114} & \num{0.0112} & \textcolor{red}{\num{0.0112}} & \num{0.0112}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{49.679} & \num{50.305} & \textcolor{red}{\num{48.971}} & \num{49.357} & \num{49.658}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{40.758} & \num{40.465} & \num{39.73} & \textcolor{red}{\num{38.605}} & \num{38.702}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{53.463} & \num{56.083} & \textcolor{red}{\num{52.202}} & \num{53.25} & \num{53.275}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{57.131} & \num{57.363} & \num{57.054} & \num{56.724} & \textcolor{red}{\num{56.708}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{86.815}} & \num{88.286} & \num{87.423} & \num{87.509} & \num{87.51}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{79.893} & \num{80.749} & \num{79.555} & \num{79.123} & \textcolor{red}{\num{79.101}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{61.151} & \num{60.705} & \num{60.325} & \num{59.263} & \textcolor{red}{\num{59.216}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{224.362} & \num{219.44} & \num{218.948} & \num{216.604} & \textcolor{red}{\num{215.915}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{198.025} & \num{204.528} & \textcolor{red}{\num{196.888}} & \num{199.061} & \num{198.938}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{94.586} & \num{95.325} & \num{93.455} & \num{93.277} & \textcolor{red}{\num{93.225}}\\*
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
     & asset01 & \num{0.000407} & \textcolor{red}{\num{0.000392}} & \num{0.000408} & \num{0.000394} & \num{0.000393}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000239} & \textcolor{red}{\num{0.000224}} & \num{0.00024} & \num{0.000226} & \num{0.000225}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000399} & \textcolor{red}{\num{0.000384}} & \num{0.000398} & \num{0.000386} & \num{0.000385}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.00051}} & \num{0.000511} & \num{0.000513} & \num{0.000512} & \num{0.000513}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.000997}} & \num{0.001015} & \num{0.001019} & \num{0.001007} & \num{0.001007}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000858} & \textcolor{red}{\num{0.000855}} & \num{0.000866} & \num{0.000866} & \num{0.000865}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000507} & \textcolor{red}{\num{0.000496}} & \num{0.00051} & \num{0.000514} & \num{0.000513}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{0.00638}} & \num{0.00668} & \num{0.00649} & \num{0.0067} & \num{0.00669}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00494} & \textcolor{red}{\num{0.00477}} & \num{0.00513} & \num{0.00494} & \num{0.00492}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{0.00169}} & \num{0.0017} & \num{0.00173} & \num{0.00173} & \num{0.00172}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0159} & \num{0.0156} & \num{0.016} & \num{0.0156} & \textcolor{red}{\num{0.0155}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0128} & \textcolor{red}{\num{0.0122}} & \num{0.0129} & \num{0.0124} & \num{0.0124}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0161} & \num{0.016} & \textcolor{red}{\num{0.0159}} & \num{0.0161} & \num{0.0161}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0179} & \num{0.0179} & \num{0.0179} & \textcolor{red}{\num{0.0178}} & \num{0.0178}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0257} & \num{0.0255} & \num{0.0258} & \textcolor{red}{\num{0.0253}} & \num{0.0254}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0238} & \textcolor{red}{\num{0.0236}} & \num{0.024} & \num{0.0238} & \num{0.0238}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0162} & \textcolor{red}{\num{0.0159}} & \num{0.0163} & \num{0.0163} & \num{0.0163}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{0.0633}} & \num{0.0648} & \num{0.0635} & \num{0.0643} & \num{0.0644}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0581} & \textcolor{red}{\num{0.0571}} & \num{0.0595} & \num{0.0577} & \num{0.0576}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0278} & \textcolor{red}{\num{0.0276}} & \num{0.028} & \num{0.0277} & \num{0.0277}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0128} & \num{0.0125} & \num{0.0128} & \num{0.0125} & \textcolor{red}{\num{0.0124}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0113} & \textcolor{red}{\num{0.0108}} & \num{0.0114} & \num{0.011} & \num{0.0109}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00686} & \num{0.00681} & \textcolor{red}{\num{0.00676}} & \num{0.00687} & \num{0.00686}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.013} & \num{0.013} & \num{0.013} & \textcolor{red}{\num{0.0129}} & \num{0.0129}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0072} & \num{0.00716} & \num{0.00724} & \textcolor{red}{\num{0.00711}} & \num{0.00712}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0258} & \textcolor{red}{\num{0.0256}} & \num{0.026} & \num{0.0258} & \num{0.0258}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0101} & \textcolor{red}{\num{0.0099}} & \num{0.0101} & \num{0.0101} & \num{0.0101}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{0.00682}} & \num{0.00698} & \num{0.00685} & \num{0.00693} & \num{0.00694}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00647} & \textcolor{red}{\num{0.00636}} & \num{0.00662} & \num{0.00642} & \num{0.00641}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.0111} & \textcolor{red}{\num{0.011}} & \num{0.0112} & \num{0.0111} & \num{0.0111}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{54.305} & \textcolor{red}{\num{52.459}} & \num{54.377} & \num{52.857} & \num{52.75}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{45.121} & \textcolor{red}{\num{43.068}} & \num{45.547} & \num{43.792} & \num{43.734}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{54.183} & \num{54.194} & \textcolor{red}{\num{53.321}} & \num{54.594} & \num{54.49}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{57.062} & \num{57.182} & \num{56.918} & \textcolor{red}{\num{56.499}} & \num{56.591}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{86.318} & \num{86.282} & \num{87.651} & \textcolor{red}{\num{85.717}} & \num{85.846}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{75.8} & \textcolor{red}{\num{74.93}} & \num{76.272} & \num{75.632} & \num{75.608}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{54.48} & \textcolor{red}{\num{53.192}} & \num{54.847} & \num{54.578} & \num{54.532}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \textcolor{red}{\num{198.839}} & \num{202.4} & \num{201.438} & \num{201.566} & \num{201.695}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{197.117} & \textcolor{red}{\num{193.481}} & \num{201.438} & \num{194.098} & \num{193.612}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{91.47} & \textcolor{red}{\num{90.799}} & \num{92.423} & \num{91.037} & \num{90.984}\\*
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

<img src="../output/figs/DGP-3-largecvonefig-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-3-largecvfivefig-1.png" width="70%" style="display: block; margin: auto;" />

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

<img src="../output/figs/DGP-3-largecvtwentyfig-1.png" width="70%" style="display: block; margin: auto;" />

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
     & asset01 & \num{0.00029} & \num{0.000297} & \textcolor{red}{\num{0.000286}} & \num{0.000287} & \num{0.000287}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000421} & \num{0.000404} & \num{0.000389} & \textcolor{red}{\num{0.000382}} & \num{0.000382}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000241} & \num{0.000256} & \textcolor{red}{\num{0.000234}} & \num{0.000241} & \num{0.000241}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000437} & \num{0.000442} & \textcolor{red}{\num{0.000437}} & \num{0.000442} & \num{0.000445}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.000801} & \num{0.000776} & \num{0.000765} & \textcolor{red}{\num{0.000763}} & \num{0.000772}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000525} & \num{0.000517} & \num{0.000508} & \num{0.000504} & \textcolor{red}{\num{0.000501}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000724} & \num{0.000748} & \num{0.000692} & \num{0.000686} & \textcolor{red}{\num{0.000682}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00238} & \num{0.00229} & \num{0.00223} & \num{0.00222} & \textcolor{red}{\num{0.00221}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00401} & \num{0.00386} & \num{0.00394} & \num{0.00381} & \textcolor{red}{\num{0.00378}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.000896} & \num{0.000881} & \num{0.000879} & \textcolor{red}{\num{0.000863}} & \num{0.000869}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.0042} & \textcolor{red}{\num{0.00384}} & \num{0.00396} & \num{0.0039} & \num{0.00389}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.00555} & \textcolor{red}{\num{0.00501}} & \num{0.00536} & \num{0.00521} & \num{0.00522}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00171} & \num{0.00161} & \num{0.00164} & \num{0.00161} & \textcolor{red}{\num{0.00161}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0139} & \num{0.014} & \num{0.0137} & \num{0.0137} & \textcolor{red}{\num{0.0136}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.016} & \num{0.0157} & \num{0.0157} & \textcolor{red}{\num{0.0156}} & \num{0.0156}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0127} & \num{0.0127} & \textcolor{red}{\num{0.0124}} & \num{0.0125} & \num{0.0125}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0172} & \num{0.017} & \num{0.0168} & \textcolor{red}{\num{0.0168}} & \num{0.017}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0225} & \textcolor{red}{\num{0.0223}} & \num{0.0225} & \num{0.0225} & \num{0.0226}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0184} & \num{0.0183} & \num{0.0184} & \textcolor{red}{\num{0.0183}} & \num{0.0183}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0222} & \num{0.023} & \textcolor{red}{\num{0.0217}} & \num{0.0219} & \num{0.0217}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0394} & \num{0.0385} & \num{0.0379} & \num{0.0378} & \textcolor{red}{\num{0.0377}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0501} & \num{0.0499} & \num{0.0497} & \num{0.049} & \textcolor{red}{\num{0.0487}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.0236} & \num{0.0237} & \num{0.0237} & \num{0.0234} & \textcolor{red}{\num{0.0234}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.0501} & \num{0.049} & \num{0.0493} & \num{0.0489} & \textcolor{red}{\num{0.0485}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.0604} & \textcolor{red}{\num{0.0572}} & \num{0.0589} & \num{0.0578} & \num{0.0578}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0289} & \num{0.0284} & \num{0.0284} & \num{0.0282} & \textcolor{red}{\num{0.0281}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0199} & \num{0.02} & \num{0.0195} & \num{0.0195} & \textcolor{red}{\num{0.0194}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0213} & \num{0.0209} & \num{0.0209} & \textcolor{red}{\num{0.0208}} & \num{0.0208}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00792} & \num{0.00795} & \textcolor{red}{\num{0.00773}} & \num{0.00781} & \num{0.00782}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.00911} & \num{0.00902} & \num{0.00893} & \textcolor{red}{\num{0.00893}} & \num{0.00901}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0106} & \textcolor{red}{\num{0.0105}} & \num{0.0106} & \num{0.0106} & \num{0.0106}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.351} & \num{0.35} & \num{0.35} & \textcolor{red}{\num{0.348}} & \num{0.349}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0135} & \num{0.0139} & \textcolor{red}{\num{0.0132}} & \num{0.0132} & \num{0.0132}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0176} & \num{0.0172} & \num{0.0169} & \num{0.0169} & \textcolor{red}{\num{0.0168}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0261} & \num{0.026} & \num{0.0259} & \num{0.0255} & \textcolor{red}{\num{0.0254}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.0335} & \num{0.0336} & \num{0.0336} & \num{0.0333} & \textcolor{red}{\num{0.0332}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.00901} & \num{0.00881} & \num{0.00887} & \num{0.0088} & \textcolor{red}{\num{0.00872}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.00858} & \textcolor{red}{\num{0.00813}} & \num{0.00837} & \num{0.00822} & \num{0.00822}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.044} & \num{0.0438} & \num{0.0437} & \textcolor{red}{\num{0.0435}} & \num{0.0435}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{50.504}} & \num{51.681} & \num{50.516} & \num{50.7} & \num{50.578}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{56.46} & \num{55.184} & \num{55.195} & \textcolor{red}{\num{54.974}} & \num{55.071}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{42.272} & \num{41.65} & \textcolor{red}{\num{40.751}} & \num{41.104} & \num{41.213}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{56.169} & \num{55.396} & \textcolor{red}{\num{54.91}} & \num{55.007} & \num{55.232}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{78.223} & \textcolor{red}{\num{75.269}} & \num{77.573} & \num{77.028} & \num{77.45}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{61.063} & \num{60.754} & \num{60.623} & \textcolor{red}{\num{60.016}} & \num{60.115}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{77.178} & \num{79.704} & \num{73.953} & \num{73.936} & \textcolor{red}{\num{73.375}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{135.884} & \num{128.908} & \num{128.304} & \num{127.506} & \textcolor{red}{\num{127.053}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{167.453} & \num{164.778} & \num{165.999} & \num{163.371} & \textcolor{red}{\num{163.052}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{85.293} & \textcolor{red}{\num{83.03}} & \num{84.52} & \num{83.523} & \num{83.504}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{173.473} & \num{167.267} & \num{170.073} & \num{167.73} & \textcolor{red}{\num{165.742}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{203.367} & \num{194.547} & \num{197.528} & \num{193.357} & \textcolor{red}{\num{192.783}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{98.945} & \num{96.514} & \num{96.662} & \num{95.688} & \textcolor{red}{\num{95.431}}\\*
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
     & asset01 & \num{0.000282} & \num{0.000292} & \num{0.000281} & \textcolor{red}{\num{0.000277}} & \num{0.000277}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000383} & \num{0.00038} & \num{0.000379} & \textcolor{red}{\num{0.000369}} & \num{0.00037}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000237} & \num{0.000242} & \num{0.000245} & \textcolor{red}{\num{0.000232}} & \num{0.000232}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000477} & \num{0.000463} & \num{0.000462} & \num{0.000462} & \textcolor{red}{\num{0.000458}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.000755} & \num{0.000767} & \textcolor{red}{\num{0.000747}} & \num{0.00076} & \num{0.000764}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000491} & \num{0.000497} & \num{0.000487} & \num{0.000486} & \textcolor{red}{\num{0.000485}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000732} & \num{0.000716} & \num{0.000725} & \num{0.000692} & \textcolor{red}{\num{0.000686}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00219} & \num{0.0022} & \num{0.00218} & \num{0.00216} & \textcolor{red}{\num{0.00214}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0039} & \num{0.00378} & \num{0.00384} & \num{0.00376} & \textcolor{red}{\num{0.00373}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.00081}} & \num{0.000846} & \num{0.000816} & \num{0.000824} & \num{0.00084}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.00381} & \num{0.00356} & \num{0.0039} & \num{0.00357} & \textcolor{red}{\num{0.00354}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.00522} & \textcolor{red}{\num{0.00467}} & \num{0.00525} & \num{0.00477} & \num{0.00477}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00161} & \num{0.00153} & \num{0.00161} & \num{0.00153} & \textcolor{red}{\num{0.00152}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0137} & \num{0.0138} & \num{0.0136} & \textcolor{red}{\num{0.0135}} & \num{0.0135}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0156} & \num{0.0156} & \num{0.0156} & \textcolor{red}{\num{0.0154}} & \num{0.0154}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0123} & \num{0.0123} & \num{0.0126} & \textcolor{red}{\num{0.0122}} & \num{0.0123}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0176} & \textcolor{red}{\num{0.0171}} & \num{0.0173} & \num{0.0172} & \num{0.0171}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0219} & \num{0.0224} & \textcolor{red}{\num{0.0218}} & \num{0.0222} & \num{0.0223}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0177} & \num{0.0179} & \num{0.0177} & \num{0.0177} & \textcolor{red}{\num{0.0177}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0224} & \num{0.0225} & \num{0.0223} & \num{0.0218} & \textcolor{red}{\num{0.0217}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0375} & \num{0.038} & \num{0.0377} & \num{0.0372} & \textcolor{red}{\num{0.0371}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0493} & \num{0.0486} & \num{0.0484} & \num{0.0483} & \textcolor{red}{\num{0.048}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.0226} & \num{0.0232} & \textcolor{red}{\num{0.0226}} & \num{0.0227} & \num{0.0229}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.0488} & \num{0.0481} & \num{0.0503} & \num{0.0474} & \textcolor{red}{\num{0.0472}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.0578} & \textcolor{red}{\num{0.0543}} & \num{0.0585} & \num{0.055} & \num{0.0551}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0281} & \num{0.0278} & \num{0.0282} & \num{0.0276} & \textcolor{red}{\num{0.0275}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0195} & \num{0.0197} & \num{0.0194} & \textcolor{red}{\num{0.0193}} & \num{0.0193}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0207} & \num{0.0208} & \num{0.0208} & \textcolor{red}{\num{0.0205}} & \num{0.0206}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00772} & \num{0.00768} & \num{0.00786} & \textcolor{red}{\num{0.00764}} & \num{0.00772}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.00936} & \textcolor{red}{\num{0.00905}} & \num{0.00916} & \num{0.00911} & \num{0.00907}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0103} & \num{0.0105} & \textcolor{red}{\num{0.0103}} & \num{0.0105} & \num{0.0105}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.338} & \num{0.342} & \num{0.338} & \num{0.337} & \textcolor{red}{\num{0.337}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0135} & \num{0.0136} & \num{0.0135} & \num{0.0132} & \textcolor{red}{\num{0.0131}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0168} & \num{0.017} & \num{0.0168} & \num{0.0166} & \textcolor{red}{\num{0.0165}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0257} & \num{0.0253} & \num{0.0252} & \num{0.0252} & \textcolor{red}{\num{0.025}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.0322} & \num{0.033} & \textcolor{red}{\num{0.0321}} & \num{0.0323} & \num{0.0326}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.00878} & \num{0.00865} & \num{0.00904} & \num{0.00853} & \textcolor{red}{\num{0.00848}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.00822} & \textcolor{red}{\num{0.00772}} & \num{0.00832} & \num{0.00783} & \num{0.00783}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.0425} & \num{0.0429} & \num{0.0425} & \num{0.0423} & \textcolor{red}{\num{0.0423}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{43.669} & \num{44.035} & \num{43.337} & \textcolor{red}{\num{42.898}} & \num{43.051}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{54.507} & \num{53.806} & \num{54.054} & \textcolor{red}{\num{53.32}} & \num{53.411}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{41.484} & \num{41.309} & \num{41.792} & \textcolor{red}{\num{40.795}} & \num{40.92}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{60.461} & \textcolor{red}{\num{57.97}} & \num{58.363} & \num{58.271} & \num{58.088}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{75.073}} & \num{76.387} & \num{75.174} & \num{75.904} & \num{76.448}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{59.975} & \num{60.593} & \num{59.961} & \num{59.822} & \textcolor{red}{\num{59.677}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{79.001} & \num{78.554} & \num{79.639} & \num{77.336} & \textcolor{red}{\num{77.027}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{126.643} & \num{126.911} & \num{128.58} & \num{126.462} & \textcolor{red}{\num{126.166}}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{168.397} & \num{165.318} & \num{164.368} & \num{164.361} & \textcolor{red}{\num{163.209}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{77.609} & \num{78.93} & \textcolor{red}{\num{77.198}} & \num{77.696} & \num{78.562}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{165.367} & \num{160.307} & \num{171.221} & \num{159.085} & \textcolor{red}{\num{158.097}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{201.849} & \textcolor{red}{\num{188.612}} & \num{203.964} & \num{190.737} & \num{191.004}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{96.169} & \num{94.394} & \num{96.471} & \num{93.891} & \textcolor{red}{\num{93.805}}\\*
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
     & asset01 & \textcolor{red}{\num{0.000277}} & \num{0.000278} & \num{0.000278} & \num{0.000279} & \num{0.000278}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000377} & \num{0.000386} & \num{0.000379} & \num{0.00038} & \textcolor{red}{\num{0.000376}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.000251}} & \num{0.000264} & \num{0.000259} & \num{0.000261} & \num{0.000271}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{0.000453}} & \num{0.000457} & \num{0.000458} & \num{0.000467} & \num{0.000459}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.000744}} & \num{0.000758} & \num{0.000752} & \num{0.00076} & \num{0.000758}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000517} & \num{0.000525} & \num{0.000518} & \num{0.000517} & \textcolor{red}{\num{0.000514}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.00062} & \num{0.000655} & \textcolor{red}{\num{0.000619}} & \num{0.000636} & \num{0.00063}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0019} & \num{0.00196} & \textcolor{red}{\num{0.00189}} & \num{0.00193} & \num{0.00192}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00403} & \num{0.00398} & \num{0.00406} & \num{0.00399} & \textcolor{red}{\num{0.00397}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.000836} & \num{0.000841} & \num{0.000841} & \textcolor{red}{\num{0.000835}} & \num{0.000837}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{0.00297}} & \num{0.00304} & \num{0.00301} & \num{0.00305} & \num{0.00302}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.00413} & \num{0.00414} & \num{0.00424} & \num{0.00407} & \textcolor{red}{\num{0.00405}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00143} & \num{0.00144} & \num{0.00144} & \num{0.00143} & \textcolor{red}{\num{0.00142}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0136} & \textcolor{red}{\num{0.0136}} & \num{0.0137} & \num{0.0137} & \num{0.0136}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.0154}} & \num{0.0157} & \num{0.0154} & \num{0.0155} & \num{0.0154}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.0124}} & \num{0.0127} & \num{0.0125} & \num{0.0126} & \num{0.0128}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0171} & \textcolor{red}{\num{0.0171}} & \num{0.0173} & \num{0.0173} & \num{0.0172}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.022}} & \num{0.0223} & \num{0.0221} & \num{0.0222} & \num{0.0223}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0187} & \num{0.0188} & \num{0.0187} & \num{0.0187} & \textcolor{red}{\num{0.0186}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{0.0208}} & \num{0.0214} & \num{0.0209} & \num{0.0212} & \num{0.0211}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0339} & \num{0.0347} & \textcolor{red}{\num{0.0335}} & \num{0.0339} & \num{0.0338}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.05} & \textcolor{red}{\num{0.0495}} & \num{0.0503} & \num{0.0497} & \num{0.0496}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.0226}} & \num{0.0229} & \num{0.0227} & \num{0.0226} & \num{0.0227}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.0432} & \num{0.0442} & \textcolor{red}{\num{0.043}} & \num{0.0437} & \num{0.0435}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.0501} & \num{0.0504} & \num{0.05} & \num{0.0489} & \textcolor{red}{\num{0.0485}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0267} & \num{0.0269} & \num{0.0267} & \num{0.0267} & \textcolor{red}{\num{0.0266}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0195} & \textcolor{red}{\num{0.0194}} & \num{0.0195} & \num{0.0195} & \num{0.0195}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.0205}} & \num{0.021} & \num{0.0205} & \num{0.0207} & \num{0.0206}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.00775}} & \num{0.00793} & \num{0.00781} & \num{0.00786} & \num{0.00803}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.00908} & \textcolor{red}{\num{0.00907}} & \num{0.00917} & \num{0.0092} & \num{0.00913}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.0104}} & \num{0.0105} & \num{0.0104} & \num{0.0105} & \num{0.0105}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.356} & \num{0.359} & \num{0.356} & \num{0.356} & \textcolor{red}{\num{0.354}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{0.0126}} & \num{0.013} & \num{0.0127} & \num{0.0128} & \num{0.0128}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0152} & \num{0.0155} & \textcolor{red}{\num{0.0149}} & \num{0.0151} & \num{0.0151}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.026} & \textcolor{red}{\num{0.0258}} & \num{0.0262} & \num{0.0259} & \num{0.0258}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.0321}} & \num{0.0325} & \num{0.0322} & \num{0.0322} & \num{0.0322}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.00777} & \num{0.00795} & \textcolor{red}{\num{0.00773}} & \num{0.00785} & \num{0.00782}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.00712} & \num{0.00716} & \num{0.00711} & \num{0.00695} & \textcolor{red}{\num{0.00689}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.0437} & \num{0.0441} & \num{0.0437} & \num{0.0437} & \textcolor{red}{\num{0.0436}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \textcolor{red}{\num{47.98}} & \num{48.021} & \num{48.412} & \num{48.32} & \num{48.217}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{49.331} & \num{51.109} & \textcolor{red}{\num{49.268}} & \num{49.902} & \num{49.756}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{43.448}} & \num{44.328} & \num{44.04} & \num{43.953} & \num{44.693}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \textcolor{red}{\num{57.469}} & \num{58.067} & \num{57.482} & \num{57.809} & \num{57.666}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{81.971}} & \num{83.155} & \num{82.565} & \num{83.419} & \num{83.629}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{64.646} & \num{64.944} & \num{64.729} & \num{64.662} & \textcolor{red}{\num{64.284}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{69.927}} & \num{71.543} & \num{70.436} & \num{71.12} & \num{70.939}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{110.085} & \num{112.098} & \textcolor{red}{\num{109.44}} & \num{110.273} & \num{109.87}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{174.022} & \textcolor{red}{\num{172.735}} & \num{175.759} & \num{173.567} & \num{173.365}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{75.238}} & \num{76.164} & \num{75.422} & \num{75.486} & \num{75.687}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{141.638} & \num{142.236} & \num{140.808} & \num{140.545} & \textcolor{red}{\num{139.657}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{164.643} & \num{167.439} & \num{165.314} & \num{163.183} & \textcolor{red}{\num{161.508}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{90.033} & \num{90.987} & \num{90.306} & \num{90.187} & \textcolor{red}{\num{89.939}}\\*
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
    MSE & \num{0.0567} & \num{8.8273} & \num{0.0114} & \num{0.0149} & \textcolor{red}{\num{0.0099}}\\
    MAE & \num{0.1444} & \num{1.9941} & \num{0.0689} & \num{0.0802} & \textcolor{red}{\num{0.0643}}\\
    MAPE & \num{0.001755} & \num{0.023966} & \num{0.000827} & \num{0.000988} & \textcolor{red}{\num{0.000793}}\\
    MASE & \num{329.562} & \num{4601.587} & \num{156.141} & \num{186.787} & \textcolor{red}{\num{145.386}}\\
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
    MSE & \num{1.044} & \num{185.217} & \num{0.122} & \num{0.198} & \textcolor{red}{\num{0.114}}\\
    MAE & \num{0.638} & \num{9.195} & \num{0.226} & \num{0.294} & \textcolor{red}{\num{0.212}}\\
    MAPE & \num{0.00771} & \num{0.11188} & \num{0.00274} & \num{0.00359} & \textcolor{red}{\num{0.00257}}\\
    MASE & \num{1425.254} & \num{20516.721} & \num{535.142} & \num{709.581} & \textcolor{red}{\num{490.457}}\\
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
    MSE & \num{15.798} & \num{2066.521} & \num{1.487} & \num{2.011} & \textcolor{red}{\num{0.889}}\\
    MAE & \num{2.486} & \num{30.535} & \num{0.754} & \num{0.941} & \textcolor{red}{\num{0.585}}\\
    MAPE & \num{0.02997} & \num{0.36739} & \num{0.00913} & \num{0.01134} & \textcolor{red}{\num{0.00704}}\\
    MASE & \num{5536.925} & \num{67150.978} & \num{1783.335} & \num{2199.036} & \textcolor{red}{\num{1297.187}}\\
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
    MSE & \num{0.00174} & \textcolor{red}{\num{0.00163}} & \num{0.00168} & \num{0.00163} & \num{0.00163}\\
    MAE & \num{0.0284} & \num{0.0276} & \num{0.0279} & \num{0.0274} & \textcolor{red}{\num{0.0274}}\\
    MAPE & \num{0.0113} & \num{0.0112} & \num{0.011} & \num{0.0109} & \textcolor{red}{\num{0.0109}}\\
    MASE & \num{95.187} & \num{92.746} & \num{93.717} & \num{91.818} & \textcolor{red}{\num{91.788}}\\
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
    MSE & \num{0.00178} & \num{0.00177} & \num{0.00175} & \num{0.00174} & \textcolor{red}{\num{0.00173}}\\
    MAE & \num{0.0287} & \num{0.0286} & \num{0.0283} & \num{0.028} & \textcolor{red}{\num{0.028}}\\
    MAPE & \num{0.0114} & \num{0.0114} & \num{0.0112} & \textcolor{red}{\num{0.0112}} & \num{0.0112}\\
    MASE & \num{94.586} & \num{95.325} & \num{93.455} & \num{93.277} & \textcolor{red}{\num{93.225}}\\
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
    MSE & \textcolor{red}{\num{0.00169}} & \num{0.0017} & \num{0.00173} & \num{0.00173} & \num{0.00172}\\
    MAE & \num{0.0278} & \textcolor{red}{\num{0.0276}} & \num{0.028} & \num{0.0277} & \num{0.0277}\\
    MAPE & \num{0.0111} & \textcolor{red}{\num{0.011}} & \num{0.0112} & \num{0.0111} & \num{0.0111}\\
    MASE & \num{91.47} & \textcolor{red}{\num{90.799}} & \num{92.423} & \num{91.037} & \num{90.984}\\
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
    MSE & \num{0.00171} & \num{0.00161} & \num{0.00164} & \num{0.00161} & \textcolor{red}{\num{0.00161}}\\
    MAE & \num{0.0289} & \num{0.0284} & \num{0.0284} & \num{0.0282} & \textcolor{red}{\num{0.0281}}\\
    MAPE & \num{0.044} & \num{0.0438} & \num{0.0437} & \textcolor{red}{\num{0.0435}} & \num{0.0435}\\
    MASE & \num{98.945} & \num{96.514} & \num{96.662} & \num{95.688} & \textcolor{red}{\num{95.431}}\\
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
    MSE & \num{0.00161} & \num{0.00153} & \num{0.00161} & \num{0.00153} & \textcolor{red}{\num{0.00152}}\\
    MAE & \num{0.0281} & \num{0.0278} & \num{0.0282} & \num{0.0276} & \textcolor{red}{\num{0.0275}}\\
    MAPE & \num{0.0425} & \num{0.0429} & \num{0.0425} & \num{0.0423} & \textcolor{red}{\num{0.0423}}\\
    MASE & \num{96.169} & \num{94.394} & \num{96.471} & \num{93.891} & \textcolor{red}{\num{93.805}}\\
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
    MSE & \num{0.00143} & \num{0.00144} & \num{0.00144} & \num{0.00143} & \textcolor{red}{\num{0.00142}}\\
    MAE & \num{0.0267} & \num{0.0269} & \num{0.0267} & \num{0.0267} & \textcolor{red}{\num{0.0266}}\\
    MAPE & \num{0.0437} & \num{0.0441} & \num{0.0437} & \num{0.0437} & \textcolor{red}{\num{0.0436}}\\
    MASE & \num{90.033} & \num{90.987} & \num{90.306} & \num{90.187} & \textcolor{red}{\num{89.939}}\\
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

    \caption{\label{tab:dgp3result}Out-of-sample forecasting performance measures for DGP3.}
    \centering
    \resizebox{\linewidth}{!}{
    \begin{tabular}[t]{cc|ccc|ccc|ccc|}
    \toprule
    \multicolumn{2}{c}{ } & \multicolumn{3}{c}{RMAFE} & \multicolumn{3}{c}{RMSFE} & \multicolumn{3}{c}{RMASE} \\
    \cmidrule(l{3pt}r{3pt}){3-5} \cmidrule(l{3pt}r{3pt}){6-8} \cmidrule(l{3pt}r{3pt}){9-11}
    \rotatebox{0}{} & \rotatebox{0}{} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$}\\
    \midrule
     & VHAR & \textcolor{black}{\num{13.806}} & \textcolor{black}{\num{14.418}} & \textcolor{black}{\num{12.284}} & \textcolor{black}{\num{155.683}} & \textcolor{black}{\num{177.449}} & \textcolor{black}{\num{130.8124}} & \textcolor{black}{\num{13.963}} & \textcolor{black}{\num{14.395}} & \textcolor{black}{\num{12.128}}\\

     & BVAR & \textcolor{black}{\num{ 0.477}} & \textcolor{black}{\num{ 0.354}} & \textcolor{black}{\num{ 0.303}} & \textcolor{black}{\num{  0.200}} & \textcolor{black}{\num{  0.117}} & \textcolor{black}{\num{  0.0941}} & \textcolor{black}{\num{ 0.474}} & \textcolor{black}{\num{ 0.375}} & \textcolor{black}{\num{ 0.322}}\\

     & BVHAR-S & \textcolor{black}{\num{ 0.555}} & \textcolor{black}{\num{ 0.462}} & \textcolor{black}{\num{ 0.378}} & \textcolor{black}{\num{  0.262}} & \textcolor{black}{\num{  0.190}} & \textcolor{black}{\num{  0.1273}} & \textcolor{black}{\num{ 0.567}} & \textcolor{black}{\num{ 0.498}} & \textcolor{black}{\num{ 0.397}}\\

    \multirow{-4}{*}{\centering\arraybackslash SMALL} & BVHAR-L & \textcolor{red}{\num{ 0.445}} & \textcolor{red}{\num{ 0.332}} & \textcolor{red}{\num{ 0.235}} & \textcolor{red}{\num{  0.175}} & \textcolor{red}{\num{  0.110}} & \textcolor{red}{\num{  0.0562}} & \textcolor{red}{\num{ 0.441}} & \textcolor{red}{\num{ 0.344}} & \textcolor{red}{\num{ 0.234}}\\
    \cmidrule{1-11}
     & VHAR & \textcolor{black}{\num{0.308}} & \textcolor{black}{\num{0.3746}} & \textcolor{black}{\num{2.01e-02}} & \textcolor{black}{\num{0.1894}} & \textcolor{black}{\num{0.14033}} & \textcolor{black}{\num{4.03e-04}} & \textcolor{black}{\num{0.317}} & \textcolor{black}{\num{0.3746}} & \textcolor{black}{\num{2.01e-02}}\\

     & BVAR & \textcolor{black}{\num{0.750}} & \textcolor{black}{\num{1.2748}} & \textcolor{black}{\num{2.67e+00}} & \textcolor{black}{\num{0.7330}} & \textcolor{black}{\num{1.62535}} & \textcolor{black}{\num{7.11e+00}} & \textcolor{black}{\num{0.782}} & \textcolor{black}{\num{1.2749}} & \textcolor{black}{\num{2.67e+00}}\\

     & BVHAR-S & \textcolor{black}{\num{0.238}} & \textcolor{black}{\num{0.0773}} & \textcolor{black}{\num{3.68e-05}} & \textcolor{black}{\num{0.1080}} & \textcolor{black}{\num{0.00597}} & \textcolor{black}{\num{1.35e-09}} & \textcolor{black}{\num{0.244}} & \textcolor{black}{\num{0.0773}} & \textcolor{black}{\num{3.68e-05}}\\

    \multirow{-4}{*}{\centering\arraybackslash MEDIUM} & BVHAR-L & \textcolor{red}{\num{0.226}} & \textcolor{red}{\num{0.0580}} & \textcolor{red}{\num{1.16e-05}} & \textcolor{red}{\num{0.0973}} & \textcolor{red}{\num{0.00336}} & \textcolor{red}{\num{1.36e-10}} & \textcolor{red}{\num{0.233}} & \textcolor{red}{\num{0.0580}} & \textcolor{red}{\num{1.16e-05}}\\
    \cmidrule{1-11}
     & VHAR & \textcolor{black}{\num{0.264}} & \textcolor{black}{\num{0.3577}} & \textcolor{black}{\num{1.67e-02}} & \textcolor{black}{\num{0.1728}} & \textcolor{black}{\num{0.12795}} & \textcolor{black}{\num{2.79e-04}} & \textcolor{black}{\num{0.280}} & \textcolor{black}{\num{0.3577}} & \textcolor{black}{\num{1.67e-02}}\\

     & BVAR & \textcolor{black}{\num{0.715}} & \textcolor{black}{\num{1.3064}} & \textcolor{black}{\num{2.95e+00}} & \textcolor{black}{\num{0.7425}} & \textcolor{black}{\num{1.70699}} & \textcolor{black}{\num{8.70e+00}} & \textcolor{black}{\num{0.764}} & \textcolor{black}{\num{1.3065}} & \textcolor{black}{\num{2.95e+00}}\\

     & BVHAR-S & \textcolor{black}{\num{0.218}} & \textcolor{black}{\num{0.0695}} & \textcolor{black}{\num{2.41e-05}} & \textcolor{black}{\num{0.1059}} & \textcolor{black}{\num{0.00483}} & \textcolor{black}{\num{5.82e-10}} & \textcolor{black}{\num{0.231}} & \textcolor{black}{\num{0.0695}} & \textcolor{black}{\num{2.41e-05}}\\

    \multirow{-4}{*}{\centering\arraybackslash LARGE} & BVHAR-L & \textcolor{red}{\num{0.209}} & \textcolor{red}{\num{0.0538}} & \textcolor{red}{\num{8.69e-06}} & \textcolor{red}{\num{0.0967}} & \textcolor{red}{\num{0.00289}} & \textcolor{red}{\num{7.54e-11}} & \textcolor{red}{\num{0.221}} & \textcolor{red}{\num{0.0538}} & \textcolor{red}{\num{8.69e-06}}\\
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
