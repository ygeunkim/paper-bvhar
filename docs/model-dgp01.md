Simulating Minnesota VAR
================
Young Geun Kim
31 Dec, 2021

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
        -   [MEDIUM](#medium-1)
        -   [LARGE](#large-1)
    -   [SMALL](#small-2)
        -   [Plots](#plots)
        -   [Tables](#tables)
    -   [MEDIUM](#medium-2)
        -   [Plots](#plots-1)
        -   [Tables](#tables-1)
    -   [LARGE](#large-2)
        -   [Plots](#plots-2)
        -   [Tables](#tables-2)
    -   [Average](#average)
        -   [SMALL](#small-3)
        -   [MEDIUM](#medium-3)
        -   [LARGE](#large-3)
        -   [RMSFE or RMAFE](#rmsfe-or-rmafe)
-   [Additional](#additional)
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
dgp <- readRDS("../data/processed/bvarsim_dgp_wn.rds")
```

# BVAR Coefficient

## Minnesota prior

    \begin{table}

    \caption{\label{tab:smalltruespec}BVAR(5) and BVHAR True Hyperparameters - SMALL}
    \centering
    \begin{tabular}[t]{lrrr}
    \toprule
    $\sigma$ & 0.05 & 0.05 & 0.05\\
    $\lambda$ & 0.20 &  & \\
    $\delta$ & 0.00 & 0.00 & 0.00\\
    \bottomrule
    \end{tabular}
    \end{table}

    \begin{table}

    \caption{\label{tab:medtruespec}BVAR(5) and BVHAR True Hyperparameters - MEDIUM}
    \centering
    \begin{tabular}[t]{lrrr}
    \toprule
    $\sigma$ & 0.05 & 0.05 & 0.05\\
    $\lambda$ & 0.20 &  & \\
    $\delta$ & 0.00 & 0.00 & 0.00\\
    \bottomrule
    \end{tabular}
    \end{table}

    \begin{table}

    \caption{\label{tab:largetruespec}BVAR(5) and BVHAR True Hyperparameters - LARGE}
    \centering
    \begin{tabular}[t]{lrrrrrrrrrrrr}
    \toprule
    $\sigma$ & 0.05 & 0.05 & 0.05 & 0.05 & 0.05 & 0.05 & 0.05 & 0.05 & 0.05 & 0.05 & 0.05 & 0.05\\
    $\lambda$ & 0.03 &  &  &  &  &  &  &  &  &  &  & \\
    $\delta$ & 0.00 & 0.00 & 0.00 & 0.00 & 0.00 & 0.00 & 0.00 & 0.00 & 0.00 & 0.00 & 0.00 & 0.00\\
    \bottomrule
    \end{tabular}
    \end{table}

``` r
bvar_spec_list <- list(
  small = bvar_small_spec,
  medium = bvar_medium_spec,
  large = bvar_large_spec
)
```

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
#> [1]  0.0712  0.0339  0.0993
#> 
#> Setting for 'lambda':
#> [1]  0.427
#> 
#> Setting for 'delta':
#> [1]  0.0338  0.0636  0.0403
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
#> [1]  0.0375  0.0419  0.0451  0.0629  0.0677  0.0297  0.0679  0.1209  0.1068
#> 
#> Setting for 'lambda':
#> [1]  0.202
#> 
#> Setting for 'delta':
#> [1]  0.0813  0.0100  0.0778  0.0100  0.0100  0.0756  0.0753  0.0100  0.0121
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
#>  [1]  0.0441  0.0472  0.0402  0.0615  0.0463  0.0505  0.0486  0.0655  0.0649
#> [10]  0.0859  0.0719  0.0925
#> 
#> Setting for 'lambda':
#> [1]  0.131
#> 
#> Setting for 'delta':
#>  [1]  0.0100  0.0100  0.0100  0.0121  0.0100  0.0349  0.0132  0.0217  0.0100
#> [10]  0.0126  0.0249  0.0229
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
  sigma = rep(.05, n_small),
  lambda = .2,
  delta = rep(.1, n_small)
)
#----------------------------
bvhar_var_medium_spec <- set_bvhar(
  sigma = rep(.05, n_medium),
  lambda = .1,
  delta = rep(.1, n_medium)
)
#----------------------------
bvhar_var_large_spec <- set_bvhar(
  sigma = rep(.05, n_large),
  lambda = .03,
  delta = rep(.1, n_large)
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
#> [1]  0.0871  0.0201  0.0944
#> 
#> Setting for 'lambda':
#> [1]  1.05
#> 
#> Setting for 'delta':
#> [1]  0.0978  0.0988  0.0993
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
#> [1]  0.0409  0.0369  0.0528  0.0633  0.0645  0.0100  0.0643  0.1358  0.1102
#> 
#> Setting for 'lambda':
#> [1]  0.224
#> 
#> Setting for 'delta':
#> [1]  0.1812  0.0786  0.1827  0.0820  0.0848  0.0895  0.0874  0.0800  0.0889
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
#>  [1]  0.0425  0.0494  0.0460  0.0637  0.0758  0.0521  0.0624  0.0737  0.0140
#> [10]  0.0912  0.0747  0.0921
#> 
#> Setting for 'lambda':
#> [1]  0.143
#> 
#> Setting for 'delta':
#>  [1]  0.0832  0.0828  0.0802  0.0826  0.0842  0.0872  0.0832  0.1358  0.0912
#> [10]  0.0833  0.0877  0.1427
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
  sigma = rep(.05, n_small),
  lambda = .2,
  daily = rep(.1, n_small),
  weekly = rep(.05, n_small),
  monthly = rep(.1, n_small)
)
#-----------------------------------------
bvhar_vhar_medium_spec <- set_weight_bvhar(
  sigma = rep(.05, n_medium),
  lambda = .2,
  daily = rep(.1, n_medium),
  weekly = rep(.05, n_medium),
  monthly = rep(.1, n_medium)
)
#-----------------------------------------
bvhar_vhar_large_spec <- set_weight_bvhar(
  sigma = rep(.05, n_large),
  lambda = .2,
  daily = rep(.1, n_large),
  weekly = rep(.05, n_large),
  monthly = rep(.1, n_large)
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
#> [1]  0.0954  0.0600  0.2432
#> 
#> Setting for 'lambda':
#> [1]  1.77
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.0998  0.1021  0.1022
#> 
#> Setting for 'weekly':
#> [1]  0.0524  0.0518  0.0524
#> 
#> Setting for 'monthly':
#> [1]  0.0997  0.1033  0.1026
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
#> [1]  0.0470  0.0436  0.0621  0.0610  0.0757  0.0140  0.0890  0.1365  0.0822
#> 
#> Setting for 'lambda':
#> [1]  0.274
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#> [1]  0.0922  0.0868  0.0926  0.0854  0.0906  0.0923  0.0902  0.0907  0.0934
#> 
#> Setting for 'weekly':
#> [1]  0.1159  0.1000  0.1006  0.0440  0.0446  0.1128  0.1056  0.0470  0.0463
#> 
#> Setting for 'monthly':
#> [1]  0.1606  0.1470  0.1615  0.0917  0.0939  0.1651  0.0936  0.0937  0.0939
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
#>  [1]  0.0410  0.0464  0.0492  0.0733  0.0686  0.0488  0.0638  0.0685  0.0241
#> [10]  0.0794  0.0714  0.0798
#> 
#> Setting for 'lambda':
#> [1]  0.206
#> 
#> Setting for 'eps':
#> [1]  1e-04
#> 
#> Setting for 'daily':
#>  [1]  0.0964  0.0965  0.0961  0.0964  0.0965  0.0975  0.0957  0.0968  0.0983
#> [10]  0.0971  0.0972  0.0980
#> 
#> Setting for 'weekly':
#>  [1]  0.0491  0.0490  0.0623  0.0623  0.0518  0.0489  0.0591  0.0492  0.0494
#> [10]  0.0601  0.0626  0.0491
#> 
#> Setting for 'monthly':
#>  [1]  0.0988  0.0987  0.1052  0.0987  0.1119  0.0987  0.1117  0.1119  0.1124
#> [10]  0.0988  0.1124  0.0985
```

``` r
fit_bvhar_small_vhar <- bvhar_vhar_small_optim$fit
fit_bvhar_medium_vhar <- bvhar_vhar_medium_optim$fit
fit_bvhar_large_vhar <- bvhar_vhar_large_optim$fit
```

# Errors

## Hyperparameters

### SMALL

    \begin{table}

    \caption{\label{tab:hyperparamsmall}SMALL Simulation - Hyperparameter Lists}
    \centering
    \begin{tabular}[t]{lrrr}
    \toprule
    \addlinespace[0.3em]
    \multicolumn{4}{l}{\textbf{BVAR}}\\
    \hspace{1em}$\sigma$ & 0.071 & 0.034 & 0.099\\
    \hspace{1em}$\lambda$ & 0.427 &  & \\
    \hspace{1em}$\delta$ & 0.034 & 0.064 & 0.040\\
    \addlinespace[0.3em]
    \multicolumn{4}{l}{\textbf{BVHAR-VAR}}\\
    \hspace{1em}$\sigma$1 & 0.087 & 0.020 & 0.094\\
    \hspace{1em}$\lambda$1 & 1.053 &  & \\
    \hspace{1em}$\delta$ 1 & 0.098 & 0.099 & 0.099\\
    \addlinespace[0.3em]
    \multicolumn{4}{l}{\textbf{BVHAR-VHAR}}\\
    \hspace{1em}$\sigma$2 & 0.095 & 0.060 & 0.243\\
    \hspace{1em}$\lambda$2 & 1.772 &  & \\
    \hspace{1em}$d_i$ & 0.100 & 0.102 & 0.102\\
    \hspace{1em}$w_i$ & 0.052 & 0.052 & 0.052\\
    \hspace{1em}$m_i$ & 0.100 & 0.103 & 0.103\\
    \bottomrule
    \end{tabular}
    \end{table}

### MEDIUM

    \begin{table}

    \caption{\label{tab:hyperparammed}MEDIUM Simulation - Hyperparameter Lists}
    \centering
    \begin{tabular}[t]{lrrrrrrrrr}
    \toprule
    \addlinespace[0.3em]
    \multicolumn{10}{l}{\textbf{BVAR}}\\
    \hspace{1em}$\sigma$ & 0.038 & 0.042 & 0.045 & 0.063 & 0.068 & 0.030 & 0.068 & 0.121 & 0.107\\
    \hspace{1em}$\lambda$ & 0.202 &  &  &  &  &  &  &  & \\
    \hspace{1em}$\delta$ & 0.081 & 0.010 & 0.078 & 0.010 & 0.010 & 0.076 & 0.075 & 0.010 & 0.012\\
    \addlinespace[0.3em]
    \multicolumn{10}{l}{\textbf{BVHAR-VAR}}\\
    \hspace{1em}$\sigma$1 & 0.041 & 0.037 & 0.053 & 0.063 & 0.064 & 0.010 & 0.064 & 0.136 & 0.110\\
    \hspace{1em}$\lambda$1 & 0.224 &  &  &  &  &  &  &  & \\
    \hspace{1em}$\delta$ 1 & 0.181 & 0.079 & 0.183 & 0.082 & 0.085 & 0.089 & 0.087 & 0.080 & 0.089\\
    \addlinespace[0.3em]
    \multicolumn{10}{l}{\textbf{BVHAR-VHAR}}\\
    \hspace{1em}$\sigma$2 & 0.047 & 0.044 & 0.062 & 0.061 & 0.076 & 0.014 & 0.089 & 0.136 & 0.082\\
    \hspace{1em}$\lambda$2 & 0.274 &  &  &  &  &  &  &  & \\
    \hspace{1em}$d_i$ & 0.092 & 0.087 & 0.093 & 0.085 & 0.091 & 0.092 & 0.090 & 0.091 & 0.093\\
    \hspace{1em}$w_i$ & 0.116 & 0.100 & 0.101 & 0.044 & 0.045 & 0.113 & 0.106 & 0.047 & 0.046\\
    \hspace{1em}$m_i$ & 0.161 & 0.147 & 0.161 & 0.092 & 0.094 & 0.165 & 0.094 & 0.094 & 0.094\\
    \bottomrule
    \end{tabular}
    \end{table}

### LARGE

    \begin{table}

    \caption{\label{tab:hyperparamlarge}LARGE Simulation - Hyperparameter Lists}
    \centering
    \begin{tabular}[t]{lrrrrrrrrrrrr}
    \toprule
    \addlinespace[0.3em]
    \multicolumn{13}{l}{\textbf{BVAR}}\\
    \hspace{1em}$\sigma$ & 0.044 & 0.047 & 0.040 & 0.062 & 0.046 & 0.050 & 0.049 & 0.066 & 0.065 & 0.086 & 0.072 & 0.092\\
    \hspace{1em}$\lambda$ & 0.131 &  &  &  &  &  &  &  &  &  &  & \\
    \hspace{1em}$\delta$ & 0.010 & 0.010 & 0.010 & 0.012 & 0.010 & 0.035 & 0.013 & 0.022 & 0.010 & 0.013 & 0.025 & 0.023\\
    \addlinespace[0.3em]
    \multicolumn{13}{l}{\textbf{BVHAR-VAR}}\\
    \hspace{1em}$\sigma$1 & 0.043 & 0.049 & 0.046 & 0.064 & 0.076 & 0.052 & 0.062 & 0.074 & 0.014 & 0.091 & 0.075 & 0.092\\
    \hspace{1em}$\lambda$1 & 0.143 &  &  &  &  &  &  &  &  &  &  & \\
    \hspace{1em}$\delta$ 1 & 0.083 & 0.083 & 0.080 & 0.083 & 0.084 & 0.087 & 0.083 & 0.136 & 0.091 & 0.083 & 0.088 & 0.143\\
    \addlinespace[0.3em]
    \multicolumn{13}{l}{\textbf{BVHAR-VHAR}}\\
    \hspace{1em}$\sigma$2 & 0.041 & 0.046 & 0.049 & 0.073 & 0.069 & 0.049 & 0.064 & 0.069 & 0.024 & 0.079 & 0.071 & 0.080\\
    \hspace{1em}$\lambda$2 & 0.206 &  &  &  &  &  &  &  &  &  &  & \\
    \hspace{1em}$d_i$ & 0.096 & 0.096 & 0.096 & 0.096 & 0.096 & 0.098 & 0.096 & 0.097 & 0.098 & 0.097 & 0.097 & 0.098\\
    \hspace{1em}$w_i$ & 0.049 & 0.049 & 0.062 & 0.062 & 0.052 & 0.049 & 0.059 & 0.049 & 0.049 & 0.060 & 0.063 & 0.049\\
    \hspace{1em}$m_i$ & 0.099 & 0.099 & 0.105 & 0.099 & 0.112 & 0.099 & 0.112 & 0.112 & 0.112 & 0.099 & 0.112 & 0.098\\
    \bottomrule
    \end{tabular}
    \end{table}

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
     & asset01 & \num{0.00098} & \num{0.000951} & \num{0.000963} & \textcolor{red}{\num{0.000946}} & \num{0.00095}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000921} & \num{0.000984} & \textcolor{red}{\num{0.000912}} & \num{0.000975} & \num{0.000974}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0149} & \num{0.0144} & \num{0.0147} & \textcolor{red}{\num{0.0143}} & \num{0.0144}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0056} & \num{0.00544} & \num{0.00552} & \textcolor{red}{\num{0.00541}} & \num{0.00543}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0255} & \num{0.0251} & \num{0.0252} & \textcolor{red}{\num{0.025}} & \num{0.0251}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0239} & \num{0.0252} & \textcolor{red}{\num{0.0237}} & \num{0.0251} & \num{0.0252}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.1011} & \num{0.0978} & \num{0.0997} & \textcolor{red}{\num{0.0975}} & \num{0.0978}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0502} & \num{0.0494} & \num{0.0495} & \textcolor{red}{\num{0.0492}} & \num{0.0494}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.00564} & \num{0.00555} & \num{0.00557} & \textcolor{red}{\num{0.00552}} & \num{0.00554}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0512} & \num{0.054} & \textcolor{red}{\num{0.0507}} & \num{0.0538} & \num{0.0539}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00495} & \num{0.00479} & \num{0.00489} & \textcolor{red}{\num{0.00478}} & \num{0.00479}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.0206} & \num{0.0214} & \textcolor{red}{\num{0.0204}} & \num{0.0214} & \num{0.0214}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{57.161} & \num{56.667} & \textcolor{red}{\num{56.434}} & \num{56.529} & \num{56.761}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{52.16} & \num{56.654} & \textcolor{red}{\num{51.445}} & \num{56.474} & \num{56.643}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{224.051} & \num{214.963} & \num{220.927} & \textcolor{red}{\num{214.518}} & \num{215.429}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{111.124} & \num{109.428} & \num{109.602} & \textcolor{red}{\num{109.173}} & \num{109.611}\\*
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
     & asset01 & \num{0.0042} & \num{0.00393} & \num{0.00405} & \textcolor{red}{\num{0.00385}} & \num{0.00386}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00321} & \num{0.00317} & \textcolor{red}{\num{0.0031}} & \num{0.00311} & \num{0.00311}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0568} & \num{0.0536} & \num{0.0547} & \num{0.0524} & \textcolor{red}{\num{0.0524}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0214} & \num{0.0202} & \num{0.0206} & \num{0.0198} & \textcolor{red}{\num{0.0198}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0545} & \num{0.0525} & \num{0.0536} & \textcolor{red}{\num{0.0524}} & \num{0.0524}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.045} & \num{0.0447} & \textcolor{red}{\num{0.0441}} & \num{0.0443} & \num{0.0444}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.197} & \num{0.187} & \num{0.193} & \textcolor{red}{\num{0.185}} & \num{0.186}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0988} & \num{0.0946} & \num{0.0969} & \textcolor{red}{\num{0.0939}} & \num{0.0944}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.012} & \num{0.0116} & \num{0.0119} & \textcolor{red}{\num{0.0116}} & \num{0.0116}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0963} & \num{0.0958} & \textcolor{red}{\num{0.0945}} & \num{0.0949} & \num{0.0952}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00965} & \num{0.00914} & \num{0.00946} & \textcolor{red}{\num{0.00907}} & \num{0.00913}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.0393} & \num{0.0389} & \num{0.0386} & \textcolor{red}{\num{0.0385}} & \num{0.0386}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{121.743} & \num{116.963} & \num{119.841} & \textcolor{red}{\num{116.416}} & \num{116.596}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{103.244} & \num{102.338} & \textcolor{red}{\num{101.239}} & \num{101.71} & \num{102.034}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{450.19} & \num{423.417} & \num{441.577} & \textcolor{red}{\num{421.424}} & \num{425.524}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{225.059} & \num{214.239} & \num{220.885} & \textcolor{red}{\num{213.183}} & \num{214.718}\\*
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
     & asset01 & \num{0.00767} & \textcolor{red}{\num{0.00623}} & \num{0.00758} & \num{0.00644} & \num{0.0063}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00374} & \textcolor{red}{\num{0.00363}} & \num{0.00364} & \num{0.00381} & \num{0.00369}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.1056} & \textcolor{red}{\num{0.0805}} & \num{0.1045} & \num{0.0826} & \num{0.0811}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.039} & \textcolor{red}{\num{0.0301}} & \num{0.0386} & \num{0.031} & \num{0.0304}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0723} & \textcolor{red}{\num{0.0633}} & \num{0.0717} & \num{0.0656} & \num{0.0641}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.049} & \textcolor{red}{\num{0.048}} & \num{0.0485} & \num{0.0499} & \num{0.0489}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.27} & \textcolor{red}{\num{0.231}} & \num{0.269} & \num{0.237} & \num{0.235}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.13} & \textcolor{red}{\num{0.114}} & \num{0.13} & \num{0.118} & \num{0.116}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.016} & \textcolor{red}{\num{0.014}} & \num{0.0158} & \num{0.0145} & \num{0.0142}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.105} & \textcolor{red}{\num{0.103}} & \num{0.104} & \num{0.107} & \num{0.105}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0132} & \textcolor{red}{\num{0.0113}} & \num{0.0132} & \num{0.0116} & \num{0.0115}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.0447} & \textcolor{red}{\num{0.0427}} & \num{0.0442} & \num{0.0443} & \num{0.0435}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{158.39} & \textcolor{red}{\num{135.982}} & \num{157.068} & \num{142.355} & \num{138.523}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{109.204} & \textcolor{red}{\num{106.14}} & \num{107.965} & \num{111.642} & \num{109.108}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{605.516} & \textcolor{red}{\num{509.756}} & \num{602.157} & \num{530.874} & \num{524.002}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-4}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{291.037} & \textcolor{red}{\num{250.626}} & \num{289.063} & \num{261.624} & \num{257.211}\\*
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
     & asset01 & \num{0.000384} & \num{0.000373} & \num{0.00036} & \num{0.000356} & \textcolor{red}{\num{0.000354}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000231} & \num{0.000254} & \textcolor{red}{\num{0.000226}} & \num{0.000237} & \num{0.000236}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000351} & \num{0.000357} & \num{0.000349} & \num{0.000343} & \textcolor{red}{\num{0.000342}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000517} & \num{0.000538} & \num{0.000506} & \num{0.000498} & \textcolor{red}{\num{0.000496}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00097} & \num{0.001027} & \textcolor{red}{\num{0.000965}} & \num{0.00101} & \num{0.001011}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.000842}} & \num{0.000853} & \num{0.000843} & \num{0.000865} & \num{0.000852}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000534} & \num{0.000547} & \textcolor{red}{\num{0.000517}} & \num{0.000545} & \num{0.000547}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00607} & \num{0.00606} & \textcolor{red}{\num{0.00582}} & \num{0.00597} & \num{0.00595}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.00414}} & \num{0.00435} & \num{0.00418} & \num{0.00442} & \num{0.00438}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00156} & \num{0.0016} & \textcolor{red}{\num{0.00153}} & \num{0.00158} & \num{0.00157}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0153} & \num{0.015} & \num{0.0146} & \textcolor{red}{\num{0.0145}} & \num{0.0145}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0125} & \num{0.0127} & \textcolor{red}{\num{0.0122}} & \num{0.0123} & \num{0.0122}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.0149}} & \num{0.0154} & \num{0.015} & \num{0.0152} & \num{0.0151}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0177} & \num{0.018} & \num{0.0173} & \num{0.0172} & \textcolor{red}{\num{0.0171}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.0237}} & \num{0.025} & \num{0.024} & \num{0.0252} & \num{0.0251}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0234} & \num{0.0239} & \textcolor{red}{\num{0.0231}} & \num{0.0239} & \num{0.0236}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0178} & \num{0.0178} & \textcolor{red}{\num{0.0172}} & \num{0.0175} & \num{0.0176}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0649} & \num{0.0646} & \textcolor{red}{\num{0.0634}} & \num{0.0637} & \num{0.0637}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.0523}} & \num{0.0538} & \num{0.053} & \num{0.0544} & \num{0.0541}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0269} & \num{0.0274} & \textcolor{red}{\num{0.0266}} & \num{0.0271} & \num{0.027}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.00894} & \num{0.00878} & \num{0.00854} & \textcolor{red}{\num{0.0085}} & \num{0.00851}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00833} & \num{0.00848} & \textcolor{red}{\num{0.00814}} & \num{0.00821} & \num{0.00819}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.00875}} & \num{0.00902} & \num{0.00878} & \num{0.0089} & \num{0.00887}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0126} & \num{0.0128} & \num{0.0123} & \num{0.0122} & \textcolor{red}{\num{0.0122}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.0274}} & \num{0.0288} & \num{0.0276} & \num{0.029} & \num{0.029}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.00398} & \num{0.00407} & \textcolor{red}{\num{0.00393}} & \num{0.00407} & \num{0.00402}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.149} & \num{0.15} & \textcolor{red}{\num{0.144}} & \num{0.147} & \num{0.148}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0242} & \num{0.0241} & \textcolor{red}{\num{0.0236}} & \num{0.0237} & \num{0.0237}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.00713}} & \num{0.00733} & \num{0.00724} & \num{0.00743} & \num{0.00738}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.0279} & \num{0.0281} & \textcolor{red}{\num{0.0272}} & \num{0.0277} & \num{0.0278}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{51.509} & \num{51.266} & \textcolor{red}{\num{49.63}} & \num{49.753} & \num{49.67}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{40.704} & \num{40.981} & \num{39.832} & \num{39.669} & \textcolor{red}{\num{39.427}}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{50.379}} & \num{51.974} & \num{50.559} & \num{51.135} & \num{50.944}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{57.845} & \num{59.858} & \num{56.75} & \num{56.785} & \textcolor{red}{\num{56.615}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{79.297}} & \num{83.505} & \num{79.896} & \num{83.849} & \num{83.801}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{81.209} & \num{82.254} & \textcolor{red}{\num{81.187}} & \num{83.617} & \num{82.789}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{59.941} & \num{59.35} & \textcolor{red}{\num{57.773}} & \num{58.816} & \num{59.213}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{212.812} & \num{211.622} & \textcolor{red}{\num{204.272}} & \num{205.46} & \num{205.282}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{178.448}} & \num{183.837} & \num{181.19} & \num{185.16} & \num{184.092}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{90.238} & \num{91.627} & \textcolor{red}{\num{89.01}} & \num{90.472} & \num{90.204}\\*
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
     & asset01 & \num{0.000364} & \num{0.000371} & \textcolor{red}{\num{0.000362}} & \num{0.000363} & \num{0.000363}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00022} & \num{0.000229} & \textcolor{red}{\num{0.000218}} & \num{0.000224} & \num{0.000223}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000409} & \num{0.000394} & \num{0.000408} & \textcolor{red}{\num{0.000385}} & \num{0.000387}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000514} & \num{0.000514} & \num{0.000504} & \num{0.000489} & \textcolor{red}{\num{0.000489}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.00113}} & \num{0.00113} & \num{0.00114} & \num{0.00113} & \num{0.00113}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.000944}} & \num{0.000975} & \num{0.000968} & \num{0.000961} & \num{0.000951}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{0.000562}} & \num{0.000576} & \num{0.000565} & \num{0.000572} & \num{0.00057}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00658} & \num{0.00664} & \textcolor{red}{\num{0.00652}} & \num{0.00654} & \num{0.00654}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00486} & \num{0.00526} & \textcolor{red}{\num{0.00486}} & \num{0.00503} & \num{0.00502}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00173} & \num{0.00179} & \textcolor{red}{\num{0.00173}} & \num{0.00174} & \num{0.00174}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0151} & \num{0.0152} & \num{0.015} & \textcolor{red}{\num{0.0149}} & \num{0.0149}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0121} & \num{0.0122} & \textcolor{red}{\num{0.0118}} & \num{0.0121} & \num{0.0121}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0168} & \num{0.0164} & \num{0.0167} & \textcolor{red}{\num{0.0162}} & \num{0.0163}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.018} & \num{0.0178} & \num{0.0176} & \num{0.0172} & \textcolor{red}{\num{0.0172}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.0265}} & \num{0.0268} & \num{0.0269} & \num{0.0268} & \num{0.0268}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.0247}} & \num{0.0259} & \num{0.0254} & \num{0.0254} & \num{0.0252}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0176} & \num{0.0182} & \textcolor{red}{\num{0.0175}} & \num{0.0178} & \num{0.0178}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0654} & \num{0.0655} & \num{0.0643} & \textcolor{red}{\num{0.0643}} & \num{0.0643}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.0566}} & \num{0.0594} & \num{0.0576} & \num{0.0584} & \num{0.0581}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{0.0281}} & \num{0.0286} & \num{0.0281} & \num{0.0281} & \num{0.0281}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.00883} & \num{0.00888} & \num{0.00882} & \textcolor{red}{\num{0.00874}} & \num{0.00875}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00809} & \num{0.00815} & \textcolor{red}{\num{0.00789}} & \num{0.00809} & \num{0.00809}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00981} & \num{0.00962} & \num{0.0098} & \textcolor{red}{\num{0.0095}} & \num{0.00952}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0128} & \num{0.0127} & \num{0.0125} & \num{0.0122} & \textcolor{red}{\num{0.0122}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{0.0305}} & \num{0.0309} & \num{0.031} & \num{0.0309} & \num{0.0308}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.0042}} & \num{0.0044} & \num{0.00433} & \num{0.00433} & \num{0.0043}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.148} & \num{0.152} & \textcolor{red}{\num{0.147}} & \num{0.149} & \num{0.15}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0243} & \num{0.0244} & \num{0.0239} & \textcolor{red}{\num{0.0239}} & \num{0.0239}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{0.00772}} & \num{0.0081} & \num{0.00785} & \num{0.00796} & \num{0.00793}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.0282} & \num{0.0288} & \textcolor{red}{\num{0.0282}} & \num{0.0283} & \num{0.0284}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{50.156} & \num{49.668} & \num{49.657} & \textcolor{red}{\num{48.97}} & \num{49.128}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{40.051} & \num{40.934} & \textcolor{red}{\num{38.826}} & \num{40.379} & \num{40.397}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{54.794} & \num{54.227} & \num{55.274} & \textcolor{red}{\num{53.162}} & \num{53.281}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{59.763} & \num{58.453} & \num{58.663} & \textcolor{red}{\num{56.679}} & \num{56.71}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \textcolor{red}{\num{87.602}} & \num{88.474} & \num{89.208} & \num{88.82} & \num{88.725}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{77.395}} & \num{82.29} & \num{81.726} & \num{81.082} & \num{80.685}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{59.53} & \num{61.16} & \textcolor{red}{\num{59.494}} & \num{60.488} & \num{60.518}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{220.892} & \num{219.956} & \num{217.212} & \textcolor{red}{\num{216.457}} & \num{216.521}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \textcolor{red}{\num{189.241}} & \num{200.779} & \num{193.942} & \num{195.501} & \num{194.993}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \textcolor{red}{\num{93.269}} & \num{95.105} & \num{93.778} & \num{93.504} & \num{93.44}\\*
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
     & asset01 & \num{0.000405} & \textcolor{red}{\num{0.000384}} & \num{0.000409} & \num{0.000405} & \num{0.000404}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000232} & \num{0.000232} & \num{0.000226} & \textcolor{red}{\num{0.000224}} & \num{0.000224}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \textcolor{red}{\num{0.000371}} & \num{0.000381} & \num{0.000374} & \num{0.000372} & \num{0.000372}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.00051} & \num{0.000508} & \num{0.000505} & \textcolor{red}{\num{0.000502}} & \num{0.000507}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.000996} & \textcolor{red}{\num{0.00099}} & \num{0.000998} & \num{0.000996} & \num{0.000994}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000948} & \textcolor{red}{\num{0.00088}} & \num{0.000966} & \num{0.000913} & \num{0.000923}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000518} & \textcolor{red}{\num{0.000514}} & \num{0.000518} & \num{0.000518} & \num{0.000518}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0066} & \num{0.00666} & \textcolor{red}{\num{0.00658}} & \num{0.00658} & \num{0.00658}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00492} & \textcolor{red}{\num{0.00486}} & \num{0.00497} & \num{0.00492} & \num{0.00493}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00172} & \textcolor{red}{\num{0.00171}} & \num{0.00173} & \num{0.00172} & \num{0.00172}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0158} & \textcolor{red}{\num{0.0155}} & \num{0.016} & \num{0.0158} & \num{0.0158}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0125} & \num{0.0123} & \num{0.0122} & \textcolor{red}{\num{0.0122}} & \num{0.0122}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0156} & \num{0.0158} & \num{0.0157} & \num{0.0156} & \textcolor{red}{\num{0.0156}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0179} & \num{0.0178} & \num{0.0177} & \textcolor{red}{\num{0.0176}} & \num{0.0177}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0253} & \textcolor{red}{\num{0.0251}} & \num{0.0254} & \num{0.0252} & \num{0.0252}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0253} & \textcolor{red}{\num{0.0244}} & \num{0.0254} & \num{0.0245} & \num{0.0247}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0164} & \num{0.0165} & \num{0.0165} & \textcolor{red}{\num{0.0164}} & \num{0.0164}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0644} & \num{0.0648} & \textcolor{red}{\num{0.0641}} & \num{0.0642} & \num{0.0642}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0579} & \textcolor{red}{\num{0.0563}} & \num{0.0581} & \num{0.0571} & \num{0.0571}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0279} & \num{0.0276} & \num{0.0279} & \textcolor{red}{\num{0.0276}} & \num{0.0277}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.00928} & \textcolor{red}{\num{0.0091}} & \num{0.00939} & \num{0.00928} & \num{0.00927}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.00833} & \num{0.00825} & \num{0.00817} & \textcolor{red}{\num{0.00814}} & \num{0.00815}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00916} & \num{0.00925} & \num{0.0092} & \num{0.00915} & \textcolor{red}{\num{0.00914}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0127} & \num{0.0127} & \num{0.0126} & \textcolor{red}{\num{0.0125}} & \num{0.0126}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0292} & \textcolor{red}{\num{0.0289}} & \num{0.0293} & \num{0.0291} & \num{0.029}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0043} & \textcolor{red}{\num{0.00416}} & \num{0.00432} & \num{0.00417} & \num{0.0042}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.138} & \num{0.139} & \num{0.138} & \textcolor{red}{\num{0.138}} & \num{0.138}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0239} & \num{0.0241} & \textcolor{red}{\num{0.0238}} & \num{0.0239} & \num{0.0239}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00789} & \textcolor{red}{\num{0.00768}} & \num{0.00792} & \num{0.0078} & \num{0.00779}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.027} & \num{0.027} & \num{0.027} & \textcolor{red}{\num{0.0269}} & \num{0.0269}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{53.348} & \textcolor{red}{\num{52.844}} & \num{54.422} & \num{54.116} & \num{53.992}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{43.814} & \num{43.198} & \num{42.698} & \textcolor{red}{\num{42.691}} & \num{42.765}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{53.139} & \num{53.81} & \num{53.167} & \num{52.89} & \textcolor{red}{\num{52.823}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{56.958} & \num{56.916} & \num{56.136} & \textcolor{red}{\num{56.019}} & \num{56.236}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{86.091} & \textcolor{red}{\num{85.69}} & \num{86.342} & \num{86.021} & \num{85.858}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{81.114} & \textcolor{red}{\num{78.585}} & \num{82.301} & \num{79.243} & \num{79.596}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{54.63} & \textcolor{red}{\num{54.583}} & \num{54.669} & \num{54.618} & \num{54.646}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{203.476} & \num{203.247} & \textcolor{red}{\num{202.139}} & \num{202.52} & \num{202.832}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{192.651} & \textcolor{red}{\num{188.692}} & \num{195.1} & \num{192.181} & \num{191.912}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-10}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{91.691} & \textcolor{red}{\num{90.841}} & \num{91.886} & \num{91.144} & \num{91.185}\\*
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
     & asset01 & \num{0.000288} & \num{0.000297} & \textcolor{red}{\num{0.000283}} & \num{0.000289} & \num{0.00029}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000419} & \num{0.000403} & \textcolor{red}{\num{0.000377}} & \num{0.000377} & \num{0.000379}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000246} & \num{0.000255} & \textcolor{red}{\num{0.000238}} & \num{0.000243} & \num{0.000243}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000443} & \textcolor{red}{\num{0.00043}} & \num{0.000465} & \num{0.000452} & \num{0.000451}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.000906} & \textcolor{red}{\num{0.000747}} & \num{0.00089} & \num{0.000782} & \num{8e-04}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000519} & \num{0.000516} & \textcolor{red}{\num{0.000495}} & \num{0.000502} & \num{0.000502}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000712} & \num{0.000762} & \textcolor{red}{\num{0.000666}} & \num{0.000695} & \num{0.000693}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00247} & \num{0.00227} & \num{0.00234} & \textcolor{red}{\num{0.00214}} & \num{0.00218}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0043} & \num{0.00388} & \num{0.00439} & \textcolor{red}{\num{0.00366}} & \num{0.00369}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.00091} & \num{0.000889} & \num{0.000872} & \num{0.000868} & \textcolor{red}{\num{0.000867}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.00404} & \num{0.00389} & \textcolor{red}{\num{0.00363}} & \num{0.00379} & \num{0.00374}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.00526} & \num{0.00507} & \num{0.00496} & \num{0.005} & \textcolor{red}{\num{0.00493}}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00171} & \num{0.00162} & \num{0.00163} & \num{0.00157} & \textcolor{red}{\num{0.00156}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0139} & \num{0.0139} & \num{0.0136} & \textcolor{red}{\num{0.0136}} & \num{0.0136}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0159} & \num{0.0157} & \textcolor{red}{\num{0.0153}} & \num{0.0154} & \num{0.0154}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0126} & \num{0.0126} & \num{0.0123} & \textcolor{red}{\num{0.0121}} & \num{0.0122}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0176} & \textcolor{red}{\num{0.017}} & \num{0.0174} & \num{0.0173} & \num{0.0173}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0231} & \textcolor{red}{\num{0.0216}} & \num{0.0234} & \num{0.0227} & \num{0.0228}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.0184} & \num{0.0183} & \textcolor{red}{\num{0.0182}} & \num{0.0183} & \num{0.0183}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0221} & \num{0.0231} & \textcolor{red}{\num{0.0215}} & \num{0.0219} & \num{0.0219}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.0401} & \num{0.0389} & \num{0.0389} & \textcolor{red}{\num{0.0372}} & \num{0.0374}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0517} & \num{0.049} & \num{0.0534} & \textcolor{red}{\num{0.0477}} & \num{0.0479}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.0236} & \num{0.0237} & \num{0.0235} & \num{0.0235} & \textcolor{red}{\num{0.0234}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.0491} & \num{0.049} & \textcolor{red}{\num{0.0467}} & \num{0.048} & \num{0.0474}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.0582} & \textcolor{red}{\num{0.0555}} & \num{0.056} & \num{0.0563} & \num{0.0558}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0289} & \num{0.0282} & \num{0.0284} & \num{0.0278} & \textcolor{red}{\num{0.0278}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0598} & \num{0.0598} & \num{0.0585} & \textcolor{red}{\num{0.0585}} & \num{0.0587}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0357} & \num{0.0352} & \textcolor{red}{\num{0.0344}} & \num{0.0346} & \num{0.0346}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00693} & \num{0.0069} & \num{0.00673} & \textcolor{red}{\num{0.00666}} & \num{0.0067}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.01032} & \textcolor{red}{\num{0.00997}} & \num{0.01026} & \num{0.01017} & \num{0.01016}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00475} & \textcolor{red}{\num{0.00445}} & \num{0.0048} & \num{0.00467} & \num{0.00468}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.054} & \num{0.0539} & \textcolor{red}{\num{0.0535}} & \num{0.0537} & \num{0.0537}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.01014} & \num{0.01056} & \textcolor{red}{\num{0.00985}} & \num{0.01004} & \num{0.01004}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00929} & \num{0.00903} & \num{0.00902} & \textcolor{red}{\num{0.00862}} & \num{0.00866}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00414} & \num{0.00392} & \num{0.00428} & \textcolor{red}{\num{0.00381}} & \num{0.00383}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{0.047} & \num{0.0471} & \num{0.0468} & \num{0.0467} & \textcolor{red}{\num{0.0466}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.00837} & \num{0.00835} & \textcolor{red}{\num{0.00796}} & \num{0.00818} & \num{0.00808}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.00496} & \textcolor{red}{\num{0.00473}} & \num{0.00477} & \num{0.00479} & \num{0.00475}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.0213} & \num{0.0212} & \num{0.0209} & \textcolor{red}{\num{0.0209}} & \num{0.0209}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{50.293} & \num{51.425} & \textcolor{red}{\num{49.918}} & \num{50.385} & \num{50.547}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{56.037} & \num{55.221} & \textcolor{red}{\num{53.907}} & \num{54.358} & \num{54.427}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{41.545} & \num{40.296} & \num{39.496} & \textcolor{red}{\num{39.038}} & \num{39.383}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{57.475} & \textcolor{red}{\num{54.74}} & \num{57.069} & \num{56.369} & \num{56.368}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{80.44} & \textcolor{red}{\num{74.721}} & \num{80.656} & \num{77.623} & \num{77.482}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{60.702} & \num{60.574} & \num{59.961} & \textcolor{red}{\num{59.885}} & \num{59.891}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{76.875} & \num{79.158} & \textcolor{red}{\num{73.556}} & \num{74.727} & \num{74.646}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{140.125} & \num{131.056} & \num{133.847} & \textcolor{red}{\num{126.029}} & \num{126.618}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{175.575} & \num{160.646} & \num{180.899} & \textcolor{red}{\num{160.342}} & \num{161.077}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{85.309} & \textcolor{red}{\num{82.656}} & \num{84.098} & \num{83.726} & \num{83.557}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{170.476} & \num{168.24} & \textcolor{red}{\num{164.134}} & \num{166.628} & \num{164.359}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{198.427} & \textcolor{red}{\num{188.649}} & \num{188.731} & \num{190.001} & \num{188.695}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{99.44} & \num{95.615} & \num{97.189} & \num{94.926} & \textcolor{red}{\num{94.754}}\\*
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
     & asset01 & \num{0.000277} & \num{0.000296} & \textcolor{red}{\num{0.000275}} & \num{0.000275} & \num{0.000275}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.000375} & \num{0.000377} & \num{0.000369} & \textcolor{red}{\num{0.000364}} & \num{0.000364}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000236} & \num{0.000249} & \num{0.000244} & \num{0.000234} & \textcolor{red}{\num{0.000232}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000476} & \num{0.000461} & \num{0.000469} & \num{0.00046} & \textcolor{red}{\num{0.000459}}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00083} & \num{0.000847} & \num{0.000855} & \textcolor{red}{\num{0.000759}} & \num{0.000764}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{0.000492} & \num{0.000498} & \num{0.000491} & \num{0.000489} & \textcolor{red}{\num{0.000488}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.000743} & \num{0.000726} & \num{0.000726} & \textcolor{red}{\num{0.000695}} & \num{0.000696}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00215} & \num{0.00216} & \num{0.00213} & \textcolor{red}{\num{0.00202}} & \num{0.00204}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00404} & \num{0.00359} & \num{0.00424} & \num{0.0036} & \textcolor{red}{\num{0.00357}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.000838}} & \num{0.00085} & \num{0.000851} & \num{0.000841} & \num{0.000843}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.00361} & \textcolor{red}{\num{0.00347}} & \num{0.00362} & \num{0.00354} & \num{0.00348}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.0051} & \textcolor{red}{\num{0.00435}} & \num{0.0051} & \num{0.00478} & \num{0.00467}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.0016} & \textcolor{red}{\num{0.00149}} & \num{0.00161} & \num{0.0015} & \num{0.00149}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0135} & \num{0.0138} & \num{0.0133} & \num{0.0133} & \textcolor{red}{\num{0.0133}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0154} & \num{0.0155} & \num{0.0154} & \textcolor{red}{\num{0.0153}} & \num{0.0153}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.012} & \num{0.0122} & \num{0.0123} & \num{0.0119} & \textcolor{red}{\num{0.0118}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0179} & \textcolor{red}{\num{0.0173}} & \num{0.0176} & \num{0.0174} & \num{0.0174}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0231} & \num{0.0231} & \num{0.0236} & \textcolor{red}{\num{0.0226}} & \num{0.0226}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.0177}} & \num{0.0179} & \num{0.0178} & \num{0.0178} & \num{0.0177}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.0224} & \num{0.0224} & \num{0.022} & \num{0.0217} & \textcolor{red}{\num{0.0217}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.037} & \num{0.0377} & \num{0.0369} & \textcolor{red}{\num{0.0362}} & \num{0.0364}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0498} & \num{0.0472} & \num{0.0509} & \num{0.0467} & \textcolor{red}{\num{0.0461}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.0229}} & \num{0.0232} & \num{0.023} & \num{0.023} & \num{0.023}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.0477} & \num{0.0475} & \num{0.0482} & \num{0.0477} & \textcolor{red}{\num{0.0472}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.0571} & \textcolor{red}{\num{0.0531}} & \num{0.0568} & \num{0.055} & \num{0.0543}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.028} & \num{0.0276} & \num{0.0282} & \num{0.0274} & \textcolor{red}{\num{0.0272}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.058} & \num{0.0595} & \num{0.0574} & \num{0.0574} & \textcolor{red}{\num{0.0574}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{0.0345} & \num{0.0349} & \num{0.0346} & \textcolor{red}{\num{0.0343}} & \num{0.0343}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0066} & \num{0.0067} & \num{0.00674} & \num{0.00652} & \textcolor{red}{\num{0.00649}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0105} & \textcolor{red}{\num{0.0101}} & \num{0.0103} & \num{0.0102} & \num{0.0102}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00475} & \num{0.00475} & \num{0.00485} & \textcolor{red}{\num{0.00463}} & \num{0.00464}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.0521}} & \num{0.0527} & \num{0.0524} & \num{0.0522} & \num{0.0522}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{0.01023} & \num{0.01027} & \num{0.01008} & \num{0.00994} & \textcolor{red}{\num{0.00993}}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00858} & \num{0.00874} & \num{0.00856} & \textcolor{red}{\num{0.0084}} & \num{0.00843}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00399} & \num{0.00378} & \num{0.00407} & \num{0.00373} & \textcolor{red}{\num{0.00369}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.0455}} & \num{0.0461} & \num{0.0458} & \num{0.0459} & \num{0.0459}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{0.00813} & \num{0.00808} & \num{0.00821} & \num{0.00813} & \textcolor{red}{\num{0.00803}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{0.00486} & \textcolor{red}{\num{0.00452}} & \num{0.00484} & \num{0.00468} & \num{0.00462}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.0207} & \num{0.0208} & \num{0.0207} & \num{0.0205} & \textcolor{red}{\num{0.0205}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{42.778} & \num{43.927} & \num{42.329} & \num{42.32} & \textcolor{red}{\num{42.287}}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{53.737} & \num{53.682} & \num{53.171} & \textcolor{red}{\num{52.903}} & \num{52.909}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{39.712} & \num{40.369} & \num{40.532} & \num{39.407} & \textcolor{red}{\num{39.211}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{60.9} & \textcolor{red}{\num{58.514}} & \num{58.959} & \num{58.823} & \num{58.647}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{78.352} & \textcolor{red}{\num{77.748}} & \num{80.625} & \num{77.766} & \num{77.906}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{60.106} & \num{60.473} & \num{60.267} & \num{60.1} & \textcolor{red}{\num{60.02}}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \num{78.059} & \num{78.998} & \num{77.649} & \textcolor{red}{\num{76.084}} & \num{76.172}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{125.207} & \num{124.547} & \num{125.772} & \textcolor{red}{\num{122.647}} & \num{123.192}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{168.387} & \num{159.074} & \num{169.079} & \num{158.753} & \textcolor{red}{\num{156.048}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \num{78.757} & \num{78.695} & \num{78.831} & \num{78.653} & \textcolor{red}{\num{78.628}}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \num{160.037} & \num{159.947} & \num{160.389} & \num{160.221} & \textcolor{red}{\num{157.871}}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \num{200.232} & \textcolor{red}{\num{184.354}} & \num{197.227} & \num{191.716} & \num{188.785}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{95.522} & \num{93.361} & \num{95.403} & \num{93.283} & \textcolor{red}{\num{92.64}}\\*
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
     & asset01 & \num{0.000291} & \textcolor{red}{\num{0.000281}} & \num{0.000289} & \num{0.000286} & \num{0.000285}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.000376}} & \num{0.000393} & \num{0.000377} & \num{0.000384} & \num{0.000383}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.000263} & \num{0.000275} & \num{0.000257} & \num{0.000244} & \textcolor{red}{\num{0.000242}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.000458} & \num{0.000443} & \textcolor{red}{\num{0.00044}} & \num{0.000444} & \num{0.000444}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.000845} & \num{0.000888} & \num{0.000796} & \num{0.000758} & \textcolor{red}{\num{0.000744}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.000512}} & \num{0.000531} & \num{0.000514} & \num{0.000521} & \num{0.00052}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{0.000633}} & \num{0.000742} & \num{0.000655} & \num{0.000689} & \num{0.000679}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.002} & \num{0.00193} & \num{0.0019} & \textcolor{red}{\num{0.00181}} & \num{0.00182}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00409} & \num{0.00392} & \num{0.00384} & \num{0.00379} & \textcolor{red}{\num{0.00379}}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.000831}} & \num{0.000847} & \num{0.000834} & \num{0.000839} & \num{0.000838}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{0.00292}} & \num{0.0035} & \num{0.00309} & \num{0.00338} & \num{0.00332}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{0.00409}} & \num{0.00483} & \num{0.00429} & \num{0.0046} & \num{0.00452}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MSE} & \cellcolor{gray}{Average} & \num{0.00144} & \num{0.00155} & \textcolor{red}{\num{0.00144}} & \num{0.00148} & \num{0.00147}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0139} & \textcolor{red}{\num{0.0135}} & \num{0.0139} & \num{0.0138} & \num{0.0138}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.0154}} & \num{0.0157} & \num{0.0155} & \num{0.0156} & \num{0.0156}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.0127} & \num{0.0129} & \num{0.0127} & \num{0.0122} & \textcolor{red}{\num{0.0122}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.0173} & \num{0.0171} & \textcolor{red}{\num{0.0168}} & \num{0.017} & \num{0.017}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.0239} & \num{0.0244} & \num{0.0232} & \num{0.0227} & \textcolor{red}{\num{0.0222}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.0187}} & \num{0.019} & \num{0.0187} & \num{0.0188} & \num{0.0188}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{0.021}} & \num{0.0231} & \num{0.0213} & \num{0.0219} & \num{0.0218}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.036} & \num{0.0356} & \num{0.0351} & \textcolor{red}{\num{0.0336}} & \num{0.0338}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.0515} & \num{0.0491} & \num{0.0501} & \textcolor{red}{\num{0.0479}} & \num{0.048}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.0226}} & \num{0.0231} & \num{0.0227} & \num{0.0228} & \num{0.0227}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{0.0425}} & \num{0.0487} & \num{0.0437} & \num{0.0472} & \num{0.0466}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{0.0504}} & \num{0.0584} & \num{0.0522} & \num{0.053} & \num{0.0524}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAE} & \cellcolor{gray}{Average} & \num{0.0272} & \num{0.0284} & \num{0.0271} & \num{0.0272} & \textcolor{red}{\num{0.0271}}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{0.0597} & \textcolor{red}{\num{0.0582}} & \num{0.0597} & \num{0.0594} & \num{0.0593}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \textcolor{red}{\num{0.0346}} & \num{0.0353} & \num{0.0347} & \num{0.0351} & \num{0.0351}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{0.00698} & \num{0.00709} & \num{0.00694} & \num{0.00672} & \textcolor{red}{\num{0.0067}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{0.01016} & \num{0.01008} & \textcolor{red}{\num{0.0099}} & \num{0.00997} & \num{0.01}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{0.00491} & \num{0.00501} & \num{0.00476} & \num{0.00465} & \textcolor{red}{\num{0.00456}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \textcolor{red}{\num{0.0549}} & \num{0.0559} & \num{0.055} & \num{0.0553} & \num{0.0552}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{0.00963}} & \num{0.01059} & \num{0.00976} & \num{0.01001} & \num{0.00997}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{0.00835} & \num{0.00824} & \num{0.00814} & \textcolor{red}{\num{0.0078}} & \num{0.00783}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{0.00412} & \num{0.00393} & \num{0.00401} & \textcolor{red}{\num{0.00383}} & \num{0.00384}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{0.0449}} & \num{0.0459} & \num{0.0451} & \num{0.0453} & \num{0.0452}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{0.00725}} & \num{0.0083} & \num{0.00745} & \num{0.00804} & \num{0.00795}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{0.00429}} & \num{0.00497} & \num{0.00444} & \num{0.00451} & \num{0.00446}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MAPE} & \cellcolor{gray}{Average} & \num{0.0208} & \num{0.0211} & \textcolor{red}{\num{0.0208}} & \num{0.0209} & \num{0.0208}\\
    \cmidrule{1-7}\pagebreak[0]
     & asset01 & \num{48.869} & \textcolor{red}{\num{47.683}} & \num{48.807} & \num{48.604} & \num{48.554}\\
    \cmidrule{2-7}\nopagebreak
     & asset02 & \num{49.07} & \num{50.378} & \textcolor{red}{\num{49.061}} & \num{49.677} & \num{49.679}\\
    \cmidrule{2-7}\nopagebreak
     & asset03 & \num{44.628} & \num{44.624} & \num{44.705} & \num{42.144} & \textcolor{red}{\num{42.121}}\\
    \cmidrule{2-7}\nopagebreak
     & asset04 & \num{57.807} & \num{57.905} & \textcolor{red}{\num{56.534}} & \num{56.636} & \num{56.928}\\
    \cmidrule{2-7}\nopagebreak
     & asset05 & \num{86.174} & \num{90.287} & \num{83.73} & \num{82.913} & \textcolor{red}{\num{81.238}}\\
    \cmidrule{2-7}\nopagebreak
     & asset06 & \num{64.421} & \num{65.208} & \textcolor{red}{\num{64.407}} & \num{64.664} & \num{64.614}\\
    \cmidrule{2-7}\nopagebreak
     & asset07 & \textcolor{red}{\num{70.038}} & \num{76.935} & \num{70.373} & \num{71.827} & \num{71.653}\\
    \cmidrule{2-7}\nopagebreak
     & asset08 & \num{116.824} & \num{116.396} & \num{112.662} & \textcolor{red}{\num{109.287}} & \num{110.071}\\
    \cmidrule{2-7}\nopagebreak
     & asset09 & \num{180.922} & \num{167.215} & \num{175.738} & \textcolor{red}{\num{166.518}} & \num{166.736}\\
    \cmidrule{2-7}\nopagebreak
     & asset10 & \textcolor{red}{\num{75.158}} & \num{76.412} & \num{75.467} & \num{75.773} & \num{75.644}\\
    \cmidrule{2-7}\nopagebreak
     & asset11 & \textcolor{red}{\num{135.615}} & \num{159.72} & \num{139.441} & \num{152.802} & \num{151.356}\\
    \cmidrule{2-7}\nopagebreak
     & asset12 & \textcolor{red}{\num{163.56}} & \num{202.085} & \num{169.89} & \num{176.787} & \num{174.615}\\
    \cmidrule{2-7}\nopagebreak
    \multirow{-13}{*}{\raggedright\arraybackslash MASE} & \cellcolor{gray}{Average} & \num{91.09} & \num{96.237} & \textcolor{red}{\num{90.901}} & \num{91.469} & \num{91.101}\\*
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
    MSE & \num{0.0056} & \num{0.00544} & \num{0.00552} & \textcolor{red}{\num{0.00541}} & \num{0.00543}\\
    MAE & \num{0.0502} & \num{0.0494} & \num{0.0495} & \textcolor{red}{\num{0.0492}} & \num{0.0494}\\
    MAPE & \num{0.0206} & \num{0.0214} & \textcolor{red}{\num{0.0204}} & \num{0.0214} & \num{0.0214}\\
    MASE & \num{111.124} & \num{109.428} & \num{109.602} & \textcolor{red}{\num{109.173}} & \num{109.611}\\
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
    MSE & \num{0.0214} & \num{0.0202} & \num{0.0206} & \num{0.0198} & \textcolor{red}{\num{0.0198}}\\
    MAE & \num{0.0988} & \num{0.0946} & \num{0.0969} & \textcolor{red}{\num{0.0939}} & \num{0.0944}\\
    MAPE & \num{0.0393} & \num{0.0389} & \num{0.0386} & \textcolor{red}{\num{0.0385}} & \num{0.0386}\\
    MASE & \num{225.059} & \num{214.239} & \num{220.885} & \textcolor{red}{\num{213.183}} & \num{214.718}\\
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
    MSE & \num{0.039} & \textcolor{red}{\num{0.0301}} & \num{0.0386} & \num{0.031} & \num{0.0304}\\
    MAE & \num{0.13} & \textcolor{red}{\num{0.114}} & \num{0.13} & \num{0.118} & \num{0.116}\\
    MAPE & \num{0.0447} & \textcolor{red}{\num{0.0427}} & \num{0.0442} & \num{0.0443} & \num{0.0435}\\
    MASE & \num{291.037} & \textcolor{red}{\num{250.626}} & \num{289.063} & \num{261.624} & \num{257.211}\\
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
    MSE & \num{0.00156} & \num{0.0016} & \textcolor{red}{\num{0.00153}} & \num{0.00158} & \num{0.00157}\\
    MAE & \num{0.0269} & \num{0.0274} & \textcolor{red}{\num{0.0266}} & \num{0.0271} & \num{0.027}\\
    MAPE & \num{0.0279} & \num{0.0281} & \textcolor{red}{\num{0.0272}} & \num{0.0277} & \num{0.0278}\\
    MASE & \num{90.238} & \num{91.627} & \textcolor{red}{\num{89.01}} & \num{90.472} & \num{90.204}\\
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
    MSE & \num{0.00173} & \num{0.00179} & \textcolor{red}{\num{0.00173}} & \num{0.00174} & \num{0.00174}\\
    MAE & \textcolor{red}{\num{0.0281}} & \num{0.0286} & \num{0.0281} & \num{0.0281} & \num{0.0281}\\
    MAPE & \num{0.0282} & \num{0.0288} & \textcolor{red}{\num{0.0282}} & \num{0.0283} & \num{0.0284}\\
    MASE & \textcolor{red}{\num{93.269}} & \num{95.105} & \num{93.778} & \num{93.504} & \num{93.44}\\
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
    MSE & \num{0.00172} & \textcolor{red}{\num{0.00171}} & \num{0.00173} & \num{0.00172} & \num{0.00172}\\
    MAE & \num{0.0279} & \num{0.0276} & \num{0.0279} & \textcolor{red}{\num{0.0276}} & \num{0.0277}\\
    MAPE & \num{0.027} & \num{0.027} & \num{0.027} & \textcolor{red}{\num{0.0269}} & \num{0.0269}\\
    MASE & \num{91.691} & \textcolor{red}{\num{90.841}} & \num{91.886} & \num{91.144} & \num{91.185}\\
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
    MSE & \num{0.00171} & \num{0.00162} & \num{0.00163} & \num{0.00157} & \textcolor{red}{\num{0.00156}}\\
    MAE & \num{0.0289} & \num{0.0282} & \num{0.0284} & \num{0.0278} & \textcolor{red}{\num{0.0278}}\\
    MAPE & \num{0.0213} & \num{0.0212} & \num{0.0209} & \textcolor{red}{\num{0.0209}} & \num{0.0209}\\
    MASE & \num{99.44} & \num{95.615} & \num{97.189} & \num{94.926} & \textcolor{red}{\num{94.754}}\\
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
    MSE & \num{0.0016} & \textcolor{red}{\num{0.00149}} & \num{0.00161} & \num{0.0015} & \num{0.00149}\\
    MAE & \num{0.028} & \num{0.0276} & \num{0.0282} & \num{0.0274} & \textcolor{red}{\num{0.0272}}\\
    MAPE & \num{0.0207} & \num{0.0208} & \num{0.0207} & \num{0.0205} & \textcolor{red}{\num{0.0205}}\\
    MASE & \num{95.522} & \num{93.361} & \num{95.403} & \num{93.283} & \textcolor{red}{\num{92.64}}\\
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
    MSE & \num{0.00144} & \num{0.00155} & \textcolor{red}{\num{0.00144}} & \num{0.00148} & \num{0.00147}\\
    MAE & \num{0.0272} & \num{0.0284} & \num{0.0271} & \num{0.0272} & \textcolor{red}{\num{0.0271}}\\
    MAPE & \num{0.0208} & \num{0.0211} & \textcolor{red}{\num{0.0208}} & \num{0.0209} & \num{0.0208}\\
    MASE & \num{91.09} & \num{96.237} & \textcolor{red}{\num{90.901}} & \num{91.469} & \num{91.101}\\
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

    \caption{\label{tab:dgp1result}Out-of-sample forecasting performance measures for DGP1.}
    \centering
    \resizebox{\linewidth}{!}{
    \begin{tabular}[t]{cc|ccc|ccc|ccc|}
    \toprule
    \multicolumn{2}{c}{ } & \multicolumn{3}{c}{RMAFE} & \multicolumn{3}{c}{RMSFE} & \multicolumn{3}{c}{RMASE} \\
    \cmidrule(l{3pt}r{3pt}){3-5} \cmidrule(l{3pt}r{3pt}){6-8} \cmidrule(l{3pt}r{3pt}){9-11}
    \rotatebox{0}{} & \rotatebox{0}{} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$} & \rotatebox{0}{$h = 1$} & \rotatebox{0}{$h = 5$} & \rotatebox{0}{$h = 20$}\\
    \midrule
     & VHAR & \textcolor{black}{\num{0.984}} & \textcolor{black}{\num{0.957}} & \textcolor{red}{\num{0.875}} & \textcolor{black}{\num{0.971}} & \textcolor{black}{\num{0.944}} & \textcolor{red}{\num{0.772}} & \textcolor{black}{\num{0.985}} & \textcolor{black}{\num{0.952}} & \textcolor{red}{\num{0.861}}\\

     & BVAR & \textcolor{black}{\num{0.988}} & \textcolor{black}{\num{0.981}} & \textcolor{black}{\num{0.993}} & \textcolor{black}{\num{0.986}} & \textcolor{black}{\num{0.964}} & \textcolor{black}{\num{0.989}} & \textcolor{black}{\num{0.986}} & \textcolor{black}{\num{0.981}} & \textcolor{black}{\num{0.993}}\\

     & BVHAR-S & \textcolor{red}{\num{0.981}} & \textcolor{red}{\num{0.950}} & \textcolor{black}{\num{0.902}} & \textcolor{red}{\num{0.966}} & \textcolor{black}{\num{0.925}} & \textcolor{black}{\num{0.794}} & \textcolor{red}{\num{0.982}} & \textcolor{red}{\num{0.947}} & \textcolor{black}{\num{0.899}}\\

    \multirow{-4}{*}{\centering\arraybackslash SMALL} & BVHAR-L & \textcolor{black}{\num{0.984}} & \textcolor{black}{\num{0.955}} & \textcolor{black}{\num{0.889}} & \textcolor{black}{\num{0.970}} & \textcolor{red}{\num{0.924}} & \textcolor{black}{\num{0.779}} & \textcolor{black}{\num{0.986}} & \textcolor{black}{\num{0.954}} & \textcolor{black}{\num{0.884}}\\
    \cmidrule{1-11}
     & VHAR & \textcolor{black}{\num{0.622}} & \textcolor{red}{\num{0.0764}} & \textcolor{red}{\num{3.88e-05}} & \textcolor{black}{\num{0.458}} & \textcolor{red}{\num{0.00583}} & \textcolor{red}{\num{1.52e-09}} & \textcolor{black}{\num{0.642}} & \textcolor{red}{\num{0.0761}} & \textcolor{red}{\num{3.88e-05}}\\

     & BVAR & \textcolor{black}{\num{0.907}} & \textcolor{black}{\num{0.7498}} & \textcolor{black}{\num{3.17e-01}} & \textcolor{black}{\num{0.793}} & \textcolor{black}{\num{0.56240}} & \textcolor{black}{\num{1.01e-01}} & \textcolor{black}{\num{0.917}} & \textcolor{black}{\num{0.7498}} & \textcolor{black}{\num{3.17e-01}}\\

     & BVHAR-S & \textcolor{black}{\num{0.578}} & \textcolor{black}{\num{0.0837}} & \textcolor{black}{\num{5.50e-05}} & \textcolor{black}{\num{0.375}} & \textcolor{black}{\num{0.00702}} & \textcolor{black}{\num{3.05e-09}} & \textcolor{black}{\num{0.599}} & \textcolor{black}{\num{0.0835}} & \textcolor{black}{\num{5.50e-05}}\\

    \multirow{-4}{*}{\centering\arraybackslash MEDIUM} & BVHAR-L & \textcolor{red}{\num{0.571}} & \textcolor{black}{\num{0.1124}} & \textcolor{black}{\num{1.76e-04}} & \textcolor{red}{\num{0.362}} & \textcolor{black}{\num{0.01268}} & \textcolor{black}{\num{3.10e-08}} & \textcolor{red}{\num{0.592}} & \textcolor{black}{\num{0.1123}} & \textcolor{black}{\num{1.76e-04}}\\
    \cmidrule{1-11}
     & VHAR & \textcolor{black}{\num{0.778}} & \textcolor{red}{\num{0.0548}} & \textcolor{red}{\num{1.06e-05}} & \textcolor{black}{\num{0.514}} & \textcolor{red}{\num{0.00298}} & \textcolor{red}{\num{1.14e-10}} & \textcolor{black}{\num{0.739}} & \textcolor{red}{\num{0.0545}} & \textcolor{red}{\num{1.06e-05}}\\

     & BVAR & \textcolor{black}{\num{0.942}} & \textcolor{black}{\num{0.7514}} & \textcolor{black}{\num{3.21e-01}} & \textcolor{black}{\num{0.836}} & \textcolor{black}{\num{0.56468}} & \textcolor{black}{\num{1.03e-01}} & \textcolor{black}{\num{0.935}} & \textcolor{black}{\num{0.7514}} & \textcolor{black}{\num{3.21e-01}}\\

     & BVHAR-S & \textcolor{black}{\num{0.758}} & \textcolor{black}{\num{0.0653}} & \textcolor{black}{\num{2.12e-05}} & \textcolor{black}{\num{0.454}} & \textcolor{black}{\num{0.00426}} & \textcolor{black}{\num{4.52e-10}} & \textcolor{black}{\num{0.720}} & \textcolor{black}{\num{0.0651}} & \textcolor{black}{\num{2.12e-05}}\\

    \multirow{-4}{*}{\centering\arraybackslash LARGE} & BVHAR-L & \textcolor{red}{\num{0.746}} & \textcolor{black}{\num{0.0874}} & \textcolor{black}{\num{6.60e-05}} & \textcolor{red}{\num{0.428}} & \textcolor{black}{\num{0.00765}} & \textcolor{black}{\num{4.38e-09}} & \textcolor{red}{\num{0.711}} & \textcolor{black}{\num{0.0872}} & \textcolor{black}{\num{6.60e-05}}\\
    \bottomrule
    \end{tabular}}
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
