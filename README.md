
# Bayesian Vector Heterogeneous Autoregressive Modeling

<!-- badges: start -->

[![STATUS](https://img.shields.io/badge/JSCS-Under%20review-brightgreen.svg)](https://img.shields.io/badge/JSCS-Under%20review-brightgreen)
<!-- badges: end -->

## Overview

This is a repository for researching Bayesian VHAR.

- Extend BVAR to vector HAR model
- Two-forms of priors: BVHAR-S and BVHAR-L
- Posterior consistency
- Compare the forecasting performance

### Files

- [R/](https://github.com/ygeunkim/paper-bvhar/tree/master/r) Codes and
  R markdown
- [data/](https://github.com/ygeunkim/paper-bvhar/tree/master/data)
  Datasets used in this research
- [docs/](https://github.com/ygeunkim/paper-bvhar/tree/master/docs)
  Rendered documents

### Tools

- `R`
- `LaTex`

### Required R packages

- [ygeunkim/bvhar](https://github.com/ygeunkim/bvhar)
  - v0.9.0
  - v0.12.0: for MVT generation
- [tidyverse](https://www.tidyverse.org)
- [knitr](https://yihui.org/knitr/)
- [kableExtra](https://haozhu233.github.io/kableExtra/)
- [parallel](https://stat.ethz.ch/R-manual/R-devel/library/parallel/doc/parallel.pdf)
- [foreach](https://github.com/RevolutionAnalytics/foreach)
- [gridExtra](https://cran.r-project.org/web/packages/gridExtra/index.html)

## Results

### Simulation

- [Consistency](https://github.com/ygeunkim/paper-bvhar/blob/master/docs/sim-consistency.md)
  Posterior Consistency Simulation
- [MVT](https://github.com/ygeunkim/paper-bvhar/blob/master/docs/sim-stylized.md)
  Simulation with MVT-generated innovation

### Empirical analysis

- [VIX](https://github.com/ygeunkim/paper-bvhar/blob/master/docs/analysis.md)
  CBOE ETF VIX

## Code of Conduct

Please note that the paper-bvhar project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
