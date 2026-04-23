# Changelog

## BayesERtools 0.2.5

### Major changes

- Updated compatibility with upcoming changes to `loo_compare()` output
  structure in the `loo` package (\> 2.9.0), which now returns a data
  frame instead of a matrix and includes additional diagnostic columns.

### Minor changes

- Allow manual breaks in
  [`plot_er()`](https://genentech.github.io/BayesERtools/reference/plot_er.md)
  to control the position of the probability summary breaks for binary
  models
- Added `return_components` option to
  [`plot_er()`](https://genentech.github.io/BayesERtools/reference/plot_er.md)
  and
  [`plot_er_gof()`](https://genentech.github.io/BayesERtools/reference/plot_er_gof.md)
  that returns individual plot components (main plot, boxplot, caption)
  for customization before recombining with
  [`combine_er_components()`](https://genentech.github.io/BayesERtools/reference/combine_er_components.md)

## BayesERtools 0.2.4

CRAN release: 2025-10-02

### Major changes

- Extended covariate effects functionality to support linear regression
  models (`ermod_lin`) in addition to binary logistic regression models
  (`ermod_bin`)
  1.  

### Minor changes

- Fix test for rstanarm update

## BayesERtools 0.2.3

CRAN release: 2025-06-16

### Major changes

### Minor changes

- Prepare for the upcoming ggplot2 release

## BayesERtools 0.2.2

CRAN release: 2025-06-06

### Major changes

- Implemented
  [`kfold()`](https://genentech.github.io/BayesERtools/reference/kfold.md)
  function to allow the estimation of ELPD to work with loo ecosystem
- Added simulated dataset for Emax model
  <https://github.com/Genentech/BayesERtools/pull/7> (1)

### Minor changes

- Enable setting the prior distribution for linear models
- Added `exp_candidates` argument to
  [`extract_coef_exp_ci()`](https://genentech.github.io/BayesERtools/reference/extract_coef_exp_ci.md)
  function to allow for the extraction of coefficients from all
  candidate models
- Update package dependencies

## BayesERtools 0.2.1

CRAN release: 2025-02-12

- Update package dependency

## BayesERtools 0.2.0

CRAN release: 2025-02-10

- Initial public release
