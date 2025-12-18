# Extract credible interval of the exposure coefficient

Extract credible interval of the exposure coefficient

## Usage

``` r
extract_coef_exp_ci(x, ci_width = 0.95, exp_candidates = FALSE)
```

## Arguments

- x:

  An object of class `ermod_bin` or `ermod_lin`

- ci_width:

  Width of the credible interval

- exp_candidates:

  Logical, whether to extract the credible interval for all exposure
  candidates. Default is `FALSE`. Only supported for models with
  exposure selection, created with
  [`dev_ermod_bin_exp_sel()`](https://genentech.github.io/BayesERtools/reference/dev_ermod_bin_exp_sel.md)
  or
  [`dev_ermod_lin_exp_sel()`](https://genentech.github.io/BayesERtools/reference/dev_ermod_bin_exp_sel.md)
  functions.

## Value

A named vector of length 2 with the lower and upper bounds of the
credible interval (.lower, .upper). If `exp_candidates = TRUE`, a matrix
with the same structure is returned, with each row corresponding to an
exposure candidate.
