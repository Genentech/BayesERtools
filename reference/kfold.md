# Run k-fold cross-validation

This function performs k-fold cross-validation using the appropriate
model development function based on the class of the `ermod` object. It
is internally used by
[`eval_ermod()`](https://genentech.github.io/BayesERtools/reference/eval_ermod.md).
The output is compatible with `loo` ecosystem, e.g. it can be used for
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
function. See
[`loo::kfold()`](https://mc-stan.org/loo/reference/kfold-generic.html)
for details.

## Usage

``` r
# S3 method for class 'ermod'
kfold(x, k = 5, newdata = NULL, seed = NULL, ...)

kfold(x, ...)

extract_kfold_loo(kfold_ermod)
```

## Arguments

- x:

  An `ermod` object containing the model and data.

- k:

  The number of folds for cross-validation. Default is 5.

- newdata:

  Optional new dataset to use instead of the original data. Default is
  NULL.

- seed:

  Random seed for reproducibility. Default is NULL.

- ...:

  Currently not used.

- kfold_ermod:

  An object of class `kfold_ermod` from `kfold()`

## Value

`kfold()` returns `kfold_ermod` class object containing the fitted
models and holdout predictions for each fold.

`extract_kfold_loo()` returns `c("kfold", "loo")` class object that
works well with `loo` ecosystem

## Examples

``` r
# \donttest{
data(d_sim_binom_cov_hgly2)

ermod_bin <- dev_ermod_bin(
  data = d_sim_binom_cov_hgly2,
  var_resp = "AEFLAG",
  var_exposure = "AUCss_1000",
  var_cov = "BHBA1C_5",
  # Settings to make the example run faster
  chains = 2,
  iter = 1000
)

cv_results <- kfold(ermod_bin, k = 3, seed = 123)

print(cv_results)
#> 
#> ── k-fold Cross-Validation for ermod object ────────────────────────────────────
#> ℹ Number of folds:  3
#> 
#> ── Structure of the object ──
#> 
#> • $l_ermod: list of ermod objects
#> • $d_truth: data frame with true response values
#> • $d_sim: data frame with holdout predictions
#> 
#> ── elpd (used in `loo` package) ──
#> 
#>            Estimate   SE
#> elpd_kfold   -254.5 12.2
#> p_kfold         4.9  1.3
#> kfoldic       509.0 24.3
# }
```
