# Evaluate exposure-response model prediction performance

This function evaluates the performance of an exposure-response model
using various metrics.

## Usage

``` r
eval_ermod(
  ermod,
  eval_type = c("training", "kfold", "test"),
  newdata = NULL,
  summary_method = c("median", "mean"),
  k = 5,
  seed_kfold = NULL
)
```

## Arguments

- ermod:

  An object of class `ermod`.

- eval_type:

  A character string specifying the evaluation dataset. Options are:

  - `training`: Use the training dataset.

  - `test`: Use a new dataset for evaluation.

  - `kfold`: Perform k-fold cross-validation (uses `newdata` if
    provided, otherwise uses the training dataset).

- newdata:

  A data frame containing new data for evaluation when `eval_type` is
  set to `test` or `kfold`.

- summary_method:

  A character string specifying how to summarize the simulation draws.
  Default is `median`.

- k:

  The number of folds for cross-validation. Default is 5.

- seed_kfold:

  Random seed for k-fold cross-validation.

## Value

A tibble with calculated performance metrics, such as AUROC or RMSE,
depending on the model type.

## Examples

``` r
# \donttest{
data(d_sim_binom_cov_hgly2)
d_split <- rsample::initial_split(d_sim_binom_cov_hgly2)
d_train <- rsample::training(d_split)
d_test <- rsample::testing(d_split)

ermod_bin <- dev_ermod_bin(
  data = d_train,
  var_resp = "AEFLAG",
  var_exposure = "AUCss_1000",
  var_cov = "BHBA1C_5",
  # Settings to make the example run faster
  chains = 2,
  iter = 1000
)

metrics_training <- eval_ermod(ermod_bin, eval_type = "training")
metrics_test <- eval_ermod(ermod_bin, eval_type = "test", newdata = d_test)
metrics_kfold <- eval_ermod(ermod_bin, eval_type = "kfold", k = 3)

print(metrics_training)
#> # A tibble: 2 × 3
#>   .metric     .estimator .estimate
#>   <chr>       <chr>          <dbl>
#> 1 roc_auc     binary         0.738
#> 2 mn_log_loss binary         0.526
print(metrics_test)
#> # A tibble: 2 × 3
#>   .metric     .estimator .estimate
#>   <chr>       <chr>          <dbl>
#> 1 roc_auc     binary         0.815
#> 2 mn_log_loss binary         0.421
print(metrics_kfold)
#> # A tibble: 6 × 4
#>   fold_id .metric     .estimator .estimate
#>     <int> <chr>       <chr>          <dbl>
#> 1       1 roc_auc     binary         0.742
#> 2       2 roc_auc     binary         0.764
#> 3       3 roc_auc     binary         0.695
#> 4       1 mn_log_loss binary         0.492
#> 5       2 mn_log_loss binary         0.512
#> 6       3 mn_log_loss binary         0.595
# }
```
