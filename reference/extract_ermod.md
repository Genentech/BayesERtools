# Extract elements from an object of class `ermod_*`

Extract elements from an object of class `ermod_*`

## Usage

``` r
# S3 method for class 'ermod'
extract_data(x)

# S3 method for class 'ermod'
extract_mod(x)

# S3 method for class 'ermod'
extract_var_resp(x)

# S3 method for class 'ermod'
extract_var_exposure(x)

# S3 method for class 'ermod'
extract_var_cov(x)

# S3 method for class 'ermod_exp_sel'
extract_exp_sel_list_model(x)

# S3 method for class 'ermod_exp_sel'
extract_exp_sel_comp(x)

# S3 method for class 'ermod_cov_sel'
extract_var_selected(x)
```

## Arguments

- x:

  An object of class `ermod_*`

## Value

- [`extract_data()`](https://genentech.github.io/BayesERtools/reference/extract_method.md)
  extracts data used for the model fit.

- [`extract_mod()`](https://genentech.github.io/BayesERtools/reference/extract_method.md)
  extracts the model fit object.

- [`extract_var_resp()`](https://genentech.github.io/BayesERtools/reference/extract_method.md)
  extracts the response variable name

- [`extract_var_exposure()`](https://genentech.github.io/BayesERtools/reference/extract_method.md)
  extracts the exposure metric name

- [`extract_var_cov()`](https://genentech.github.io/BayesERtools/reference/extract_method.md)
  extracts the covariates name

- [`extract_exp_sel_list_model()`](https://genentech.github.io/BayesERtools/reference/extract_method.md)
  extracts the list of fitted models for each exposure metrics.

- [`extract_exp_sel_comp()`](https://genentech.github.io/BayesERtools/reference/extract_method.md)
  extracts the comparison results of the exposure metrics.

- [`extract_var_selected()`](https://genentech.github.io/BayesERtools/reference/extract_method.md)
  extracts the selected variables (both exposure and covariates)in the
  final model after covariate selection.
