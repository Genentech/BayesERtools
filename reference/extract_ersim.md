# Extract elements from objects of the classes `ersim_*` and \`ersim_med_qi\_\*“

Extract elements from objects of the classes `ersim_*` and
\`ersim_med_qi\_\*“

## Usage

``` r
# S3 method for class 'ersim'
extract_data(x)

# S3 method for class 'ersim_med_qi'
extract_data(x)

# S3 method for class 'ersim'
extract_var_resp(x)

# S3 method for class 'ersim_med_qi'
extract_var_resp(x)

# S3 method for class 'ersim'
extract_var_exposure(x)

# S3 method for class 'ersim_med_qi'
extract_var_exposure(x)

# S3 method for class 'ersim'
extract_var_cov(x)

# S3 method for class 'ersim_med_qi'
extract_var_cov(x)
```

## Arguments

- x:

  An object of class `ersim_*`

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
