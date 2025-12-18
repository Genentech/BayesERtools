# Perform simulation of covariate effects for ER model

Perform simulation of covariate effects for ER model

## Usage

``` r
sim_coveff(
  ermod,
  data = NULL,
  spec_coveff = NULL,
  output_type = "median_qi",
  qi_width = 0.9,
  qi_width_cov = 0.9
)
```

## Arguments

- ermod:

  an object of class `ermod` (supports `ermod_bin` and `ermod_lin`)

- data:

  an optional data frame to derive the covariate values for forest
  plots. If NULL (default), the data used to fit the model is used.

- spec_coveff:

  you can supply spec_coveff to `sim_coveff()` or
  [`plot_coveff()`](https://genentech.github.io/BayesERtools/reference/plot_coveff.md),
  if you have already built it manually or with
  [`build_spec_coveff()`](https://genentech.github.io/BayesERtools/reference/build_spec_coveff.md).
  See
  [`build_spec_coveff()`](https://genentech.github.io/BayesERtools/reference/build_spec_coveff.md)
  for detail.

- output_type:

  Type of output. Currently only supports "median_qi" which returns the
  median and quantile interval.

- qi_width:

  the width of the credible interval on the covariate effect. This
  translate to the width of the error bars in the forest plot.

- qi_width_cov:

  the width of the quantile interval for continuous covariates in the
  forest plot. Default is 0.9 (i.e. visualize effect of covariate effect
  at their 5th and 95th percentile values).

## Value

A data frame with class `coveffsim` containing the median and quantile
interval of the covariate effects. For binary models (`ermod_bin`),
returns odds ratios. For linear models (`ermod_lin`), returns response
differences.

## Examples

``` r
# \donttest{
data(d_sim_binom_cov_hgly2)

ermod_bin <- dev_ermod_bin(
  data = d_sim_binom_cov_hgly2,
  var_resp = "AEFLAG",
  var_exposure = "AUCss_1000",
  var_cov = "BHBA1C_5",
)

sim_coveff(ermod_bin)
#> # A tibble: 6 × 12
#>   var_order var_name  var_label value_order value_annot value_label is_ref_value
#>       <dbl> <chr>     <chr>           <int> <chr>       <chr>       <lgl>       
#> 1         1 AUCss_10… AUCss_10…           1 5th         0.868       FALSE       
#> 2         1 AUCss_10… AUCss_10…           2 median      2.21        TRUE        
#> 3         1 AUCss_10… AUCss_10…           3 95th        5.30        FALSE       
#> 4         2 BHBA1C_5  BHBA1C_5            1 5th         5.75        FALSE       
#> 5         2 BHBA1C_5  BHBA1C_5            2 median      7.97        TRUE        
#> 6         2 BHBA1C_5  BHBA1C_5            3 95th        10.4        FALSE       
#> # ℹ 5 more variables: show_ref_value <lgl>, is_covariate <lgl>,
#> #   .odds_ratio <dbl>, .lower <dbl>, .upper <dbl>

# Linear regression model example
data(d_sim_lin)

ermod_lin <- dev_ermod_lin(
  data = d_sim_lin,
  var_resp = "response",
  var_exposure = "AUCss",
  var_cov = c("SEX", "BAGE"),
)

sim_coveff(ermod_lin)
#> # A tibble: 8 × 12
#>   var_order var_name var_label value_order value_annot value_label is_ref_value
#>       <dbl> <chr>    <chr>           <int> <chr>       <chr>       <lgl>       
#> 1         1 AUCss    AUCss               1 5th         5.00        FALSE       
#> 2         1 AUCss    AUCss               2 median      50.0        TRUE        
#> 3         1 AUCss    AUCss               3 95th        95.0        FALSE       
#> 4         2 SEX      SEX                 1 1st freq    F           TRUE        
#> 5         2 SEX      SEX                 2 2nd freq    M           FALSE       
#> 6         3 BAGE     BAGE                1 5th         33.5        FALSE       
#> 7         3 BAGE     BAGE                2 median      50.4        TRUE        
#> 8         3 BAGE     BAGE                3 95th        67.0        FALSE       
#> # ℹ 5 more variables: show_ref_value <lgl>, is_covariate <lgl>,
#> #   .response_diff <dbl>, .lower <dbl>, .upper <dbl>
# }
```
