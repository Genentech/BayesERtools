# Sample simulated data for Emax exposure-response models with covariates.

Sample simulated data for Emax exposure-response models with covariates.

## Usage

``` r
d_sim_emax
```

## Format

A data frame with columns:

- dose:

  Nominal dose, units not specified

- exposure:

  Exposure value, units and metric not specified

- response_1:

  Continuous response value (units not specified)

- response_2:

  Binary response value (group labels not specified)

- cnt_a:

  Continuous valued covariate

- cnt_b:

  Continuous valued covariate

- cnt_c:

  Continuous valued covariate

- bin_d:

  Binary valued covariate

- bin_e:

  Binary valued covariate

## Details

This simulated dataset is entirely synthetic. It is a generic data set
that can be used to illustrate Emax modeling. It contains variables
corresponding to dose and exposure, and includes both a continuous
response variable and a binary response variable. Three continuous
valued covariates are included, along with two binary covariates.

You can find the data generating code in the package source code, under
`data-raw/d_sim_emax.R`.

## Examples

``` r
d_sim_emax
#> # A tibble: 300 × 9
#>     dose exposure response_1 response_2 cnt_a cnt_b cnt_c bin_d bin_e
#>    <dbl>    <dbl>      <dbl>      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1   100    4151.       12.8          1  5.71  2.33  7.83     0     1
#>  2   100    8067.       14.6          1  4.92  4.66  6.74     1     1
#>  3   100    4878.       12.8          1  4.88  4.21  4.68     1     1
#>  4   100    9713.       16.6          1  8.42  6.56  1.29     0     1
#>  5   100   11491.       14.4          0  4.37  3.96  3.55     0     1
#>  6   100    2452.       12.6          1  8.69  7.60  3.64     0     0
#>  7   100    5652.       14.8          1  6.61  3.95  5.13     0     0
#>  8   100    9939.       15.2          1  5.35  7.77  8.29     0     1
#>  9   100    5817.       14.6          0  5.61  2.24  9.60     0     1
#> 10   100    5176.       13.7          1  6.06  1.79  8.74     0     1
#> # ℹ 290 more rows
```
