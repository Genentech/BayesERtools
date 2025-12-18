# Sample simulated data for Emax exposure-response models with covariates and placebo

Sample simulated data for Emax exposure-response models with covariates
and placebo

## Usage

``` r
d_sim_placebo
```

## Format

A data frame with columns:

- id:

  Subject identifier

- dose:

  Nominal dose, units not specified

- exp_1:

  Exposure value on metric 1, units and metric not specified

- exp_2:

  Exposure value on metric 2, units and metric not specified

- rsp_1:

  Continuous response value (units not specified)

- rsp_2:

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
that can be used to illustrate Emax modeling with placebo data. It
contains variables corresponding to dose and exposure, and includes both
a continuous response variable and a binary response variable. Three
continuous valued covariates are included, along with two binary
covariates. Data from two exposure metrics are included.

You can find the data generating code in the package source code, under
`data-raw/d_sim_placebo.R`.

## Examples

``` r
d_sim_placebo
#> # A tibble: 400 × 11
#>       id  dose exp_1 exp_2 rsp_1 rsp_2 cnt_a cnt_b cnt_c bin_d bin_e
#>    <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     1     0     0     0  6.49     0  3.06  6.50 5.93      0     1
#>  2     2     0     0     0  7.81     0  5.72  3.84 5.60      0     1
#>  3     3     0     0     0  7.26     0  4.31  3.68 8.16      0     0
#>  4     4     0     0     0  7.45     0  4.03  2.86 9.38      1     1
#>  5     5     0     0     0  6.33     0  2.46  3.36 8.29      0     1
#>  6     6     0     0     0  7.13     0  4.87  8.90 7.06      0     0
#>  7     7     0     0     0  6.07     0  2.87  4.85 0.989     0     1
#>  8     8     0     0     0  5.47     1  1.07  5.34 3.34      1     1
#>  9     9     0     0     0  7.12     0  3.94  5.68 4.01      1     1
#> 10    10     0     0     0  8.21     1  7.46  8.16 6.92      1     1
#> # ℹ 390 more rows
```
