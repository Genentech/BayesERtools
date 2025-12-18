# Sample simulated data for exposure-response with continuous endpoint using linear model.

Sample simulated data for exposure-response with continuous endpoint
using linear model.

## Usage

``` r
d_sim_lin
```

## Format

A data frame with columns:

- ID:

  Subject ID

- AUCss:

  Steady-state area under the curve

- Cmaxss:

  Steady-state maximum (peak) concentration

- BAGE:

  Baseline age in years

- SEX:

  M or F

- response:

  Response

## Details

True model is defined as `0.5 * AUCss + 0.5 * BAGE + 5 * SEX`, with
variability added with standard deviation of 10. You can find the data
generating code in the package source code, under
`data-raw/d_sim_lin.R`.

## Examples

``` r
d_sim_lin
#> # A tibble: 101 × 6
#>       ID AUCss Cmaxss  BAGE SEX   response
#>    <int> <int>  <dbl> <dbl> <chr>    <dbl>
#>  1     1     0   0     45.3 F         37.8
#>  2     2     1   5.42  50.7 M         35.4
#>  3     3     2  13.3   45.0 F         36.7
#>  4     4     3   4.44  41.7 M         38.6
#>  5     5     4  22.6   51.7 F         35.2
#>  6     6     5  28.8   41.0 M         30.9
#>  7     7     6  24.8   51.7 F         20.3
#>  8     8     7  29.3   53.5 F         30.9
#>  9     9     8  33.2   49.5 F         26.9
#> 10    10     9  33.0   48.0 M         42.9
#> # ℹ 91 more rows
```
