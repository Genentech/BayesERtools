# Sample simulated data for exposure-response with binary endpoint.

Sample simulated data for exposure-response with binary endpoint.

## Usage

``` r
d_sim_binom_cov

d_sim_binom_cov_hgly2
```

## Format

A data frame with columns:

- ID:

  Subject ID

- AETYPE:

  Adverse event type: hgly2 (Gr2+ hyperglycemia), dr2 (Gr2+ Diarrhea),
  ae_covsel_test (hypothetical AE for covariate selection function test)

- AEFLAG:

  Adverse event flag: 0 - no event, 1 - event

- Dose_mg:

  Dose in mg: 200, 400

- AUCss:

  Steady-state area under the curve

- Cmaxss:

  Steady-state maximum (peak) concentration

- Cminss:

  Steady-state minimum (trough) concentration

- BAGE:

  Baseline age in years

- BWT:

  Baseline weight in kg

- BGLUC:

  Baseline glucose in mmol/L

- BHBA1C:

  Baseline HbA1c in percentage

- RACE:

  Race: White, Black, Asian

- VISC:

  Visceral disease: No, Yes

- AUCss_1000:

  AUCss/1000

- BAGE_10:

  BAGE/10

- BWT_10:

  BWT/10

- BHBA1C_5:

  BHBA1C/5

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 500
rows and 17 columns.

## Details

This simulated dataset is very loosely inspired by ER analysis of
ipatasertib by Kotani (2022) at:

https://doi.org/10.1007/s00280-022-04488-2

You can find the data generating code in the package source code, under
`data-raw/d_sim_binom_cov.R`.

d_sim_binom_cov_hgly2 is a subset of this dataset with only hgly2 AE
type and some columns added for testing.

## Examples

``` r
d_sim_binom_cov
#> # A tibble: 1,500 × 13
#>       ID AETYPE      AEFLAG Dose_mg AUCss Cmaxss Cminss  BAGE   BWT BGLUC BHBA1C
#>    <int> <chr>        <int>   <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl>
#>  1     1 hgly2            0     200  866.   64.3   10.1  84.4  74.1  4.65   31.5
#>  2     1 dr2              0     200  866.   64.3   10.1  84.4  74.1  4.65   31.5
#>  3     1 ae_covsel_…      0     200  866.   64.3   10.1  84.4  74.1  4.65   31.5
#>  4     2 hgly2            0     200 1707.  166.    27.3  59.1  88.2  7.24   41.9
#>  5     2 dr2              0     200 1707.  166.    27.3  59.1  88.2  7.24   41.9
#>  6     2 ae_covsel_…      1     200 1707.  166.    27.3  59.1  88.2  7.24   41.9
#>  7     3 hgly2            0     200  746.   68.2   15.8  64.3  88.0  5.73   47.7
#>  8     3 dr2              0     200  746.   68.2   15.8  64.3  88.0  5.73   47.7
#>  9     3 ae_covsel_…      0     200  746.   68.2   15.8  64.3  88.0  5.73   47.7
#> 10     4 hgly2            0     200 1984.  287.    37.2  65.5 114.   5.26   23.4
#> # ℹ 1,490 more rows
#> # ℹ 2 more variables: RACE <chr>, VISC <chr>
d_sim_binom_cov_hgly2
#> # A tibble: 500 × 17
#>       ID AETYPE AEFLAG Dose_mg AUCss Cmaxss Cminss  BAGE   BWT BGLUC BHBA1C
#>    <int> <chr>   <int>   <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl>
#>  1     1 hgly2       0     200  866.   64.3   10.1  84.4  74.1  4.65   31.5
#>  2     2 hgly2       0     200 1707.  166.    27.3  59.1  88.2  7.24   41.9
#>  3     3 hgly2       0     200  746.   68.2   15.8  64.3  88.0  5.73   47.7
#>  4     4 hgly2       0     200 1984.  287.    37.2  65.5 114.   5.26   23.4
#>  5     5 hgly2       0     200 2045.  275.    53.1  67.5  64.6  4.49   43.1
#>  6     6 hgly2       0     200  632.   47.3   12.0  67.0  73.0  6.41   43.6
#>  7     7 hgly2       0     200 2274.  136.    33.6  73.7  63.6  7.22   35.9
#>  8     8 hgly2       0     200 1347.  130.    21.7  59.9  99.7  5.34   36.2
#>  9     9 hgly2       0     200 1101.   60.7   11.2  65.8  82.3  5.47   36.0
#> 10    10 hgly2       0     200 1822.  179.    49.3  65.5  62.3  5.70   33.7
#> # ℹ 490 more rows
#> # ℹ 6 more variables: RACE <chr>, VISC <fct>, AUCss_1000 <dbl>, BAGE_10 <dbl>,
#> #   BWT_10 <dbl>, BHBA1C_5 <dbl>
```
