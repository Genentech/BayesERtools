# Simulate from ER model

Simulate from ER model

## Usage

``` r
sim_er(
  ermod,
  newdata = NULL,
  n_draws_sim = NULL,
  seed_sample_draws = NULL,
  output_type = c("draws", "median_qi"),
  qi_width = 0.95,
  .nrow_cov_data = NULL
)
```

## Arguments

- ermod:

  An object of class `ermod`

- newdata:

  New data to use for simulation. Default is NULL (use the data in the
  model object).

- n_draws_sim:

  Number of draws for simulation. If NULL (default), all draws in the
  model object are used.

- seed_sample_draws:

  Seed for sampling draws. Default is NULL.

- output_type:

  Type of output. "draws" returns the raw draws from the simulation, and
  "median_qi" returns the median and quantile interval.

- qi_width:

  Width of the quantile interval. Default is 0.95. Only used when
  `output_type = "median_qi"`.

- .nrow_cov_data:

  Number of rows in the covariate data, used for internal purposes.
  Users should not set this argument.

## Value

`ersim` object, which is a tibble with the simulated responses with some
additional information in object attributes. It has three types of
predictions - `.linpred`, `.epred`, `.prediction`. `.linpred` and
`.epred` are similar in a way that they both represent "expected
response", i.e. without residual variability. They are the same for
models with continuous endpoits (Emax model). For models with binary
endpoints, `.linpred` is the linear predictor (i.e. on the logit scale)
and `.epred` is on the probability scale. `.prediction` is the predicted
response with residual variability (or in case of binary endpoint, the
predicted yes (1) or no (0) for event occurrence). See
[`tidybayes::add_epred_draws()`](https://mjskay.github.io/tidybayes/reference/add_predicted_draws.html)
for more details.

In case of `output_type = "median_qi"`, it returns `ersim_med_qi`
object.

## See also

[`calc_ersim_med_qi()`](https://genentech.github.io/BayesERtools/reference/calc_ersim_med_qi.md)
for calculating median and quantile interval from `ersim` object
(generated with `output_type = "draws"`).

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

ersim <- sim_er(
  ermod_bin,
  n_draws_sim = 500, # This is set to make the example run faster
  output_type = "draws"
)

ersim_med_qi <- sim_er(
  ermod_bin,
  n_draws_sim = 500, # This is set to make the example run faster
  output_type = "median_qi"
)

ersim
#> # A tibble: 250,000 × 24
#> # Groups:   ID, AETYPE, AEFLAG, Dose_mg, AUCss, Cmaxss, Cminss, BAGE, BWT,
#> #   BGLUC, BHBA1C, RACE, VISC, AUCss_1000, BAGE_10, BWT_10, BHBA1C_5, .row
#> #   [500]
#>       ID AETYPE AEFLAG Dose_mg AUCss Cmaxss Cminss  BAGE   BWT BGLUC BHBA1C
#>    <int> <chr>   <int>   <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl>
#>  1     1 hgly2       0     200  866.   64.3   10.1  84.4  74.1  4.65   31.5
#>  2     1 hgly2       0     200  866.   64.3   10.1  84.4  74.1  4.65   31.5
#>  3     1 hgly2       0     200  866.   64.3   10.1  84.4  74.1  4.65   31.5
#>  4     1 hgly2       0     200  866.   64.3   10.1  84.4  74.1  4.65   31.5
#>  5     1 hgly2       0     200  866.   64.3   10.1  84.4  74.1  4.65   31.5
#>  6     1 hgly2       0     200  866.   64.3   10.1  84.4  74.1  4.65   31.5
#>  7     1 hgly2       0     200  866.   64.3   10.1  84.4  74.1  4.65   31.5
#>  8     1 hgly2       0     200  866.   64.3   10.1  84.4  74.1  4.65   31.5
#>  9     1 hgly2       0     200  866.   64.3   10.1  84.4  74.1  4.65   31.5
#> 10     1 hgly2       0     200  866.   64.3   10.1  84.4  74.1  4.65   31.5
#> # ℹ 249,990 more rows
#> # ℹ 13 more variables: RACE <chr>, VISC <fct>, AUCss_1000 <dbl>, BAGE_10 <dbl>,
#> #   BWT_10 <dbl>, BHBA1C_5 <dbl>, .row <int>, .chain <int>, .iteration <int>,
#> #   .draw <int>, .epred <dbl>, .linpred <dbl>, .prediction <int>
ersim_med_qi
#> # A tibble: 500 × 27
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
#> # ℹ 16 more variables: RACE <chr>, VISC <fct>, AUCss_1000 <dbl>, BAGE_10 <dbl>,
#> #   BWT_10 <dbl>, BHBA1C_5 <dbl>, .row <int>, .epred <dbl>, .epred.lower <dbl>,
#> #   .epred.upper <dbl>, .linpred <dbl>, .linpred.lower <dbl>,
#> #   .linpred.upper <dbl>, .width <dbl>, .point <chr>, .interval <chr>
# }
```
