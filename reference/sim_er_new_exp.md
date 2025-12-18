# Simulate from ER model at specified exposure values

Simulate from ER model at specified exposure values

## Usage

``` r
sim_er_new_exp(
  ermod,
  exposure_to_sim_vec = NULL,
  data_cov = NULL,
  n_draws_sim = NULL,
  seed_sample_draws = NULL,
  output_type = c("draws", "median_qi"),
  qi_width = 0.95
)

sim_er_curve(
  ermod,
  exposure_range = NULL,
  num_exposures = 51,
  data_cov = NULL,
  n_draws_sim = NULL,
  seed_sample_draws = NULL,
  output_type = c("draws", "median_qi"),
  qi_width = 0.95
)
```

## Arguments

- ermod:

  An object of class `ermod`

- exposure_to_sim_vec:

  Vector of exposure values to simulate.

- data_cov:

  Data frame containing covariates to use for simulation, see details
  below.

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

- exposure_range:

  Range of exposure values to simulate. If NULL (default), it is set to
  the range of the exposure variable in the original data for model
  development.

- num_exposures:

  Number of exposure values to simulate.

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

## Details

Simulation dataset will be all combinations of covariates in `data_cov`
and exposure values in `exposure_to_sim_vec`, so the run time can become
very long if `data_cov` has many rows.

`data_cov` has to be supplied if `ermod` is a model with covariates. It
is recommended that `data_cov` contains subject identifiers such as `ID`
for post-processing.

Exposure values in `data_cov` will be ignored.

`sim_er_curve()` is a wrapper function for `sim_er_new_exp()` that use a
range of exposure values to simulate the expected responses.
Particularly useful for plotting the exposure-response curve.

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

ersim_new_exp_med_qi <- sim_er_new_exp(
  ermod_bin,
  exposure_to_sim_vec = seq(2, 6, by = 0.2),
  data_cov = dplyr::tibble(BHBA1C_5 = 4:10),
  n_draws_sim = 500, # This is set to make the example run faster
  output_type = "median_qi"
)

ersim_new_exp_med_qi
#> # A tibble: 147 × 12
#>    AUCss_1000 BHBA1C_5  .row .epred .epred.lower .epred.upper .linpred
#>         <dbl>    <int> <int>  <dbl>        <dbl>        <dbl>    <dbl>
#>  1        2          4     1 0.0231       0.0107       0.0465   -3.74 
#>  2        2          5     2 0.0411       0.0226       0.0710   -3.15 
#>  3        2          6     3 0.0708       0.0457       0.107    -2.57 
#>  4        2          7     4 0.120        0.0891       0.157    -1.99 
#>  5        2          8     5 0.198        0.160        0.237    -1.40 
#>  6        2          9     6 0.305        0.255        0.363    -0.824
#>  7        2         10     7 0.440        0.358        0.526    -0.242
#>  8        2.2        4     8 0.0252       0.0118       0.0507   -3.66 
#>  9        2.2        5     9 0.0445       0.0247       0.0768   -3.07 
#> 10        2.2        6    10 0.0771       0.0498       0.117    -2.48 
#> # ℹ 137 more rows
#> # ℹ 5 more variables: .linpred.lower <dbl>, .linpred.upper <dbl>, .width <dbl>,
#> #   .point <chr>, .interval <chr>
# }
```
