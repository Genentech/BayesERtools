# Calculate marginal expected response for specified exposure values

Responses at specified exposure values are calculated for `n_subj_sim`
subjects with different covariates (sampled from `newdata`), and the
predicted responses are "marginalized" (averaged), resulting in marginal
expected response on the population of interest.

## Usage

``` r
sim_er_new_exp_marg(
  ermod,
  exposure_to_sim_vec = NULL,
  data_cov = extract_data(ermod),
  n_subj_sim = 100,
  n_draws_sim = 500,
  seed_sample_draws = NULL,
  output_type = c("draws", "median_qi"),
  qi_width = 0.95
)

sim_er_curve_marg(
  ermod,
  exposure_range = NULL,
  num_exposures = 51,
  data_cov = extract_data(ermod),
  n_subj_sim = 100,
  n_draws_sim = 500,
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

  Data frame containing covariates to use for simulation. Different from
  [`sim_er_new_exp()`](https://genentech.github.io/BayesERtools/reference/sim_er_new_exp.md),
  `data_cov` can be large as long as `n_subj_sim` is set to a reasonable
  number. Default is set to `extract_data(ermod)` which is the full data
  used to fit the model.

- n_subj_sim:

  Maximum number of subjects to simulate. Default of 100 should be
  sufficient in many cases, as it's only used for marginal response
  calculation. Set to NULL to use all subjects in `data_cov` without
  resampling; in this case, be mindful of the computation time.

- n_draws_sim:

  Number of draws for simulation. Default is set to 500 to reduce
  computation time for marginal response calculation.

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

`ersim_marg` object, which is a tibble with the simulated marginal
expected response with some additional information in object attributes.
In case of `output_type = "median_qi"`, it returns `ersim_marg_med_qi`
object.

## Details

`sim_er_new_exp_marg()` returns a tibble with the marginal expected
response for each exposure value in `exposure_to_sim_vec`.

`sim_er_curve_marg()` is a wrapper function for `sim_er_new_exp_marg()`
that use a range of exposure values to simulate the marginal expected
responses. Particularly useful for plotting the exposure-response curve.

## See also

[`calc_ersim_med_qi()`](https://genentech.github.io/BayesERtools/reference/calc_ersim_med_qi.md)
for calculating median and quantile interval from `ersim_marg` object
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

ersim_new_exp_marg_med_qi <- sim_er_new_exp_marg(
  ermod_bin,
  exposure_to_sim_vec = seq(2, 6, by = 0.2),
  data_cov = dplyr::tibble(BHBA1C_5 = 4:10),
  n_subj_sim = NULL,
  n_draws_sim = 500, # This is set to make the example run faster
  output_type = "median_qi"
)

ersim_new_exp_marg_med_qi
#> # A tibble: 21 × 14
#>    .id_exposure AUCss_1000 .epred .epred.lower .epred.upper .linpred
#>           <int>      <dbl>  <dbl>        <dbl>        <dbl>    <dbl>
#>  1            1        2    0.173        0.146        0.207    -1.96
#>  2            2        2.2  0.185        0.158        0.219    -1.88
#>  3            3        2.4  0.196        0.170        0.232    -1.78
#>  4            4        2.6  0.208        0.182        0.244    -1.69
#>  5            5        2.8  0.221        0.194        0.256    -1.61
#>  6            6        3    0.234        0.205        0.271    -1.51
#>  7            7        3.2  0.247        0.217        0.286    -1.43
#>  8            8        3.4  0.261        0.227        0.300    -1.34
#>  9            9        3.6  0.276        0.240        0.316    -1.25
#> 10           10        3.8  0.290        0.250        0.334    -1.16
#> # ℹ 11 more rows
#> # ℹ 8 more variables: .linpred.lower <dbl>, .linpred.upper <dbl>,
#> #   .prediction <dbl>, .prediction.lower <dbl>, .prediction.upper <dbl>,
#> #   .width <dbl>, .point <chr>, .interval <chr>
# }
```
