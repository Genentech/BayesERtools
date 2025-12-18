# Exposure metrics selection for Emax models

This functions is used to develop an Emax model with binary and
continuous endpoint, using various exposure metrics and selecting the
best one.

## Usage

``` r
dev_ermod_emax_exp_sel(
  data,
  var_resp,
  var_exp_candidates,
  verbosity_level = 1,
  chains = 4,
  iter = 2000,
  gamma_fix = 1,
  e0_fix = NULL,
  emax_fix = NULL,
  priors = NULL,
  seed = sample.int(.Machine$integer.max, 1)
)

dev_ermod_bin_emax_exp_sel(
  data,
  var_resp,
  var_exp_candidates,
  verbosity_level = 1,
  chains = 4,
  iter = 2000,
  gamma_fix = 1,
  e0_fix = NULL,
  emax_fix = NULL,
  priors = NULL,
  seed = sample.int(.Machine$integer.max, 1)
)
```

## Arguments

- data:

  Input data for E-R analysis

- var_resp:

  Response variable name in character

- var_exp_candidates:

  Candidate exposure variable names in character vector

- verbosity_level:

  Verbosity level. 0: No output, 1: Display steps, 2: Display progress
  in each step, 3: Display MCMC sampling.

- chains:

  Number of chains for Stan.

- iter:

  Number of iterations for Stan.

- gamma_fix:

  Hill coefficient, default fixed to 1. See details in
  [`rstanemax::stan_emax()`](https://yoshidk6.github.io/rstanemax/reference/stan_emax.html)
  or
  [`rstanemax::stan_emax_binary()`](https://yoshidk6.github.io/rstanemax/reference/stan_emax_binary.html)

- e0_fix:

  See details in
  [`rstanemax::stan_emax()`](https://yoshidk6.github.io/rstanemax/reference/stan_emax.html)
  or
  [`rstanemax::stan_emax_binary()`](https://yoshidk6.github.io/rstanemax/reference/stan_emax_binary.html)

- emax_fix:

  See details in
  [`rstanemax::stan_emax()`](https://yoshidk6.github.io/rstanemax/reference/stan_emax.html)
  or
  [`rstanemax::stan_emax_binary()`](https://yoshidk6.github.io/rstanemax/reference/stan_emax_binary.html)

- priors:

  See details in
  [`rstanemax::stan_emax()`](https://yoshidk6.github.io/rstanemax/reference/stan_emax.html)
  or
  [`rstanemax::stan_emax_binary()`](https://yoshidk6.github.io/rstanemax/reference/stan_emax_binary.html)

- seed:

  Random seed for Stan model execution, see details in
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  which is used in
  [`rstanemax::stan_emax()`](https://yoshidk6.github.io/rstanemax/reference/stan_emax.html)
  or
  [`rstanemax::stan_emax_binary()`](https://yoshidk6.github.io/rstanemax/reference/stan_emax_binary.html)

## Value

An object of class `ermod_emax_exp_sel` or `ermod_bin_emax_exp_sel`.

## Examples

``` r
# \donttest{
data_er_cont <- rstanemax::exposure.response.sample
noise <- 1 + 0.5 * stats::rnorm(length(data_er_cont$exposure))
data_er_cont$exposure2 <- data_er_cont$exposure * noise
# Replace exposure < 0 with 0
data_er_cont$exposure2[data_er_cont$exposure2 < 0] <- 0

ermod_emax_exp_sel <-
  dev_ermod_emax_exp_sel(
    data = data_er_cont,
    var_resp = "response",
    var_exp_candidates = c("exposure", "exposure2")
  )
#> Warning: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.
#> ℹ The exposure metric selected was: exposure

ermod_emax_exp_sel
#> 
#> ── Emax model & exposure metric selection ──────────────────────────────────────
#> ℹ Use `plot_er_exp_sel()` for ER curve of all exposure metrics
#> ℹ Use `plot_er()` with `show_orig_data = TRUE` for ER curve of the selected exposure metric
#> 
#> ── Exposure metrics comparison ──
#> 
#>           elpd_diff se_diff
#> exposure    0.00      0.00 
#> exposure2 -16.16     10.63 
#> 
#> ── Selected model ──
#> 
#> ---- Emax model fit with rstanemax ----
#>        mean se_mean    sd  2.5%   25%   50%   75%  97.5%   n_eff Rhat
#> emax  91.89    0.12  6.04 79.53 87.76 92.09 96.07 103.40 2670.28    1
#> e0     5.93    0.10  4.64 -3.09  2.89  5.92  9.02  15.17 2253.00    1
#> ec50  76.07    0.43 20.72 44.65 61.66 73.05 86.88 124.14 2333.33    1
#> gamma  1.00     NaN  0.00  1.00  1.00  1.00  1.00   1.00     NaN  NaN
#> sigma 16.60    0.03  1.60 13.77 15.45 16.49 17.62  20.01 2870.66    1
#> * Use `extract_stanfit()` function to extract raw stanfit object
#> * Use `extract_param()` function to extract posterior draws of key parameters
#> * Use `plot()` function to visualize model fit
#> * Use `posterior_predict()` or `posterior_predict_quantile()` function to get
#>   raw predictions or make predictions on new data
#> * Use `extract_obs_mod_frame()` function to extract raw data 
#>   in a processed format (useful for plotting)
# }

# \donttest{
data_er_bin <- rstanemax::exposure.response.sample.binary

noise <- 1 + 0.5 * stats::rnorm(length(data_er_bin$conc))
data_er_bin$conc2 <- data_er_bin$conc * noise
data_er_bin$conc2[data_er_bin$conc2 < 0] <- 0

ermod_bin_emax_exp_sel <-
  dev_ermod_bin_emax_exp_sel(
    data = data_er_bin,
    var_resp = "y",
    var_exp_candidates = c("conc", "conc2")
  )
#> ℹ The exposure metric selected was: conc
# }
```
