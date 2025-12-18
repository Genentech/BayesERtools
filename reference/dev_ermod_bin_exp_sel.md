# Exposure metrics selection for linear ER models

This functions is used to develop an linear ER model with binary and
continuous endpoint, using various exposure metrics and selecting the
best one.

## Usage

``` r
dev_ermod_bin_exp_sel(
  data,
  var_resp,
  var_exp_candidates,
  prior = rstanarm::default_prior_coef(stats::binomial()),
  prior_intercept = rstanarm::default_prior_intercept(stats::binomial()),
  verbosity_level = 1,
  chains = 4,
  iter = 2000
)

dev_ermod_lin_exp_sel(
  data,
  var_resp,
  var_exp_candidates,
  prior = rstanarm::default_prior_coef(stats::binomial()),
  prior_intercept = rstanarm::default_prior_intercept(stats::binomial()),
  prior_aux = rstanarm::exponential(autoscale = TRUE),
  verbosity_level = 1,
  chains = 4,
  iter = 2000
)
```

## Arguments

- data:

  Input data for E-R analysis

- var_resp:

  Response variable name in character

- var_exp_candidates:

  Candidate exposure variable names in character vector

- prior, prior_intercept, prior_aux:

  See
  [`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html)

- verbosity_level:

  Verbosity level. 0: No output, 1: Display steps, 2: Display progress
  in each step, 3: Display MCMC sampling.

- chains:

  Number of chains for Stan.

- iter:

  Number of iterations for Stan.

## Value

An object of class `ermod_bin_exp_sel`.or `ermod_lin_exp_sel`

## Examples

``` r
# \donttest{
data(d_sim_binom_cov_hgly2)

ermod_bin_exp_sel <-
  dev_ermod_bin_exp_sel(
    data = d_sim_binom_cov_hgly2,
    var_resp = "AEFLAG",
    var_exp_candidates = c("AUCss_1000", "Cmaxss", "Cminss")
  )
#> ℹ The exposure metric selected was: AUCss_1000

ermod_bin_exp_sel
#> 
#> ── Binary ER model & exposure metric selection ─────────────────────────────────
#> ℹ Use `plot_er_exp_sel()` for ER curve of all exposure metrics
#> ℹ Use `plot_er()` with `show_orig_data = TRUE` for ER curve of the selected exposure metric
#> 
#> ── Exposure metrics comparison ──
#> 
#>            elpd_diff se_diff
#> AUCss_1000  0.00      0.00  
#> Cminss     -4.41      3.13  
#> Cmaxss     -4.94      2.87  
#> 
#> ── Selected model ──
#> 
#> stan_glm
#>  family:       binomial [logit]
#>  formula:      AEFLAG ~ AUCss_1000
#>  observations: 500
#>  predictors:   2
#> ------
#>             Median MAD_SD
#> (Intercept) -2.05   0.23 
#> AUCss_1000   0.41   0.07 
#> ------
#> * For help interpreting the printed output see ?print.stanreg
#> * For info on the priors used see ?prior_summary.stanreg
# }

# \donttest{
data(d_sim_lin)

ermod_lin_exp_sel <- dev_ermod_lin_exp_sel(
  data = d_sim_lin,
  var_resp = "response",
  var_exp_candidates = c("AUCss", "Cmaxss")
)
#> ℹ The exposure metric selected was: AUCss

ermod_lin_exp_sel
#> 
#> ── Linear ER model & exposure metric selection ─────────────────────────────────
#> ℹ Use `plot_er_exp_sel()` for ER curve of all exposure metrics
#> ℹ Use `plot_er()` with `show_orig_data = TRUE` for ER curve of the selected exposure metric
#> 
#> ── Exposure metrics comparison ──
#> 
#>        elpd_diff se_diff
#> AUCss    0.00      0.00 
#> Cmaxss -13.33      4.52 
#> 
#> ── Selected model ──
#> 
#> stan_glm
#>  family:       gaussian [identity]
#>  formula:      response ~ AUCss
#>  observations: 101
#>  predictors:   2
#> ------
#>             Median MAD_SD
#> (Intercept) 31.32   2.38 
#> AUCss        0.47   0.04 
#> Auxiliary parameter(s):
#>       Median MAD_SD
#> sigma 11.76   0.83 
#> ------
#> * For help interpreting the printed output see ?print.stanreg
#> * For info on the priors used see ?prior_summary.stanreg
# }
```
