# Develop linear ER model for binary or continuous endpoint

These functions are used to develop an linear ER model with binary
(`dev_ermod_bin()`) or continuous (`dev_ermod_lin()`) endpoint. You can
also specify covariates to be included in the model.

## Usage

``` r
dev_ermod_bin(
  data,
  var_resp,
  var_exposure,
  var_cov = NULL,
  prior = rstanarm::default_prior_coef(stats::binomial()),
  prior_intercept = rstanarm::default_prior_intercept(stats::binomial()),
  verbosity_level = 1,
  chains = 4,
  iter = 2000
)

dev_ermod_lin(
  data,
  var_resp,
  var_exposure,
  var_cov = NULL,
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

- var_exposure:

  Exposure variable names in character

- var_cov:

  Covariate variable names in character vector

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

An object of class `ermod_bin` or `ermod_lin`.

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

ermod_bin
#> 
#> ── Binary ER model ─────────────────────────────────────────────────────────────
#> ℹ Use `plot_er()` to visualize ER curve
#> 
#> ── Developed model ──
#> 
#> stan_glm
#>  family:       binomial [logit]
#>  formula:      AEFLAG ~ AUCss_1000 + BHBA1C_5
#>  observations: 500
#>  predictors:   3
#> ------
#>             Median MAD_SD
#> (Intercept) -6.94   0.78 
#> AUCss_1000   0.45   0.08 
#> BHBA1C_5     0.58   0.08 
#> ------
#> * For help interpreting the printed output see ?print.stanreg
#> * For info on the priors used see ?prior_summary.stanreg
# }

# \donttest{
data(d_sim_lin)

ermod_lin <- dev_ermod_lin(
  data = d_sim_lin,
  var_resp = "response",
  var_exposure = "AUCss",
  var_cov = c("SEX", "BAGE")
)

ermod_lin
#> 
#> ── Linear ER model ─────────────────────────────────────────────────────────────
#> ℹ Use `plot_er()` to visualize ER curve
#> 
#> ── Developed model ──
#> 
#> stan_glm
#>  family:       gaussian [identity]
#>  formula:      response ~ AUCss + SEX + BAGE
#>  observations: 101
#>  predictors:   4
#> ------
#>             Median MAD_SD
#> (Intercept)  8.49   5.57 
#> AUCss        0.46   0.03 
#> SEXM        -4.10   2.12 
#> BAGE         0.50   0.10 
#> Auxiliary parameter(s):
#>       Median MAD_SD
#> sigma 10.49   0.75 
#> ------
#> * For help interpreting the printed output see ?print.stanreg
#> * For info on the priors used see ?prior_summary.stanreg
# }
```
