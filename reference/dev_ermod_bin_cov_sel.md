# Perform covariate selection for linear ER model

This functions is used to develop an ER model with covariates for binary
and continuous endpoints. `projpred` package is used for variable
selection.

## Usage

``` r
dev_ermod_bin_cov_sel(
  data,
  var_resp,
  var_exposure,
  var_cov_candidates,
  cv_method = c("LOO", "kfold"),
  k = 5,
  validate_search = FALSE,
  nterms_max = NULL,
  .reduce_obj_size = TRUE,
  prior = rstanarm::default_prior_coef(stats::binomial()),
  prior_intercept = rstanarm::default_prior_intercept(stats::binomial()),
  verbosity_level = 1,
  chains = 4,
  iter = 2000
)

dev_ermod_lin_cov_sel(
  data,
  var_resp,
  var_exposure,
  var_cov_candidates,
  cv_method = c("LOO", "kfold"),
  k = 5,
  validate_search = FALSE,
  nterms_max = NULL,
  .reduce_obj_size = TRUE,
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

- var_cov_candidates:

  Candidate covariate names in character vector

- cv_method:

  Cross-validation method. Default is "LOO" (recommended). Use "kfold"
  if you see warnings on Pareto k estimates.

- k:

  Number of folds for kfold CV. Only used if cv_method is "kfold".

- validate_search:

  Whether to validate the search. Default is FALSE. Recommend to set to
  TRUE for kfold CV. Do not use for LOO (run time would become too
  long).

- nterms_max:

  Maximum number of terms to consider in the model. Default is NULL (all
  terms are considered).

- .reduce_obj_size:

  Whether to reduce object size by removing some elements from projpred
  outputs that are not necessary for the functionality of this package.

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

An object of class `ermod_bin_cov_sel` or `ermod_lin_cov_sel`.

## Examples

``` r
# \donttest{
data(d_sim_binom_cov_hgly2)

er_binary_cov_model <- dev_ermod_bin_cov_sel(
  data = d_sim_binom_cov_hgly2,
  var_resp = "AEFLAG",
  var_exposure = "AUCss_1000",
  var_cov_candidates = c(
    "BAGE_10", "BWT_10", "BGLUC",
    "BHBA1C_5", "RACE", "VISC"
  )
)
#> 
#> ── Step 1: Full reference model fit ──
#> 
#> ── Step 2: Variable selection ──
#> 
#> ℹ The variables selected were: AUCss_1000, BHBA1C_5, BGLUC
#> 
#> ── Step 3: Final model fit ──
#> 
#> ── Cov mod dev complete ──
#> 

er_binary_cov_model
#> ── Binary ER model & covariate selection ───────────────────────────────────────
#> ℹ Use `plot_submod_performance()` to see variable selection performance
#> ℹ Use `plot_er()` with `marginal = TRUE` to visualize marginal ER curve
#> 
#> ── Selected model ──
#> 
#> stan_glm
#>  family:       binomial [logit]
#>  formula:      AEFLAG ~ AUCss_1000 + BHBA1C_5 + BGLUC
#>  observations: 500
#>  predictors:   4
#> ------
#>             Median MAD_SD
#> (Intercept) -10.95   1.18
#> AUCss_1000    0.50   0.09
#> BHBA1C_5      0.50   0.09
#> BGLUC         0.74   0.14
#> ------
#> * For help interpreting the printed output see ?print.stanreg
#> * For info on the priors used see ?prior_summary.stanreg
# }
# \donttest{
data(d_sim_lin)

ermod_lin_cov_sel <- dev_ermod_lin_cov_sel(
  data = d_sim_lin,
  var_resp = "response",
  var_exposure = "AUCss",
  var_cov_candidates = c("BAGE", "SEX")
)
#> 
#> ── Step 1: Full reference model fit ──
#> 
#> ── Step 2: Variable selection ──
#> 
#> ℹ The variables selected were: AUCss, BAGE
#> 
#> ── Step 3: Final model fit ──
#> 
#> ── Cov mod dev complete ──
#> 

ermod_lin_cov_sel
#> ── Linear ER model & covariate selection ───────────────────────────────────────
#> ℹ Use `plot_submod_performance()` to see variable selection performance
#> ℹ Use `plot_er()` with `marginal = TRUE` to visualize marginal ER curve
#> 
#> ── Selected model ──
#> 
#> stan_glm
#>  family:       gaussian [identity]
#>  formula:      response ~ AUCss + BAGE
#>  observations: 101
#>  predictors:   3
#> ------
#>             Median MAD_SD
#> (Intercept) 6.74   5.62  
#> AUCss       0.46   0.04  
#> BAGE        0.50   0.11  
#> Auxiliary parameter(s):
#>       Median MAD_SD
#> sigma 10.66   0.79 
#> ------
#> * For help interpreting the printed output see ?print.stanreg
#> * For info on the priors used see ?prior_summary.stanreg
# }
```
