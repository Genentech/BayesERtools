# Internal functions for developing an ER model with covariates for binary endpoint

These functions are not intended to be used directly by users.

## Usage

``` r
.dev_ermod_refmodel(
  data,
  var_resp,
  var_exposure,
  var_cov_candidates,
  verbosity_level = 1,
  chains = 4,
  iter = 2000,
  fun_family = quote(stats::binomial()),
  prior = rstanarm::default_prior_coef(stats::binomial()),
  prior_intercept = rstanarm::default_prior_intercept(stats::binomial()),
  prior_aux = rstanarm::exponential(autoscale = TRUE)
)

.select_cov_projpred(
  refm_obj,
  var_exposure,
  var_cov_candidates,
  nterms_max = NULL,
  cv_method = c("LOO", "kfold"),
  k = 5,
  .reduce_obj_size = TRUE,
  validate_search = FALSE,
  verbosity_level = 1
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

- verbosity_level:

  Verbosity level. 0: No output, 1: Display steps, 2: Display progress
  in each step, 3: Display MCMC sampling.

- chains:

  Number of chains for Stan.

- iter:

  Number of iterations for Stan.

- fun_family:

  Family function for the model. Default is binomial.

- prior, prior_intercept, prior_aux:

  See
  [`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html)

- refm_obj:

  Reference model object used for variable selection

- nterms_max:

  Maximum number of terms to consider in the model. Default is NULL (all
  terms are considered).

- cv_method:

  Cross-validation method. Default is "LOO" (recommended). Use "kfold"
  if you see warnings on Pareto k estimates.

- k:

  Number of folds for kfold CV. Only used if cv_method is "kfold".

- .reduce_obj_size:

  Whether to reduce object size by removing some elements from projpred
  outputs that are not necessary for the functionality of this package.

- validate_search:

  Whether to validate the search. Default is FALSE. Recommend to set to
  TRUE for kfold CV. Do not use for LOO (run time would become too
  long).

## Value

`.dev_ermod_refmodel()`: The reference model object that can be used for
variable selection.

`.select_cov_projpred()`: The selected variables

## Details

`.dev_ermod_refmodel()` is used to fit the refmodel (full reference
model) necessary for projpred

`.select_cov_projpred()` is used to select variables with `projpred`
package
