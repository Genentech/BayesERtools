# Package index

## Step 1: Develop ER models

### Linear model

- [`dev_ermod_bin()`](https://genentech.github.io/BayesERtools/reference/dev_ermod_bin.md)
  [`dev_ermod_lin()`](https://genentech.github.io/BayesERtools/reference/dev_ermod_bin.md)
  : Develop linear ER model for binary or continuous endpoint
- [`dev_ermod_bin_exp_sel()`](https://genentech.github.io/BayesERtools/reference/dev_ermod_bin_exp_sel.md)
  [`dev_ermod_lin_exp_sel()`](https://genentech.github.io/BayesERtools/reference/dev_ermod_bin_exp_sel.md)
  : Exposure metrics selection for linear ER models
- [`dev_ermod_bin_cov_sel()`](https://genentech.github.io/BayesERtools/reference/dev_ermod_bin_cov_sel.md)
  [`dev_ermod_lin_cov_sel()`](https://genentech.github.io/BayesERtools/reference/dev_ermod_bin_cov_sel.md)
  : Perform covariate selection for linear ER model

### Emax model

- [`dev_ermod_emax()`](https://genentech.github.io/BayesERtools/reference/dev_ermod_emax.md)
  [`dev_ermod_bin_emax()`](https://genentech.github.io/BayesERtools/reference/dev_ermod_emax.md)
  : Develop Emax model for continuous and binary endpoint
- [`dev_ermod_emax_exp_sel()`](https://genentech.github.io/BayesERtools/reference/dev_ermod_emax_exp_sel.md)
  [`dev_ermod_bin_emax_exp_sel()`](https://genentech.github.io/BayesERtools/reference/dev_ermod_emax_exp_sel.md)
  : Exposure metrics selection for Emax models

## Step 2: Simulate responses

- [`sim_er()`](https://genentech.github.io/BayesERtools/reference/sim_er.md)
  : Simulate from ER model
- [`sim_er_new_exp()`](https://genentech.github.io/BayesERtools/reference/sim_er_new_exp.md)
  [`sim_er_curve()`](https://genentech.github.io/BayesERtools/reference/sim_er_new_exp.md)
  : Simulate from ER model at specified exposure values
- [`sim_er_new_exp_marg()`](https://genentech.github.io/BayesERtools/reference/sim_er_new_exp_marg.md)
  [`sim_er_curve_marg()`](https://genentech.github.io/BayesERtools/reference/sim_er_new_exp_marg.md)
  : Calculate marginal expected response for specified exposure values
- [`calc_ersim_med_qi()`](https://genentech.github.io/BayesERtools/reference/calc_ersim_med_qi.md)
  : Calculate median and quantile intervals from ersim object

## Step 3: Plot ER

- [`plot_er()`](https://genentech.github.io/BayesERtools/reference/plot_er.md)
  : Plot ER model simulations
- [`plot_er_gof()`](https://genentech.github.io/BayesERtools/reference/plot_er_gof.md)
  : Default GOF plot for ER model
- [`plot_er_exp_sel()`](https://genentech.github.io/BayesERtools/reference/plot_er_exp_sel.md)
  : Plot exposure metric selection comparison

## Step 4: Model evaluation

- [`eval_ermod()`](https://genentech.github.io/BayesERtools/reference/eval_ermod.md)
  : Evaluate exposure-response model prediction performance
- [`loo()`](https://genentech.github.io/BayesERtools/reference/loo.md) :
  Efficient approximate leave-one-out cross-validation (LOO)
- [`kfold()`](https://genentech.github.io/BayesERtools/reference/kfold.md)
  [`extract_kfold_loo()`](https://genentech.github.io/BayesERtools/reference/kfold.md)
  : Run k-fold cross-validation

## Other post-processing functions

### Covariate selection

- [`plot_submod_performance()`](https://genentech.github.io/BayesERtools/reference/plot_cov_sel.md)
  [`plot_var_ranking()`](https://genentech.github.io/BayesERtools/reference/plot_cov_sel.md)
  : Plot variable selection performance

### Covariate effect forest plot

- [`build_spec_coveff()`](https://genentech.github.io/BayesERtools/reference/build_spec_coveff.md)
  : Build specifications for covariate effect simulation/visualization
- [`build_spec_coveff_one_variable()`](https://genentech.github.io/BayesERtools/reference/edit_spec_coveff.md)
  [`replace_spec_coveff()`](https://genentech.github.io/BayesERtools/reference/edit_spec_coveff.md)
  : Customize specifications for covariate effect
  simulations/visualizations
- [`sim_coveff()`](https://genentech.github.io/BayesERtools/reference/sim_coveff.md)
  : Perform simulation of covariate effects for ER model
- [`plot_coveff()`](https://genentech.github.io/BayesERtools/reference/plot_coveff.md)
  : Visualize the covariate effects for ER model
- [`print_coveff()`](https://genentech.github.io/BayesERtools/reference/print_coveff.md)
  : Format the covariate effect simulation results for printing

### Others

- [`as_draws()`](https://genentech.github.io/BayesERtools/reference/as_draws.md)
  [`as_draws_list()`](https://genentech.github.io/BayesERtools/reference/as_draws.md)
  [`as_draws_array()`](https://genentech.github.io/BayesERtools/reference/as_draws.md)
  [`as_draws_df()`](https://genentech.github.io/BayesERtools/reference/as_draws.md)
  [`as_draws_matrix()`](https://genentech.github.io/BayesERtools/reference/as_draws.md)
  [`as_draws_rvars()`](https://genentech.github.io/BayesERtools/reference/as_draws.md)
  :

  Transform to `draws` objects

- [`prior_summary()`](https://genentech.github.io/BayesERtools/reference/prior_summary.md)
  : Summarize the priors used for linear or linear logistic regression
  models

- [`p_direction(`*`<ermod_bin>`*`)`](https://genentech.github.io/BayesERtools/reference/p_direction.md)
  : Probability of Direction (pd)

## Utility functions

- [`extract_data()`](https://genentech.github.io/BayesERtools/reference/extract_method.md)
  [`extract_mod()`](https://genentech.github.io/BayesERtools/reference/extract_method.md)
  [`extract_var_resp()`](https://genentech.github.io/BayesERtools/reference/extract_method.md)
  [`extract_var_exposure()`](https://genentech.github.io/BayesERtools/reference/extract_method.md)
  [`extract_var_cov()`](https://genentech.github.io/BayesERtools/reference/extract_method.md)
  [`extract_exp_sel_list_model()`](https://genentech.github.io/BayesERtools/reference/extract_method.md)
  [`extract_exp_sel_comp()`](https://genentech.github.io/BayesERtools/reference/extract_method.md)
  [`extract_var_selected()`](https://genentech.github.io/BayesERtools/reference/extract_method.md)
  : Extract elements from S3 objects
- [`extract_coef_exp_ci()`](https://genentech.github.io/BayesERtools/reference/extract_coef_exp_ci.md)
  : Extract credible interval of the exposure coefficient

## Data

- [`d_sim_binom_cov`](https://genentech.github.io/BayesERtools/reference/d_sim_binom_cov.md)
  [`d_sim_binom_cov_hgly2`](https://genentech.github.io/BayesERtools/reference/d_sim_binom_cov.md)
  : Sample simulated data for exposure-response with binary endpoint.
- [`d_sim_lin`](https://genentech.github.io/BayesERtools/reference/d_sim_lin.md)
  : Sample simulated data for exposure-response with continuous endpoint
  using linear model.
- [`d_sim_emax`](https://genentech.github.io/BayesERtools/reference/d_sim_emax.md)
  : Sample simulated data for Emax exposure-response models with
  covariates.
- [`d_sim_placebo`](https://genentech.github.io/BayesERtools/reference/d_sim_placebo.md)
  : Sample simulated data for Emax exposure-response models with
  covariates and placebo

## S3 methods

- [`print(`*`<ermod>`*`)`](https://genentech.github.io/BayesERtools/reference/ermod_method.md)
  [`plot(`*`<ermod_bin>`*`)`](https://genentech.github.io/BayesERtools/reference/ermod_method.md)
  [`coef(`*`<ermod>`*`)`](https://genentech.github.io/BayesERtools/reference/ermod_method.md)
  [`summary(`*`<ermod>`*`)`](https://genentech.github.io/BayesERtools/reference/ermod_method.md)
  :

  S3 methods for the classes `ermod_*`

- [`plot(`*`<ersim>`*`)`](https://genentech.github.io/BayesERtools/reference/ersim_method.md)
  [`plot(`*`<ersim_med_qi>`*`)`](https://genentech.github.io/BayesERtools/reference/ersim_method.md)
  :

  S3 methods for the classes `ersim_*` and `ersim_med_qi_*`

- [`print(`*`<ermod_exp_sel>`*`)`](https://genentech.github.io/BayesERtools/reference/ermod_exp_sel_method.md)
  [`plot(`*`<ermod_exp_sel>`*`)`](https://genentech.github.io/BayesERtools/reference/ermod_exp_sel_method.md)
  :

  S3 methods for the classes `ermod_exp_sel`

- [`print(`*`<ermod_cov_sel>`*`)`](https://genentech.github.io/BayesERtools/reference/ermod_cov_sel_method.md)
  [`plot(`*`<ermod_cov_sel>`*`)`](https://genentech.github.io/BayesERtools/reference/ermod_cov_sel_method.md)
  :

  S3 methods for the classes `ermod_bin_cov_sel`
