# BayesERtools 0.2.5 (in development)

## Major changes
  
## Minor changes

# BayesERtools 0.2.4

## Major changes

* Extended covariate effects functionality to support linear regression models 
  (`ermod_lin`) in addition to binary logistic regression models (`ermod_bin`)
  (@djnavarro)
  
## Minor changes

* Fix test for rstanarm update

# BayesERtools 0.2.3

## Major changes
  
## Minor changes

* Prepare for the upcoming ggplot2 release

# BayesERtools 0.2.2

## Major changes

* Implemented `kfold()` function to allow the estimation of ELPD to work with
  loo ecosystem
* Added simulated dataset for Emax model
  https://github.com/Genentech/BayesERtools/pull/7 (@djnavarro)
  
## Minor changes

* Enable setting the prior distribution for linear models
* Added `exp_candidates` argument to `extract_coef_exp_ci()` function to allow
  for the extraction of coefficients from all candidate models
* Update package dependencies

# BayesERtools 0.2.1

* Update package dependency

# BayesERtools 0.2.0

* Initial public release
