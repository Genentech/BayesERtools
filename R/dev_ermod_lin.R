# Logistic --------------------------------------------------------------------

#' Develop linear ER model for binary or continuous endpoint
#'
#' These functions are used to develop an linear ER model with binary
#' ([dev_ermod_bin()]) or continuous ([dev_ermod_lin()]) endpoint.
#' You can also specify covariates to be included in the model.
#'
#' @export
#' @param data Input data for E-R analysis
#' @param var_resp Response variable name in character
#' @param var_exposure Exposure variable names in character
#' @param var_cov Covariate variable names in character vector
#' @param options_placebo_handling List of for placebo handling.
#' Possible options include:
#'   - `include_placebo`: Logical, whether the placebo group should be
#'   used when estimating the model. Default is observed data. Default
#'   is `FALSE`.
#'   - `method`: Character, specifying the method used to detect placebo
#'   group samples. Possible values include `"zero_exposure_as_placebo"`
#'   (the default), in which rows with zero values for the exposure
#'   variable are automatically assigned to a placebo group,
#'   `"var_placebo"`, indicating that the user will supply the name of
#'   a data column specifying the placebo group, and `"none"`, in which
#'   case no placebo handling takes place.
#'   - `var_placebo`: Character, specifying the name of a column in the
#'   data set that indicates which rows belong to the placebo group. This
#'   column is interpreted as a logical vector. This value is ignored
#'   unless `method = "var_placebo"`.
#' @param prior,prior_intercept,prior_aux See [rstanarm::stan_glm()]
#' @param verbosity_level Verbosity level. 0: No output, 1: Display steps,
#' 2: Display progress in each step, 3: Display MCMC sampling.
#' @param chains Number of chains for Stan.
#' @param iter Number of iterations for Stan.
#'
#' @return An object of class `ermod_bin` or `ermod_lin`.
#'
#' @examples
#' \donttest{
#' data(d_sim_binom_cov_hgly2)
#'
#' ermod_bin <- dev_ermod_bin(
#'   data = d_sim_binom_cov_hgly2,
#'   var_resp = "AEFLAG",
#'   var_exposure = "AUCss_1000",
#'   var_cov = "BHBA1C_5",
#' )
#'
#' ermod_bin
#' }
#'
dev_ermod_bin <- function(
    data,
    var_resp,
    var_exposure,
    var_cov = NULL,
    options_placebo_handling = list(),
    prior = rstanarm::default_prior_coef(stats::binomial()),
    prior_intercept = rstanarm::default_prior_intercept(stats::binomial()),
    verbosity_level = 1,
    chains = 4,
    iter = 2000) {

  stopifnot(verbosity_level %in% c(0, 1, 2, 3))
  refresh <- dplyr::if_else(verbosity_level >= 3, iter %/% 4, 0)

  input_args <- capture_selected_args(
    c("chains", "iter"),
    environment()
  )

  options_placebo_handling <- .apply_placebo_handling_defaults(options_placebo_handling)

  check_data_columns(
    data = data,
    var_exposure = var_exposure,
    var_resp = var_resp,
    var_cov = var_cov,
    is_binary = TRUE
  )

  var_full <- c(var_exposure, var_cov)

  formula_final <-
    stats::formula(
      paste(var_resp, "~", paste(var_full, collapse = " + "))
    )

  stan_data <- .apply_placebo_handling(data, options_placebo_handling, var_exposure)

  # Need to construct call and then evaluate. Directly calling
  # rstanarm::stan_glm with formula_final does not work for the cross-validation
  call_stan_glm <- rlang::call2(
    rstanarm::stan_glm,
    formula = formula_final,
    family = stats::binomial(),
    data = quote(stan_data),
    prior = prior,
    prior_intercept = prior_intercept,
    QR = dplyr::if_else(length(var_full) > 1, TRUE, FALSE),
    refresh = refresh, chains = chains, iter = iter
  )
  mod <- eval(call_stan_glm)

  new_ermod_bin(
    mod = mod,
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    var_cov = var_cov,
    options_placebo_handling = options_placebo_handling,
    input_args = input_args
  )
}


#' Exposure metrics selection for linear ER models
#'
#' This functions is used to develop an linear ER model with binary
#' and continuous endpoint, using various exposure metrics and
#' selecting the best one.
#'
#' @export
#' @inheritParams dev_ermod_bin
#' @param var_exp_candidates Candidate exposure variable names
#' in character vector
#'
#' @return An object of class `ermod_bin_exp_sel`.or `ermod_lin_exp_sel`
#'
#' @examples
#' \donttest{
#' data(d_sim_binom_cov_hgly2)
#'
#' ermod_bin_exp_sel <-
#'   dev_ermod_bin_exp_sel(
#'     data = d_sim_binom_cov_hgly2,
#'     var_resp = "AEFLAG",
#'     var_exp_candidates = c("AUCss_1000", "Cmaxss", "Cminss")
#'   )
#'
#' ermod_bin_exp_sel
#' }
#'
dev_ermod_bin_exp_sel <- function(
    data,
    var_resp,
    var_exp_candidates,
    options_placebo_handling = list(),
    prior = rstanarm::default_prior_coef(stats::binomial()),
    prior_intercept = rstanarm::default_prior_intercept(stats::binomial()),
    verbosity_level = 1,
    chains = 4,
    iter = 2000) {

  options_placebo_handling <- .apply_placebo_handling_defaults(options_placebo_handling)

  fun_dev_ermod <-
    purrr::partial(
      dev_ermod_bin,
      prior = prior,
      prior_intercept = prior_intercept
    )

  l_out <-
    .dev_ermod_exp_sel(
      data = data,
      var_resp = var_resp,
      var_exp_candidates = var_exp_candidates,
      options_placebo_handling = options_placebo_handling,
      verbosity_level = verbosity_level,
      chains = chains,
      iter = iter,
      fun_dev_ermod = fun_dev_ermod
    )

  new_ermod_bin_exp_sel(l_out)
}


#' Perform covariate selection for linear ER model
#'
#' This functions is used to develop an ER model with covariates for binary
#' and continuous endpoints.
#' `projpred` package is used for variable selection.
#'
#' @export
#' @inheritParams dev_ermod_bin
#' @param var_cov_candidates Candidate covariate names in character vector
#' @param cv_method Cross-validation method. Default is "LOO" (recommended).
#' Use "kfold" if you see warnings on Pareto k estimates.
#' @param k Number of folds for kfold CV.
#' Only used if cv_method is "kfold".
#' @param validate_search Whether to validate the search. Default is FALSE.
#' Recommend to set to TRUE for kfold CV. Do not use for LOO (run time would
#' become too long).
#' @param nterms_max Maximum number of terms to consider in the model.
#'  Default is NULL (all terms are considered).
#' @param .reduce_obj_size Whether to reduce object size by removing some
#' elements from projpred outputs that are not necessary for the functionality
#' of this package.
#'
#' @return An object of class `ermod_bin_cov_sel` or `ermod_lin_cov_sel`.
#'
#' @examplesIf BayesERtools:::.if_run_ex_covsel()
#' \donttest{
#' data(d_sim_binom_cov_hgly2)
#'
#' er_binary_cov_model <- dev_ermod_bin_cov_sel(
#'   data = d_sim_binom_cov_hgly2,
#'   var_resp = "AEFLAG",
#'   var_exposure = "AUCss_1000",
#'   var_cov_candidates = c(
#'     "BAGE_10", "BWT_10", "BGLUC",
#'     "BHBA1C_5", "RACE", "VISC"
#'   )
#' )
#'
#' er_binary_cov_model
#' }
#'
dev_ermod_bin_cov_sel <- function(
    data,
    var_resp,
    var_exposure,
    var_cov_candidates,
    options_placebo_handling = list(),
    cv_method = c("LOO", "kfold"),
    k = 5,
    validate_search = FALSE,
    nterms_max = NULL,
    .reduce_obj_size = TRUE,
    prior = rstanarm::default_prior_coef(stats::binomial()),
    prior_intercept = rstanarm::default_prior_intercept(stats::binomial()),
    verbosity_level = 1,
    chains = 4,
    iter = 2000) {

  options_placebo_handling <- .apply_placebo_handling_defaults(options_placebo_handling)

  fun_dev_ermod <-
    purrr::partial(
      dev_ermod_bin,
      prior = prior,
      prior_intercept = prior_intercept
    )

  ll <- .dev_ermod_cov_sel(
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    var_cov_candidates = var_cov_candidates,
    options_placebo_handling = options_placebo_handling,
    cv_method = cv_method,
    k = k,
    validate_search = validate_search,
    nterms_max = nterms_max,
    .reduce_obj_size = .reduce_obj_size,
    verbosity_level = verbosity_level,
    chains = chains,
    iter = iter,
    fun_dev_ermod = fun_dev_ermod,
    fun_family = quote(stats::binomial()),
    prior = prior,
    prior_intercept = prior_intercept
  )

  with(ll, new_ermod_bin_cov_sel(
    mod = extract_mod(mod),
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    var_cov_candidates = var_cov_candidates,
    var_cov = var_cov,
    var_selected = var_selected,
    options_placebo_handling = options_placebo_handling,
    cv_method = cv_method,
    cvvs = cvvs,
    rk = rk,
    input_args = mod$input_args
  ))
}

# Linear ----------------------------------------------------------------------

#' @export
#' @rdname dev_ermod_bin
#' @examples
#' \donttest{
#' data(d_sim_lin)
#'
#' ermod_lin <- dev_ermod_lin(
#'   data = d_sim_lin,
#'   var_resp = "response",
#'   var_exposure = "AUCss",
#'   var_cov = c("SEX", "BAGE")
#' )
#'
#' ermod_lin
#' }
#'
dev_ermod_lin <- function(
    data,
    var_resp,
    var_exposure,
    var_cov = NULL,
    options_placebo_handling = list(),
    prior = rstanarm::default_prior_coef(stats::binomial()),
    prior_intercept = rstanarm::default_prior_intercept(stats::binomial()),
    prior_aux = rstanarm::exponential(autoscale = TRUE),
    verbosity_level = 1,
    chains = 4,
    iter = 2000) {

  stopifnot(verbosity_level %in% c(0, 1, 2, 3))
  refresh <- dplyr::if_else(verbosity_level >= 3, iter %/% 4, 0)

  input_args <- capture_selected_args(
    c("chains", "iter"),
    environment()
  )

  options_placebo_handling <- .apply_placebo_handling_defaults(options_placebo_handling)

  check_data_columns(
    data = data,
    var_exposure = var_exposure,
    var_resp = var_resp,
    var_cov = var_cov
  )

  var_full <- c(var_exposure, var_cov)

  formula_final <-
    stats::formula(
      paste(var_resp, "~", paste(var_full, collapse = " + "))
    )

  stan_data <- .apply_placebo_handling(data, options_placebo_handling, var_exposure)

  mod <- rstanarm::stan_glm(
    formula_final,
    family = stats::gaussian(),
    data = stan_data,
    prior = prior,
    prior_intercept = prior_intercept,
    prior_aux = prior_aux,
    QR = dplyr::if_else(length(var_full) > 1, TRUE, FALSE),
    refresh = refresh,
    chains = chains,
    iter = iter
  )

  new_ermod_lin(
    mod = mod,
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    var_cov = var_cov,
    options_placebo_handling = options_placebo_handling,
    input_args = input_args
  )
}


#' @export
#' @rdname dev_ermod_bin_exp_sel
#' @examples
#' \donttest{
#' data(d_sim_lin)
#'
#' ermod_lin_exp_sel <- dev_ermod_lin_exp_sel(
#'   data = d_sim_lin,
#'   var_resp = "response",
#'   var_exp_candidates = c("AUCss", "Cmaxss"),
#'   options_placebo_handling = list(include_placebo = TRUE)
#' )
#'
#' ermod_lin_exp_sel
#' }
#'
dev_ermod_lin_exp_sel <- function(
    data,
    var_resp,
    var_exp_candidates,
    options_placebo_handling = list(),
    prior = rstanarm::default_prior_coef(stats::binomial()),
    prior_intercept = rstanarm::default_prior_intercept(stats::binomial()),
    prior_aux = rstanarm::exponential(autoscale = TRUE),
    verbosity_level = 1,
    chains = 4,
    iter = 2000) {

  options_placebo_handling <- .apply_placebo_handling_defaults(options_placebo_handling)

  fun_dev_ermod <-
    purrr::partial(
      dev_ermod_lin,
      prior = prior,
      prior_intercept = prior_intercept,
      prior_aux = prior_aux
    )

  l_out <-
    .dev_ermod_exp_sel(
      data = data,
      var_resp = var_resp,
      var_exp_candidates = var_exp_candidates,
      verbosity_level = verbosity_level,
      options_placebo_handling = options_placebo_handling,
      chains = chains,
      iter = iter,
      fun_dev_ermod = fun_dev_ermod
    )

  new_ermod_lin_exp_sel(l_out)
}


#' @export
#' @rdname dev_ermod_bin_cov_sel
#' @examplesIf BayesERtools:::.if_run_ex_covsel()
#' \donttest{
#' data(d_sim_lin)
#'
#' ermod_lin_cov_sel <- dev_ermod_lin_cov_sel(
#'   data = d_sim_lin,
#'   var_resp = "response",
#'   var_exposure = "AUCss",
#'   var_cov_candidates = c("BAGE", "SEX")
#' )
#'
#' ermod_lin_cov_sel
#' }
#'
dev_ermod_lin_cov_sel <- function(
    data,
    var_resp,
    var_exposure,
    var_cov_candidates,
    options_placebo_handling = list(),
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
    iter = 2000) {

  options_placebo_handling <- .apply_placebo_handling_defaults(options_placebo_handling)

  fun_dev_ermod <-
    purrr::partial(
      dev_ermod_lin,
      prior = prior,
      prior_intercept = prior_intercept,
      prior_aux = prior_aux
    )

  ll <- .dev_ermod_cov_sel(
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    var_cov_candidates = var_cov_candidates,
    options_placebo_handling = options_placebo_handling,
    cv_method = cv_method,
    k = k,
    validate_search = validate_search,
    nterms_max = nterms_max,
    .reduce_obj_size = .reduce_obj_size,
    verbosity_level = verbosity_level,
    chains = chains,
    iter = iter,
    fun_dev_ermod = fun_dev_ermod,
    fun_family = quote(stats::gaussian()),
    prior = prior,
    prior_intercept = prior_intercept,
    prior_aux = prior_aux
  )

  with(ll, new_ermod_lin_cov_sel(
    mod = extract_mod(mod),
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    var_cov_candidates = var_cov_candidates,
    options_placebo_handling = options_placebo_handling,
    var_cov = var_cov,
    var_selected = var_selected,
    cv_method = cv_method,
    cvvs = cvvs,
    rk = rk,
    input_args = mod$input_args
  ))
}

# Internal functions ----------------------------------------------------------

.dev_ermod_exp_sel <- function(
    data,
    var_resp,
    var_exp_candidates,
    options_placebo_handling,
    verbosity_level = 1,
    chains = 4,
    iter = 2000,
    fun_dev_ermod) {

  stopifnot(verbosity_level %in% c(0, 1, 2, 3))

  verbose <- dplyr::if_else(verbosity_level == 2, TRUE, FALSE)

  check_data_columns(
    data = data,
    var_resp = var_resp,
    var_exp_candidates = var_exp_candidates
  )

  l_mod_exposures <-
    var_exp_candidates |>
    purrr::set_names() |>
    purrr::map(
      function(.x) {
        fun_dev_ermod(
          data,
          var_resp,
          .x,
          chains = chains,
          iter = iter,
          verbosity_level = verbosity_level,
          options_placebo_handling = options_placebo_handling,
        )
      },
      .progress = verbose
    )

  if (length(var_exp_candidates) == 1) {
    if (verbosity_level >= 1) {
      cli::cli_alert_info(paste(
        "Only one exposure metric",
        "{var_exp_candidates[[1]]} was provided."
      ))
    }

    var_exposure <- var_exp_candidates[[1]]
    loo_comp_exposures <- NULL
  } else {
    l_loo_exposures <- purrr::map(
      l_mod_exposures,
      function(.x) loo(.x)
    )
    comp_exposures <- loo::loo_compare(l_loo_exposures)
    var_exposure <- rownames(comp_exposures)[[1]]

    if (verbosity_level >= 1) {
      cli::cli_alert_info("The exposure metric selected was: {var_exposure}")
    }

    loo_comp_exposures <- comp_exposures
  }

  list(
    mod = extract_mod(l_mod_exposures[[var_exposure]]),
    data = data,
    var_resp = var_resp,
    var_exp_candidates = var_exp_candidates,
    var_exposure = var_exposure,
    l_mod_exposures = l_mod_exposures,
    loo_comp_exposures = loo_comp_exposures,
    input_args = l_mod_exposures[[var_exposure]]$input_args
  )
}


.dev_ermod_cov_sel <- function(
    data,
    var_resp,
    var_exposure,
    var_cov_candidates,
    options_placebo_handling,
    cv_method = c("LOO", "kfold"),
    k = 5,
    validate_search = FALSE,
    nterms_max = NULL,
    .reduce_obj_size = TRUE,
    verbosity_level = 1,
    chains = 4,
    iter = 2000,
    fun_dev_ermod,
    fun_family,
    prior = rstanarm::default_prior_coef(stats::binomial()),
    prior_intercept = rstanarm::default_prior_intercept(stats::binomial()),
    prior_aux = rstanarm::exponential(autoscale = TRUE)) {

  stopifnot(verbosity_level %in% c(0, 1, 2, 3))

  rlang::check_installed("projpred")

  if (length(var_cov_candidates) == 0) {
    stop("At least one covariate candidate should be provided")
  }

  check_data_columns(
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    var_cov_candidates = var_cov_candidates
  )

  if (verbosity_level >= 1) cli::cli_h2("Step 1: Full reference model fit")
  refm_obj <- .dev_ermod_refmodel(
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    var_cov_candidates = var_cov_candidates,
    options_placebo_handling = options_placebo_handling,
    verbosity_level = verbosity_level,
    chains = chains, iter = iter,
    fun_family = fun_family,
    prior = prior,
    prior_intercept = prior_intercept,
    prior_aux = prior_aux
  )

  if (verbosity_level >= 1) cli::cli_h2("Step 2: Variable selection")
  var_selected <- .select_cov_projpred(
    refm_obj = refm_obj,
    var_exposure = var_exposure,
    var_cov_candidates = var_cov_candidates,
    nterms_max = nterms_max,
    cv_method = cv_method,
    k = k,
    .reduce_obj_size = .reduce_obj_size,
    validate_search = validate_search,
    verbosity_level = verbosity_level
  )

  if (verbosity_level >= 1) cli::cli_h2("Step 3: Final model fit")

  if (length(var_selected) == 1) {
    var_cov <- NULL
  } else {
    var_cov <- setdiff(var_selected, var_exposure)
  }

  mod_final <- fun_dev_ermod(
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    var_cov = var_cov,
    options_placebo_handling = options_placebo_handling,
    verbosity_level = verbosity_level,
    chains = chains, iter = iter
  )

  if (verbosity_level >= 1) cli::cli_h2("Cov mod dev complete")

  list(
    mod = mod_final,
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    var_cov_candidates = var_cov_candidates,
    var_cov = var_cov,
    var_selected = as.character(var_selected),
    options_placebo_handling = options_placebo_handling,
    cv_method = cv_method,
    cvvs = attr(var_selected, "cvvs"),
    rk = attr(var_selected, "rk")
  )
}


#' Internal functions for developing an ER model with covariates
#' for binary endpoint
#'
#' These functions are not intended to be used directly by users.
#'
#' @name dev_ermod_bin_cov_functions
#' @keywords internal
#' @inheritParams dev_ermod_bin
#' @inheritParams dev_ermod_bin_cov_sel
NULL


#' @export
#' @rdname dev_ermod_bin_cov_functions
#' @param fun_family Family function for the model. Default is binomial.
#' @keywords internal
#' @details
#' [.dev_ermod_refmodel()] is used to fit the refmodel (full reference
#' model) necessary for projpred
#' @return
#' [.dev_ermod_refmodel()]: The reference model object that can be used
#' for variable selection.
.dev_ermod_refmodel <- function(
    data,
    var_resp,
    var_exposure,
    var_cov_candidates,
    options_placebo_handling,
    verbosity_level = 1,
    chains = 4,
    iter = 2000,
    fun_family = quote(stats::binomial()),
    prior = rstanarm::default_prior_coef(stats::binomial()),
    prior_intercept = rstanarm::default_prior_intercept(stats::binomial()),
    prior_aux = rstanarm::exponential(autoscale = TRUE)) {

  stopifnot(verbosity_level %in% c(0, 1, 2, 3))
  refresh <- dplyr::if_else(verbosity_level >= 3, iter %/% 4, 0)

  if (length(var_exposure) != 1) {
    stop("Only one exposure metric should be provided")
  }

  check_data_columns(
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    var_cov_candidates = var_cov_candidates
  )

  stan_data <- .apply_placebo_handling(data, options_placebo_handling, var_exposure)

  varnames <- paste(c(var_exposure, var_cov_candidates), collapse = " + ")
  formula_full <-
    stats::formula(paste(var_resp, "~", varnames))

  # Need to construct call and then evaluate. Directly calling
  # rstanarm::stan_glm with formula_full does not work for the cross-validation
  call_fit_ref <-
    rlang::call2(
      rstanarm::stan_glm,
      formula = formula_full,
      family = fun_family,
      data = quote(stan_data),
      QR = TRUE,
      refresh = refresh,
      chains = chains,
      iter = iter,
      prior = prior,
      prior_intercept = prior_intercept,
      prior_aux = prior_aux
    )
  fit_ref <- eval(call_fit_ref)

  refm_obj <- projpred::get_refmodel(fit_ref)

  return(refm_obj)
}


#' @export
#' @rdname dev_ermod_bin_cov_functions
#' @keywords internal
#' @param refm_obj Reference model object used for variable selection
#' @details
#' [.select_cov_projpred()] is used to select variables with `projpred` package
#' @return
#' [.select_cov_projpred()]: The selected variables
.select_cov_projpred <- function(
    refm_obj,
    var_exposure,
    var_cov_candidates,
    nterms_max = NULL,
    cv_method = c("LOO", "kfold"),
    k = 5,
    .reduce_obj_size = TRUE,
    validate_search = FALSE,
    verbosity_level = 1) {
  cv_method <- match.arg(cv_method)
  stopifnot(verbosity_level %in% c(0, 1, 2, 3))

  if (length(var_exposure) != 1) {
    stop("Only one exposure metric should be provided")
  }

  search_terms <- projpred::force_search_terms(
    forced_terms = var_exposure,
    optional_terms = var_cov_candidates
  )

  verbose <- dplyr::if_else(verbosity_level == 2, TRUE, FALSE)

  if (cv_method == "LOO" && validate_search) {
    stop(paste(
      "validate_search should be set to FALSE for LOO,",
      "otherwise run time would be too long"
    ))
  }

  cvvs <- projpred::cv_varsel(
    refm_obj,
    search_terms = search_terms,
    nterms_max = nterms_max,
    cv_method = cv_method,
    K = k,
    method = "forward",
    validate_search = validate_search,
    refit_prj = TRUE,
    verbose = verbose
  )

  rk <- projpred::ranking(cvvs)

  n_var_select <- projpred::suggest_size(cvvs)
  if (n_var_select == 0 && verbosity_level >= 1) {
    cli::cli_alert_info(paste(
      "No variables selected, exposure variable was",
      "forced into the final model."
    ))
  }
  n_var_select <- max(1, n_var_select)

  var_selected <- utils::head(rk[["fulldata"]], n_var_select)

  if (verbosity_level >= 1) {
    cli::cli_alert_info(
      paste(
        "The variables selected were:",
        paste(var_selected, collapse = ", ")
      )
    )
  }

  if (.reduce_obj_size) {
    cvvs <- .reduce_cvvs_size(cvvs)
  }

  attr(var_selected, "cvvs") <- cvvs
  attr(var_selected, "rk") <- rk
  attr(var_selected, "var_exposure") <- var_exposure

  return(var_selected)
}

.reduce_cvvs_size <- function(cvvs) {
  family <- cvvs$refmodel$family
  cvvs$refmodel <- NULL
  cvvs$refmodel$family <- family
  cvvs$search_path <- NULL

  return(cvvs)
}

check_data_columns <- function(
    data,
    var_resp = NULL,
    var_exp_candidates = NULL,
    var_exposure = NULL,
    var_cov_candidates = NULL,
    var_cov = NULL,
    is_binary = FALSE) {

  if (!is.data.frame(data)) {
    stop("data should be a data frame")
  }

  if (!is.null(var_resp) && length(var_resp) != 1) {
    stop("Only one response variable should be provided")
  }

  if (is_binary && !all(data[[var_resp]] %in% c(0, 1))) {
    stop("Response variable must be a numeric of 0 and 1 for binary models")
  }

  if (!is.null(var_exposure) && length(var_exposure) != 1) {
    stop("Only one exposure metric should be provided")
  }

  check_columns_exist(data, var_resp, "response")
  check_columns_exist(data, var_exp_candidates, "exposure metric")
  check_columns_exist(data, var_exposure, "exposure metric")
  check_columns_exist(data, var_cov_candidates, "covariate")
  check_columns_exist(data, var_cov, "covariate")

  check_na_values(data, c(var_exp_candidates, var_exposure), "exposure metric")
  check_na_values(data, var_cov_candidates, "covariate")
  check_na_values(data, var_cov, "covariate")
  check_na_values(data, var_resp, "response")
}

check_columns_exist <- function(data, cols, column_type) {
  if (!is.null(cols) && any(!(cols %in% names(data)))) {
    cols_str <- paste(cols, collapse = ", ")
    stop(sprintf('"%s" columns should be in data', cols_str))
  }
}

check_na_values <- function(data, cols, column_type) {
  if (!is.null(cols)) {
    # Need to separate, otherwise styler & lintr complains
    if (any(sapply(cols, function(col) any(is.na(data[[col]]))))) {
      stop(paste("NA values not allowed in the", column_type, "column(s)"))
    }
  }
}

# Capture the specified arguments and their values from the given environment
capture_selected_args <- function(arg_names, env) {
  input_args <- lapply(arg_names, function(arg) get(arg, envir = env))

  # Name the list with the argument names
  names(input_args) <- arg_names

  return(input_args)
}

.if_run_ex_covsel <- function() {
  requireNamespace("projpred", quietly = TRUE)
}

.apply_placebo_handling <- function(data, options, var_exposure = NULL) {

  # do nothing if user wants to retain placebo
  if (options$include_placebo) return(data)

  # "none" method: no filtering, also do nothing
  if (options$method == "none") return(data)

  # "var_placebo" method: filter rows using named column
  if (options$method == "var_placebo") {
    keep <- !data[[options$var_placebo]]
    return(data[keep, ])
  }

  # "zero_exposure_as_placebo" method: drop zero-exposure rows
  if (options$method == "zero_exposure_as_placebo") {
    keep <- data[[var_exposure]] != 0
    return(data[keep, ])
  }

  stop("unknown placebo handling method", call. = FALSE)
}
