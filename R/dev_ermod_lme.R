# Linear mixed-effects ------------------------------------------------------

#' Develop linear mixed-effects ER model for continuous endpoint
#'
#' This function is used to develop a linear mixed-effects ER model with
#' continuous endpoint, for data with repeated measurements per subject.
#' The model is fitted with [rstanarm::stan_glmer()] with Gaussian family
#' (equivalent to [rstanarm::stan_lmer()]), which uses precompiled Stan
#' models.
#'
#' @export
#' @inheritParams dev_ermod_bin
#' @param var_random Name of the grouping variable for the random effects
#' (typically subject ID) in character.
#' @param random_effect_type Type of random effects to include for each level
#' of `var_random`:
#'   - `"both"` (default): correlated random intercept and random slope on the
#'     exposure, i.e. `(1 + exposure | ID)`
#'   - `"intercept"`: random intercept only, i.e. `(1 | ID)`
#'   - `"slope"`: random slope on the exposure only, i.e. `(0 + exposure | ID)`
#' @param prior_covariance Prior for the covariance matrices of the random
#' effects. See [rstanarm::decov()] and [rstanarm::stan_glmer()].
#' @param adapt_delta Target average acceptance probability for the NUTS
#' sampler. Default is 0.99, higher than the default of
#' [rstanarm::stan_glmer()] (0.95), as occasional divergent transitions were
#' observed with random slopes at 0.95. Lower values make sampling faster.
#' See [rstanarm::adapt_delta] for details.
#'
#' @details
#' The prior on the random effects covariance ([rstanarm::decov()]) is not
#' scaled to the units of the predictors. When a random slope is included,
#' an exposure variable with very large or small values (e.g. concentration in
#' ng/mL ranging up to thousands) can lead to poor sampling and biased
#' variance estimates. A warning is shown in such cases; consider rescaling
#' the exposure (e.g. ng/mL to ug/mL) so that its standard deviation is
#' roughly between 0.1 and 10.
#'
#' @return An object of class `ermod_lme`.
#'
#' @examples
#' \donttest{
#' data(d_sim_cqt)
#'
#' ermod_lme <- dev_ermod_lme(
#'   data = d_sim_cqt,
#'   var_resp = "DQTCF",
#'   var_exposure = "CONC_1000",
#'   var_cov = "TRT",
#'   var_random = "ID"
#' )
#'
#' ermod_lme
#'
#' # Population-level prediction (random effects set to zero)
#' sim_er_curve(
#'   ermod_lme,
#'   data_cov = data.frame(TRT = "Active"),
#'   num_exposures = 5,
#'   n_draws_sim = 500,
#'   output_type = "median_qi"
#' )
#'
#' # Prediction for new subjects, with random effects sampled from the
#' # estimated between-subject distribution
#' sim_er_curve(
#'   ermod_lme,
#'   data_cov = data.frame(ID = 1:3, TRT = "Active"),
#'   num_exposures = 5,
#'   n_draws_sim = 500,
#'   re_type = "new_subject",
#'   output_type = "median_qi"
#' )
#' }
#'
dev_ermod_lme <- function(
    data,
    var_resp,
    var_exposure,
    var_cov = NULL,
    var_random,
    random_effect_type = c("both", "intercept", "slope"),
    prior = rstanarm::default_prior_coef(stats::gaussian()),
    prior_intercept = rstanarm::default_prior_intercept(stats::gaussian()),
    prior_aux = rstanarm::exponential(autoscale = TRUE),
    prior_covariance = rstanarm::decov(),
    adapt_delta = 0.99,
    verbosity_level = 1,
    chains = 4,
    iter = 2000) {
  stopifnot(verbosity_level %in% c(0, 1, 2, 3))
  random_effect_type <- match.arg(random_effect_type)
  refresh <- dplyr::if_else(verbosity_level >= 3, iter %/% 4, 0)

  input_args <- capture_selected_args(
    c(
      "random_effect_type", "prior", "prior_intercept", "prior_aux",
      "prior_covariance", "adapt_delta", "chains", "iter"
    ),
    environment()
  )

  check_data_columns(
    data = data,
    var_exposure = var_exposure,
    var_resp = var_resp,
    var_cov = var_cov,
    var_random = var_random
  )

  if (random_effect_type != "intercept") {
    check_exposure_scale_lme(data[[var_exposure]], var_exposure)
  }

  var_full <- c(var_exposure, var_cov)

  formula_final <- build_formula_lme(
    var_resp, var_full, var_exposure, var_random, random_effect_type
  )

  # Need to construct call and then evaluate, so that the data can be found
  # when the model is refitted (e.g. `loo()` with `k_threshold`).
  # stan_glmer() with gaussian family is used instead of stan_lmer(), as the
  # latter evaluates `stan_glmer()` in the calling environment.
  call_stan_lmer <- rlang::call2(
    rstanarm::stan_glmer,
    formula = formula_final,
    family = stats::gaussian(),
    data = quote(data),
    prior = prior,
    prior_intercept = prior_intercept,
    prior_aux = prior_aux,
    prior_covariance = prior_covariance,
    QR = dplyr::if_else(length(var_full) > 1, TRUE, FALSE),
    adapt_delta = adapt_delta,
    refresh = refresh, chains = chains, iter = iter
  )
  mod <- eval(call_stan_lmer)

  new_ermod_lme(
    mod = mod,
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    var_cov = var_cov,
    var_random = var_random,
    random_effect_type = random_effect_type,
    input_args = input_args
  )
}


#' @export
#' @rdname dev_ermod_bin_exp_sel
#' @inheritParams dev_ermod_lme
#' @examples
#' \donttest{
#' data(d_sim_cqt)
#'
#' ermod_lme_exp_sel <- dev_ermod_lme_exp_sel(
#'   data = d_sim_cqt,
#'   var_resp = "DQTCF",
#'   var_exp_candidates = c("CONC_1000", "MCONC_1000"),
#'   var_random = "ID",
#'   random_effect_type = "intercept"
#' )
#'
#' ermod_lme_exp_sel
#' }
#'
dev_ermod_lme_exp_sel <- function(
    data,
    var_resp,
    var_exp_candidates,
    var_random,
    random_effect_type = c("both", "intercept", "slope"),
    prior = rstanarm::default_prior_coef(stats::gaussian()),
    prior_intercept = rstanarm::default_prior_intercept(stats::gaussian()),
    prior_aux = rstanarm::exponential(autoscale = TRUE),
    prior_covariance = rstanarm::decov(),
    adapt_delta = 0.99,
    verbosity_level = 1,
    chains = 4,
    iter = 2000) {
  random_effect_type <- match.arg(random_effect_type)

  fun_dev_ermod <-
    purrr::partial(
      dev_ermod_lme,
      var_random = var_random,
      random_effect_type = random_effect_type,
      prior = prior,
      prior_intercept = prior_intercept,
      prior_aux = prior_aux,
      prior_covariance = prior_covariance,
      adapt_delta = adapt_delta
    )

  l_out <-
    .dev_ermod_exp_sel(
      data = data,
      var_resp = var_resp,
      var_exp_candidates = var_exp_candidates,
      verbosity_level = verbosity_level,
      chains = chains,
      iter = iter,
      fun_dev_ermod = fun_dev_ermod
    )

  l_out$var_random <- var_random
  l_out$random_effect_type <- random_effect_type

  new_ermod_lme_exp_sel(l_out)
}

# Internal functions ----------------------------------------------------------

build_formula_lme <- function(
    var_resp, var_full, var_exposure, var_random, random_effect_type) {
  term_random <- switch(random_effect_type,
    both = paste0("(1 + ", var_exposure, " | ", var_random, ")"),
    intercept = paste0("(1 | ", var_random, ")"),
    slope = paste0("(0 + ", var_exposure, " | ", var_random, ")")
  )

  stats::formula(
    paste(
      var_resp, "~", paste(c(var_full, term_random), collapse = " + ")
    )
  )
}

check_exposure_scale_lme <- function(x_exposure, var_exposure) {
  sd_exposure <- stats::sd(x_exposure)
  if (is.na(sd_exposure) || sd_exposure == 0) {
    return(invisible())
  }
  if (sd_exposure > 10 || sd_exposure < 0.1) {
    warning(
      "The standard deviation of the exposure variable `", var_exposure,
      "` is ", signif(sd_exposure, 3), ". ",
      "The prior on the random effects covariance is not scaled to the ",
      "units of the exposure, and a random slope on a badly scaled exposure ",
      "can lead to poor sampling and biased variance estimates.\n",
      "Consider rescaling the exposure (e.g. ng/mL to ug/mL) so that ",
      "its standard deviation is roughly between 0.1 and 10.",
      call. = FALSE
    )
  }
  invisible()
}
