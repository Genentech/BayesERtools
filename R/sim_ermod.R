#' Simulate from ER model
#'
#' @export
#' @name sim_er
#' @param ermod An object of class \code{ermod}
#' @param newdata New data to use for simulation.
#' Default is NULL (use the data in the model object).
#' @param n_draws_sim Number of draws for simulation. If NULL (default),
#' all draws in the model object are used.
#' @param seed_sample_draws Seed for sampling draws. Default is NULL.
#' @param output_type Type of output. "draws" returns the raw draws from the
#' simulation, and "median_qi" returns the median and quantile interval.
#' @param qi_width Width of the quantile interval. Default is 0.95. Only
#' used when `output_type = "median_qi"`.
#' @param .nrow_cov_data Number of rows in the covariate data,
#' used for internal purposes. Users should not set this argument.
#'
#' @return `ersim` object, which is a tibble with the simulated responses
#' with some additional information in object attributes.
#' It has three types of predictions - `.linpred`, `.epred`, `.prediction`.
#' `.linpred` and `.epred` are similar in a way that they both represent
#' "expected response", i.e. without residual variability. They are the same
#' for models with continuous endpoits (Emax model). For models with binary
#' endpoints, `.linpred` is the linear predictor (i.e. on the logit scale) and
#' `.epred` is on the probability scale. `.prediction` is the predicted
#' response with residual variability (or in case of binary endpoint,
#' the predicted yes (1) or no (0) for event occurrence).
#' See [tidybayes::add_epred_draws()] for more details.
#'
#' In case of `output_type = "median_qi"`, it returns `ersim_med_qi` object.
#'
#' @seealso [calc_ersim_med_qi()] for calculating median and quantile interval
#' from `ersim` object (generated with `output_type = "draws"`).
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
#' ersim <- sim_er(
#'   ermod_bin,
#'   n_draws_sim = 500, # This is set to make the example run faster
#'   output_type = "draws"
#' )
#'
#' ersim_med_qi <- sim_er(
#'   ermod_bin,
#'   n_draws_sim = 500, # This is set to make the example run faster
#'   output_type = "median_qi"
#' )
#'
#' ersim
#' ersim_med_qi
#' }
#'
sim_er <- function(
    ermod,
    newdata = NULL,
    n_draws_sim = NULL,
    seed_sample_draws = NULL,
    output_type = c("draws", "median_qi"),
    qi_width = 0.95,
    .nrow_cov_data = NULL) {
  stopifnot(inherits(ermod, "ermod"))
  output_type <- match.arg(output_type)

  if (is.null(seed_sample_draws)) {
    seed_sample_draws <- sample.int(.Machine$integer.max, 1)
  }

  if (is.null(newdata)) newdata <- extract_data(ermod)
  check_data_columns(newdata,
    var_exposure = ermod$var_exposure,
    var_cov = ermod$var_cov
  )
  if (is.null(.nrow_cov_data)) {
    .nrow_cov_data <- nrow(newdata)
  }

  mod <- extract_mod(ermod)
  n_draws_sim <- chech_ndraws(mod, n_draws_sim)

  simdata_epred <-
    tidybayes::add_epred_draws(newdata, mod,
      ndraws = n_draws_sim,
      seed = seed_sample_draws
    )
  simdata_linpred <-
    tidybayes::add_linpred_draws(newdata, mod,
      ndraws = n_draws_sim,
      seed = seed_sample_draws
    ) |>
    dplyr::ungroup() |>
    dplyr::select(.draw, .row, .linpred)
  simdata_predicted <-
    tidybayes::add_predicted_draws(newdata, mod,
      ndraws = n_draws_sim,
      seed = seed_sample_draws
    ) |>
    dplyr::ungroup() |>
    dplyr::select(.draw, .row, .prediction)

  simdata <-
    simdata_epred |>
    dplyr::left_join(simdata_linpred, by = dplyr::join_by(.draw, .row)) |>
    dplyr::left_join(simdata_predicted, by = dplyr::join_by(.draw, .row))

  if (output_type == "draws") {
    return(new_ersim(
      simdata,
      ermod,
      nrow_cov_data = .nrow_cov_data
    ))
  }

  simdata_med_qi <-
    simdata |>
    tidybayes::median_qi(.width = qi_width) |>
    dplyr::as_tibble()

  if (inherits(ermod, "ermod_bin")) {
    simdata_med_qi <-
      simdata_med_qi |>
      dplyr::select(-dplyr::starts_with(".prediction"))
  }

  return(new_ersim_med_qi(simdata_med_qi, ermod,
    nrow_cov_data = .nrow_cov_data,
    qi_width = qi_width
  ))
}


#' Simulate from ER model at specified exposure values
#'
#' @export
#' @name sim_er_new_exp
#' @inheritParams sim_er
#' @inherit sim_er return
#' @param exposure_to_sim_vec Vector of exposure values to simulate.
#' @param data_cov Data frame containing covariates to use for simulation,
#' see details below.
#'
#' @details
#' Simulation dataset will be all combinations of covariates in `data_cov`
#' and exposure values in `exposure_to_sim_vec`,
#' so the run time can become very long if `data_cov` has many rows.
#'
#' `data_cov` has to be supplied if `ermod` is a model with covariates.
#' It is recommended that `data_cov` contains subject identifiers such as
#' `ID` for post-processing.
#'
#' Exposure values in `data_cov` will be ignored.
#'
#' @seealso [calc_ersim_med_qi()] for calculating median and quantile interval
#' from `ersim` object (generated with `output_type = "draws"`).
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
#' ersim_new_exp_med_qi <- sim_er_new_exp(
#'   ermod_bin,
#'   exposure_to_sim_vec = seq(2, 6, by = 0.2),
#'   data_cov = dplyr::tibble(BHBA1C_5 = 4:10),
#'   n_draws_sim = 500, # This is set to make the example run faster
#'   output_type = "median_qi"
#' )
#'
#' ersim_new_exp_med_qi
#' }
#'
sim_er_new_exp <- function(
    ermod,
    exposure_to_sim_vec = NULL,
    data_cov = NULL,
    n_draws_sim = NULL,
    seed_sample_draws = NULL,
    output_type = c("draws", "median_qi"),
    qi_width = 0.95) {
  stopifnot(inherits(ermod, "ermod"))
  output_type <- match.arg(output_type)

  var_exposure_sym <- rlang::sym(extract_var_exposure(ermod))

  newdata <- dplyr::tibble(!!var_exposure_sym := exposure_to_sim_vec)

  var_cov <- extract_var_cov(ermod)

  # Handle data_cov
  ## No need to use data_cov if there are no covariates in the model
  if (is.null(var_cov)) {
    .nrow_cov_data <- 0
  }
  if (!is.null(var_cov) && is.null(data_cov)) {
    stop("data_cov must be supplied for models with covariates.")
  }
  ## This is the only situation where data_cov is used
  if (!is.null(var_cov) && !is.null(data_cov)) {
    .nrow_cov_data <- nrow(data_cov)

    # remove exposure column from data_cov, if it exists
    if (extract_var_exposure(ermod) %in% colnames(data_cov)) {
      data_cov <- data_cov |> dplyr::select(-!!var_exposure_sym)
    }
    newdata <- tidyr::expand_grid(newdata, data_cov)
  }

  sim_er(
    ermod = ermod,
    newdata = newdata,
    n_draws_sim = n_draws_sim,
    seed_sample_draws = seed_sample_draws,
    output_type = output_type,
    qi_width = qi_width,
    .nrow_cov_data = .nrow_cov_data
  )
}


#' @export
#' @rdname sim_er_new_exp
#' @param exposure_range Range of exposure values to simulate. If NULL
#' (default), it is set to the range of the exposure variable in the original
#' data for model development.
#' @param num_exposures Number of exposure values to simulate.
#'
#' @details
#' [sim_er_curve()] is a wrapper function for [sim_er_new_exp()]
#' that use a range of exposure values to simulate the expected responses.
#' Particularly useful for plotting the exposure-response curve.
#'
sim_er_curve <- function(
    ermod,
    exposure_range = NULL,
    num_exposures = 51,
    data_cov = NULL,
    n_draws_sim = NULL,
    seed_sample_draws = NULL,
    output_type = c("draws", "median_qi"),
    qi_width = 0.95) {

  if (is.null(exposure_range)) {
    exposure_range <- .default_exposure_range(ermod)
  } else {
    stopifnot(length(exposure_range) == 2)
  }

  exposure_to_sim_vec <-
    seq(exposure_range[1], exposure_range[2], length.out = num_exposures)

  sim_er_new_exp(
    ermod = ermod,
    exposure_to_sim_vec = exposure_to_sim_vec,
    data_cov = data_cov,
    n_draws_sim = n_draws_sim,
    seed_sample_draws = seed_sample_draws,
    output_type = output_type,
    qi_width = qi_width
  )
}

.default_exposure_range <- function(ermod) {
  exp <- extract_var_exposure(ermod)
  dat <- extract_data(ermod, method = "processed")
  range(dat[[exp]])
}

#' Calculate marginal expected response for specified exposure values
#'
#' Responses at specified exposure values are calculated for `n_subj_sim`
#' subjects with different covariates (sampled from `newdata`),
#' and the predicted responses are "marginalized" (averaged),
#' resulting in marginal expected response on the
#' population of interest.
#'
#' @export
#' @name sim_er_new_exp_marg
#' @inheritParams sim_er_new_exp
#' @param data_cov Data frame containing covariates to use for simulation.
#' Different from [sim_er_new_exp()], `data_cov` can be large as long as
#' `n_subj_sim` is set to a reasonable number. Default is set to
#' `extract_data(ermod)` which is the full data used to fit the model.
#' @param n_subj_sim Maximum number of subjects to simulate. Default of 100
#' should be sufficient in many cases, as it's only used for marginal
#' response calculation.
#' Set to NULL to use all subjects in `data_cov` without resampling;
#' in this case, be mindful of the computation time.
#' @param n_draws_sim Number of draws for simulation. Default is set to 500
#' to reduce computation time for marginal response calculation.
#'
#' @return `ersim_marg` object, which is a tibble with the simulated marginal
#' expected response with some additional information in object attributes.
#' In case of `output_type = "median_qi"`, it returns `ersim_marg_med_qi`
#'  object.
#'
#' @details
#' [sim_er_new_exp_marg()] returns a tibble with the marginal expected
#' response for each exposure value in `exposure_to_sim_vec`.
#'
#' @seealso [calc_ersim_med_qi()] for calculating median and quantile interval
#' from `ersim_marg` object (generated with `output_type = "draws"`).
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
#' ersim_new_exp_marg_med_qi <- sim_er_new_exp_marg(
#'   ermod_bin,
#'   exposure_to_sim_vec = seq(2, 6, by = 0.2),
#'   data_cov = dplyr::tibble(BHBA1C_5 = 4:10),
#'   n_subj_sim = NULL,
#'   n_draws_sim = 500, # This is set to make the example run faster
#'   output_type = "median_qi"
#' )
#'
#' ersim_new_exp_marg_med_qi
#' }
#'
sim_er_new_exp_marg <- function(
    ermod,
    exposure_to_sim_vec = NULL,
    data_cov = extract_data(ermod),
    n_subj_sim = 100,
    n_draws_sim = 500,
    seed_sample_draws = NULL,
    output_type = c("draws", "median_qi"),
    qi_width = 0.95) {
  stopifnot(inherits(ermod, "ermod"))
  stopifnot(!is.null(data_cov))
  output_type <- match.arg(output_type)

  var_exposure_sym <- rlang::sym(extract_var_exposure(ermod))

  # remove exposure column from data_cov, if it exists
  if (extract_var_exposure(ermod) %in% colnames(data_cov)) {
    data_cov <- data_cov |> dplyr::select(-!!var_exposure_sym)
  }

  if (is.null(n_subj_sim)) {
    data_to_expand <- data_cov
  } else {
    if (n_subj_sim > nrow(data_cov)) {
      warning(paste0(
        "n_subj_sim (", n_subj_sim, ") is greater than the number of ",
        "subjects in data_cov (", nrow(data_cov), "). ",
        "Consider setting `n_subj_sim = NULL` to use all subjects in data_cov."
      ))
    }
    data_to_expand <-
      data_cov |>
      dplyr::slice_sample(n = n_subj_sim, replace = TRUE)
  }

  newdata <-
    dplyr::tibble(!!var_exposure_sym := exposure_to_sim_vec) |>
    dplyr::mutate(.id_exposure = dplyr::row_number()) |>
    tidyr::expand_grid(data_to_expand)

  ersim_raw <-
    sim_er(
      ermod = ermod,
      newdata = newdata,
      n_draws_sim = n_draws_sim,
      seed_sample_draws = seed_sample_draws,
      # Need to marginalize first before taking median and qi
      output_type = "draws"
    )

  # Calculate marginal expected response for each exposure value and draw
  simdata <-
    ersim_raw |>
    dplyr::ungroup() |>
    dplyr::summarize(
      .epred = mean(.epred),
      .linpred = mean(.linpred),
      .prediction = mean(.prediction),
      .by = c(.id_exposure, !!var_exposure_sym, .draw)
    )

  # Marginalizing is done, so set .nrow_cov_data to 0
  .nrow_cov_data <- 0

  if (output_type == "draws") {
    return(new_ersim_marg(simdata, ermod,
      nrow_cov_data = .nrow_cov_data
    ))
  }

  # Group simdata with everything except .draw and response
  simdata_med_qi <-
    simdata |>
    dplyr::group_by(.id_exposure, !!var_exposure_sym) |>
    tidybayes::median_qi(.width = qi_width) |>
    dplyr::as_tibble()

  return(new_ersim_marg_med_qi(simdata_med_qi, ermod,
    nrow_cov_data = .nrow_cov_data,
    qi_width = qi_width
  ))
}


#' @export
#' @rdname sim_er_new_exp_marg
#' @param exposure_range Range of exposure values to simulate. If NULL
#' (default), it is set to the range of the exposure variable in the original
#' data for model development.
#' @param num_exposures Number of exposure values to simulate.
#'
#' @details
#' [sim_er_curve_marg()] is a wrapper function for [sim_er_new_exp_marg()]
#' that use a range of exposure values to simulate the marginal expected
#' responses. Particularly useful for plotting the exposure-response curve.
#'
sim_er_curve_marg <- function(
    ermod,
    exposure_range = NULL,
    num_exposures = 51,
    data_cov = extract_data(ermod),
    n_subj_sim = 100,
    n_draws_sim = 500,
    seed_sample_draws = NULL,
    output_type = c("draws", "median_qi"),
    qi_width = 0.95) {
  if (is.null(exposure_range)) {
    exposure_range <- .default_exposure_range(ermod)
  } else {
    stopifnot(length(exposure_range) == 2)
  }

  exposure_to_sim_vec <-
    seq(exposure_range[1], exposure_range[2], length.out = num_exposures)

  sim_er_new_exp_marg(
    ermod = ermod,
    exposure_to_sim_vec = exposure_to_sim_vec,
    data_cov = data_cov,
    n_subj_sim = n_subj_sim,
    n_draws_sim = n_draws_sim,
    seed_sample_draws = seed_sample_draws,
    output_type = output_type,
    qi_width = qi_width
  )
}


# Check and set the number of draws for simulation
chech_ndraws <- function(mod, n_draws_sim) {
  ndraws_mod <- nrow(as.matrix(mod$stanfit))
  if (is.null(n_draws_sim)) {
    n_draws_sim <- ndraws_mod
  } else {
    if (n_draws_sim > ndraws_mod) {
      warning(paste0(
        "n_draws_sim (", n_draws_sim, ") is greater than the number of ",
        "MCMC draws in the model (", ndraws_mod, "). ",
        "Setting n_draws_sim to ", ndraws_mod, "."
      ))
      n_draws_sim <- ndraws_mod
    }
  }
  return(n_draws_sim)
}
