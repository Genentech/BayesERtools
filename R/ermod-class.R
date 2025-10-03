# ermod_lin -------------------------------------------------------------------

#' Class constructors for the binary exposure-response models
#'
#' @name ermod_lin-class
#' @noRd
#' @param mod A stanreg object
#' @param data Data frame used to fit the model
#' @param var_resp Name of the response variable
#' @param var_exposure Name of the exposure variable
#' @param var_cov Name of the covariate variable
#' @param input_args Captured inputs
#' @param options_placebo_handling List specifying how placebo groups are handled
new_ermod_lin <- function(
    mod,
    data,
    var_resp = character(),
    var_exposure = character(),
    var_cov = NULL,
    input_args = list(),
    options_placebo_handling = list()) {

  coef_exp_draws <- .get_coef_exp_draws(mod, var_exposure)
  options_placebo_handling <- .apply_placebo_defaults(options_placebo_handling)

  check_input_new_ermod(
    mod = mod,
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    var_cov = var_cov,
    input_args = input_args,
    options_placebo_handling = options_placebo_handling,
    coef_exp_draws = coef_exp_draws,
    basemodclass = "stanreg"
  )

  structure(
    list(
      mod = mod,
      data = data,
      var_resp = var_resp,
      var_exposure = var_exposure,
      var_cov = var_cov,
      input_args = input_args,
      options_placebo_handling = options_placebo_handling,
      coef_exp_draws = coef_exp_draws,
      endpoint_type = "continuous"
    ),
    class = c("ermod_lin", "ermod")
  )
}


#' @rdname ermod_lin-class
#' @noRd
new_ermod_lin_exp_sel <- function(l_ermod_exp_sel) {
  check_l_ermod_exp_sel(l_ermod_exp_sel)

  coef_exp_draws <-
    .get_coef_exp_draws(l_ermod_exp_sel$mod, l_ermod_exp_sel$var_exposure)

  l_ermod_exp_sel$coef_exp_draws <- coef_exp_draws
  l_ermod_exp_sel$endpoint_type <- "continuous"

  structure(
    l_ermod_exp_sel,
    class = c("ermod_lin_exp_sel", "ermod_exp_sel", "ermod_lin", "ermod")
  )
}


#' @rdname ermod_lin-class
#' @noRd
new_ermod_lin_cov_sel <- function(
    mod,
    data,
    var_resp = character(),
    var_exposure = character(),
    var_cov_candidates = character(),
    var_cov = NULL,
    var_selected = character(),
    options_placebo_handling = list(),
    cv_method = c("LOO", "kfold"),
    cvvs = NULL,
    rk = NULL,
    input_args = list()) {

  cv_method <- match.arg(cv_method)
  coef_exp_draws <- .get_coef_exp_draws(mod, var_exposure)

  check_input_new_ermod(
    mod = mod,
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    var_cov_candidates = var_cov_candidates,
    var_cov = var_cov,
    var_selected = var_selected,
    options_placebo_handling = options_placebo_handling,
    cvvs = cvvs,
    rk = rk,
    input_args = input_args,
    coef_exp_draws = coef_exp_draws,
    basemodclass = "stanreg"
  )

  structure(
    list(
      mod = mod,
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
      input_args = input_args,
      coef_exp_draws = coef_exp_draws,
      endpoint_type = "continuous"
    ),
    class = c("ermod_lin_cov_sel", "ermod_cov_sel", "ermod_lin", "ermod")
  )
}

# ermod_bin -------------------------------------------------------------------

#' Class constructors for the binary exposure-response models
#'
#' @name ermod_bin-class
#' @noRd
#' @param mod A stanreg object
#' @param data Data frame used to fit the model
#' @param var_resp Name of the response variable
#' @param var_exposure Name of the exposure variable
#' @param var_cov Name of the covariate variable
#' @param input_args Captured inputs
#' @param options_placebo_handling List specifying how placebo groups are handled
new_ermod_bin <- function(
  mod,
  data,
  var_resp = character(),
  var_exposure = character(),
  var_cov = NULL,
  input_args = list(),
  options_placebo_handling = list()
) {

  coef_exp_draws <- .get_coef_exp_draws(mod, var_exposure)
  options_placebo_handling <- .apply_placebo_defaults(options_placebo_handling)

  check_input_new_ermod(
    mod = mod,
    data = data,
    var_resp = var_resp,
    input_args = input_args,
    options_placebo_handling = options_placebo_handling,
    coef_exp_draws = coef_exp_draws,
    basemodclass = "stanreg"
  )

  structure(
    list(
      mod = mod,
      data = data,
      var_resp = var_resp,
      var_exposure = var_exposure,
      var_cov = var_cov,
      input_args = input_args,
      options_placebo_handling = options_placebo_handling,
      coef_exp_draws = coef_exp_draws,
      endpoint_type = "binary"
    ),
    class = c("ermod_bin", "ermod")
  )
}


#' @rdname ermod_bin-class
#' @noRd
new_ermod_bin_exp_sel <- function(l_ermod_exp_sel) {
  check_l_ermod_exp_sel(l_ermod_exp_sel)

  coef_exp_draws <-
    .get_coef_exp_draws(l_ermod_exp_sel$mod, l_ermod_exp_sel$var_exposure)

  l_ermod_exp_sel$coef_exp_draws <- coef_exp_draws
  l_ermod_exp_sel$endpoint_type <- "binary"

  structure(
    l_ermod_exp_sel,
    class = c("ermod_bin_exp_sel", "ermod_exp_sel", "ermod_bin", "ermod")
  )
}


#' @rdname ermod_bin-class
#' @noRd
new_ermod_bin_cov_sel <- function(
    mod,
    data,
    var_resp = character(),
    var_exposure = character(),
    var_cov_candidates = character(),
    var_cov = NULL,
    var_selected = character(),
    options_placebo_handling = list(),
    cv_method = c("LOO", "kfold"),
    cvvs = NULL,
    rk = NULL,
    input_args = list()) {

  cv_method <- match.arg(cv_method)
  coef_exp_draws <- .get_coef_exp_draws(mod, var_exposure)

  check_input_new_ermod(
    mod = mod,
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    var_cov_candidates = var_cov_candidates,
    var_cov = var_cov,
    var_selected = var_selected,
    options_placebo_handling = options_placebo_handling,
    cvvs = cvvs,
    rk = rk,
    input_args = input_args,
    coef_exp_draws = coef_exp_draws,
    basemodclass = "stanreg"
  )

  structure(
    list(
      mod = mod,
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
      input_args = input_args,
      coef_exp_draws = coef_exp_draws,
      endpoint_type = "binary"
    ),
    class = c("ermod_bin_cov_sel", "ermod_cov_sel", "ermod_bin", "ermod")
  )
}

# ermod_emax ------------------------------------------------------------------

#' Class constructors for the Emax models
#'
#' @name ermod_emax-class
#' @noRd
#' @param mod A stanreg object
#' @param data Data frame used to fit the model
#' @param var_resp Name of the response variable
#' @param var_exposure Name of the exposure variable
#' @param options_placebo_handling List specifying how placebo groups are handled
#' @param l_var_cov List of the covariate variables
new_ermod_emax <- function(
    mod,
    data,
    var_resp = character(),
    var_exposure = character(),
    options_placebo_handling = list(),
    l_var_cov = NULL,
    input_args = list()) {

  check_input_new_ermod(
    mod = mod,
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    options_placebo_handling = options_placebo_handling,
    l_var_cov = l_var_cov,
    basemodclass = "stanemax"
  )

  structure(
    list(
      mod = mod,
      data = data,
      var_resp = var_resp,
      var_exposure = var_exposure,
      options_placebo_handling = options_placebo_handling,
      l_var_cov = l_var_cov,
      input_args = input_args,
      endpoint_type = "continuous"
    ),
    class = c("ermod_emax", "ermod")
  )
}


#' @rdname ermod_emax-class
#' @noRd
new_ermod_emax_exp_sel <- function(l_ermod_exp_sel) {
  check_l_ermod_exp_sel(l_ermod_exp_sel, basemodclass = "stanemax")

  l_ermod_exp_sel$endpoint_type <- "continuous"

  structure(
    l_ermod_exp_sel,
    class = c("ermod_emax_exp_sel", "ermod_exp_sel", "ermod_emax", "ermod")
  )
}

# ermod_bin_emax --------------------------------------------------------------

#' Class constructors for the binary Emax models
#'
#' @name ermod_bin_emax-class
#' @noRd
#' @param mod A stanreg object
#' @param data Data frame used to fit the model
#' @param var_resp Name of the response variable
#' @param var_exposure Name of the exposure variable
#' @param options_placebo_handling List specifying how placebo groups are handled
#' @param l_var_cov List of the covariate variables
new_ermod_bin_emax <- function(
    mod,
    data,
    var_resp = character(),
    var_exposure = character(),
    options_placebo_handling = list(),
    l_var_cov = NULL,
    input_args = list()) {

  check_input_new_ermod(
    mod = mod,
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    options_placebo_handling = options_placebo_handling,
    l_var_cov = l_var_cov,
    basemodclass = "stanemaxbin"
  )

  structure(
    list(
      mod = mod,
      data = data,
      var_resp = var_resp,
      var_exposure = var_exposure,
      options_placebo_handling = options_placebo_handling,
      l_var_cov = l_var_cov,
      input_args = input_args,
      endpoint_type = "binary"
    ),
    class = c("ermod_bin_emax", "ermod")
  )
}


#' @rdname ermod_bin_emax-class
#' @noRd
new_ermod_bin_emax_exp_sel <- function(l_ermod_exp_sel) {
  check_l_ermod_exp_sel(l_ermod_exp_sel, basemodclass = "stanemaxbin")

  l_ermod_exp_sel$endpoint_type <- "binary"

  structure(
    l_ermod_exp_sel,
    class = c(
      "ermod_bin_emax_exp_sel", "ermod_exp_sel",
      "ermod_bin_emax", "ermod"
    )
  )
}

# utils -----------------------------------------------------------------------

# TODO: add checks for options_placebo_handling()
check_input_new_ermod <- function(
    mod,
    data,
    var_resp = character(),
    var_exposure = character(),
    var_exp_candidates = character(),
    var_cov_candidates = character(),
    var_cov = character(),
    l_var_cov = NULL,
    var_selected = character(),
    l_mod_exposures = NULL,
    cvvs = NULL,
    rk = NULL,
    input_args = list(),
    options_placebo_handling = list(),
    coef_exp_draws = NULL,
    basemodclass = "stanreg") {

  stopifnot(inherits(mod, basemodclass))
  stopifnot(is.data.frame(data))
  stopifnot(is.character(var_resp))
  stopifnot(is.character(var_exposure))
  stopifnot(is.null(var_exp_candidates) || is.character(var_exp_candidates))
  stopifnot(is.null(var_cov) || is.character(var_cov))
  stopifnot(is.null(l_var_cov) || is.list(l_var_cov))
  stopifnot(is.character(var_cov_candidates))
  stopifnot(is.character(var_selected))
  stopifnot(is.null(cvvs) || inherits(cvvs, "vsel"))
  stopifnot(is.null(rk) || inherits(rk, "ranking"))
  stopifnot(is.list(input_args))
  stopifnot(is.null(coef_exp_draws) || inherits(coef_exp_draws, "numeric"))
  stopifnot(is.null(l_mod_exposures) || inherits(l_mod_exposures, "list"))
}


check_l_ermod_exp_sel <- function(l_ermod_exp_sel, basemodclass = "stanreg") {
  ll <- l_ermod_exp_sel

  check_input_new_ermod(
    mod = ll$mod,
    data = ll$data,
    var_resp = ll$var_resp,
    var_exp_candidates = ll$var_exp_candidates,
    var_exposure = ll$var_exposure,
    l_mod_exposures = ll$l_mod_exposures,
    basemodclass = basemodclass
  )
}

.get_coef_exp_draws <- function(mod, var_exposure) {
  posterior::as_draws_df(mod)[[var_exposure]]
}

.apply_placebo_defaults <- function(options) {
  placebo_defaults <- list(
      include_placebo = FALSE,
      method = "zero_exposure_as_placebo", # alternatives: "var_placebo", "none"
      var_placebo = NULL # if specified must be a string referring to a logical vector
    )
  if (is.null(options)) return(placebo_defaults)
  utils::modifyList(placebo_defaults, options)
}
