#' Class constructors for simulations from exposure-response models
#'
#' @name ersim-class
#' @aliases ersim
#' @aliases ersim_marg
#' @aliases ersim_med_qi
#' @aliases ersim_marg_med_qi
#' @noRd
#' @param simdata A data frame with the simulated data
#' @param ermod An exposure-response model
#' @param nrow_cov_data The number of rows in the covariate data
#'
new_ersim <- function(simdata, ermod, nrow_cov_data) {
  stopifnot(is.data.frame(simdata))
  stopifnot(inherits(ermod, "ermod"))

  ersim <- simdata
  origdata <- extract_data(ermod)

  # Add attributes
  attr(ersim, "var_exposure") <- ermod$var_exposure
  attr(ersim, "var_resp") <- ermod$var_resp
  attr(ersim, "var_cov") <- ermod$var_cov
  attr(ersim, "origdata") <- origdata
  attr(ersim, "nrow_cov_data") <- nrow_cov_data
  attr(ersim, "coef_exp_draws") <- ermod$coef_exp_draws
  attr(ersim, "ermod_class") <- class(ermod)
  attr(ersim, "endpoint_type") <- ermod$endpoint_type
  attr(ersim, "options_placebo_handling") <- ermod$options_placebo_handling

  # Add ersim class
  class(ersim) <- c("ersim", class(ersim))

  return(ersim)
}

#' @rdname ersim-class
#' @noRd
new_ersim_marg <- function(simdata, ermod, nrow_cov_data) {
  ersim <- new_ersim(simdata, ermod, nrow_cov_data)

  # Add ersim_marg class
  class(ersim) <- c("ersim_marg", class(ersim))

  return(ersim)
}

#' @rdname ersim-class
#' @noRd
new_ersim_med_qi <- function(simdata, ermod, nrow_cov_data, qi_width) {
  ersim_med_qi <- new_ersim(simdata, ermod, nrow_cov_data)

  # Add attributes
  attr(ersim_med_qi, "qi_width") <- qi_width

  # Replace ersim class with ersim_med_qi
  class(ersim_med_qi) <-
    c(
      "ersim_med_qi",
      setdiff(class(ersim_med_qi), "ersim")
    )

  return(ersim_med_qi)
}

#' @rdname ersim-class
#' @noRd
new_ersim_marg_med_qi <- function(simdata, ermod, nrow_cov_data, qi_width) {
  ersim_marg_med_qi <-
    new_ersim_med_qi(simdata, ermod, nrow_cov_data, qi_width)

  # Add ersim_marg class
  class(ersim_marg_med_qi) <-
    c("ersim_marg_med_qi", class(ersim_marg_med_qi))

  return(ersim_marg_med_qi)
}
