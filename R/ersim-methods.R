#' S3 methods for the classes `ersim_*` and `ersim_med_qi_*`
#' @name ersim_method
#' @param x An object of the classes `ersim_*` or `ersim_med_qi_*`
#' @param ... Additional arguments passed to functions
#'
#' @return No return value, called for print or plot side effects
NULL

#' @export
#' @rdname ersim_method
#' @inheritParams plot_er
plot.ersim <- function(x, show_orig_data = FALSE, ...) {
  plot_er(x, show_orig_data = show_orig_data)
}

#' @export
#' @rdname ersim_method
plot.ersim_med_qi <- function(x, show_orig_data = FALSE, ...) {
  plot_er(x, show_orig_data = show_orig_data)
}


# Extract elements ------------------------------------------------------------

#' Extract elements from objects of the classes `ersim_*` and `ersim_med_qi_*``
#'
#' @export
#' @name extract_ersim
#' @keywords internal
#' @inherit extract_method return
#' @param x An object of class \code{ersim_*}
extract_data.ersim <- function(x, ...) attr(x, "origdata")

#' @export
#' @rdname extract_ersim
extract_data.ersim_med_qi <- function(x, ...) attr(x, "origdata")

#' @export
#' @rdname extract_ersim
extract_var_resp.ersim <- function(x) attr(x, "var_resp")

#' @export
#' @rdname extract_ersim
extract_var_resp.ersim_med_qi <- function(x) attr(x, "var_resp")

#' @export
#' @rdname extract_ersim
extract_var_exposure.ersim <- function(x) attr(x, "var_exposure")

#' @export
#' @rdname extract_ersim
extract_var_exposure.ersim_med_qi <- function(x) attr(x, "var_exposure")

#' @export
#' @rdname extract_ersim
extract_var_cov.ersim <- function(x) attr(x, "var_cov")

#' @export
#' @rdname extract_ersim
extract_var_cov.ersim_med_qi <- function(x) attr(x, "var_cov")


#' Calculate median and quantile intervals from ersim object
#'
#' This is useful when you performed simulation with output_type = "draws" and
#' want to calculate median and quantile intervals without re-simulating.
#'
#' @export
#' @param x An object of class `ersim` or `ersim_marg`
#' @param qi_width Width of the quantile interval
#' @return An object of class `ersim_med_qi` or `ersim_marg_med_qi`
#'
calc_ersim_med_qi <- function(x, qi_width = 0.95) {
  var_exposure_sym <- rlang::sym(extract_var_exposure(x))
  x_grouped <- x

  if (inherits(x, "ersim_marg")) {
    x_grouped <- dplyr::group_by(x, .id_exposure, !!var_exposure_sym)
  }

  simdata_med_qi <-
    x_grouped |>
    tidybayes::median_qi(.width = qi_width) |>
    dplyr::as_tibble()

  # Create dummy ermod to feed to ersim class constructor
  ermod <-
    structure(
      list(
        data = attr(x, "origdata"),
        var_resp = attr(x, "var_resp"),
        var_exposure = attr(x, "var_exposure"),
        var_cov = attr(x, "var_cov"),
        endpoint_type = attr(x, "endpoint_type")
      ),
      class = c("ermod")
    )
  if (inherits(x, "ersim_marg")) {
    return(new_ersim_marg_med_qi(simdata_med_qi, ermod,
      nrow_cov_data = attr(x, "nrow_cov_data"),
      qi_width = qi_width
    ))
  } else if (inherits(x, "ersim")) {
    return(new_ersim_med_qi(simdata_med_qi, ermod,
      nrow_cov_data = attr(x, "nrow_cov_data"),
      qi_width = qi_width
    ))
  }
}
