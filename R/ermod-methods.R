# `ermod_bin*` S3 methods for print, plot, ... --------------------------------
## Regular `ermod` class ------------------------------------------------------

#' S3 methods for the classes \code{ermod_*}
#' @name ermod_method
#' @param x An object of class \code{ermod_*}
#' @param object An object of class \code{ermod_*}
#' @param digits Number of digits to print
#' @param ... Additional arguments passed to functions
#' @return
#' - `print()` and `plot()`: No return value, called for side effects
#' - `coef()`: Coefficients of the model
#' - `summary()`: Summary of the model
NULL


get_mod_type_name <- function(mod) {
  if (inherits(mod, "ermod_bin")) {
    return("Binary ER model")
  } else if (inherits(mod, "ermod_lin")) {
    return("Linear ER model")
  } else if (inherits(mod, "ermod_emax")) {
    return("Emax model")
  } else if (inherits(mod, "ermod_bin_emax")) {
    return("Binary Emax model")
  } else {
    stop("Unknown model type")
  }
}

#' @export
#' @rdname ermod_method
print.ermod <- function(x, digits = 2, ...) {
  mod_type_name <- get_mod_type_name(x)

  cli::cli({
    cli::cli_h1(mod_type_name)
    cli::cli_alert_info(paste(
      "Use `plot_er()` to visualize ER curve"
    ))
    cli::cli_h2("Developed model")
    print(x$mod, digits = digits, ...) |>
      utils::capture.output() |>
      cli::cli_code()
  })
}

#' @export
#' @rdname ermod_method
#' @inheritParams plot_er
plot.ermod_bin <- function(x, show_orig_data = FALSE, ...) {
  plot_er(x, show_orig_data = show_orig_data)
}

#' @export
#' @rdname ermod_method
coef.ermod <- function(object, ...) {
  if (!inherits(object, c("ermod_bin", "ermod_lin"))) {
    stop("coef() only supported for linear models")
  }

  stats::coef(object$mod, ...)
}

#' @export
#' @rdname ermod_method
summary.ermod <- function(object, ...) {
  if (!inherits(object, c("ermod_bin", "ermod_lin"))) {
    stop("summary() only supported for linear models")
  }

  summary(object$mod, ...)
}

## ermod_exp_sel class ----------------------------------------------------

#' S3 methods for the classes `ermod_exp_sel`
#'
#' @export
#' @name ermod_exp_sel_method
#' @inheritParams ermod_method
#' @param x An object of class `ermod_bin_exp_sel`
#' @return No return value, called for print or plot side effects
print.ermod_exp_sel <- function(x, digits = 2, ...) {
  mod_type_name <- get_mod_type_name(x)

  cli::cli({
    cli::cli_h1(paste(mod_type_name, "& exposure metric selection"))
    cli::cli_alert_info(paste(
      "Use `plot_er_exp_sel()` for ER curve of all exposure metrics"
    ))
    cli::cli_alert_info(paste(
      "Use `plot_er()` with `show_orig_data = TRUE` for ER curve of the",
      "selected exposure metric"
    ))

    if (length(x$var_exp_candidates) > 1) {
      cli::cli_h2("Exposure metrics comparison")
      print(x$loo_comp_exposures, digits = digits, ...) |>
        utils::capture.output() |>
        cli::cli_code()
    } else {
      cli::cli_alert_info(paste0(
        "Only `",
        x$var_exp_candidates,
        "` was provided, no selection done"
      ))
    }

    cli::cli_h2("Selected model")
    print(x$mod, digits = digits, ...) |>
      utils::capture.output() |>
      cli::cli_code()
  })
}

#' @export
#' @rdname ermod_exp_sel_method
plot.ermod_exp_sel <- function(x, ...) {
  plot_er_exp_sel(x, ...)
}

## ermod_cov_sel class ----------------------------------------------------

#' S3 methods for the classes `ermod_bin_cov_sel`
#' @export
#' @name ermod_cov_sel_method
#' @inheritParams ermod_method
#' @param x An object of class `ermod_bin_cov_sel`
#' @return No return value, called for print or plot side effects
print.ermod_cov_sel <- function(x, digits = 2, ...) {
  mod_type_name <- get_mod_type_name(x)

  cli::cli({
    cli::cli_h1(paste(mod_type_name, "& covariate selection"))

    cli::cli_alert_info(paste(
      "Use `plot_submod_performance()` to see variable",
      "selection performance\n"
    ))
    if (!is.null(x$rk$foldwise)) {
      cli::cli_alert_info(paste(
        "Use `plot_var_ranking()` to see variable ranking"
      ))
    }
    cli::cli_alert_info(paste(
      "Use `plot_er()` with `marginal = TRUE` to visualize",
      "marginal ER curve"
    ))

    cli::cli_h2("Selected model")
    print(x$mod, digits = digits, ...) |>
      utils::capture.output() |>
      cli::cli_code()
  })
}

#' @export
#' @rdname ermod_cov_sel_method
plot.ermod_cov_sel <- function(x, ...) {
  plot_submod_performance(x, ...)
}


# Extract elements ------------------------------------------------------------

#' Extract elements from an object of class \code{ermod_*}
#'
#' @export
#' @name extract_ermod
#' @keywords internal
#' @inherit extract_method return
#' @param x An object of class \code{ermod_*}
#' @param method If \code{method="raw"} (the default), the original data set is returned.
#' When \code{method="processed"}, the data set returned is one that has the placebo handling
#' options applied. A \code{method="internal"} option also exists, used for internal testing.
#'
extract_data.ermod <- function(x, ..., method = "raw") {
  if (method == "raw") return(x$data)
  if (method == "processed") {
    opt <- .apply_placebo_defaults(x$options_placebo_handling)
    dat <- .apply_placebo_handling(
      data = x$data,
      options = opt,
      var_exposure = extract_var_exposure(x)
    )
    return(dat)
  }
  if (method == "internal") {
    if (inherits(x$mod, "stanreg")) return(x$mod$data)
    if (inherits(x$mod, "stanemax")) return(as.data.frame(x$mod$standata))
    if (inherits(x$mod, "stanemaxbin")) return(as.data.frame(x$mod$standata))
  }
  stop("unknown `method`", call. = FALSE)
}

#' @export
#' @rdname extract_ermod
extract_mod.ermod <- function(x) x$mod

#' @export
#' @rdname extract_ermod
extract_var_resp.ermod <- function(x) x$var_resp

#' @export
#' @rdname extract_ermod
extract_var_exposure.ermod <- function(x) x$var_exposure

#' @export
#' @rdname extract_ermod
extract_var_cov.ermod <- function(x) {
  if (inherits(x, c("ermod_bin", "ermod_lin"))) {
    return(x$var_cov)
  } else if (inherits(x, c("ermod_emax", "ermod_bin_emax"))) {
    return(x$l_var_cov |> unlist())
  } else {
    stop(
      "extract_var_cov() only supported for `ermod_bin`, `ermod_lin`",
      "`ermod_emax`, and `ermod_bin_emax`, and their subclasses"
    )
  }
}

#' @export
#' @rdname extract_ermod
extract_exp_sel_list_model.ermod_exp_sel <- function(x) x$l_mod_exposures

#' @export
#' @rdname extract_ermod
extract_exp_sel_comp.ermod_exp_sel <- function(x) x$loo_comp_exposures

#' @export
#' @rdname extract_ermod
extract_var_selected.ermod_cov_sel <- function(x) x$var_selected


#' Extract credible interval of the exposure coefficient
#'
#' @export
#' @param x An object of class `ermod_bin` or `ermod_lin`
#' @param ci_width Width of the credible interval
#' @param exp_candidates Logical, whether to extract the credible interval for
#' all exposure candidates. Default is `FALSE`. Only supported for models with
#' exposure selection, created with [dev_ermod_bin_exp_sel()] or
#' [dev_ermod_lin_exp_sel()] functions.
#' @return A named vector of length 2 with the lower and upper bounds of the
#'  credible interval (.lower, .upper). If `exp_candidates = TRUE`, a matrix
#'  with the same structure is returned, with each row corresponding to an
#'  exposure candidate.
#'
extract_coef_exp_ci <- function(
    x, ci_width = 0.95, exp_candidates = FALSE) {
  # Check that input x is linear ermod object
  if (!inherits(x, c("ermod_bin", "ermod_lin"))) {
    stop("extract_coef_exp_ci() only supported for linear models")
  }
  if (exp_candidates && !inherits(x, "ermod_exp_sel")) {
    stop(
      "exp_candidates = TRUE only supported for",
      " models with exposure selection"
    )
  }

  if (exp_candidates) {
    coef_exp_ci <- lapply(
      x$l_mod_exposures,
      function(mod) {
        stats::quantile(
          mod$coef_exp_draws,
          c(0.5 - ci_width / 2, 0.5 + ci_width / 2)
        ) |>
          stats::setNames(c(".lower", ".upper"))
      }
    )

    coef_exp_ci <- do.call(rbind, coef_exp_ci)
  } else {
    coef_exp_ci <-
      stats::quantile(
        x$coef_exp_draws,
        c(0.5 - ci_width / 2, 0.5 + ci_width / 2)
      )

    names(coef_exp_ci) <- c(".lower", ".upper")
  }

  return(coef_exp_ci)
}

# as_draws --------------------------------------------------------------------

#' Transform to `draws` objects
#'
#' See [posterior::as_draws()] for details.
#'
#' @importFrom posterior as_draws as_draws_list as_draws_array as_draws_df
#' as_draws_matrix as_draws_rvars
#' @param x An object of class `ermod`
#' @param ... Arguments passed to individual methods (if applicable).
#' @return A draws object from the `posterior` package.
#' @name as_draws
NULL

#' @rdname as_draws
#' @importFrom posterior as_draws
#' @export
posterior::as_draws

#' @rdname as_draws
#' @importFrom posterior as_draws_list
#' @export
posterior::as_draws_list

#' @rdname as_draws
#' @importFrom posterior as_draws_array
#' @export
posterior::as_draws_array

#' @rdname as_draws
#' @importFrom posterior as_draws_df
#' @export
posterior::as_draws_df

#' @rdname as_draws
#' @importFrom posterior as_draws_matrix
#' @export
posterior::as_draws_matrix

#' @rdname as_draws
#' @importFrom posterior as_draws_rvars
#' @export
posterior::as_draws_rvars

#' @rdname as_draws
#' @export
as_draws.ermod <- function(x, ...) {
  posterior::as_draws(x$mod, ...)
}

#' @rdname as_draws
#' @export
as_draws_list.ermod <- function(x, ...) {
  posterior::as_draws_list(x$mod, ...)
}

#' @rdname as_draws
#' @export
as_draws_array.ermod <- function(x, ...) {
  posterior::as_draws_array(x$mod, ...)
}

#' @rdname as_draws
#' @export
as_draws_df.ermod <- function(x, ...) {
  posterior::as_draws_df(x$mod, ...)
}

#' @rdname as_draws
#' @export
as_draws_matrix.ermod <- function(x, ...) {
  posterior::as_draws_matrix(x$mod, ...)
}

#' @rdname as_draws
#' @export
as_draws_rvars.ermod <- function(x, ...) {
  posterior::as_draws_rvars(x$mod, ...)
}

# prior_summary --------------------------------------------------------------


#' @name prior_summary
#' @importFrom rstanarm prior_summary
#' @export
rstanarm::prior_summary

#' Summarize the priors used for linear or linear logistic regression models
#'
#' See [rstanarm::prior_summary()] for details.
#'
#' @export
#' @rdname prior_summary
#' @param object An object of class `ermod`
#' @param ... Additional arguments passed to `rstanarm::prior_summary()`
#' @return An object of class `prior_summary.stanreg`
#'
prior_summary.ermod <- function(object, ...) {
  # Check that input x is linear ermod object
  if (!inherits(object, c("ermod_bin", "ermod_lin"))) {
    stop("prior_summary.ermod() only supported for linear models")
  }

  rstanarm::prior_summary(object$mod, ...)
}
