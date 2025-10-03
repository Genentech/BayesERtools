#' Extract elements from S3 objects
#'
#' S3 methods are defined for `ermod_*` (see [extract_ermod]) and
#' `ersim_*` (see [extract_ersim]) classes.
#'
#' @name extract_method
#' @param x An object to extract elements from
#' @param ... Additional arguments passed to methods
#' @return
#' - [extract_data()] extracts data used for the model fit.
#' - [extract_mod()] extracts the model fit object.
#' - [extract_var_resp()] extracts the response variable name
#' - [extract_var_exposure()] extracts the exposure metric name
#' - [extract_var_cov()] extracts the covariates name
#' - [extract_exp_sel_list_model()] extracts the list of fitted models for
#'   each exposure metrics.
#' - [extract_exp_sel_comp()] extracts the comparison results of the exposure
#'   metrics.
#' - [extract_var_selected()] extracts the selected variables (both exposure
#'   and covariates)in the final model after covariate selection.
NULL

#' @export
#' @rdname extract_method
extract_data <- function(x, ...) UseMethod("extract_data")

#' @export
#' @rdname extract_method
extract_mod <- function(x) UseMethod("extract_mod")

#' @export
#' @rdname extract_method
extract_var_resp <- function(x) UseMethod("extract_var_resp")

#' @export
#' @rdname extract_method
extract_var_exposure <- function(x) UseMethod("extract_var_exposure")

#' @export
#' @rdname extract_method
extract_var_cov <- function(x) UseMethod("extract_var_cov")

#' @export
#' @rdname extract_method
extract_exp_sel_list_model <- function(x) {
  UseMethod("extract_exp_sel_list_model")
}

#' @export
#' @rdname extract_method
extract_exp_sel_comp <- function(x) UseMethod("extract_exp_sel_comp")

#' @export
#' @rdname extract_method
extract_var_selected <- function(x) UseMethod("extract_var_selected")
