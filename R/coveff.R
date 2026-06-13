#' Perform simulation of covariate effects for ER model
#'
#' @export
#' @param ermod an object of class `ermod` (supports `ermod_bin` and `ermod_lin`)
#' @param data an optional data frame to derive the covariate values for
#' forest plots. If NULL (default), the data used to fit the model is used.
#' @param spec_coveff you can supply spec_coveff to [sim_coveff()] or
#' [plot_coveff()], if you have already built it manually or with
#' [build_spec_coveff()]. See [build_spec_coveff()] for detail.
#' @param output_type Type of output. Currently only supports "median_qi"
#' which returns the median and quantile interval.
#' @param qi_width the width of the credible interval on the covariate effect.
#' This translate to the width of the error bars in the forest plot.
#' @param qi_width_cov the width of the quantile interval for continuous
#' covariates in the forest plot. Default is 0.9 (i.e. visualize effect of
#' covariate effect at their 5th and 95th percentile values).
#' @return A data frame with class `coveffsim` containing the median and
#' quantile interval of the covariate effects. For binary models (`ermod_bin`),
#' returns odds ratios. For linear models (`ermod_lin`), returns response differences.
#'
#' @examplesIf BayesERtools:::.if_run_ex_coveff()
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
#' sim_coveff(ermod_bin)
#'
#' # Linear regression model example
#' data(d_sim_lin)
#'
#' ermod_lin <- dev_ermod_lin(
#'   data = d_sim_lin,
#'   var_resp = "response",
#'   var_exposure = "AUCss",
#'   var_cov = c("SEX", "BAGE"),
#' )
#'
#' sim_coveff(ermod_lin)
#' }
#'
sim_coveff <- function(
    ermod, data = NULL, spec_coveff = NULL,
    output_type = "median_qi",
    qi_width = 0.9,
    qi_width_cov = 0.9) {
  stopifnot(inherits(ermod, "ermod"))
  output_type <- match.arg(output_type)

  if (is.null(data)) {
    data <- ermod$data
  }
  if (is.null(spec_coveff)) {
    spec_coveff <- build_spec_coveff(ermod,
      data = data,
      qi_width_cov = qi_width_cov
    )
  }

  df_for_sim <- spec_coveff_to_df_sim(spec_coveff)

  linpred_draws <-
    .pp_matrix_to_draws_tbl(
      rstantools::posterior_linpred(extract_mod(ermod), newdata = df_for_sim),
      df_for_sim, ".linpred"
    )

  linpred_draws_ref <-
    linpred_draws |>
    dplyr::filter(is_ref) |>
    dplyr::ungroup() |>
    dplyr::select(.draw, .linpred_ref = .linpred)

  linpred_draws_2 <-
    linpred_draws |>
    dplyr::filter(!is_ref) |>
    dplyr::left_join(linpred_draws_ref, by = ".draw") |>
    dplyr::mutate(.delta_linpred = .linpred - .linpred_ref)

  if (inherits(ermod, "ermod_bin")) {
    linpred_draws_3 <-
      linpred_draws_2 |>
      dplyr::mutate(.odds_ratio = exp(.delta_linpred)) |>
      dplyr::select(-.linpred, -.linpred_ref, -.delta_linpred)
  } else if (inherits(ermod, "ermod_lin")) {
    linpred_draws_3 <-
      linpred_draws_2 |>
      dplyr::mutate(.response_diff = .delta_linpred) |>
      dplyr::select(-.linpred, -.linpred_ref, -.delta_linpred)
  } else {
    stop(
      "Only binary E-R model (`ermod_bin`) and linear E-R model ",
      "(`ermod_lin`) are supported for now"
    )
  }

  if (output_type == "draws") {
    return(coveffsim)
  }

  linpred_med_qi <-
    linpred_draws_3 |>
    dplyr::group_by(dplyr::across(
      !dplyr::any_of(c(".chain", ".iteration", ".draw",
        ".odds_ratio", ".response_diff"))
    )) |>
    ggdist::median_qi(.width = qi_width) |>
    dplyr::arrange(var_order, value_order)

  # Determine which columns to select based on model type
  if (inherits(ermod, "ermod_bin")) {
    effect_col <- ".odds_ratio"
    ref_value <- 1
  } else {
    effect_col <- ".response_diff"
    ref_value <- 0
  }

  linpred_med_qi_to_join <-
    linpred_med_qi |>
    dplyr::select(var_order, value_order, !!effect_col, .lower, .upper)

  coveffsim <-
    spec_coveff |>
    dplyr::select(-value_cont, -value_cat) |>
    dplyr::left_join(linpred_med_qi_to_join,
      by = dplyr::join_by(var_order, value_order)
    ) |>
    dplyr::mutate(
      !!effect_col := ifelse(is_ref_value, ref_value, !!rlang::sym(effect_col))
    )

  # Add coveffsim class and model attribute
  class(coveffsim) <- c("coveffsim", class(coveffsim))
  attr(coveffsim, "model_type") <- if (inherits(ermod, "ermod_bin")) "binary" else "linear"

  return(coveffsim)
}

#' Visualize the covariate effects for ER model
#'
#' @export
#' @name plot_coveff
#' @inheritParams sim_coveff
#' @param x an object of class `ermod`, `coveffsim`,
#' or their subclasses
#' @param ... currently not used
#'
#' @return A ggplot object. For binary models (`ermod_bin`), plots odds ratios
#' on a log scale with reference line at 1. For linear models (`ermod_lin`),
#' plots response differences on a linear scale with reference line at 0.
#'
#' @examplesIf BayesERtools:::.if_run_ex_coveff()
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
#' plot_coveff(ermod_bin)
#'
#' # Linear regression model example
#' data(d_sim_lin)
#'
#' ermod_lin <- dev_ermod_lin(
#'   data = d_sim_lin,
#'   var_resp = "response",
#'   var_exposure = "AUCss",
#'   var_cov = c("SEX", "BAGE"),
#' )
#'
#' plot_coveff(ermod_lin)
#' }
#'
plot_coveff <- function(x, ...) UseMethod("plot_coveff")


#' @export
#' @rdname plot_coveff
plot_coveff.ermod <- function(
    x, data = NULL, spec_coveff = NULL,
    qi_width = 0.9,
    qi_width_cov = 0.9, ...) {
  coveffsim <- sim_coveff(
    ermod = x, data = data, spec_coveff = spec_coveff,
    output_type = "median_qi", qi_width = qi_width,
    qi_width_cov = qi_width_cov
  )

  plot_coveff.coveffsim(coveffsim)
}


#' @export
#' @rdname plot_coveff
plot_coveff.coveffsim <- function(x, ...) {
  # Need the followings for plotting
  rlang::check_installed("ggforce")
  rlang::check_installed("xgxr")

  coveffsim <- x

  # Get model information from attributes
  is_binary_model <- attr(coveffsim, "model_type") == "binary"

  coveffsim_for_plot <-
    coveffsim |>
    # Only show ref value if show_ref_value is TRUE
    dplyr::filter(!is_ref_value | show_ref_value) |>
    dplyr::arrange(var_order, value_order) |>
    dplyr::mutate(
      var_value_index_num = paste0(var_order, "_", value_order),
      var_value_index_num = factor(var_value_index_num,
        levels = unique(var_value_index_num)
      ),
      var_value_index_num = as.numeric(var_value_index_num),
      value_label_plot = paste0(var_label, ": ", value_label)
    ) |>
    dplyr::mutate(
      .lower = ifelse(is_ref_value, ifelse(is_binary_model, 1, 0), .lower),
      .upper = ifelse(is_ref_value, ifelse(is_binary_model, 1, 0), .upper)
    )

  # Choose appropriate aesthetic and scale based on model type
  if (is_binary_model) {
    x_aes <- ggplot2::aes(x = .odds_ratio, y = var_value_index_num)
    x_scale <- xgxr::xgx_scale_x_log10(guide = ggplot2::guide_axis(minor.ticks = TRUE))
    x_lab <- "Odds ratio"
    x_intercept <- 1
  } else {
    x_aes <- ggplot2::aes(x = .response_diff, y = var_value_index_num)
    x_scale <- ggplot2::scale_x_continuous()
    x_lab <- "Response difference"
    x_intercept <- 0
  }

  gg <-
    coveffsim_for_plot |>
    ggplot2::ggplot(x_aes) +
    ggplot2::geom_point(size = 4) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .lower, xmax = .upper),
      orientation = "y",
      width = 0.3
    ) +
    ggplot2::geom_vline(xintercept = x_intercept, linetype = "dashed") +
    x_scale +
    ggforce::facet_col(~var_label, scales = "free_y", space = "free") +
    ggplot2::labs(x = x_lab) +
    ggplot2::scale_y_continuous(
      breaks = coveffsim_for_plot$var_value_index_num,
      labels = coveffsim_for_plot$value_label_plot,
      transform = "reverse",
      sec.axis = ggplot2::sec_axis(
        transform = ~., name = "Second Axis",
        breaks = coveffsim_for_plot$var_value_index_num,
        labels = coveffsim_for_plot$value_annot
      )
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(hjust = 0),
      panel.grid.minor.y = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    )

  return(gg)
}

#' Format the covariate effect simulation results for printing
#'
#' @export
#' @inheritParams build_spec_coveff
#' @param coveffsim an object of class `coveffsim`
#' @details
#' Note that `n_sigfig`, `use_seps`, and `drop_trailing_dec_mark` are only
#' applied to the response difference/odds ratio and 95% CI columns; value_label column was
#' already generated in an earlier step in [build_spec_coveff()] or
#' [sim_coveff()].
#' @return A data frame with the formatted covariate effect simulation results
#' with the following columns:
#' - `var_label`: the label of the covariate
#' - `value_label`: the label of the covariate value
#' - `value_annot`: the annotation of the covariate value
#' - `Odds ratio` or `Response difference`: the odds ratio (for binary models) or
#'   response difference (for linear models) of the covariate effect
#' - `95% CI`: the 95% credible interval of the covariate effect
#'
#' @examplesIf BayesERtools:::.if_run_ex_coveff()
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
#' print_coveff(sim_coveff(ermod_bin))
#'
#' # Linear regression model example
#' data(d_sim_lin)
#'
#' ermod_lin <- dev_ermod_lin(
#'   data = d_sim_lin,
#'   var_resp = "response",
#'   var_exposure = "AUCss",
#'   var_cov = c("SEX", "BAGE"),
#' )
#'
#' print_coveff(sim_coveff(ermod_lin))
#' }
#'
print_coveff <- function(
    coveffsim, n_sigfig = 3, use_seps = TRUE, drop_trailing_dec_mark = TRUE) {
  rlang::check_installed("gt")

  # Get model information from attributes
  is_binary_model <- attr(coveffsim, "model_type") == "binary"

  coveffsim_non_ref <-
    coveffsim |>
    dplyr::filter(!is_ref_value)

  # Format values for printing in non-reference rows
  effect_col <- if (is_binary_model) ".odds_ratio" else ".response_diff"
  if (is_binary_model) {
    cols_to_format <- c(".odds_ratio", ".lower", ".upper")
    effect_col_name <- "Odds ratio"
  } else {
    cols_to_format <- c(".response_diff", ".lower", ".upper")
    effect_col_name <- "Response difference"
  }

  coveffsim_non_ref <-
    coveffsim_non_ref |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(cols_to_format),
      .fns = function(x) {
        gt::vec_fmt_number(x,
          n_sigfig = n_sigfig,
          use_seps = use_seps
        )
      }
    ))

  if (drop_trailing_dec_mark) {
    coveffsim_non_ref <-
      coveffsim_non_ref |>
      dplyr::mutate(dplyr::across(
        .cols = dplyr::all_of(cols_to_format),
        .fns = function(x) sub("\\.$", "", x)
      ))
  }

  # Format reference rows (set effect to reference value and blank for 95% CI)
  coveffsim_2 <-
    coveffsim |>
    # Only show ref value if show_ref_value is TRUE
    dplyr::filter(is_ref_value & show_ref_value) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(cols_to_format),
        .fns = as.character
      )
    ) |>
    dplyr::bind_rows(coveffsim_non_ref) |>
    dplyr::arrange(var_order, value_order)

  coveffsim_2 |>
    dplyr::mutate(
      !!effect_col_name := !!rlang::sym(effect_col),
      `95% CI` =
        dplyr::if_else(is_ref_value, " ",
          paste0("[", .lower, ", ", .upper, "]")
        )
    ) |>
    dplyr::select(
      var_label, value_label, value_annot,
      !!effect_col_name, `95% CI`
    )
}


spec_coveff_to_df_sim <- function(spec_coveff) {
  # First build reference data frame
  df_ref_cont <- spec_coveff |>
    dplyr::filter(is_ref_value, !is.na(value_cont)) |>
    dplyr::select(var_name, value_cont) |>
    tidyr::pivot_wider(names_from = var_name, values_from = value_cont)
  df_ref_cat <- spec_coveff |>
    dplyr::filter(is_ref_value, !is.na(value_cat)) |>
    dplyr::select(var_name, value_cat) |>
    tidyr::pivot_wider(names_from = var_name, values_from = value_cat)

  df_ref <- dplyr::bind_cols(df_ref_cont, df_ref_cat)
  if (all(is.na(spec_coveff$value_cont))) df_ref <- df_ref_cat
  if (all(is.na(spec_coveff$value_cat))) df_ref <- df_ref_cont

  # Append the reference data frame to every row of the spec
  spec_coveff_2 <-
    spec_coveff |>
    dplyr::filter(!is_ref_value) |>
    dplyr::select(-is_ref_value) |>
    tidyr::expand_grid(df_ref)

  # Replace the corresponding values in the data columns with the
  # values in the spec
  df_for_sim <- spec_coveff_2 |>
    dplyr::mutate(.row_id = dplyr::row_number()) |>
    tidyr::nest(.by = .row_id, .key = "df_one_row") |>
    dplyr::mutate(
      df_one_row_new =
        purrr::map(df_one_row, replace_value_for_sim)
    ) |>
    dplyr::select(-.row_id, -df_one_row) |>
    tidyr::unnest(cols = df_one_row_new)

  df_for_sim_2 <-
    dplyr::bind_rows(
      dplyr::mutate(df_ref, is_ref = TRUE, var_order = 0),
      dplyr::mutate(df_for_sim, is_ref = FALSE)
    )

  return(df_for_sim_2)
}

replace_value_for_sim <- function(df_one_row) {
  var_name <- df_one_row$var_name
  value <- ifelse(
    is.na(df_one_row$value_cont),
    df_one_row$value_cat,
    df_one_row$value_cont
  )

  df_one_row[[var_name]] <- value

  return(df_one_row)
}

.if_run_ex_coveff <- function() {
  requireNamespace("ggforce", quietly = TRUE) &&
    requireNamespace("xgxr", quietly = TRUE) &&
    requireNamespace("gt", quietly = TRUE)
}
