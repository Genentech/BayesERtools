# Variable selection ----------------------------------------------------------

#' Plot variable selection performance
#' @export
#' @name plot_cov_sel
#' @param x An object of class \code{ermod_bin_cov_sel}
#' @details
#' [plot_submod_performance()] plots the performance of submodels
#' evaluated during variable selection.
#'
#' @return No return value, called for plotting side effect.
#' @examplesIf BayesERtools:::.if_run_ex_covsel()
#' \donttest{
#' data(d_sim_binom_cov_hgly2)
#'
#' er_binary_cov_model_kfold <- dev_ermod_bin_cov_sel(
#'   data = d_sim_binom_cov_hgly2,
#'   var_resp = "AEFLAG",
#'   var_exposure = "AUCss_1000",
#'   var_cov_candidate = c(
#'     "BAGE_10", "BWT_10", "BGLUC",
#'     "BHBA1C_5", "RACE", "VISC"
#'   ),
#'   cv_method = "kfold",
#'   k = 3, # Choose 3 to make the example go fast
#'   validate_search = TRUE,
#' )
#'
#' plot_submod_performance(er_binary_cov_model_kfold)
#' plot_var_ranking(er_binary_cov_model_kfold)
#' }
plot_submod_performance <- function(x) {
  stopifnot(inherits(x, "ermod_cov_sel"))
  plot(x$cvvs, text_angle = 45, show_cv_proportions = FALSE, deltas = TRUE)
}


#' @export
#' @rdname plot_cov_sel
#' @details
#' [plot_var_ranking()] plots the variable ranking
#' evaluated during variable selection.
plot_var_ranking <- function(x) {
  if (is.null(x$rk$foldwise)) {
    stop(
      'ranking only supported for "kfold" with ',
      'validate_search = TRUE"'
    )
  }
  plot(x$rk)
}


# Exposure metric selection ---------------------------------------------------

#' Plot exposure metric selection comparison
#'
#' Plot ER curve for each exposure metric and compare them.
#'
#' @export
#' @param x An object of class `ermod_bin_exp_sel`
#' @param n_draws_sim Number of draws to simulate response for each exposure
#' value. Default is NULL (use all draws in the model object)
#'
#' @return No return value, called for plotting side effect.
#' @examplesIf BayesERtools:::.if_run_ex_plot_er()
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
#' plot_er_exp_sel(ermod_bin_exp_sel) +
#'   xgxr::xgx_scale_x_log10(guide = ggplot2::guide_axis(minor.ticks = TRUE))
#' }
#'
plot_er_exp_sel <- function(x, n_draws_sim = NULL) {
  stopifnot(inherits(x, "ermod_exp_sel"))
  is_binary_mod <- inherits(x, c("ermod_bin", "ermod_bin_emax"))

  var_resp <- extract_var_resp(x)
  var_exp_order <-
    rownames(x$loo_comp_exposures)

  df_map_var_exp <-
    dplyr::tibble(.exp_metric = var_exp_order) |>
    dplyr::mutate(
      .exp_met_fct = factor(.exp_metric, levels = unique(.exp_metric))
    )

  origdata <-
    extract_data(x, method = "processed") |>
    tidyr::pivot_longer(dplyr::all_of(var_exp_order),
      names_to = ".exp_metric", values_to = ".exposure"
    ) |>
    dplyr::left_join(df_map_var_exp, by = ".exp_metric")

  data_sim <-
    purrr::map2(
      x$l_mod_exposures,
      names(x$l_mod_exposures),
      function(.x, .y) sim_er_each_exp_met(.x, .y, n_draws_sim = n_draws_sim)
    ) |>
    purrr::list_rbind() |>
    dplyr::left_join(df_map_var_exp, by = ".exp_metric")

  gg <-
    ggplot2::ggplot(
      data = data_sim,
      ggplot2::aes(x = .exposure, y = .epred)
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .epred.lower, ymax = .epred.upper),
      alpha = 0.3
    ) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~.exp_met_fct, scales = "free_x") +
    ggplot2::labs(x = "Exposure")

  if (is_binary_mod) {
    gg <-
      gg +
      ggplot2::scale_y_continuous(
        breaks = c(0, .5, 1),
        labels = scales::percent
      ) +
      ggplot2::geom_jitter(
        data = origdata,
        ggplot2::aes(y = .data[[var_resp]]),
        width = 0, height = 0.05, alpha = 0.5
      ) +
      xgxr::xgx_stat_ci(
        data = origdata,
        ggplot2::aes(y = .data[[var_resp]]),
        bins = 4, conf_level = 0.95, distribution = "binomial",
        geom = c("point"), shape = 0, size = 4
      ) +
      xgxr::xgx_stat_ci(
        data = origdata,
        ggplot2::aes(y = .data[[var_resp]]),
        bins = 4, conf_level = 0.95, distribution = "binomial",
        geom = c("errorbar"), linewidth = 0.5
      ) +
      ggplot2::labs(y = "Probability of event") +
      ggplot2::coord_cartesian(ylim = c(-0.05, 1.05))
  } else {
    gg <- gg +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .prediction.lower, ymax = .prediction.upper),
        alpha = 0.15
      ) +
      ggplot2::geom_point(
        data = origdata,
        ggplot2::aes(y = .data[[var_resp]])
      ) +
      ggplot2::labs(y = "Response")
  }

  return(gg)
}


sim_er_each_exp_met <- function(.x, .y, n_draws_sim) {
  sim <- sim_er_curve(.x, output_type = "median_qi", n_draws_sim = n_draws_sim)
  sim$.exposure <- sim[[.y]]
  sim[[.y]] <- NULL
  sim$.exp_metric <- .y
  sim
}
