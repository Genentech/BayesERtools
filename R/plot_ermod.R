#' Plot ER model simulations
#'
#' @export
#' @param x an object of class `ermod`, `ersim`,`ersim_med_qi`,
#' or their subclasses
#' @param ... currently not used
#' @param show_orig_data logical, whether to show the data points in the
#' model development dataset. Default is `FALSE`. Only support plotting
#' with data that was used in the model development. If you want to use
#' other data, consider adding geom_point() to the plot manually.
#' @param show_coef_exp logical, whether to show the credible interval
#' of the exposure coefficient. Default is `FALSE`. This is only available
#' for linear and linear logistic regression models.
#' @param show_caption logical, whether to show the caption note for the plot.
#' Default is `FALSE`.
#' @param options_orig_data List of options for configuring how original data
#' is displayed. Possible options include:
#'   - `add_boxplot`: Logical, whether to add a boxplot of exposure values.
#'     Default is `FALSE`.
#'   - `boxplot_height`: Height of the boxplot relative to the main plot.
#'     Default is `0.15`.
#'   - `show_boxplot_y_title`: Logical, whether to show the y-axis title
#'     for the boxplot. Default is `TRUE`.
#'   - `var_group`: The column to use for grouping data for plotting.
#'     If specified, observed data points and boxplot will be grouped
#'     and colored by this column. Default is `NULL`.
#'   - `n_bins`: Number of bins to use for observed probability
#'     summary. Only relevant for binary models. Default is `4`.
#'   - `qi_width`: Width of the quantile interval (confidence interval) for
#'     the observed probability summary. Only relevant for binary models.
#'     Default is `0.95`.
#' @param options_coef_exp List of options for configuring how the exposure
#' coefficient credible interval is displayed. Possible options include:
#'  - `qi_width`: Width of the quantile interval (credible interval) for
#'    the exposure coefficient. Default is `0.95`.
#'  - `n_sigfig`: Number of significant figures to display. Default is `3`.
#'  - `pos_x`: x-coordinate of the text label. If `NULL` (default), it is
#'    set to the minimum value for the exposure variable.
#'  - `pos_y`: y-coordinate of the text label. If `NULL` (default), it is
#'    set to 0.9 for logistic regression models and the maximum value of the
#'    response variable in the original data for linear regression models.
#'  - `size`: Size of the text label. Default is `4`.
#' @param options_caption List of options for configuring the caption note.
#' Possible options include:
#'   - `orig_data`: Logical, whether to show the caption note for the
#'   observed data. Default is `FALSE`.
#'   - `orig_data_summary`: Logical, whether to show the caption note for the
#'   observed data summary. Default is `FALSE`.
#'   Only relevant for binary models.
#'   - `coef_exp`: Logical, whether to show the caption note for the
#'   exposure coefficient credible interval. Default is `FALSE`.
#' @param qi_width_sim Width of the quantile interval to summarize simulated
#' draws.
#' @param ... currently not used
#'
#' @return A ggplot object
#' @details
#' Plotting with `ermod` is done with some default values. If they are not
#' suitable, you can always perform the simulation manually and use
#' `plot_er()` on the simulated data.
#'
#' @examplesIf BayesERtools:::.if_run_ex_plot_er()
#' \donttest{
#' data(d_sim_binom_cov_hgly2)
#'
#' ermod_bin <- dev_ermod_bin(
#'   data = d_sim_binom_cov_hgly2,
#'   var_resp = "AEFLAG",
#'   var_exposure = "AUCss_1000"
#' )
#'
#' ersim_med_qi <- sim_er_curve(
#'   ermod_bin,
#'   output_type = "median_qi"
#' )
#'
#' plot_er(ersim_med_qi, show_orig_data = TRUE) +
#'   # Use log10 scale for exposure
#'   xgxr::xgx_scale_x_log10(guide = ggplot2::guide_axis(minor.ticks = TRUE))
#' }
#'
plot_er <- function(x, ...) {
  UseMethod("plot_er")
}

#' @export
#' @rdname plot_er
plot_er.ersim_med_qi <- function(
    x,
    show_orig_data = FALSE,
    show_coef_exp = FALSE,
    show_caption = FALSE,
    options_orig_data = list(),
    options_coef_exp = list(),
    options_caption = list(),
    ...) {
  options_orig_data <-
    utils::modifyList(
      list(
        add_boxplot = FALSE, boxplot_height = 0.15,
        show_boxplot_y_title = TRUE, var_group = NULL,
        n_bins = 4, qi_width = 0.95
      ),
      options_orig_data
    )
  options_coef_exp <-
    utils::modifyList(
      list(
        qi_width = 0.95, n_sigfig = 3, pos_x = NULL, pos_y = NULL, size = 4
      ),
      options_coef_exp
    )
  options_caption <-
    utils::modifyList(
      list(
        orig_data = FALSE, orig_data_summary = FALSE, coef_exp = FALSE
      ),
      options_caption
    )

  origdata <- extract_data(x, method = "processed")
  var_exposure <- extract_var_exposure(x)
  add_boxplot <- options_orig_data$add_boxplot
  boxplot_height <- options_orig_data$boxplot_height

  # Check input
  check_plot_er_input(x, show_orig_data)
  origdata <-
    check_group_col(origdata, show_orig_data, options_orig_data$var_group)

  # Main plot -----------------------------------------------------------------
  gg <-
    ggplot2::ggplot(
      data = x,
      ggplot2::aes(x = .data[[var_exposure]], y = .epred)
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .epred.lower, ymax = .epred.upper),
      alpha = 0.3
    ) +
    ggplot2::geom_line()

  if (attr(x, "endpoint_type") == "binary") {
    gg <- .plot_er_binary(
      gg, x, origdata, show_orig_data, options_orig_data
    )
  } else {
    gg <- .plot_er_continuous(
      gg, x, origdata, show_orig_data, options_orig_data
    )
  }

  # Add exposure coefficient CI annotation ------------------------------------
  if (show_coef_exp) {
    coef_exp <- calc_coef_exp(x, show_coef_exp, options_coef_exp)
    coef_exp_str <-
      gt::vec_fmt_number(coef_exp, n_sigfig = options_coef_exp$n_sigfig)

    pos_ci_annot <-
      set_pos_ci_annot(
        x, options_coef_exp$pos_x, options_coef_exp$pos_y,
        var_exposure, extract_var_resp(x)
      )

    gg <- gg +
      ggplot2::annotate(
        "label",
        x = pos_ci_annot[1], y = pos_ci_annot[2],
        label = paste0(
          coef_exp_str[1], "-", coef_exp_str[2],
          " (", options_coef_exp$qi_width * 100, "% CI)"
        ),
        label.padding = grid::unit(0.4, "lines"),
        alpha = 0.7,
        hjust = 0, vjust = 1, size = options_coef_exp$size
      )
  }

  # Add boxplot ---------------------------------------------------------------
  if (add_boxplot) {
    rlang::check_installed("patchwork")

    var_group <- options_orig_data$var_group

    if (is.null(var_group)) {
      gg_boxplot <-
        ggplot2::ggplot(data = origdata, ggplot2::aes(
          x = .data[[var_exposure]],
          y = 0
        )) +
        ggplot2::geom_boxplot(alpha = 0.5) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.2)) +
        ggplot2::theme(
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank()
        )
    } else {
      # Define the numeric variable name with a leading dot
      var_group_num <- paste0(".", var_group, "_num")

      # Convert the categorical variable to a numeric index
      origdata[[var_group_num]] <- as.numeric(factor(origdata[[var_group]]))

      gg_boxplot <-
        ggplot2::ggplot(data = origdata, ggplot2::aes(
          x = .data[[var_exposure]],
          y = .data[[var_group_num]]
        )) +
        ggplot2::geom_boxplot(
          ggplot2::aes(
            fill = .data[[var_group]],
            color = .data[[var_group]]
          ),
          alpha = 0.5
        ) +
        ggplot2::scale_y_continuous(
          name = var_group,
          breaks = unique(origdata[[var_group_num]]),
          labels = levels(factor(origdata[[var_group]])),
          expand = ggplot2::expansion(mult = 0.2)
        ) +
        ggplot2::theme(
          panel.grid.minor.y = ggplot2::element_blank()
        )
      if (!options_orig_data$show_boxplot_y_title) {
        gg_boxplot <- gg_boxplot +
          ggplot2::theme(axis.title.y = ggplot2::element_blank())
      }
    }

    gg_boxplot <- gg_boxplot +
      ggplot2::theme(
        legend.position = "none",
        plot.margin = ggplot2::margin(t = 0)
      )

    gg <- gg +
      ggplot2::theme(
        plot.margin = ggplot2::margin(b = 0)
      )

    gg <-
      patchwork::wrap_plots(list(gg, gg_boxplot),
        nrow = 2, heights = c(1 - boxplot_height, boxplot_height)
      ) +
      patchwork::plot_layout(axes = "collect", guides = "collect")
  }

  # Add caption note ----------------------------------------------------------
  if (show_caption) {
    caption <- .build_caption(
      x, show_orig_data, show_coef_exp,
      options_orig_data, options_coef_exp, options_caption
    )
    gg <- gg +
      ggplot2::labs(caption = caption) +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(family = "mono", hjust = 0)
      )
  }

  return(gg)
}


.build_caption <- function(
    x, show_orig_data, show_coef_exp,
    options_orig_data, options_coef_exp, options_caption) {
  qi_width_sim <- attr(x, "qi_width")

  # Base caption line
  lines <- list(
    paste0(
      "Line/Shade: Median/",
      qi_width_sim * 100, "% CI of model sim."
    )
  )

  # Append additional lines based on conditions
  if (show_orig_data && options_caption$orig_data_summary) {
    if (attr(x, "endpoint_type") == "binary") {
      lines <- c(lines, paste0(
        "Squares/bars: Median/", options_orig_data$qi_width * 100,
        "% CI of obs prob."
      ))
    }
  }

  if (show_orig_data && options_caption$orig_data) {
    lines <- c(lines, "Circles: Observed data points")
  }

  if (show_coef_exp && options_caption$coef_exp) {
    lines <- c(lines, paste0(
      "Text: ", options_coef_exp$qi_width * 100,
      "% CI of exposure effect coef."
    ))
  }

  # Combine lines into a single caption string
  caption <- paste(lines, collapse = "\n")

  return(caption)
}


.plot_er_binary <- function(
    gg, x, origdata, show_orig_data, options_orig_data) {
  var_resp <- extract_var_resp(x)
  var_exposure <- extract_var_exposure(x)

  coord_bin_default <- ggplot2::coord_cartesian(ylim = c(-0.05, 1.05))
  coord_bin_default$default <- TRUE

  gg <-
    gg + coord_bin_default +
    ggplot2::scale_y_continuous(
      breaks = c(0, .5, 1),
      labels = scales::percent
    ) +
    ggplot2::labs(x = var_exposure, y = "Probability of event")

  if (show_orig_data) {
    rlang::check_installed("xgxr")

    var_group <- options_orig_data$var_group
    n_bins <- options_orig_data$n_bins
    qi_width <- options_orig_data$qi_width

    # Convert the response variable to a numeric index
    origdata$.resp_num <- as.numeric(factor(origdata[[var_resp]])) - 1

    # binned probability ------------------------------------------------------
    breaks <-
      stats::quantile(origdata[[var_exposure]], probs = seq(0, 1, 1 / n_bins))

    # Show error when the breaks are not unique
    if (length(unique(breaks)) != length(breaks)) {
      stop(
        "The breaks for the binned probability are not unique, ",
        "possibly due to too few unique values in the exposure variable.\n",
        "Please adjust the number of bins or the exposure variable."
      )
    }

    gg <- gg +
      ggplot2::geom_vline(
        xintercept = breaks, linetype = "dashed",
        alpha = 0.3
      ) +
      xgxr::xgx_stat_ci(
        data = origdata,
        ggplot2::aes(x = .data[[var_exposure]], y = .data[[".resp_num"]]),
        bins = n_bins, conf_level = qi_width, distribution = "binomial",
        geom = c("point"), shape = 0, size = 4
      ) +
      xgxr::xgx_stat_ci(
        data = origdata,
        ggplot2::aes(x = .data[[var_exposure]], y = .data[[".resp_num"]]),
        bins = n_bins, conf_level = qi_width, distribution = "binomial",
        geom = c("errorbar"), linewidth = 0.5
      )


    # jitter plots -----------------------------------------------------------
    if (!is.null(var_group)) {
      gg <- gg +
        ggplot2::geom_jitter(
          data = origdata,
          ggplot2::aes(
            x = .data[[var_exposure]], y = .data[[".resp_num"]],
            color = .data[[var_group]]
          ),
          width = 0, height = 0.05, alpha = 0.5
        ) +
        ggplot2::scale_color_discrete(
          guide = ggplot2::guide_legend(reverse = TRUE)
        )
    } else {
      gg <- gg +
        ggplot2::geom_jitter(
          data = origdata,
          ggplot2::aes(x = .data[[var_exposure]], y = .data[[".resp_num"]]),
          width = 0, height = 0.05, alpha = 0.5
        )
    }
  }

  return(gg)
}

.plot_er_continuous <- function(
    gg, x, origdata, show_orig_data, options_orig_data) {
  var_resp <- extract_var_resp(x)
  var_exposure <- extract_var_exposure(x)

  gg <- gg +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .prediction.lower, ymax = .prediction.upper),
      alpha = 0.15
    ) +
    ggplot2::labs(x = var_exposure, y = "Response")

  if (show_orig_data) {
    var_group <- options_orig_data$var_group

    if (!is.null(var_group)) {
      gg <- gg +
        ggplot2::geom_point(
          data = origdata,
          ggplot2::aes(
            x = .data[[var_exposure]], y = .data[[var_resp]],
            color = .data[[var_group]]
          )
        ) +
        ggplot2::scale_color_discrete(
          guide = ggplot2::guide_legend(reverse = TRUE)
        )
    } else {
      gg <- gg +
        ggplot2::geom_point(
          data = origdata,
          ggplot2::aes(x = .data[[var_exposure]], y = .data[[var_resp]])
        )
    }
  }

  return(gg)
}

#' @export
#' @rdname plot_er
plot_er.ersim <- function(
    x,
    show_orig_data = FALSE,
    show_coef_exp = FALSE,
    show_caption = FALSE,
    options_orig_data = list(),
    options_coef_exp = list(),
    options_caption = list(),
    qi_width_sim = 0.95,
    ...) {
  ersim_med_qi <- calc_ersim_med_qi(x, qi_width = qi_width_sim)

  plot_er(
    ersim_med_qi,
    show_orig_data = show_orig_data,
    show_coef_exp = show_coef_exp,
    show_caption = show_caption,
    options_orig_data = options_orig_data,
    options_coef_exp = options_coef_exp,
    options_caption = options_caption
  )
}


#' @export
#' @rdname plot_er
#' @inheritParams sim_er
#' @param n_draws_sim Number of draws to simulate response for each exposure
#' value. Set to NULL to use all draws in the model object. Default is NULL
#' unless marginal is set to TRUE (in that case 200 by default to reduce
#' computation time).
#' @param marginal logical, whether to use marginal ER simulation. Default
#' to `FALSE`. Need to set to `TRUE` if the model has covariates for the
#' plot to work.
#' @param exposure_range Only relevant when the input x is an `ermod` object.
#' Range of exposure values to simulate. If NULL
#' (default), it is set to the range of the exposure variable in the original
#' data for model development.
#' @param num_exposures Only relevant as with `exposure_range`.
#' Number of exposure values to simulate.
#'
plot_er.ermod <- function(
    x,
    show_orig_data = FALSE,
    show_coef_exp = FALSE,
    show_caption = FALSE,
    options_orig_data = list(),
    options_coef_exp = list(),
    options_caption = list(),
    n_draws_sim = if (marginal) 200 else NULL,
    seed_sample_draws = NULL,
    marginal = FALSE,
    exposure_range = NULL,
    num_exposures = 51,
    qi_width_sim = 0.95,
    ...) {
  if (!marginal && !is.null(extract_var_cov(x))) {
    stop(
      "Model has covariate(s), and you cannot use this function directly ",
      "on `ermod` object unless you set `marginal = TRUE`, if",
      "that is what you want.\n",
      "Otherwise, perform the simulation manually with e.g. `sim_er_curve()`",
      "and use `plot_er()` on the simulated data."
    )
  }

  if (marginal) {
    ersim_curve_med_pi <- sim_er_curve_marg(
      x,
      exposure_range = exposure_range,
      num_exposures = num_exposures,
      n_draws_sim = n_draws_sim,
      seed_sample_draws = seed_sample_draws,
      output_type = "median_qi",
      qi_width = qi_width_sim
    )
  } else {
    ersim_curve_med_pi <- sim_er_curve(
      x,
      exposure_range = exposure_range,
      num_exposures = num_exposures,
      n_draws_sim = n_draws_sim,
      seed_sample_draws = seed_sample_draws,
      output_type = "median_qi",
      qi_width = qi_width_sim
    )
  }

  plot_er(
    ersim_curve_med_pi,
    show_orig_data = show_orig_data,
    show_coef_exp = show_coef_exp,
    show_caption = show_caption,
    options_orig_data = options_orig_data,
    options_coef_exp = options_coef_exp,
    options_caption = options_caption
  )
}

#' Default GOF plot for ER model
#'
#' This is a wrapper function for [plot_er()] with default options for
#' goodness-of-fit (GOF) plots for ER models.
#'
#' @export
#' @inheritParams plot_er
#' @param add_boxplot Logical, whether to add a boxplot of exposure values.
#' Default is `TRUE` if `var_group` is specified, otherwise `FALSE`.
#' @param boxplot_height Height of the boxplot relative to the main plot.
#' Default is `0.15`.
#' @param show_boxplot_y_title Logical, whether to show the y-axis title
#' for the boxplot. Default is `FALSE`.
#' @param var_group The column to use for grouping data for plotting.
#' If specified, observed data points and boxplot will be grouped
#' and colored by this column. Default is `NULL`.
#' @param n_bins Number of bins to use for observed probability
#' summary. Only relevant for binary models. Default is `4`.
#' @param qi_width_obs Confidence level for the observed probability
#' summary. Default is `0.95`.
#' @param show_coef_exp Logical, whether to show the credible interval
#' of the exposure coefficient. Default is `FALSE`. This is only available
#' for linear and linear logistic regression models.
#' @param coef_pos_x x-coordinate of the text label. If `NULL` (default), it is
#' set to the minimum value for the exposure variable.
#' @param coef_pos_y y-coordinate of the text label. If `NULL` (default), it is
#' set to 0.9 for logistic regression models and the maximum value of the
#' response variable in the original data for linear regression models.
#' @param coef_size Size of the text label. Default is `4`.
#' @param qi_width_coef Width of the credible interval for the exposure
#' coefficient. Default is `0.95`.
#' @param qi_width_sim Width of the quantile interval to summarize simulated
#' draws. Default is `0.95`.
#' @param show_caption Logical, whether to show the caption note for the
#' plot. Default is `TRUE`.
#'
#' @details
#' The following code will generate the same plot:
#' \preformatted{
#' plot_er(
#'   x,
#'   show_orig_data = TRUE,
#'   show_coef_exp = show_coef_exp,
#'   show_caption = show_caption,
#'   options_orig_data = list(
#'     add_boxplot = add_boxplot, boxplot_height = boxplot_height,
#'     show_boxplot_y_title = show_boxplot_y_title,
#'     var_group = var_group,
#'     n_bins = n_bins, qi_width = qi_width_obs
#'   ),
#'   options_coef_exp = list(
#'     qi_width = qi_width_coef, pos_x = coef_pos_x, pos_y = coef_pos_y,
#'     size = coef_size
#'   ),
#'   options_caption = list(
#'     orig_data_summary = TRUE, coef_exp = show_coef_exp
#'   ),
#'   qi_width_sim = qi_width_sim
#' )
#' }
#'
#' @return A ggplot object
#' @examplesIf BayesERtools:::.if_run_ex_plot_er()
#' \donttest{
#' data(d_sim_binom_cov_hgly2)
#'
#' ermod_bin <- dev_ermod_bin(
#'   data = d_sim_binom_cov_hgly2,
#'   var_resp = "AEFLAG",
#'   var_exposure = "AUCss_1000"
#' )
#'
#' plot_er_gof(ermod_bin, var_group = "Dose_mg", show_coef_exp = TRUE) *
#'   # Use log10 scale for exposure, need to use `*` instead of `+`
#'   xgxr::xgx_scale_x_log10(guide = ggplot2::guide_axis(minor.ticks = TRUE))
#' }
#'
plot_er_gof <- function(
    x, add_boxplot = !is.null(var_group), boxplot_height = 0.15,
    show_boxplot_y_title = FALSE,
    var_group = NULL, n_bins = 4, qi_width_obs = 0.95,
    show_coef_exp = FALSE,
    coef_pos_x = NULL, coef_pos_y = NULL, coef_size = 4,
    qi_width_coef = 0.95,
    qi_width_sim = 0.95,
    show_caption = TRUE) {
  plot_er(
    x,
    show_orig_data = TRUE,
    show_coef_exp = show_coef_exp,
    show_caption = show_caption,
    options_orig_data = list(
      add_boxplot = add_boxplot, boxplot_height = boxplot_height,
      show_boxplot_y_title = show_boxplot_y_title,
      var_group = var_group,
      n_bins = n_bins, qi_width = qi_width_obs
    ),
    options_coef_exp = list(
      qi_width = qi_width_coef, pos_x = coef_pos_x, pos_y = coef_pos_y,
      size = coef_size
    ),
    options_caption = list(
      orig_data_summary = TRUE, coef_exp = show_coef_exp
    ),
    qi_width_sim = qi_width_sim
  )
}

# Check if covariates were used in the model, which can affect potting
# and comparison with data
# x: ersim object
check_plot_er_input <- function(x, show_orig_data) {
  var_cov <- extract_var_cov(x)
  nrow_cov_data <- attr(x, "nrow_cov_data")
  is_marginal <- inherits(x, "ersim_marg_med_qi")

  if (is.null(var_cov)) {
    return()
  }

  if (nrow_cov_data > 1) {
    stop(
      "Model has covariate(s) and multiple covariate data rows were ",
      "provided. Consider manual plotting or using marginal ER simulation."
    )
  }

  if (show_orig_data == TRUE && !is_marginal) {
    warning(
      "Model has covariate(s), and only one covariate data row was provided. ",
      "Data points shown can be misleading. Consider manual plotting or ",
      "using marginal ER simulation."
    )
  }
}

# Check var_group if original data is to be shown
check_group_col <- function(origdata, show_orig_data, var_group) {
  if (!show_orig_data || is.null(var_group)) {
    return(origdata)
  }

  if (!var_group %in% names(origdata)) {
    stop(
      "Column `", var_group, "` not found in the original data. ",
      "Please check the column name."
    )
  }
  # if var_group is numeric, warn
  if (is.numeric(origdata[[var_group]])) {
    # Stop if there are 10 or more levels
    if (length(unique(origdata[[var_group]])) > 10) {
      stop(
        "Column `", var_group, "` is numeric and has > 10 unique values. ",
        "Please convert to factor for grouping."
      )
    }

    origdata[[var_group]] <- factor(origdata[[var_group]])
  }

  return(origdata)
}


calc_coef_exp <- function(x, show_coef_exp, options_coef_exp) {
  if (any(c("ermod_bin", "ermod_lin") %in% attr(x, "ermod_class"))) {
    qi_width <- options_coef_exp$qi_width
    coef_exp_draws <- attr(x, "coef_exp_draws")
    coef_exp <-
      stats::quantile(
        coef_exp_draws,
        c(0.5 - qi_width / 2, 0.5 + qi_width / 2)
      )
  } else {
    stop("`show_coef_exp =TRUE` is only available for linear models.")
  }

  return(coef_exp)
}

set_pos_ci_annot <- function(x, pos_x, pos_y, var_exposure, var_resp) {
  if (is.null(pos_x)) {
    pos_x <- min(x[[var_exposure]])
  }
  if (is.null(pos_y)) {
    if (attr(x, "endpoint_type") == "binary") {
      pos_y <- 0.9
    } else {
      pos_y <- max(extract_data(x, method = "processed")[[var_resp]])
    }
  }
  return(c(pos_x, pos_y))
}

.if_run_ex_plot_er <- function() {
  requireNamespace("xgxr", quietly = TRUE)
}
