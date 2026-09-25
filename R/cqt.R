# Concentration-QTc (C-QT) analysis -------------------------------------------

#' Develop concentration-QTc (C-QT) model
#'
#' This function develops a linear mixed-effects model for
#' concentration-QTc (C-QT) analysis, following the structure of the
#' pre-specified model recommended in the scientific white paper by
#' Garnett et al. (2018). It is a wrapper of [dev_ermod_lme()], which uses
#' [rstanarm::stan_lmer()] (precompiled Stan models) for model fitting.
#'
#' The model is:
#'
#' \deqn{\Delta QTc_{ij} = (\theta_0 + \eta_{0,i}) + \theta_1 TRT_{i} +
#' (\theta_2 + \eta_{2,i}) C_{ij} + \theta_{3,j} TIME_{j} +
#' \theta_4 (QTc_{i,baseline} - \overline{QTc_{baseline}}) + \epsilon_{ij}}
#'
#' where \eqn{TRT} is the indicator for active treatment (treatment-specific
#' intercept), \eqn{C} is the drug concentration, \eqn{TIME} is the nominal
#' time as a categorical variable, and \eqn{QTc_{baseline}} is the baseline
#' QTc. The random effects on the intercept and slope are specified with
#' `random_effect_type`. Each of the treatment, time, and baseline terms is
#' only included if the corresponding variable is specified.
#'
#' @export
#' @inheritParams dev_ermod_lme
#' @param var_resp Name of the response variable, typically the change from
#' baseline in QTc (\eqn{\Delta QTc}), in character.
#' @param var_exposure Name of the drug concentration variable in character.
#' The concentration should be 0 for placebo.
#' @param var_trt Name of the treatment variable in character. If `NULL`
#' (default), the treatment-specific intercept is not included and the
#' placebo-adjusted observed \eqn{\Delta\Delta QTc} can not be calculated
#' (i.e. no placebo data).
#' @param trt_placebo Value of `var_trt` that indicates placebo. All other
#' values are considered as active treatment. If `NULL` (default), `0` is used
#' for numeric or logical (`FALSE`) `var_trt`, and `"Placebo"` for character
#' or factor `var_trt`.
#' @param var_time Name of the nominal time variable in character. If
#' specified, it is included in the model as a categorical variable
#' (time-specific intercept) and used for time-matched placebo adjustment of
#' the observed data.
#' @param var_baseline Name of the baseline QTc variable in character. If
#' specified, it is included in the model after centering by its mean.
#' @param var_cov Names of additional covariates in character vector.
#'
#' @details
#' The following derived columns are added to the data and used in the model:
#' - `<var_trt>_active`: 1 for active treatment and 0 for placebo
#' - `<var_time>_fct`: `var_time` as a factor
#' - `<var_baseline>_cent`: `var_baseline` centered by its mean
#'
#' If you supply `newdata` for simulation functions such as [sim_er()], it
#' needs to contain these derived columns. [sim_cqt_ddqtc()] and
#' [plot_cqt_gof()] take care of them internally.
#'
#' @references
#' Garnett C, et al. Scientific white paper on concentration-QTc modeling.
#' J Pharmacokinet Pharmacodyn. 2018;45(3):383-397.
#' \doi{10.1007/s10928-017-9558-5}
#'
#' @return An object of class `ermod_cqt`, which is a subclass of `ermod_lme`.
#' @seealso [sim_cqt_ddqtc()], [plot_cqt_gof()], [plot_cqt_hysteresis()]
#'
#' @examples
#' \donttest{
#' data(d_sim_cqt)
#'
#' ermod_cqt <- dev_ermod_cqt(
#'   data = d_sim_cqt,
#'   var_resp = "DQTCF",
#'   var_exposure = "CONC_1000",
#'   var_random = "ID",
#'   var_trt = "TRT",
#'   var_time = "NTIME",
#'   var_baseline = "QTCFBL",
#'   # Settings to make the example run faster
#'   chains = 2,
#'   iter = 1000
#' )
#'
#' ermod_cqt
#'
#' # Predicted placebo-adjusted change from baseline QTc at
#' # geometric mean Cmax of therapeutic and supratherapeutic doses
#' sim_cqt_ddqtc(ermod_cqt, c("200 mg" = 0.95, "800 mg" = 4.52))
#'
#' plot_cqt_gof(ermod_cqt, exposure_to_mark = c("200 mg" = 0.95, "800 mg" = 4.52))
#' }
#'
dev_ermod_cqt <- function(
    data,
    var_resp,
    var_exposure,
    var_random,
    var_trt = NULL,
    trt_placebo = NULL,
    var_time = NULL,
    var_baseline = NULL,
    var_cov = NULL,
    random_effect_type = c("both", "intercept", "slope"),
    prior = rstanarm::default_prior_coef(stats::gaussian()),
    prior_intercept = rstanarm::default_prior_intercept(stats::gaussian()),
    prior_aux = rstanarm::exponential(autoscale = TRUE),
    prior_covariance = rstanarm::decov(),
    adapt_delta = NULL,
    verbosity_level = 1,
    chains = 4,
    iter = 2000) {
  random_effect_type <- match.arg(random_effect_type)

  check_data_columns(
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    var_cov = c(var_trt, var_time, var_baseline, var_cov),
    var_random = var_random
  )

  spec_cqt <- build_spec_cqt(
    data,
    var_exposure = var_exposure, var_trt = var_trt,
    trt_placebo = trt_placebo, var_time = var_time,
    var_baseline = var_baseline
  )
  data <- add_cols_cqt(data, spec_cqt)

  var_cov_model <- c(
    spec_cqt$var_trt_model, spec_cqt$var_time_model,
    spec_cqt$var_baseline_model, var_cov
  )

  ermod <- dev_ermod_lme(
    data = data,
    var_resp = var_resp,
    var_exposure = var_exposure,
    var_cov = var_cov_model,
    var_random = var_random,
    random_effect_type = random_effect_type,
    prior = prior,
    prior_intercept = prior_intercept,
    prior_aux = prior_aux,
    prior_covariance = prior_covariance,
    adapt_delta = adapt_delta,
    verbosity_level = verbosity_level,
    chains = chains,
    iter = iter
  )

  new_ermod_cqt(ermod, spec_cqt)
}


#' Simulate placebo-adjusted change from baseline QTc
#'
#' Calculate the model-predicted placebo-adjusted change from baseline QTc
#' (\eqn{\Delta\Delta QTc}) at specified concentrations, e.g. the geometric
#' mean Cmax at therapeutic and supratherapeutic doses. The prediction is at
#' the population level (random effects set to zero), and is defined as the
#' difference in the predicted \eqn{\Delta QTc} between active treatment at
#' the specified concentration and placebo (concentration of 0), at the same
#' time point and baseline QTc. In the model from [dev_ermod_cqt()], it
#' equals \eqn{\theta_1 + \theta_2 C}.
#'
#' @export
#' @param ermod An object of class `ermod_cqt`, from [dev_ermod_cqt()].
#' @param exposure_to_sim_vec Vector of concentrations to simulate. If named,
#' the names are returned in the `.label` column.
#' @param threshold Threshold of \eqn{\Delta\Delta QTc} (in the unit of the
#' response) for calculating the posterior probability of exceeding it.
#' Default is 10 (ms), following ICH E14.
#' @param qi_width Width of the credible interval. Default is 0.9, i.e. the
#' upper bound corresponds to the one-sided 95% upper bound used in ICH E14.
#' @param output_type Type of output. "median_qi" (default) returns the
#' median and credible interval, and "draws" returns the posterior draws.
#' @param n_draws_sim Number of draws for simulation. If NULL (default),
#' all draws in the model object are used.
#' @param seed_sample_draws Seed for sampling draws. Default is NULL.
#'
#' @return A tibble. With `output_type = "median_qi"`, it has one row per
#' concentration with the following columns:
#' - `.label`: names of `exposure_to_sim_vec` (only if named)
#' - `<var_exposure>`: concentration
#' - `.ddqtc`, `.lower`, `.upper`: median and credible interval of
#'   \eqn{\Delta\Delta QTc}
#' - `.width`: width of the credible interval
#' - `.prob_exceed`: posterior probability of \eqn{\Delta\Delta QTc}
#'   exceeding `threshold`
#'
#' With `output_type = "draws"`, it has one row per concentration and draw,
#' with `.draw` and `.ddqtc` columns.
#' @inherit dev_ermod_cqt examples
#'
sim_cqt_ddqtc <- function(
    ermod,
    exposure_to_sim_vec,
    threshold = 10,
    qi_width = 0.9,
    output_type = c("median_qi", "draws"),
    n_draws_sim = NULL,
    seed_sample_draws = NULL) {
  stopifnot(inherits(ermod, "ermod_cqt"))
  stopifnot(is.numeric(exposure_to_sim_vec), length(exposure_to_sim_vec) > 0)
  output_type <- match.arg(output_type)

  var_exposure <- extract_var_exposure(ermod)
  mod <- extract_mod(ermod)

  # Draws of the linear predictor for active (at each concentration) and
  # placebo (concentration of 0), with the other covariates fixed at the
  # same values
  df_ref <- extract_data(ermod)[1, , drop = FALSE]
  n_exp <- length(exposure_to_sim_vec)
  df_active <- df_ref[rep(1, n_exp), , drop = FALSE]
  df_active[[var_exposure]] <- unname(exposure_to_sim_vec)
  df_placebo <- df_ref
  df_placebo[[var_exposure]] <- 0

  spec_cqt <- ermod$spec_cqt
  if (!is.null(spec_cqt$var_trt_model)) {
    df_active[[spec_cqt$var_trt_model]] <- 1
    df_placebo[[spec_cqt$var_trt_model]] <- 0
  }

  # Single call so that the same draws are used for active and placebo
  mat_linpred <- rstantools::posterior_linpred(
    mod,
    newdata = dplyr::bind_rows(df_active, df_placebo), re.form = NA
  )
  mat_ddqtc <-
    mat_linpred[, seq_len(n_exp), drop = FALSE] - mat_linpred[, n_exp + 1]

  n_draws_sim <- chech_ndraws(mod, n_draws_sim)
  if (n_draws_sim < nrow(mat_ddqtc)) {
    if (!is.null(seed_sample_draws)) set.seed(seed_sample_draws)
    idx_draws <- sort(sample.int(nrow(mat_ddqtc), n_draws_sim))
    mat_ddqtc <- mat_ddqtc[idx_draws, , drop = FALSE]
  }

  df_exposure <- dplyr::tibble(!!var_exposure := unname(exposure_to_sim_vec))
  if (!is.null(names(exposure_to_sim_vec))) {
    df_exposure <- dplyr::tibble(.label = names(exposure_to_sim_vec)) |>
      dplyr::bind_cols(df_exposure)
  }

  draws <-
    df_exposure |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    dplyr::left_join(
      dplyr::tibble(
        .row = rep(seq_len(n_exp), each = nrow(mat_ddqtc)),
        .draw = rep(seq_len(nrow(mat_ddqtc)), times = n_exp),
        .ddqtc = as.vector(mat_ddqtc)
      ),
      by = ".row"
    )

  if (output_type == "draws") {
    return(draws |> dplyr::select(-".row"))
  }

  prob_exceed <-
    draws |>
    dplyr::summarize(
      .prob_exceed = mean(.data$.ddqtc > threshold),
      .by = ".row"
    )

  draws |>
    dplyr::select(-".draw") |>
    dplyr::group_by(dplyr::across(!".ddqtc")) |>
    ggdist::median_qi(.width = qi_width) |>
    dplyr::ungroup() |>
    dplyr::left_join(prob_exceed, by = ".row") |>
    # Keep the order of exposure_to_sim_vec
    dplyr::arrange(.data$.row) |>
    dplyr::select(-dplyr::any_of(c(".row", ".point", ".interval")))
}


#' Goodness-of-fit plot for concentration-QTc model
#'
#' Plot the model-predicted placebo-adjusted change from baseline QTc
#' (\eqn{\Delta\Delta QTc}) against concentration, overlaid with the observed
#' \eqn{\Delta\Delta QTc} summarized in concentration bins (e.g. deciles).
#' The observed \eqn{\Delta\Delta QTc} is calculated for active treatment
#' by subtracting the mean \eqn{\Delta QTc} of placebo at the matching
#' nominal time (if `var_time` was specified in [dev_ermod_cqt()]; otherwise
#' the overall placebo mean). If there is no placebo data, the observed
#' \eqn{\Delta QTc} is shown instead.
#'
#' @export
#' @inheritParams sim_cqt_ddqtc
#' @param n_bins Number of concentration bins (quantiles) for the observed
#' data summary. Default is 10 (deciles).
#' @param qi_width_obs Width of the confidence interval for the mean of
#' observed \eqn{\Delta\Delta QTc} in each bin. Default is 0.9.
#' @param qi_width_sim Width of the credible interval for the model
#' prediction. Default is 0.9.
#' @param threshold Value to draw a horizontal reference line. Default is 10
#' (ms). Set to `NULL` to omit.
#' @param exposure_to_mark Named or unnamed vector of concentrations (e.g.
#' geometric mean Cmax at therapeutic and supratherapeutic doses) at which
#' the model-predicted \eqn{\Delta\Delta QTc} is highlighted with point
#' ranges. Default is `NULL` (not shown).
#' @param show_obs_points Logical, whether to show individual observed
#' \eqn{\Delta\Delta QTc} values. Default is `FALSE`.
#' @param show_caption Logical, whether to show the caption note for the plot.
#' Default is `TRUE`.
#' @param num_exposures Number of concentration values to simulate for the
#' model prediction line.
#'
#' @return A ggplot object
#' @inherit dev_ermod_cqt examples
#'
plot_cqt_gof <- function(
    ermod,
    n_bins = 10,
    qi_width_obs = 0.9,
    qi_width_sim = 0.9,
    threshold = 10,
    exposure_to_mark = NULL,
    show_obs_points = FALSE,
    show_caption = TRUE,
    num_exposures = 51,
    n_draws_sim = NULL,
    seed_sample_draws = NULL) {
  stopifnot(inherits(ermod, "ermod_cqt"))

  var_exposure <- extract_var_exposure(ermod)
  has_placebo <- !is.null(ermod$spec_cqt$var_trt_model)
  label_y <- label_y_cqt(has_placebo)

  # Observed data
  d_obs <- calc_obs_ddqtc(ermod)
  d_obs_bin <- summarize_obs_bin_cqt(d_obs, var_exposure, n_bins, qi_width_obs)

  # Model prediction
  exposure_max <- max(d_obs[[var_exposure]], exposure_to_mark)
  d_sim <- sim_cqt_ddqtc(
    ermod,
    exposure_to_sim_vec = seq(0, exposure_max, length.out = num_exposures),
    threshold = if (is.null(threshold)) 10 else threshold,
    qi_width = qi_width_sim,
    n_draws_sim = n_draws_sim, seed_sample_draws = seed_sample_draws
  )

  gg <-
    ggplot2::ggplot(
      data = d_sim,
      ggplot2::aes(x = .data[[var_exposure]], y = .data$.ddqtc)
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "grey50")

  if (!is.null(threshold)) {
    gg <- gg +
      ggplot2::geom_hline(yintercept = threshold, linetype = "dashed")
  }

  if (show_obs_points) {
    gg <- gg +
      ggplot2::geom_point(
        data = d_obs,
        ggplot2::aes(y = .data$.ddqtc_obs),
        alpha = 0.2, color = "grey40"
      )
  }

  gg <- gg +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$.lower, ymax = .data$.upper),
      alpha = 0.3
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_pointrange(
      data = d_obs_bin,
      ggplot2::aes(
        x = .data$.exposure_median, y = .data$.mean,
        ymin = .data$.mean_lower, ymax = .data$.mean_upper
      ),
      shape = 0, size = 0.5
    ) +
    ggplot2::labs(x = var_exposure, y = label_y)

  if (!is.null(exposure_to_mark)) {
    d_mark <- sim_cqt_ddqtc(
      ermod,
      exposure_to_sim_vec = exposure_to_mark,
      qi_width = qi_width_sim,
      n_draws_sim = n_draws_sim, seed_sample_draws = seed_sample_draws
    )
    if (!".label" %in% names(d_mark)) {
      d_mark$.label <- format(d_mark[[var_exposure]], digits = 3)
    }
    d_mark$.label <- factor(d_mark$.label, levels = unique(d_mark$.label))

    gg <- gg +
      ggplot2::geom_pointrange(
        data = d_mark,
        ggplot2::aes(
          ymin = .data$.lower, ymax = .data$.upper, color = .data$.label
        ),
        shape = 17, size = 0.7, linewidth = 1
      ) +
      ggplot2::labs(color = NULL)
  }

  if (show_caption) {
    lines <- c(
      paste0(
        "Line/Shade: Median/", qi_width_sim * 100, "% CI of model prediction"
      ),
      paste0(
        "Squares/bars: Mean/", qi_width_obs * 100, "% CI of observed data in ",
        n_bins, " conc. bins"
      )
    )
    if (has_placebo) {
      lines <- c(lines, "  (observed data: time-matched placebo-adjusted)")
    }
    if (!is.null(exposure_to_mark)) {
      lines <- c(lines, paste0(
        "Triangles/bars: Median/", qi_width_sim * 100,
        "% CI of model prediction at specified conc."
      ))
    }
    gg <- gg +
      ggplot2::labs(caption = paste(lines, collapse = "\n")) +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(family = "mono", hjust = 0)
      )
  }

  gg
}


#' Hysteresis plot for concentration-QTc analysis
#'
#' Plot the mean observed placebo-adjusted change from baseline QTc
#' (\eqn{\Delta\Delta QTc}) against the mean concentration at each nominal
#' time point, connected in the order of time. A counter-clockwise loop
#' suggests a delay between the concentration and the QTc effect
#' (hysteresis), in which case the direct-effect model from [dev_ermod_cqt()]
#' may not be appropriate.
#'
#' @export
#' @param ermod An object of class `ermod_cqt`, from [dev_ermod_cqt()].
#' `var_time` needs to have been specified.
#' @param var_group Name of the variable to group the active treatment data
#' (e.g. dose) in character. Default is `NULL` (all active treatment data
#' pooled).
#' @param show_time_label Logical, whether to label each point with the
#' nominal time. Default is `TRUE`.
#'
#' @return A ggplot object
#' @examples
#' \donttest{
#' data(d_sim_cqt)
#'
#' ermod_cqt <- dev_ermod_cqt(
#'   data = d_sim_cqt,
#'   var_resp = "DQTCF",
#'   var_exposure = "CONC_1000",
#'   var_random = "ID",
#'   var_trt = "TRT",
#'   var_time = "NTIME",
#'   var_baseline = "QTCFBL",
#'   chains = 2,
#'   iter = 1000
#' )
#'
#' plot_cqt_hysteresis(ermod_cqt, var_group = "DOSE")
#' }
#'
plot_cqt_hysteresis <- function(
    ermod, var_group = NULL, show_time_label = TRUE) {
  stopifnot(inherits(ermod, "ermod_cqt"))

  var_time <- ermod$spec_cqt$var_time
  if (is.null(var_time)) {
    stop("`var_time` needs to be specified in `dev_ermod_cqt()`.")
  }
  var_exposure <- extract_var_exposure(ermod)
  has_placebo <- !is.null(ermod$spec_cqt$var_trt_model)
  label_y <- label_y_cqt(has_placebo)

  d_obs <- calc_obs_ddqtc(ermod)
  if (!is.null(var_group)) {
    check_columns_exist(d_obs, var_group, "group")
    d_obs[[var_group]] <- factor(d_obs[[var_group]])
  }

  d_mean <-
    d_obs |>
    dplyr::summarize(
      .exposure_mean = mean(.data[[var_exposure]]),
      .ddqtc_mean = mean(.data$.ddqtc_obs),
      .by = dplyr::all_of(c(var_group, var_time))
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(var_group, var_time))))

  aes_group <- if (is.null(var_group)) {
    ggplot2::aes()
  } else {
    ggplot2::aes(color = .data[[var_group]], group = .data[[var_group]])
  }

  gg <-
    ggplot2::ggplot(
      d_mean,
      ggplot2::aes(x = .data$.exposure_mean, y = .data$.ddqtc_mean)
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "grey50") +
    ggplot2::geom_path(
      aes_group,
      arrow = grid::arrow(length = grid::unit(0.1, "inches"), type = "closed")
    ) +
    ggplot2::geom_point(aes_group) +
    ggplot2::labs(
      x = paste0(var_exposure, " (mean)"),
      y = bquote(.(label_y) ~ "(mean)")
    )

  if (show_time_label) {
    gg <- gg +
      ggplot2::geom_text(
        ggplot2::aes(label = .data[[var_time]]),
        vjust = -0.8, size = 3, show.legend = FALSE
      )
  }

  gg
}


# Internal functions ----------------------------------------------------------

# Axis label as plotmath expression, so that it can be rendered in any locale
# and graphics device (e.g. pdf() can not render the unicode Delta character)
label_y_cqt <- function(has_placebo) {
  if (has_placebo) quote(Delta * Delta * QTc) else quote(Delta * QTc)
}

new_ermod_cqt <- function(ermod, spec_cqt) {
  stopifnot(inherits(ermod, "ermod_lme"))
  ermod$spec_cqt <- spec_cqt
  class(ermod) <- c("ermod_cqt", class(ermod))
  ermod
}

# Specification of the C-QT model terms and derived columns
build_spec_cqt <- function(
    data, var_exposure, var_trt = NULL, trt_placebo = NULL,
    var_time = NULL, var_baseline = NULL) {
  spec <- list(
    var_trt = var_trt, trt_placebo = NULL, var_trt_model = NULL,
    var_time = var_time, var_time_model = NULL,
    var_baseline = var_baseline, var_baseline_model = NULL,
    baseline_mean = NULL
  )

  if (!is.null(var_trt)) {
    x_trt <- data[[var_trt]]
    if (is.null(trt_placebo)) {
      trt_placebo <- if (is.numeric(x_trt) || is.logical(x_trt)) 0 else "Placebo"
    }
    if (!any(x_trt == trt_placebo)) {
      stop(
        "No placebo data found: `", var_trt, "` does not contain `",
        trt_placebo, "`. Specify the placebo value with `trt_placebo`."
      )
    }
    if (all(x_trt == trt_placebo)) {
      stop("No active treatment data found in `", var_trt, "`.")
    }
    if (any(data[[var_exposure]][x_trt == trt_placebo] != 0)) {
      warning(
        "Non-zero concentrations found for placebo. ",
        "Concentrations should be 0 for placebo."
      )
    }
    spec$trt_placebo <- trt_placebo
    spec$var_trt_model <- paste0(var_trt, "_active")
  }

  if (!is.null(var_time)) {
    spec$var_time_model <- paste0(var_time, "_fct")
    spec$time_levels <- sort(unique(data[[var_time]]))
  }

  if (!is.null(var_baseline)) {
    spec$var_baseline_model <- paste0(var_baseline, "_cent")
    spec$baseline_mean <- mean(data[[var_baseline]])
  }

  spec
}

add_cols_cqt <- function(data, spec_cqt) {
  if (!is.null(spec_cqt$var_trt_model)) {
    data[[spec_cqt$var_trt_model]] <-
      as.numeric(data[[spec_cqt$var_trt]] != spec_cqt$trt_placebo)
  }
  if (!is.null(spec_cqt$var_time_model)) {
    data[[spec_cqt$var_time_model]] <-
      factor(data[[spec_cqt$var_time]], levels = spec_cqt$time_levels)
  }
  if (!is.null(spec_cqt$var_baseline_model)) {
    data[[spec_cqt$var_baseline_model]] <-
      data[[spec_cqt$var_baseline]] - spec_cqt$baseline_mean
  }
  data
}

# Observed (time-matched placebo-adjusted) ddQTc for active treatment
# Returns the active treatment data with `.ddqtc_obs` column
calc_obs_ddqtc <- function(ermod) {
  data <- extract_data(ermod)
  var_resp <- extract_var_resp(ermod)
  spec_cqt <- ermod$spec_cqt

  if (is.null(spec_cqt$var_trt_model)) {
    data$.ddqtc_obs <- data[[var_resp]]
    return(data)
  }

  is_active <- data[[spec_cqt$var_trt_model]] == 1
  d_placebo <- data[!is_active, , drop = FALSE]
  d_active <- data[is_active, , drop = FALSE]

  var_time <- spec_cqt$var_time
  if (is.null(var_time)) {
    d_active$.ddqtc_obs <- d_active[[var_resp]] - mean(d_placebo[[var_resp]])
    return(d_active)
  }

  d_placebo_mean <-
    d_placebo |>
    dplyr::summarize(
      .placebo_mean = mean(.data[[var_resp]]),
      .by = dplyr::all_of(var_time)
    )

  d_active <- dplyr::left_join(d_active, d_placebo_mean, by = var_time)
  if (anyNA(d_active$.placebo_mean)) {
    stop(
      "Placebo data are not available for some of the `", var_time,
      "` values in the active treatment data, so time-matched placebo ",
      "adjustment is not possible."
    )
  }

  d_active |>
    dplyr::mutate(.ddqtc_obs = .data[[var_resp]] - .data$.placebo_mean) |>
    dplyr::select(-".placebo_mean")
}

# Summarize observed ddQTc by concentration bins (quantiles)
summarize_obs_bin_cqt <- function(d_obs, var_exposure, n_bins, qi_width) {
  breaks <- stats::quantile(
    d_obs[[var_exposure]],
    probs = seq(0, 1, length.out = n_bins + 1)
  ) |>
    unique()

  d_obs |>
    dplyr::mutate(
      .bin = cut(.data[[var_exposure]], breaks = breaks, include.lowest = TRUE)
    ) |>
    dplyr::summarize(
      .exposure_median = stats::median(.data[[var_exposure]]),
      .n = dplyr::n(),
      .mean = mean(.data$.ddqtc_obs),
      .se = stats::sd(.data$.ddqtc_obs) / sqrt(dplyr::n()),
      .by = ".bin"
    ) |>
    dplyr::mutate(
      .t = stats::qt(0.5 + qi_width / 2, df = pmax(.data$.n - 1, 1)),
      .mean_lower = .data$.mean - .data$.t * .data$.se,
      .mean_upper = .data$.mean + .data$.t * .data$.se
    ) |>
    dplyr::select(-".t") |>
    dplyr::arrange(.data$.bin)
}
