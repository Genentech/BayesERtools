data(d_sim_cqt)

set.seed(1234)
ermod_cqt <- suppressWarnings(dev_ermod_cqt(
  data = d_sim_cqt,
  var_resp = "DQTCF",
  var_exposure = "CONC_1000",
  var_random = "ID",
  var_trt = "TRT",
  var_time = "NTIME",
  var_baseline = "QTCFBL",
  verbosity_level = 0,
  chains = 1,
  iter = 400
))

# Model without placebo data and without time effect
d_active <- d_sim_cqt |> dplyr::filter(TRT == "Active")
ermod_cqt_no_pbo <- suppressWarnings(dev_ermod_cqt(
  data = d_active,
  var_resp = "DQTCF",
  var_exposure = "CONC_1000",
  var_random = "ID",
  random_effect_type = "intercept",
  verbosity_level = 0,
  chains = 1,
  iter = 400
))


# dev_ermod_cqt ---------------------------------------------------------------

test_that("dev_ermod_cqt returns ermod_cqt object", {
  expect_s3_class(ermod_cqt, c("ermod_cqt", "ermod_lme", "ermod"), exact = TRUE)
  expect_equal(
    extract_var_cov(ermod_cqt),
    c("TRT_active", "NTIME_fct", "QTCFBL_cent")
  )
  expect_equal(
    names(rstanarm::fixef(extract_mod(ermod_cqt)))[1:3],
    c("(Intercept)", "CONC_1000", "TRT_active")
  )

  data_mod <- extract_data(ermod_cqt)
  expect_equal(data_mod$TRT_active, as.numeric(d_sim_cqt$TRT == "Active"))
  expect_equal(levels(data_mod$NTIME_fct), as.character(sort(unique(d_sim_cqt$NTIME))))
  expect_equal(mean(data_mod$QTCFBL_cent), 0)
  expect_equal(ermod_cqt$spec_cqt$trt_placebo, "Placebo")
  expect_equal(ermod_cqt$spec_cqt$baseline_mean, mean(d_sim_cqt$QTCFBL))

  expect_equal(
    get_mod_type_name(ermod_cqt),
    "Concentration-QTc linear mixed-effects model"
  )
  out <- cli::cli_fmt(print(ermod_cqt))
  expect_true(any(grepl("sim_cqt_ddqtc", out)))

  expect_null(extract_var_cov(ermod_cqt_no_pbo))
  expect_null(ermod_cqt_no_pbo$spec_cqt$var_trt_model)
})

test_that("build_spec_cqt handles treatment variable", {
  d <- d_sim_cqt |> dplyr::mutate(TRTN = as.numeric(TRT == "Active"))
  spec <- build_spec_cqt(d, "CONC_1000", var_trt = "TRTN")
  expect_equal(spec$trt_placebo, 0)
  expect_equal(spec$var_trt_model, "TRTN_active")

  d_lgl <- d |> dplyr::mutate(TRTL = TRTN == 1)
  spec_lgl <- build_spec_cqt(d_lgl, "CONC_1000", var_trt = "TRTL")
  expect_equal(add_cols_cqt(d_lgl, spec_lgl)$TRTL_active, d$TRTN)

  # Reversed placebo/active; concentrations of the "placebo" are non-zero
  expect_warning(
    spec_pbo <- build_spec_cqt(d, "CONC_1000", var_trt = "TRT", trt_placebo = "Active"),
    "Non-zero concentrations found for placebo"
  )
  expect_equal(add_cols_cqt(d, spec_pbo)$TRT_active, 1 - d$TRTN)

  expect_error(
    build_spec_cqt(d, "CONC_1000", var_trt = "TRT", trt_placebo = "PBO"),
    "No placebo data found"
  )
  expect_error(
    build_spec_cqt(d_active, "CONC_1000", var_trt = "TRT"),
    "No placebo data found"
  )
  expect_error(
    build_spec_cqt(
      dplyr::filter(d, TRT == "Placebo"), "CONC_1000",
      var_trt = "TRT"
    ),
    "No active treatment data found"
  )
  d_bad <- d
  d_bad$CONC_1000[d_bad$TRT == "Placebo"][1] <- 1
  expect_warning(
    build_spec_cqt(d_bad, "CONC_1000", var_trt = "TRT"),
    "Non-zero concentrations found for placebo"
  )
})

test_that("dev_ermod_cqt input checks", {
  expect_error(
    dev_ermod_cqt(d_sim_cqt, "DQTCF", "CONC_1000", "ID", var_trt = "ARM"),
    '"ARM" columns should be in data'
  )
})


# sim_cqt_ddqtc ---------------------------------------------------------------

test_that("sim_cqt_ddqtc equals treatment effect + slope * concentration", {
  conc <- c("200 mg" = 0.95, "800 mg" = 4.52)
  draws <- sim_cqt_ddqtc(ermod_cqt, conc, output_type = "draws")
  expect_equal(names(draws), c(".label", "CONC_1000", ".draw", ".ddqtc"))
  expect_equal(nrow(draws), 2 * 200)

  draws_param <- posterior::as_draws_df(ermod_cqt)
  for (i in seq_along(conc)) {
    expect_equal(
      draws$.ddqtc[draws$.label == names(conc)[i]],
      draws_param$TRT_active + draws_param$CONC_1000 * conc[[i]],
      ignore_attr = TRUE
    )
  }

  med_qi <- sim_cqt_ddqtc(ermod_cqt, conc, threshold = 5, qi_width = 0.8)
  expect_equal(
    names(med_qi),
    c(".label", "CONC_1000", ".ddqtc", ".lower", ".upper", ".width", ".prob_exceed")
  )
  expect_equal(med_qi$.label, names(conc))
  expect_equal(med_qi$.width, c(0.8, 0.8))
  ddqtc_800 <- draws$.ddqtc[draws$.label == "800 mg"]
  expect_equal(med_qi$.prob_exceed[2], mean(ddqtc_800 > 5))
  expect_equal(med_qi$.ddqtc[2], stats::median(ddqtc_800))
  expect_equal(
    med_qi$.upper[2],
    unname(stats::quantile(ddqtc_800, 0.9))
  )

  # Order of the input is kept
  med_qi_order <- sim_cqt_ddqtc(ermod_cqt, c("b" = 2, "c" = 3, "a" = 1))
  expect_equal(med_qi_order$.label, c("b", "c", "a"))
  expect_equal(med_qi_order$CONC_1000, c(2, 3, 1))

  # Unnamed vector and subsampled draws
  med_qi_unnamed <- sim_cqt_ddqtc(ermod_cqt, c(1, 2, 3), n_draws_sim = 50)
  expect_false(".label" %in% names(med_qi_unnamed))
  expect_equal(nrow(med_qi_unnamed), 3)
  draws_sub <- sim_cqt_ddqtc(
    ermod_cqt, 1,
    n_draws_sim = 50, output_type = "draws", seed_sample_draws = 1
  )
  expect_equal(nrow(draws_sub), 50)

  # Without placebo, ddQTc equals slope * concentration
  draws_no_pbo <- sim_cqt_ddqtc(ermod_cqt_no_pbo, 2, output_type = "draws")
  expect_equal(
    draws_no_pbo$.ddqtc,
    posterior::as_draws_df(ermod_cqt_no_pbo)$CONC_1000 * 2,
    ignore_attr = TRUE
  )

  expect_error(sim_cqt_ddqtc(ermod_cqt_no_pbo$mod, 1))
})


# Observed ddQTc and plots ------------------------------------------------------

test_that("calc_obs_ddqtc performs time-matched placebo adjustment", {
  d_obs <- calc_obs_ddqtc(ermod_cqt)
  expect_equal(nrow(d_obs), sum(d_sim_cqt$TRT == "Active"))
  expect_true(all(d_obs$TRT == "Active"))

  pbo_mean_05 <- mean(d_sim_cqt$DQTCF[d_sim_cqt$TRT == "Placebo" & d_sim_cqt$NTIME == 0.5])
  row_1 <- d_obs[d_obs$ID == 1 & d_obs$NTIME == 0.5, ]
  expect_equal(row_1$.ddqtc_obs, row_1$DQTCF - pbo_mean_05)

  # No placebo: observed dQTc is used
  d_obs_no_pbo <- calc_obs_ddqtc(ermod_cqt_no_pbo)
  expect_equal(d_obs_no_pbo$.ddqtc_obs, d_active$DQTCF)

  d_bin <- summarize_obs_bin_cqt(d_obs, "CONC_1000", n_bins = 4, qi_width = 0.9)
  expect_equal(nrow(d_bin), 4)
  expect_equal(sum(d_bin$.n), nrow(d_obs))
  expect_true(all(d_bin$.mean_lower < d_bin$.mean & d_bin$.mean < d_bin$.mean_upper))
})

test_that("plot_cqt_gof", {
  gg <- plot_cqt_gof(ermod_cqt, n_draws_sim = 100)
  expect_s3_class(gg, "ggplot")
  expect_no_error(ggplot2::ggplot_build(gg))

  gg_mark <- plot_cqt_gof(
    ermod_cqt,
    exposure_to_mark = c("200 mg" = 0.95, "800 mg" = 4.52),
    show_obs_points = TRUE, threshold = NULL, show_caption = FALSE,
    n_draws_sim = 100
  )
  expect_no_error(ggplot2::ggplot_build(gg_mark))
  expect_no_error(ggplot2::ggplot_build(
    plot_cqt_gof(ermod_cqt, exposure_to_mark = c(1, 2), n_draws_sim = 100)
  ))
  expect_equal(gg_mark$labels$y, quote(Delta * Delta * QTc))

  gg_no_pbo <- plot_cqt_gof(ermod_cqt_no_pbo, n_draws_sim = 100)
  expect_equal(gg_no_pbo$labels$y, quote(Delta * QTc))

  # Plots can be rendered on a pdf device (no unicode characters)
  f_pdf <- tempfile(fileext = ".pdf")
  grDevices::pdf(f_pdf)
  expect_no_error(print(gg_mark))
  expect_no_error(print(gg_no_pbo))
  grDevices::dev.off()
  unlink(f_pdf)
})

test_that("general ermod_lme functions work with ermod_cqt", {
  ersim <- sim_er(ermod_cqt, n_draws_sim = 20, re_type = "existing_subject")
  expect_equal(nrow(ersim), nrow(d_sim_cqt) * 20)

  skip_if_not(.if_run_ex_eval_mod())
  kfold_cqt <- suppressWarnings(kfold(ermod_cqt, k = 2, seed = 1))
  expect_s3_class(kfold_cqt, "kfold_ermod")
})
