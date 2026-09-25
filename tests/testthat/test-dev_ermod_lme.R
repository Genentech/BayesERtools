data(d_sim_cqt)

# Use a subset of subjects to make the tests run faster
d_lme <- d_sim_cqt |> dplyr::filter(ID %in% c(1:24, 31:32, 39:40, 47:48))

set.seed(1234)
ermod_lme <- suppressWarnings(dev_ermod_lme(
  data = d_lme,
  var_resp = "DQTCF",
  var_exposure = "CONC_1000",
  var_cov = "QTCFBL",
  var_random = "ID",
  verbosity_level = 0,
  chains = 1,
  iter = 400
))

ermod_lme_int <- suppressWarnings(dev_ermod_lme(
  data = d_lme,
  var_resp = "DQTCF",
  var_exposure = "CONC_1000",
  var_random = "ID",
  random_effect_type = "intercept",
  verbosity_level = 0,
  chains = 1,
  iter = 400
))

ermod_lme_exp_sel <- suppressWarnings(dev_ermod_lme_exp_sel(
  data = d_lme,
  var_resp = "DQTCF",
  var_exp_candidates = c("CONC_1000", "MCONC_1000"),
  var_random = "ID",
  random_effect_type = "intercept",
  verbosity_level = 0,
  chains = 1,
  iter = 400
))


# dev_ermod_lme ---------------------------------------------------------------

test_that("dev_ermod_lme returns ermod_lme object", {
  expect_s3_class(ermod_lme, c("ermod_lme", "ermod"), exact = TRUE)
  expect_equal(ermod_lme$var_random, "ID")
  expect_equal(ermod_lme$random_effect_type, "both")
  expect_equal(ermod_lme$endpoint_type, "continuous")
  expect_equal(extract_var_random(ermod_lme), "ID")
  expect_equal(extract_var_cov(ermod_lme), "QTCFBL")
  expect_equal(length(ermod_lme$coef_exp_draws), 200)
  expect_equal(
    extract_mod(ermod_lme)$glmod$reTrms$cnms$ID,
    c("(Intercept)", "CONC_1000")
  )
  expect_equal(extract_mod(ermod_lme_int)$glmod$reTrms$cnms$ID, "(Intercept)")

  # adapt_delta default (0.99) is passed to Stan and kept for kfold refits
  expect_equal(
    extract_mod(ermod_lme)$stanfit@stan_args[[1]]$control$adapt_delta, 0.99
  )
  expect_equal(ermod_lme$input_args$adapt_delta, 0.99)
})

test_that("build_formula_lme builds random effects terms", {
  expect_equal(
    deparse(build_formula_lme("y", c("x", "z"), "x", "ID", "both")),
    "y ~ x + z + (1 + x | ID)"
  )
  expect_equal(
    deparse(build_formula_lme("y", "x", "x", "ID", "intercept")),
    "y ~ x + (1 | ID)"
  )
  expect_equal(
    deparse(build_formula_lme("y", "x", "x", "ID", "slope")),
    "y ~ x + (0 + x | ID)"
  )
})

test_that("dev_ermod_lme input checks", {
  expect_error(
    dev_ermod_lme(d_lme, "DQTCF", "CONC_1000", var_random = "SUBJ"),
    '"SUBJ" columns should be in data'
  )
  d_na <- d_lme
  d_na$ID[1] <- NA
  expect_error(
    dev_ermod_lme(d_na, "DQTCF", "CONC_1000", var_random = "ID"),
    "NA values not allowed in the random effect grouping"
  )
  expect_warning(
    check_exposure_scale_lme(d_lme$CONC, "CONC"),
    "Consider rescaling the exposure"
  )
  expect_no_warning(check_exposure_scale_lme(d_lme$CONC_1000, "CONC_1000"))
})

test_that("S3 methods for ermod_lme", {
  out <- cli::cli_fmt(print(ermod_lme))
  expect_true(any(grepl("Linear mixed-effects ER model", out)))
  expect_true(any(grepl("Error terms", out)))
  expect_equal(names(coef(ermod_lme)), c("(Intercept)", "CONC_1000", "QTCFBL"))
  expect_s3_class(summary(ermod_lme), "summary.stanreg")
  expect_s3_class(prior_summary(ermod_lme), "prior_summary.stanreg")
  expect_equal(names(extract_coef_exp_ci(ermod_lme)), c(".lower", ".upper"))
  # Short test fit, so Pareto k warnings are expected
  expect_s3_class(suppressWarnings(loo(ermod_lme)), "loo")
  expect_equal(get_mod_type_name(ermod_lme), "Linear mixed-effects ER model")
})

test_that("dev_ermod_lme_exp_sel", {
  expect_s3_class(
    ermod_lme_exp_sel,
    c("ermod_lme_exp_sel", "ermod_exp_sel", "ermod_lme", "ermod"),
    exact = TRUE
  )
  expect_equal(ermod_lme_exp_sel$var_exposure, "CONC_1000")
  expect_equal(ermod_lme_exp_sel$var_random, "ID")
  expect_null(extract_var_cov(ermod_lme_exp_sel))
  expect_equal(
    dim(extract_coef_exp_ci(ermod_lme_exp_sel, exp_candidates = TRUE)),
    c(2, 2)
  )
  out <- cli::cli_fmt(print(ermod_lme_exp_sel))
  expect_true(any(grepl("exposure metric selection", out)))
  expect_s3_class(plot_er_exp_sel(ermod_lme_exp_sel), "ggplot")
})


# sim_er with re_type ---------------------------------------------------------

test_that("sim_er population-level prediction does not need ID", {
  newdata <- dplyr::tibble(CONC_1000 = c(0, 1), QTCFBL = 400)
  ersim <- sim_er(ermod_lme, newdata = newdata, n_draws_sim = 50)
  expect_s3_class(ersim, "ersim")
  expect_equal(attr(ersim, "re_type"), "population")
  expect_equal(attr(ersim, "var_random"), "ID")
  expect_equal(nrow(ersim), 100)
  expect_equal(ersim$.epred, ersim$.linpred)

  # Same as rstanarm prediction without random effects
  draws <- posterior::as_draws_df(ermod_lme)
  epred_manual <- draws$`(Intercept)` + draws$CONC_1000 + draws$QTCFBL * 400
  ersim_all <- sim_er(ermod_lme, newdata = newdata[2, ])
  expect_equal(sort(ersim_all$.epred), sort(epred_manual))
})

test_that("sim_er with re_type = existing_subject", {
  newdata <- d_lme |> dplyr::filter(ID %in% c(1, 2))
  ersim <- sim_er(
    ermod_lme,
    newdata = newdata, re_type = "existing_subject", seed_sample_draws = 1
  )
  expect_equal(attr(ersim, "re_type"), "existing_subject")

  epred_ref <- rstantools::posterior_epred(
    extract_mod(ermod_lme),
    newdata = newdata
  )
  expect_equal(
    ersim |> dplyr::summarize(m = mean(.epred), .by = .row) |> dplyr::pull(m),
    unname(colMeans(epred_ref))
  )

  # Individual predictions are closer to the data than population predictions
  rmse <- function(x) {
    x |>
      dplyr::summarize(e = mean(.epred), y = DQTCF[1], .by = .row) |>
      dplyr::summarize(sqrt(mean((e - y)^2))) |>
      dplyr::pull()
  }
  ersim_pop <- sim_er(ermod_lme, newdata = newdata)
  expect_lt(rmse(ersim), rmse(ersim_pop))

  expect_error(
    sim_er(
      ermod_lme,
      newdata = dplyr::mutate(newdata, ID = 999),
      re_type = "existing_subject"
    ),
    "Not found: 999"
  )
  expect_error(
    sim_er(
      ermod_lme,
      newdata = dplyr::select(newdata, -ID),
      re_type = "existing_subject"
    ),
    '"ID" columns should be in data'
  )
})

test_that("sim_er with re_type = new_subject", {
  # Two new subjects, three concentrations each
  newdata <- tidyr::expand_grid(
    ID = c("new1", "new2"),
    CONC_1000 = c(0, 1, 2),
    QTCFBL = 400
  )
  ersim <- sim_er(
    ermod_lme,
    newdata = newdata, re_type = "new_subject", seed_sample_draws = 1
  )
  expect_equal(attr(ersim, "re_type"), "new_subject")
  expect_equal(nrow(ersim), 6 * 200)
  expect_equal(ersim$.epred, ersim$.linpred)

  mat_epred <-
    ersim |>
    dplyr::select(.draw, .row, .epred) |>
    tidyr::pivot_wider(names_from = .row, values_from = .epred) |>
    dplyr::select(-.draw) |>
    as.matrix()

  # Random effects are shared within a subject: linear in concentration
  expect_equal(
    mat_epred[, 3] - mat_epred[, 2],
    mat_epred[, 2] - mat_epred[, 1]
  )
  # ... but independent between subjects
  expect_false(isTRUE(all.equal(mat_epred[, 1], mat_epred[, 4])))
  expect_lt(abs(stats::cor(mat_epred[, 1], mat_epred[, 4])), 0.5)

  # Between-subject variability adds to the population-level uncertainty
  ersim_pop <- sim_er(ermod_lme, newdata = newdata, seed_sample_draws = 1)
  expect_gt(stats::sd(ersim$.epred), stats::sd(ersim_pop$.epred))

  # Reproducible with the same seed
  ersim_2 <- sim_er(
    ermod_lme,
    newdata = newdata, re_type = "new_subject", seed_sample_draws = 1
  )
  expect_equal(ersim$.prediction, ersim_2$.prediction)

  # Existing IDs are treated as new subjects
  newdata_exist <- d_lme |> dplyr::filter(ID == 1)
  expect_no_error(sim_er(
    ermod_lme,
    newdata = newdata_exist, re_type = "new_subject", n_draws_sim = 10
  ))
})

test_that("mat_sqrt_psd works for singular matrix", {
  x <- matrix(c(4, 2, 2, 1), 2)
  r <- mat_sqrt_psd(x)
  expect_equal(t(r) %*% r, x)
  x2 <- matrix(c(4, 1, 1, 2), 2)
  r2 <- mat_sqrt_psd(x2)
  expect_equal(t(r2) %*% r2, x2)
})

test_that("sim_er_new_exp and sim_er_curve with re_type", {
  ersim_curve <- sim_er_curve(
    ermod_lme_int,
    num_exposures = 5,
    data_cov = dplyr::tibble(ID = 1:3),
    n_draws_sim = 20,
    re_type = "new_subject",
    output_type = "median_qi"
  )
  expect_s3_class(ersim_curve, "ersim_med_qi")
  expect_equal(nrow(ersim_curve), 15)
  expect_equal(attr(ersim_curve, "re_type"), "new_subject")

  expect_error(
    sim_er_curve(ermod_lme_int, re_type = "new_subject"),
    "data_cov with the random effect grouping variable `ID` must be supplied"
  )

  ersim_curve_pop <- sim_er_curve(
    ermod_lme_int,
    num_exposures = 5, n_draws_sim = 20
  )
  expect_equal(nrow(ersim_curve_pop), 5 * 20)

  ersim_draws <- sim_er_curve(
    ermod_lme_int,
    num_exposures = 5, data_cov = dplyr::tibble(ID = 1:2),
    n_draws_sim = 20, re_type = "new_subject"
  )
  expect_equal(
    attr(calc_ersim_med_qi(ersim_draws), "re_type"), "new_subject"
  )
})

test_that("re_type other than population errors for non-lme models", {
  data(d_sim_lin)
  ermod_lin <- suppressWarnings(dev_ermod_lin(
    d_sim_lin, "response", "AUCss",
    verbosity_level = 0, chains = 1, iter = 200
  ))
  expect_error(
    sim_er(ermod_lin, re_type = "new_subject"),
    "only available for mixed-effects models"
  )
})


# Post-processing ------------------------------------------------------------

test_that("plot_er and marginal simulation for ermod_lme", {
  expect_s3_class(
    plot_er(ermod_lme_int, show_orig_data = TRUE, n_draws_sim = 50),
    "ggplot"
  )
  expect_s3_class(
    plot_er(ermod_lme, marginal = TRUE, n_draws_sim = 50),
    "ggplot"
  )
  expect_s3_class(
    plot_er_gof(ermod_lme_int, show_coef_exp = TRUE),
    "ggplot"
  )
})

test_that("covariate effects for ermod_lme", {
  skip_if_not(.if_run_ex_coveff())
  coveffsim <- sim_coveff(ermod_lme)
  expect_s3_class(coveffsim, "coveffsim")
  expect_equal(attr(coveffsim, "model_type"), "linear")

  # Response difference at the 95th percentile of exposure vs median equals
  # slope times the difference in exposure (random effects cancel out)
  spec <- build_spec_coveff(ermod_lme)
  exp_values <- spec$value_cont[spec$var_name == "CONC_1000"]
  expected <- stats::median(ermod_lme$coef_exp_draws) *
    (exp_values[3] - exp_values[2])
  expect_equal(
    coveffsim$.response_diff[coveffsim$var_name == "CONC_1000"][3],
    expected,
    tolerance = 1e-6
  )

  expect_s3_class(plot_coveff(ermod_lme), "ggplot")
})

test_that("kfold for ermod_lme splits by subject", {
  skip_if_not(.if_run_ex_eval_mod())
  kfold_lme <- suppressWarnings(kfold(ermod_lme_int, k = 3, seed = 1))
  expect_s3_class(kfold_lme, "kfold_ermod")
  expect_equal(length(kfold_lme$l_ermod), 3)
  expect_s3_class(kfold_lme$l_ermod[[1]], "ermod_lme")
  expect_equal(nrow(kfold_lme$pointwise), nrow(d_lme))

  # Each subject is only in one fold
  d_fold <-
    kfold_lme$d_truth |>
    dplyr::ungroup() |>
    dplyr::mutate(ID = d_lme$ID[.row]) |>
    dplyr::distinct(ID, fold_id)
  expect_equal(nrow(d_fold), length(unique(d_lme$ID)))

  metrics <- suppressWarnings(
    eval_ermod(ermod_lme_int, eval_type = "kfold", k = 3, seed_kfold = 1)
  )
  # Metrics are calculated for each fold
  expect_equal(unique(metrics$.metric), c("rmse", "rsq", "rsq_trad"))
  expect_equal(nrow(metrics), 3 * 3)

  metrics_train <- eval_ermod(ermod_lme_int, eval_type = "training")
  expect_equal(nrow(metrics_train), 3)

  expect_error(kfold(ermod_lme_int, k = 100), "is smaller than k")
})
