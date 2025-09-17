# Read d_sim_binom_cov data, which is exported to the package
d_sim_binom_cov <- d_sim_binom_cov

id_to_sample <- seq(1, max(d_sim_binom_cov$ID), by = 1)
id_to_sample2 <- seq(1, max(d_sim_binom_cov$ID), by = 5)

df_er_ae_covsel_test <-
  d_sim_binom_cov |>
  dplyr::filter(
    AETYPE == "ae_covsel_test",
    ID %in% id_to_sample
  ) |>
  dplyr::mutate(
    AUCss_1000 = AUCss / 1000, BAGE_10 = BAGE / 10,
    BWT_10 = BWT / 10, BHBA1C_5 = BHBA1C / 5
  )

df_er_ae_covsel_test_small <-
  d_sim_binom_cov |>
  dplyr::filter(
    AETYPE == "ae_covsel_test",
    ID %in% id_to_sample2
  ) |>
  dplyr::mutate(
    AUCss_1000 = AUCss / 1000, BAGE_10 = BAGE / 10,
    BWT_10 = BWT / 10, BHBA1C_5 = BHBA1C / 5
  )

df_er_dr2 <-
  d_sim_binom_cov |>
  dplyr::filter(
    AETYPE == "dr2",
    ID %in% id_to_sample
  ) |>
  dplyr::mutate(
    AUCss_1000 = AUCss / 1000, BAGE_10 = BAGE / 10,
    BWT_10 = BWT / 10
  )

var_resp <- "AEFLAG"
var_exp_candidates <- c("AUCss_1000", "Cmaxss", "Cminss")
# Exclude BGLUC to reduce test time
var_cov_ae_covsel_test <- c("BAGE_10", "BWT_10", "BHBA1C_5", "RACE", "VISC")


# Data for normal linear model
data(d_sim_lin)

# dev_ermod_bin_exp_sel -------------------------------------------------
set.seed(1234)
ermod_bin_exp_sel <-
  suppressWarnings(dev_ermod_bin_exp_sel(
    data = df_er_ae_covsel_test_small,
    var_resp = var_resp,
    var_exp_candidates = var_exp_candidates,
    prior = rstanarm::normal(location = 0, scale = 5, autoscale = TRUE),
    prior_intercept =
      rstanarm::normal(location = 0, scale = 4, autoscale = TRUE),
    verbosity_level = 0,
    chains = 1,
    iter = 200
  ))

ermod_bin_exp_sel_other_rank <- ermod_bin_exp_sel
rownames(ermod_bin_exp_sel_other_rank$loo_comp_exposures) <-
  c("Cminss", "AUCss_1000", "Cmaxss")

ermod_bin_exp_sel_one_metric <-
  suppressWarnings(dev_ermod_bin_exp_sel(
    data = df_er_ae_covsel_test_small,
    var_resp = var_resp,
    var_exp_candidates = "AUCss_1000",
    verbosity_level = 0,
    chains = 1,
    iter = 200
  ))


# dev_ermod_bin_cov_sel steps ------------------------------------------
if (requireNamespace("projpred")) {
  refm_obj <- .dev_ermod_refmodel(
    data = df_er_ae_covsel_test,
    var_resp = var_resp,
    var_exposure = "AUCss_1000",
    var_cov_candidates = var_cov_ae_covsel_test,
    options_placebo_handling = list(use_placebo = TRUE),
    verbosity_level = 0,
    # Below option to make the test fast
    chains = 2, iter = 1000
  )

  var_selected <- .select_cov_projpred(
    refm_obj = refm_obj,
    var_exposure = "AUCss_1000",
    var_cov_candidates = var_cov_ae_covsel_test,
    # Set nterms_max to 4 to make the test fast
    nterms_max = 3,
    cv_method = "LOO",
    validate_search = FALSE,
    verbosity_level = 0
  )

  mod_final <- dev_ermod_bin(
    data = df_er_ae_covsel_test,
    var_resp = var_resp,
    var_exposure = "AUCss_1000",
    var_cov = "BHBA1C_5",
    verbosity_level = 0,
    # Below option to make the test fast
    chains = 2, iter = 1000
  )

  ## Test k-fold CV
  set.seed(1234)
  var_selected_kfold <-
    suppressMessages(.select_cov_projpred(
      refm_obj = refm_obj,
      var_exposure = "AUCss_1000",
      var_cov_candidates = var_cov_ae_covsel_test,
      # Set nterms_max to 3 to make the test fast
      nterms_max = 3,
      cv_method = "kfold",
      k = 2,
      validate_search = TRUE,
      verbosity_level = 0
    ))

  ermod_bin_cov_sel_kfold_dummy <- list(
    cvvs = attr(var_selected_kfold, "cvvs"),
    rk = attr(var_selected_kfold, "rk")
  )
  class(ermod_bin_cov_sel_kfold_dummy) <-
    c("ermod_bin_cov_sel", "ermod_cov_sel", "ermod_bin", "ermod")

  # dev_ermod_bin_cov_sel ------------------------------------------------
  prior_intercept <-
    rstanarm::normal(location = 0, scale = 4, autoscale = TRUE)
  set.seed(1234)
  ermod_bin_cov_sel <-
    dev_ermod_bin_cov_sel(
      data = df_er_ae_covsel_test,
      var_resp = var_resp,
      var_exposure = "AUCss_1000",
      var_cov_candidates = var_cov_ae_covsel_test,
      cv_method = "LOO",
      prior = rstanarm::normal(location = 0, scale = 5, autoscale = TRUE),
      prior_intercept = prior_intercept,
      nterms_max = 3,
      verbosity_level = 0,
      chains = 2,
      iter = 1000
    )
}

# lm -------------------------------------------------------------------
set.seed(1234)

ermod_lin <- dev_ermod_lin(
  data = d_sim_lin,
  var_resp = "response",
  var_exposure = "AUCss",
  var_cov = c("SEX", "BAGE"),
  prior = rstanarm::normal(location = 0, scale = 5, autoscale = TRUE),
  prior_aux = rstanarm::exponential(rate = 6, autoscale = TRUE),
  chains = 2,
  iter = 1000
)

ermod_lin_exp_sel <- suppressMessages(dev_ermod_lin_exp_sel(
  data = d_sim_lin,
  var_resp = "response",
  var_exp_candidates = c("AUCss", "Cmaxss"),
  prior = rstanarm::normal(location = 0, scale = 5, autoscale = TRUE),
  prior_aux = rstanarm::exponential(rate = 6, autoscale = TRUE),
  chains = 2,
  iter = 1000
))


if (requireNamespace("projpred")) {
  ermod_lin_cov_sel <- suppressMessages(suppressWarnings(dev_ermod_lin_cov_sel(
    data = d_sim_lin,
    var_resp = "response",
    var_exposure = "AUCss",
    var_cov_candidates = c("BAGE", "SEX"),
    nterms_max = 2,
    prior = rstanarm::normal(location = 0, scale = 5, autoscale = TRUE),
    prior_aux = rstanarm::exponential(rate = 6, autoscale = TRUE),
    verbosity_level = 1,
    chains = 2,
    iter = 1000
  )))
}

# Convert factor as response variable into number
data("mtcars")
mtcars$am2 <- factor(mtcars$am, labels = c("Automatic", "Manual"))
mtcars$am3 <- as.numeric(mtcars$am2) - 1

mod_mtcars <- dev_ermod_bin(
  data = mtcars,
  var_resp = "am3",
  var_exposure = "cyl",
  verbosity_level = 0,
  # Below option to make the test fast
  chains = 2, iter = 1000
)

# Test ------------------------------------------------------------------

test_that("Exposure metrics selection", {
  expect_equal(extract_var_exposure(ermod_bin_exp_sel), "AUCss_1000")
  expect_equal(
    extract_exp_sel_comp(ermod_bin_exp_sel)[, 1],
    c(AUCss_1000 = 0.000000, Cmaxss = -0.6490312, Cminss = -2.7563957)
  )

  priors <- prior_summary(ermod_bin_exp_sel$l_mod_exposures[[2]])
  expect_equal(priors$prior$scale, 5)
  expect_equal(priors$prior_intercept$scale, 4)

  if (.if_run_ex_plot_er()) {
    g1 <- plot_er_exp_sel(ermod_bin_exp_sel, n_draws_sim = 10)
    g2 <- plot_er_exp_sel(ermod_bin_exp_sel_other_rank, n_draws_sim = 10)

    expect_silent(plot(g1))

    expect_equal(
      levels(g1$data$.exp_met_fct), c("AUCss_1000", "Cmaxss", "Cminss")
    )
    expect_equal(
      levels(g2$data$.exp_met_fct), c("Cminss", "AUCss_1000", "Cmaxss")
    )
  }
})

test_that("Full model dev", {
  if (requireNamespace("projpred")) {
    expect_equal(coef(refm_obj$fit)[[1]], -12.407539)
  }
})

test_that("Variable selection", {
  if (requireNamespace("projpred")) {
    expect_equal(as.character(var_selected), c("AUCss_1000", "BHBA1C_5"))
    expect_equal(
      extract_var_selected(ermod_bin_cov_sel),
      c("AUCss_1000", "BHBA1C_5")
    )

    priors <- prior_summary(extract_mod(ermod_bin_cov_sel))
    expect_equal(priors$prior$scale, c(5, 5))
    expect_equal(priors$prior_intercept$scale, 4)

    rk <- attr(var_selected_kfold, "rk")
    expect_equal(rk$foldwise[, 3], c("RACE", "VISC"))

    plot_submod_performance(ermod_bin_cov_sel) |>
      expect_silent()
    plot_var_ranking(ermod_bin_cov_sel_kfold_dummy) |>
      expect_silent()
  }
})

test_that("Final model", {
  if (requireNamespace("projpred")) {
    expect_equal(coef(ermod_bin_cov_sel)[[1]], -10.9133063)
  }
})

test_that("No ER case", {
  set.seed(1234)

  if (requireNamespace("projpred")) {
    refm_obj_dr2 <- .dev_ermod_refmodel(
      data = df_er_dr2,
      var_resp = var_resp,
      var_exposure = "AUCss_1000",
      var_cov_candidates = c("BAGE_10", "BWT_10"),
      verbosity_level = 0,
      # Below option to make the test fast
      chains = 2, iter = 1000
    )

    .select_cov_projpred(
      refm_obj = refm_obj_dr2,
      var_exposure = "AUCss_1000",
      var_cov_candidates = c("BAGE_10", "BWT_10"),
      # Set nterms_max to 4 to make the test fast
      nterms_max = 1,
      cv_method = "LOO",
      validate_search = FALSE,
      verbosity_level = 1
    ) |>
      expect_message("No variables selected") |>
      expect_message("The variables selected were: AUCss_1000")
  }
})


test_that("NAs in data", {
  df_na_test <- df_er_ae_covsel_test
  df_na_test$AUCss_1000[1] <- NA

  check_data_columns(
    data = df_na_test,
    var_exp_candidates = var_exp_candidates
  ) |>
    expect_error("NA values not allowed in the exposure metric")

  df_na_test <- df_er_ae_covsel_test
  df_na_test$BHBA1C_5[1] <- NA

  check_data_columns(
    data = df_na_test,
    var_cov_candidates = var_cov_ae_covsel_test
  ) |>
    expect_error("NA values not allowed in the covariate column")

  df_na_test <- df_er_ae_covsel_test
  df_na_test$AEFLAG[1] <- NA

  check_data_columns(
    data = df_na_test,
    var_resp = var_resp
  ) |>
    expect_error("NA values not allowed in the response column")
})

test_that("Missing column", {
  df_na_test <- df_er_ae_covsel_test

  check_data_columns(
    data = df_na_test,
    var_exp_candidates = "AUCaa"
  ) |>
    expect_error('"AUCaa" columns should be in data')


  check_data_columns(
    data = df_na_test,
    var_cov_candidates = c("TEST", "TEST2", "BHBA1C_5", "test3")
  ) |>
    expect_error('"TEST, TEST2, BHBA1C_5, test3" columns should be in data')
})

test_that("lm", {
  as_draws(ermod_lin) |>
    dim() |>
    expect_equal(c(1000, 8))
  priors <- prior_summary(extract_mod(ermod_lin))
  expect_equal(priors$prior$scale, rep(5, 3))
  expect_equal(priors$prior_aux$rate, 6)

  ermod_lin_exp_sel$loo_comp_exposures |> expect_s3_class("compare.loo")
  priors <- prior_summary(extract_mod(ermod_lin_exp_sel$l_mod_exposures[[2]]))
  expect_equal(priors$prior$scale, 5)
  expect_equal(priors$prior_aux$rate, 6)

  if (requireNamespace("projpred")) {
    ermod_lin_cov_sel$cvvs |> expect_s3_class("vsel")
    expect_equal(ermod_lin_cov_sel$var_selected, c("AUCss", "BAGE"))
    priors <- prior_summary(extract_mod(ermod_lin_cov_sel))
    expect_equal(priors$prior$scale, c(5, 5))
    expect_equal(priors$prior_aux$rate, 6)
  }
})

test_that("plot_er_gof for lin", {
  if (.if_run_ex_plot_er()) {
    plot_er_gof(ermod_lin_exp_sel, show_coef_exp = TRUE) |>
      expect_silent()
  }
})

test_that("Convert factor as response variable into number", {
  dev_ermod_bin(
    data = mtcars,
    var_resp = "am2",
    var_exposure = "cyl",
  ) |>
    expect_error("Response variable must be a numeric of 0 and 1")

  dev_ermod_bin(
    data = mtcars,
    var_resp = "cyl",
    var_exposure = "am",
  ) |>
    expect_error("Response variable must be a numeric of 0 and 1")

  if (.if_run_ex_plot_er()) {
    plot_er_gof(mod_mtcars, n_bins = 4) |>
      expect_error("The breaks for the binned probability ")
    plot_er_gof(mod_mtcars, n_bins = 2) |> expect_silent()
  }
})

test_that("print.ermod_exp_sel prints correct information", {
  out <- cli::cli_fmt({
    print(ermod_bin_exp_sel)
  })
  expect_true(any(grepl("Binary ER model", out)))
  expect_true(any(grepl("& exposure metric selection", out)))
  expect_true(any(grepl("Use \\`plot_er_exp_sel\\(\\)\\` for ER curve", out)))
})

test_that("print.ermod_cov_sel prints correct information", {
  if (requireNamespace("projpred")) {
    out <- cli::cli_fmt({
      print(ermod_bin_cov_sel_kfold_dummy)
    })
    expect_true(any(grepl("Binary ER model", out)))
    expect_true(any(grepl("& covariate selection", out)))
    expect_true(any(grepl("Use \\`plot_submod_performance\\(\\)\\`", out)))
  }
})

# plot.ermod_exp_sel
test_that("plot.ermod_exp_sel calls plot_er_exp_sel", {
  if (.if_run_ex_plot_er()) {
    expect_silent(plot.ermod_exp_sel(ermod_bin_exp_sel))
  }
})

# plot.ermod_cov_sel
test_that("plot.ermod_cov_sel calls plot_submod_performance", {
  if (requireNamespace("projpred")) {
    expect_silent(plot.ermod_cov_sel(ermod_lin_cov_sel))
  }
})

test_that("test for errors and warnings", {
  if (requireNamespace("projpred")) {
    .select_cov_projpred(
      var_exposure = c("AUC", "Cmax"),
      validate_search = TRUE
    ) |>
      expect_error(
        "Only one exposure metric should be provided"
      )
    .select_cov_projpred(
      var_exposure = "AUC",
      var_cov_candidates = "cov",
      cv_method = "LOO",
      validate_search = TRUE
    ) |>
      expect_error(
        "validate_search should be set to FALSE for LOO,"
      )
  }
})

test_that("extract_coef_exp_ci for exp_candidates", {
  extract_coef_exp_ci(ermod_bin_exp_sel, exp_candidates = TRUE) |>
    dim() |>
    expect_equal(c(3, 2))
  extract_coef_exp_ci(ermod_lin, exp_candidates = TRUE) |>
    expect_error("exp_candidates = TRUE only supported")
})
