if (.if_run_ex_eval_mod()) {
  # Linear logistic regression ------------------------------------------------
  set.seed(1234)
  data(d_sim_binom_cov_hgly2)
  d_split <- rsample::initial_split(d_sim_binom_cov_hgly2)
  d_train <- rsample::training(d_split)
  d_test <- rsample::testing(d_split)

  opts_placebo <- list(include_placebo = TRUE)  # allow zeros in test
  ermod_bin <- dev_ermod_bin(
    data = d_train |> head(100),
    var_resp = "AEFLAG",
    var_exposure = "AUCss_1000",
    var_cov = "BHBA1C_5",
    options_placebo_handling = opts_placebo,
    chains = 2,
    iter = 500
  )

  met_bin_training <- eval_ermod(ermod_bin, eval_type = "training")
  met_bin_test <- eval_ermod(ermod_bin, eval_type = "test", newdata = d_test)
  met_bin_kfold <-
    eval_ermod(ermod_bin, eval_type = "kfold", newdata = d_test, k = 3) |>
    suppressWarnings()

  # Emax logistic regression --------------------------------------------------
  set.seed(1234)
  data_er_bin <- rstanemax::exposure.response.sample.binary
  ermod_bin_emax <-
    suppressWarnings(dev_ermod_bin_emax(
      data = data_er_bin,
      var_exposure = "conc",
      var_resp = "y_cov",
      l_var_cov = list(emax = "sex"),
      options_placebo_handling = opts_placebo,
      verbosity_level = 0,
      chains = 2,
      iter = 200,
      seed = 1
    ))

  met_bin_emax_training <- eval_ermod(ermod_bin_emax, eval_type = "training")
  met_bin_emax_kfold <-
    eval_ermod(ermod_bin_emax, eval_type = "kfold", k = 3) |>
    suppressWarnings()

  # Linear regression ---------------------------------------------------------
  set.seed(1234)
  ermod_lin <- dev_ermod_lin(
    data = d_sim_lin,
    var_resp = "response",
    var_exposure = "AUCss",
    var_cov = c("SEX", "BAGE"),
    options_placebo_handling = opts_placebo,
    chains = 2,
    iter = 500
  )

  met_lin_training <- eval_ermod(ermod_lin, eval_type = "training")
  met_lin_kfold <-
    eval_ermod(ermod_lin, eval_type = "kfold", k = 3) |>
    suppressWarnings()

  # Emax model ----------------------------------------------------------------
  set.seed(1234)
  data_er_cont_cov <- rstanemax::exposure.response.sample.with.cov

  ermod_emax <-
    suppressWarnings(dev_ermod_emax(
      data = data_er_cont_cov,
      var_exposure = "conc",
      var_resp = "resp",
      l_var_cov = list(emax = "cov2", ec50 = "cov3", e0 = "cov1"),
      options_placebo_handling = opts_placebo,
      verbosity_level = 0,
      chains = 2,
      iter = 200,
      seed = 1
    ))

  met_emax_training <- eval_ermod(ermod_emax, eval_type = "training")
  met_emax_kfold <-
    eval_ermod(ermod_emax, eval_type = "kfold", k = 3) |>
    suppressWarnings()

  # Test ----------------------------------------------------------------------
  test_that("Binary model", {
    # Training
    ersim_bin <- sim_er(ermod_bin, output_type = "median_qi") |>
      dplyr::mutate(AEFLAG = factor(AEFLAG, levels = c(1, 0)))

    estimates <- c(
      yardstick::roc_auc_vec(ersim_bin$AEFLAG, ersim_bin$.epred),
      yardstick::mn_log_loss_vec(ersim_bin$AEFLAG, ersim_bin$.epred)
    )

    expect_equal(met_bin_training$.estimate, estimates)

    # Test
    ersim_bin_test <-
      sim_er(ermod_bin, newdata = d_test, output_type = "median_qi") |>
      dplyr::mutate(AEFLAG = factor(AEFLAG, levels = c(1, 0)))

    estimates_test <- c(
      yardstick::roc_auc_vec(ersim_bin_test$AEFLAG, ersim_bin_test$.epred),
      yardstick::mn_log_loss_vec(ersim_bin_test$AEFLAG, ersim_bin_test$.epred)
    )

    expect_equal(met_bin_test$.estimate, estimates_test)

    # kfold
    expect_equal(dim(met_bin_kfold), c(6, 4))

    # Binary emax
    expect_error(
      eval_ermod(ermod_bin_emax, eval_type = "test", newdata = d_test),
      'conc" columns should be in data'
    )

    expect_equal(dim(met_bin_emax_training), c(2, 3))
    expect_equal(dim(met_bin_emax_kfold), c(6, 4))
  })

  test_that("Continuous model", {
    ersim_cont <- sim_er(ermod_lin, output_type = "median_qi")

    estimates <- c(
      yardstick::rmse_vec(ersim_cont$response, ersim_cont$.epred),
      yardstick::rsq_vec(ersim_cont$response, ersim_cont$.epred),
      yardstick::rsq_trad_vec(ersim_cont$response, ersim_cont$.epred)
    )

    expect_equal(met_lin_training$.estimate, estimates)

    # Emax
    expect_equal(dim(met_emax_training), c(3, 3))
    expect_equal(dim(met_emax_kfold), c(9, 4))
  })
}
