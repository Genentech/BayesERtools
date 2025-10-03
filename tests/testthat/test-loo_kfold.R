if (.if_run_ex_eval_mod()) {
  d_sim_binom_cov <- d_sim_binom_cov

  id_to_sample <- seq(1, max(d_sim_binom_cov$ID), by = 5)

  df_er_ae_covsel_test <-
    d_sim_binom_cov |>
    dplyr::filter(
      AETYPE == "ae_covsel_test",
      ID %in% id_to_sample
    ) |>
    dplyr::mutate(AUCss_1000 = AUCss / 1000, BHBA1C_5 = BHBA1C / 5)

  data_er_cont <- rstanemax::exposure.response.sample
  data_er_cont_cov <- rstanemax::exposure.response.sample.with.cov
  data_er_bin <- rstanemax::exposure.response.sample.binary

  # Create  ermod_bin objects for testing --------------------------------------
  set.seed(1234)
  ermod_bin <- dev_ermod_bin(
    data = df_er_ae_covsel_test,
    var_resp = "AEFLAG",
    var_exposure = "AUCss_1000",
    var_cov = "BHBA1C_5",
    verbosity_level = 0,
    # Below option to make the test fast
    chains = 2, iter = 1000
  )

  ermod_bin_wo_cov <- dev_ermod_bin(
    data = df_er_ae_covsel_test,
    var_resp = "AEFLAG",
    var_exposure = "AUCss_1000",
    verbosity_level = 0,
    # Below option to make the test fast
    chains = 2, iter = 1000
  )

  set.seed(1234)
  ermod_emax_w_cov <-
    suppressWarnings(dev_ermod_emax(
      data = data_er_cont_cov,
      var_exposure = "conc",
      var_resp = "resp",
      l_var_cov = list(emax = "cov2", ec50 = "cov3", e0 = "cov1"),
      verbosity_level = 0,
      chains = 2,
      iter = 1000,
      seed = 1
    ))

  ermod_bin_emax <-
    suppressWarnings(dev_ermod_bin_emax(
      data = df_er_ae_covsel_test,
      var_resp = "AEFLAG",
      var_exposure = "AUCss_1000",
      verbosity_level = 0,
      # Increase iter for better convergence as exact reproducibility
      # over different machines doesn't seem realistic
      chains = 2,
      iter = 1000,
      seed = 1
    ))

  set.seed(1234)

  kfold_ermod_bin <- kfold(ermod_bin, k = 3, seed = 1)
  kfold_ermod_bin_rstanarm <- suppressMessages(rstanarm::kfold(
    extract_mod(ermod_bin),
    K = 3,
    folds = kfold_ermod_bin$d_truth$fold_id # Use the same fold
  ))

  comp <- loo::loo_compare(kfold_ermod_bin_rstanarm, kfold_ermod_bin)

  set.seed(1234)
  kfold_ermod_emax <- suppressWarnings(
    kfold(ermod_emax_w_cov, k = 3, seed = 123)
  )

  # Test ----------------------------------------------------------------------
  test_that("loo", {

    #skip("temporarily disable loo test")

    loo_ermod_bin <- loo(ermod_bin)
    loo_ermod_emax_w_cov <- suppressWarnings(loo(ermod_emax_w_cov))
    loo_ermod_bin_emax <- loo(ermod_bin_emax)

    expect_equal(
      loo_ermod_bin$estimates[, 1],
      c(elpd_loo = -38.5289466, p_loo = 3.3262640, looic = 77.0578931),
      tolerance = 0.1
    )
    # expect_equal(
    #   loo_ermod_emax_w_cov$estimates[, 1],
    #   c(elpd_loo = -216.539552, p_loo = 7.765598, looic = 433.079103),
    #   tolerance = 0.1
    # )
    expect_equal(
      loo_ermod_bin_emax$estimates[, 1],
      c(elpd_loo = -60.787274, p_loo = 1.427765, looic = 121.574548),
      tolerance = 0.1
    )

    expect_silent(loo::loo_compare(loo_ermod_bin, loo_ermod_bin_emax))
  })

  test_that("kfold", {

    #skip("temporarily disable kfold test")

    expect_gt(comp[[2, 1]], -0.5)
    expect_equal(
      kfold_ermod_bin$estimates[, 1],
      c(elpd_kfold = -38.242947, p_kfold = 3.040264, kfoldic = 76.485893), 
      tolerance = .01
    )
    expect_equal(
      class(extract_kfold_loo(kfold_ermod_bin)),
      c("kfold", "loo")
    )
    # expect_equal(
    #   kfold_ermod_emax$estimates[, 1],
    #   c(elpd_kfold = -218, p_kfold = 9, kfoldic = 435),
    #   tolerance = 0.1
    # )
  })

  # Test for other models are covered in test-eval_ermod.R with kfold-cv eval
  test_that("binary model", {
    expect_equal(
      names(kfold_ermod_bin$d_truth),
      c(".row", "truth", "fold_id")
    )
    expect_equal(
      names(kfold_ermod_bin$d_sim),
      c(".row", ".draw", "pred", "fold_id")
    )
    expect_equal(dim(kfold_ermod_bin$d_sim), c(100 * 1000, 4))

    out <- cli::cli_fmt({
      print(kfold_ermod_bin)
    })
    expect_true(any(grepl("k-fold Cross-Validation for ermod object", out)))
  })
}
