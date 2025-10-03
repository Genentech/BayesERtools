if (.if_run_ex_coveff()) {
  data(d_sim_binom_cov_hgly2)

  id_to_sample <- seq(1, max(d_sim_binom_cov_hgly2$ID), by = 5)

  df_er_ae_hgly2 <-
    d_sim_binom_cov_hgly2 |>
    dplyr::filter(ID %in% id_to_sample)

  var_resp <- "AEFLAG"

  # develop models ------------------------------------------------------------
  set.seed(1234)
  ermod_bin <- dev_ermod_bin(
    data = df_er_ae_hgly2,
    var_resp = var_resp,
    var_exposure = "AUCss_1000",
    var_cov = c("BHBA1C_5", "BGLUC", "RACE", "VISC"),
    verbosity_level = 0,
    # Below option to make the test fast
    chains = 2, iter = 1000
  )

  spec_coveff <- build_spec_coveff(ermod_bin)
  coveffsim <- sim_coveff(ermod_bin)
  coveffsim_2 <- sim_coveff(ermod_bin, spec_coveff = spec_coveff)

  # Linear regression model tests ---------------------------------------------
  data(d_sim_lin)

  set.seed(1234)
  ermod_lin <- dev_ermod_lin(
    data = d_sim_lin,
    var_resp = "response",
    var_exposure = "AUCss",
    var_cov = c("SEX", "BAGE"),
    verbosity_level = 0,
    # Below option to make the test fast
    chains = 2, iter = 1000
  )

  spec_coveff_lin <- build_spec_coveff(ermod_lin)
  coveffsim_lin <- sim_coveff(ermod_lin)
  coveffsim_lin_2 <- sim_coveff(ermod_lin, spec_coveff = spec_coveff_lin)

  # Tests ---------------------------------------------------------------------
  test_that("sim_coveff binary model", {
    expect_equal(coveffsim, coveffsim_2)
    expect_equal(dim(coveffsim), c(14, 12))
    expect_equal(
      coveffsim$.odds_ratio,
      c(
        0.394045222892321, 1, 4.80902749409125, 0.0814990385480039, 1,
        11.0151856281521, 0.148013228492832, 1,
        3.93067614853305, 1, 2.77732164370792, 2.05372376449102, 1, 0.46514853
      )
    )

    # Test model type attribute
    expect_equal(attr(coveffsim, "model_type"), "binary")
  })

  test_that("sim_coveff linear model", {
    expect_equal(coveffsim_lin, coveffsim_lin_2)
    expect_equal(dim(coveffsim_lin), c(8, 12))
    expect_true(".response_diff" %in% colnames(coveffsim_lin))
    expect_true(all(!is.na(coveffsim_lin$.response_diff)))
    expect_true(all(coveffsim_lin$.response_diff[coveffsim_lin$is_ref_value] == 0))

    # Test model type attribute
    expect_equal(attr(coveffsim_lin, "model_type"), "linear")
  })

  test_that("plot_coveff binary model", {
    g1 <- plot_coveff(coveffsim) |>
      expect_silent()
    g2 <- plot_coveff(ermod_bin) |>
      expect_silent()
    g3 <-
      coveffsim |>
      dplyr::mutate(show_ref_value = FALSE) |>
      plot_coveff() |>
      expect_silent()

    expect_equal(dim(g1$data), c(14, 14))
    expect_equal(dim(g2$data), c(14, 14))
    expect_equal(dim(g3$data), c(9, 14))
  })

  test_that("plot_coveff linear model", {
    g1 <- plot_coveff(coveffsim_lin) |>
      expect_silent()
    g2 <- plot_coveff(ermod_lin) |>
      expect_silent()
    g3 <-
      coveffsim_lin |>
      dplyr::mutate(show_ref_value = FALSE) |>
      plot_coveff() |>
      expect_silent()

    expect_equal(dim(g1$data), c(8, 14))
    expect_equal(dim(g2$data), c(8, 14))
    expect_equal(dim(g3$data), c(5, 14))
  })

  test_that("print_coveff binary model", {
    table_coveff <- print_coveff(coveffsim)

    expect_equal(
      table_coveff$`Odds ratio`,
      c(
        "0.394", "1", "4.81", "0.0815", "1", "11.0", "0.148", "1", "3.93",
        "1", "2.78", "2.05", "1", "0.465"
      )
    )
  })

  test_that("print_coveff linear model", {
    table_coveff_lin <- print_coveff(coveffsim_lin)

    expect_equal(
      table_coveff_lin$`Response difference`,
      c(
        "−20.6", "0", "20.6", "0", "−3.95", "−8.46", "0", "8.30"
      )
    )
  })

  test_that("build_spec_coveff binary model", {
    expect_equal(dim(spec_coveff), c(14, 11))
    expect_equal(
      spec_coveff$value_label,
      c(
        "0.824", "2.31", "4.80", "6.01", "8.19", "10.3", "4.42", "6.09",
        "7.28", "White", "Asian", "Black", "No", "Yes"
      )
    )
  })

  test_that("build_spec_coveff linear model", {
    expect_equal(dim(spec_coveff_lin), c(8, 11))
    expect_equal(
      spec_coveff_lin$value_label,
      c(
        "5.00", "50.0", "95.0", "F", "M", "33.5", "50.4", "67.0"
      )
    )
  })

  test_that("calc_summary_col", {
    spec_coveff <- build_spec_coveff(ermod_bin)

    calc_summary_col_cont(0:100) |> expect_equal(
      dplyr::tibble(
        value_cont = c(5, 50, 95),
        value_order = c(1, 2, 3),
        value_annot = c("5th", "median", "95th"),
        is_ref_value = c(FALSE, TRUE, FALSE),
        value_label = c("5.00", "50.0", "95.0")
      )
    )

    calc_summary_col_categ(c("A", "A", "B", "C", "C", "D")) |> expect_equal(
      dplyr::tibble(
        value_cat = c("A", "C", "B", "D"),
        value_order = c(1, 2, 3, 4),
        value_annot = c("1st freq", "2nd freq", "3rd freq", "4th freq"),
        is_ref_value = c(TRUE, FALSE, FALSE, FALSE),
        value_label = c("A", "C", "B", "D")
      )
    )
  })
}
