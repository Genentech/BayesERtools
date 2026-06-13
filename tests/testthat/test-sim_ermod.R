d_sim_binom_cov <- d_sim_binom_cov

id_to_sample <- seq(1, max(d_sim_binom_cov$ID), by = 5)
id_to_sample_2 <- seq(2, max(d_sim_binom_cov$ID), by = 10)

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

df_er_ae_covsel_test_2 <-
  d_sim_binom_cov |>
  dplyr::filter(
    AETYPE == "ae_covsel_test",
    ID %in% id_to_sample_2
  ) |>
  dplyr::mutate(
    AUCss_1000 = AUCss / 1000, BAGE_10 = BAGE / 10,
    BWT_10 = BWT / 10, BHBA1C_5 = BHBA1C / 5
  )


# Create  ermod_bin objects for testing ---------------------------------------
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

ermod_bin_cov_sel <- new_ermod_bin_cov_sel(
  mod = extract_mod(ermod_bin),
  data = df_er_ae_covsel_test,
  var_resp = "AEFLAG",
  var_exposure = "AUCss_1000",
  var_cov_candidates = c("BAGE_10", "BWT_10", "BHBA1C_5", "RACE", "VISC"),
  var_cov = c("BHBA1C_5"),
  var_selected = c("AUCss_1000", "BHBA1C_5"),
  cv_method = c("LOO", "kfold"),
  cvvs = NULL,
  rk = NULL
)

# Simulate responses ----------------------------------------------------------
ersim <- sim_er(
  ermod_bin,
  newdata = NULL,
  n_draws_sim = 200,
  output_type = "draws"
)

ersim_med_qi <- sim_er(
  ermod_bin,
  newdata = df_er_ae_covsel_test_2,
  n_draws_sim = 200,
  output_type = c("median_qi"),
  qi_width = 0.95
)

## Specific exposures
ersim_new_exp <- sim_er_new_exp(
  ermod_bin,
  exposure_to_sim_vec = c(2, 2:6),
  data_cov = dplyr::tibble(BHBA1C_5 = 4:10, AUCss_1000 = 4:10),
  n_draws_sim = 200,
  output_type = "draws"
)

ersim_new_exp_med_qi <- sim_er_new_exp(
  ermod_bin,
  exposure_to_sim_vec = c(2, 2:6),
  data_cov = dplyr::tibble(BHBA1C_5 = 4:10, AUCss_1000 = 4:10),
  n_draws_sim = 200,
  output_type = "median_qi"
)

ersim_curve <- sim_er_curve(
  ermod_bin,
  data_cov = dplyr::tibble(BHBA1C_5 = c(4, 7)),
  n_draws_sim = 200,
  output_type = "draws"
)

## Marginal response
ersim_new_exp_marg <- sim_er_new_exp_marg(
  ermod_bin_cov_sel,
  exposure_to_sim_vec = c(2, 2:6),
  data_cov = dplyr::tibble(BHBA1C_5 = 4:10, AUCss_1000 = 4:10),
  n_subj_sim = NULL,
  n_draws_sim = 200
)

ersim_new_exp_marg_2 <- sim_er_new_exp_marg(
  ermod_bin_cov_sel,
  exposure_to_sim_vec = c(2, 2:6),
  n_subj_sim = 4,
  n_draws_sim = 200
)

ersim_new_exp_marg_med_qi <- sim_er_new_exp_marg(
  ermod_bin_cov_sel,
  exposure_to_sim_vec = c(2, 2:6),
  data_cov = dplyr::tibble(BHBA1C_5 = 4:10, AUCss_1000 = 4:10),
  n_subj_sim = NULL,
  n_draws_sim = 200,
  output_type = "median_qi"
)

ersim_curve_marg_med_qi <- sim_er_curve_marg(
  ermod_bin,
  exposure_range = c(2, 6),
  num_exposures = 11,
  data_cov = dplyr::tibble(BHBA1C_5 = seq(4, 10, by = 0.1)),
  n_subj_sim = NULL,
  n_draws_sim = 200,
  output_type = "median_qi"
)

ersim_curve_med_qi_2 <- calc_ersim_med_qi(ersim_curve)
ersim_new_exp_marg_med_qi_2 <- calc_ersim_med_qi(ersim_new_exp_marg)

# Test ------------------------------------------------------------------------

test_that("sim_ermod", {
  expect_equal(dim(ersim), c(20000, 24))
  expect_equal(nrow(ersim_med_qi), 50)
  expect_equal(min(ersim_med_qi$.linpred), -10.27, tolerance = 0.05)

  inv_logit <- \(x) exp(x) / (1 + exp(x))
  expect_equal(ersim$.epred, inv_logit(ersim$.linpred))
})

test_that("sim_er_new_exp", {
  expect_equal(nrow(ersim_new_exp), 8400)
  expect_equal(ncol(ersim_new_exp), 9)
  expect_equal(nrow(ersim_new_exp_med_qi), 42)
  expect_equal(max(ersim_new_exp_med_qi$.epred), 0.975, tolerance = 0.01)
  expect_equal(
    unique(ersim_curve$AUCss_1000),
    seq(0.398670752493232, 6.3639878,
      length.out = 51
    )
  )
  expect_equal(dim(ersim_curve_med_qi_2), c(102, 15))
})

test_that("sim_er_new_exp_marg", {
  expect_equal(nrow(ersim_new_exp_marg), 6 * 200)
  expect_equal(ncol(ersim_new_exp_marg), 6)
  expect_equal(nrow(ersim_new_exp_marg_2), 6 * 200)
  expect_equal(nrow(ersim_new_exp_marg_med_qi), 6)
  expect_equal(nrow(ersim_new_exp_marg_med_qi_2), 6)
  expect_equal(max(ersim_new_exp_marg_med_qi$.epred), 0.37, tolerance = 0.03)
  expect_equal(ersim_curve_marg_med_qi$AUCss_1000, seq(2, 6, by = 0.4))
})

test_that("test for errors and warnings", {
  sim_er_curve_marg(
    ermod_bin,
    exposure_range = c(2, 6),
    num_exposures = 11,
    data_cov = dplyr::tibble(BHBA1C_5 = seq(4, 10, by = 1)),
    n_subj_sim = 10,
    n_draws_sim = 200,
    output_type = "median_qi"
  ) |>
    expect_warning("n_subj_sim \\(10\\) is greater than")

  sim_er_new_exp(
    ermod_bin,
    exposure_to_sim_vec = c(2, 2:6),
    data_cov = dplyr::tibble(BHBA1C_5 = 4:10, AUCss_1000 = 4:10),
    n_draws_sim = 2000,
    output_type = "draws"
  ) |>
    expect_warning("n_draws_sim \\(2000\\) is greater than")
})
