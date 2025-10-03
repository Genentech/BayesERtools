set.seed(1234)
data_er_cont <- rstanemax::exposure.response.sample
data_er_cont_cov <- rstanemax::exposure.response.sample.with.cov

noise <- 1 + 0.5 * stats::rnorm(length(data_er_cont$exposure))
data_er_cont$exposure2 <- data_er_cont$exposure * noise
# Replace exposure < 0 with 0
data_er_cont$exposure2[data_er_cont$exposure2 < 0] <- 0


data_er_bin <- rstanemax::exposure.response.sample.binary

noise <- 1 + 0.5 * stats::rnorm(length(data_er_bin$conc))
data_er_bin$conc2 <- data_er_bin$conc * noise
# Replace exposure < 0 with 0
data_er_bin$conc2[data_er_bin$conc2 < 0] <- 0


set.seed(1234)

# Test ermod_emax ------------------------------------------------------------
ermod_emax <-
  suppressWarnings(dev_ermod_emax(
    data = data_er_cont,
    var_exposure = "exposure",
    var_resp = "response",
    options_placebo_handling = list(include_placebo = TRUE),
    verbosity_level = 0,
    # Increase iter for better convergence as exact reproducibility
    # over different machines doesn't seem realistic
    chains = 2,
    iter = 1000,
    seed = 1
  ))

ermod_emax_w_cov <-
  suppressWarnings(dev_ermod_emax(
    data = data_er_cont_cov,
    var_exposure = "conc",
    var_resp = "resp",
    l_var_cov = list(emax = "cov2", ec50 = "cov3", e0 = "cov1"),
    options_placebo_handling = list(include_placebo = TRUE),
    verbosity_level = 0,
    chains = 2,
    iter = 200,
    seed = 1
  ))

ersim_med_qi <- sim_er(ermod_emax, output_type = "median_qi")

# TODO
ermod_emax_exp_sel <-
  suppressWarnings(dev_ermod_emax_exp_sel(
    data = data_er_cont,
    var_resp = "response",
    var_exp_candidates = c("exposure", "exposure2"),
    options_placebo_handling = list(include_placebo = TRUE),
    emax_fix = 100,
    e0_fix = 0,
    verbosity_level = 0,
    chains = 2,
    iter = 200,
    seed = 1
  ))


# Test ermod_bin_emax ---------------------------------------------------------
ermod_bin_emax <-
  suppressWarnings(dev_ermod_bin_emax(
    data = data_er_bin,
    var_exposure = "conc",
    var_resp = "y",
    options_placebo_handling = list(include_placebo = TRUE),
    verbosity_level = 0,
    # Increase iter for better convergence as exact reproducibility
    # over different machines doesn't seem realistic
    chains = 2,
    iter = 1000,
    seed = 1
  ))

ermod_bin_emax_w_cov <-
  suppressWarnings(dev_ermod_bin_emax(
    data = data_er_bin,
    var_exposure = "conc",
    var_resp = "y_cov",
    options_placebo_handling = list(include_placebo = TRUE),
    l_var_cov = list(emax = "sex"),
    verbosity_level = 0,
    chains = 2,
    iter = 200,
    seed = 1
  ))

ersim_bin_med_qi <- sim_er(ermod_bin_emax, output_type = "median_qi")

ermod_bin_emax_exp_sel <-
  suppressWarnings(dev_ermod_bin_emax_exp_sel(
    data = data_er_bin,
    var_resp = "y",
    var_exp_candidates = c("conc", "conc2"),
    verbosity_level = 0,
    options_placebo_handling = list(include_placebo = TRUE),
    chains = 2,
    iter = 200,
    seed = 1
  ))


# Convert factor as response variable into number
data("mtcars")
mtcars$am2 <- factor(mtcars$am, labels = c("Automatic", "Manual"))
mtcars$am3 <- as.numeric(mtcars$am2) - 1

mod_mtcars_emax <- suppressWarnings(dev_ermod_bin_emax(
  data = mtcars,
  var_resp = "am3",
  var_exposure = "cyl",
  verbosity_level = 0,
  options_placebo_handling = list(include_placebo = TRUE),
  # Below option to make the test fast
  chains = 2, iter = 1000
))

# Test ------------------------------------------------------------------------
test_that("sim_ermod", {
  expect_equal(dim(sim_er(ermod_emax_w_cov, n_draws_sim = 10)), c(600, 14))
  expect_equal(dim(ersim_med_qi), c(60, 17))

  par_names <- c("emax", "e0", "ec50", "gamma", "sigma")
  par_means <-
    rstan::summary(ermod_emax$mod$stanfit, pars = par_names)$summary[, 1]
  expect_equal(
    par_means,
    c(
      `emax[1]` = 92, `e0[1]` = 5.8, `ec50[1]` = 77,
      gamma = 1, sigma = 16.5
    ),
    tolerance = 0.1
  )
  expect_equal(
    unique(ersim_med_qi$.epred)[1:10],
    c(
      5.767171, 20.640204, 20.969320, 21.941925, 22.306456,
      23.911237, 24.403143, 24.444787, 26.956913, 27.852625
    ),
    tolerance = 0.1
  )
})

test_that("plot_ermod", {
  if (.if_run_ex_plot_er()) {
    plot(plot_er(ersim_med_qi)) |> expect_silent()
    plot(plot_er(ersim_med_qi, show_orig_data = TRUE)) |> expect_silent()
    plot_er(ermod_emax_w_cov) |>
      expect_error("Model has covariate\\(s\\), and you cannot use this")
    plot_er_exp_sel(ermod_emax_exp_sel) |> expect_silent()
    plot_er(ermod_emax,
      show_orig_data = TRUE,
      options_orig_data = list(var_group = "dose")
    ) |> expect_silent()
  }
})


test_that("sim_ermod_bin_emax", {
  expect_equal(
    dim(sim_er(ermod_bin_emax_w_cov, n_draws_sim = 10)),
    c(1010, 17)
  )
  expect_equal(dim(ersim_bin_med_qi), c(101, 23))

  par_names <- c("emax", "e0", "ec50", "gamma")
  stanfit <- ermod_bin_emax_w_cov$mod$stanfit
  par_means <-
    rstan::summary(stanfit, pars = par_names)$summary[, 1]
  expect_equal(
    par_means,
    c(
      `emax[1]` = 1.8, `emax[2]` = 1.25, `e0[1]` = -1, `ec50[1]` = 240,
      gamma = 1
    ),
    tolerance = 0.1
  )
  expect_equal(
    unique(ersim_med_qi$.epred)[1:10],
    c(
      6.153473, 20.922569, 21.249333, 22.248391, 22.616190,
      24.137570, 24.636718, 24.672612, 27.069008, 27.936741
    ),
    tolerance = 0.1
  )
})

test_that("plot_ermod_bin_emax", {
  if (.if_run_ex_plot_er()) {
    plot(plot_er(ersim_bin_med_qi)) |> expect_silent()
    plot(plot_er(ersim_bin_med_qi, show_orig_data = TRUE)) |> expect_silent()
    plot_er(ermod_bin_emax_w_cov) |>
      expect_error("Model has covariate\\(s\\), and you cannot use this")
    plot(plot_er_exp_sel(ermod_bin_emax_exp_sel)) |> expect_silent()
  }
})

test_that("Convert factor as response variable into number", {
  if (.if_run_ex_plot_er()) {
    plot_er_gof(mod_mtcars_emax, n_bins = 4) |>
      expect_error("The breaks for the binned probability ")
    plot_er_gof(mod_mtcars_emax, n_bins = 2) |> expect_silent()
  }
})

test_that("checking e0_fix and emax_fix in dev_ermod_bin_emax", {
  dev_ermod_bin_emax(
    e0_fix = 0,
    verbosity_level = -1 # To stop the function from running
  ) |>
    expect_warning(
      "e0_fix is set to 0"
    ) |>
    expect_error()

  dev_ermod_bin_emax(
    e0_fix = 1,
    emax_fix = 2,
    verbosity_level = -1 # To stop the function from running
  ) |>
    expect_message(
      "e0_fix is set at 1"
    ) |>
    expect_message(
      "emax_fix is set at 2"
    ) |>
    expect_error()
})
