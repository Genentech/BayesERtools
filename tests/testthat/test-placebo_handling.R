
# setup cases to test ----------------------------------------------------

# two versions of the data to test
dat <- list(
  placebo = d_sim_emax |> dplyr::slice_sample(n = 10, by = dose),
  noplacebo = d_sim_emax |> dplyr::filter(dose != 0) |> dplyr::slice_sample(n = 10, by = dose)
)

# two versions of the placebo handling options
opts <- list(
  drop_placebo = list(include_placebo = FALSE),
  use_placebo = list(include_placebo = TRUE)
)

# four versions of the modelling function
dev_ermod <- list(
  dev_ermod_lin = dev_ermod_lin,
  dev_ermod_bin = dev_ermod_bin,
  dev_ermod_emax = dev_ermod_emax,
  dev_ermod_bin_emax = dev_ermod_bin_emax
)

# four versions of the exposure metric selection function
dev_expsel <- list(
  dev_ermod_lin_exp_sel = dev_ermod_lin_exp_sel,
  dev_ermod_bin_exp_sel = dev_ermod_bin_exp_sel,
  dev_ermod_emax_exp_sel = dev_ermod_emax_exp_sel,
  dev_ermod_bin_emax_exp_sel = dev_ermod_bin_emax_exp_sel
)

# two versions of the covariate selection function
dev_covsel <- list(
  dev_ermod_lin_cov_sel = dev_ermod_lin_cov_sel,
  dev_ermod_bin_cov_sel = dev_ermod_bin_cov_sel
)

# all test cases for basic er model
cases_ermod <- tidyr::expand_grid(
  dat = names(dat),
  dev_ermod = names(dev_ermod),
  opts = names(opts),
) |> dplyr::mutate(
  var_resp = dplyr::case_when(
    dev_ermod == "dev_ermod_lin" ~ "response_1",
    dev_ermod == "dev_ermod_bin" ~ "response_2",
    dev_ermod == "dev_ermod_emax" ~ "response_1",
    dev_ermod == "dev_ermod_bin_emax" ~ "response_2"
  ),
  inner_n = NA_integer_,
  outer_n = NA_integer_
)
n_cases_ermod <- nrow(cases_ermod)

# all test cases for exposure selection functions
cases_expsel <- tidyr::expand_grid(
  dat = names(dat),
  dev_expsel = names(dev_expsel),
  opts = names(opts),
) |> dplyr::mutate(
  var_resp = dplyr::case_when(
    dev_expsel == "dev_ermod_lin_exp_sel" ~ "response_1",
    dev_expsel == "dev_ermod_bin_exp_sel" ~ "response_2",
    dev_expsel == "dev_ermod_emax_exp_sel" ~ "response_1",
    dev_expsel == "dev_ermod_bin_emax_exp_sel" ~ "response_2"
  ),
  inner_n = NA_integer_,
  outer_n = NA_integer_
)
n_cases_expsel <- nrow(cases_expsel)

if (require("projpred")) {
  # all test cases for covariate selection functions
  cases_covsel <- tidyr::expand_grid(
    dat = names(dat),
    dev_covsel = names(dev_covsel),
    opts = names(opts),
  ) |> dplyr::mutate(
    var_resp = dplyr::case_when(
      dev_covsel == "dev_ermod_lin_cov_sel" ~ "response_1",
      dev_covsel == "dev_ermod_bin_cov_sel" ~ "response_2",
    ),
    inner_n = NA_integer_,
    outer_n = NA_integer_
  )
  n_cases_covsel <- nrow(cases_covsel)
}

# tests that the model fitting runs without error ------------------------

test_that("basic model fitting works with and without placebo", {
  mods <- list()
  for (r in seq_len(n_cases_ermod)) {

    # which function, dataset, and options to use
    f <- purrr::quietly(dev_ermod[[cases_ermod$dev_ermod[r]]])
    d <- dat[[cases_ermod$dat[r]]]
    o <- opts[[cases_ermod$opts[r]]]
    set.seed(123L)

    # estimate the model quietly, test that it runs without error
    expect_no_error(
      m <- d |> f(
        var_exposure = "exposure_1",
        var_resp = cases_ermod$var_resp[r],
        options_placebo_handling = o,
        chains = 1,
        iter = 100
      )
    )
    mods[[r]] <- m$result
  }

  # keep a copy in the cases_ermod data frame so that the models
  # persist without the test environment
  cases_ermod$mod <<- mods
})

test_that("exposure selection fitting works with and without placebo", {
  mods <- list()
  for (r in seq_len(n_cases_expsel)) {

    # which function, dataset, and options to use
    f <- purrr::quietly(dev_expsel[[cases_expsel$dev_expsel[r]]])
    d <- dat[[cases_expsel$dat[r]]]
    o <- opts[[cases_expsel$opts[r]]]
    set.seed(123L)

    # estimate the model quietly, test that it runs without error
    expect_no_error(
      m <- d |> f(
        var_exp_candidates = c("exposure_1", "exposure_2"),
        var_resp = cases_expsel$var_resp[r],
        options_placebo_handling = o,
        chains = 1,
        iter = 100
      )
    )
    mods[[r]] <- m$result
  }

  # keep a copy in the cases_expsel data frame so that the models
  # persist without the test environment
  cases_expsel$mod <<- mods
})

if (require("projpred")) {
  test_that("covariate selection fitting works with and without placebo", {
    mods <- list()
    for (r in seq_len(n_cases_covsel)) {

      # which function, dataset, and options to use
      f <- purrr::quietly(dev_covsel[[cases_covsel$dev_covsel[r]]])
      d <- dat[[cases_covsel$dat[r]]]
      o <- opts[[cases_covsel$opts[r]]]
      set.seed(123L)

      # estimate the model quietly, test that it runs without error
      expect_no_error(
        m <- d |> f(
          var_exposure = "exposure_1",
          var_cov_candidates = c("cnt_a", "cnt_b"),
          var_resp = cases_covsel$var_resp[r],
          options_placebo_handling = o,
          chains = 1,
          iter = 100
        )
      )
      mods[[r]] <- m$result
    }
    # keep a copy in the cases_covsel data frame so that the models
    # persist without the test environment
    cases_covsel$mod <<- mods
  })
}


# tests to extract internal data ---------------------------------

test_that("extract_data.ermod respects internal_data argument (ermod)", {
  for (r in seq_len(n_cases_ermod)) {
    m <- cases_ermod$mod[[r]]
    expect_no_error(extract_data(m, method = "internal"))
    expect_no_error(extract_data(m, method = "raw"))
    inner_data <- extract_data(m, method = "internal")
    outer_data <- extract_data(m, method = "raw")
    expect_true(inherits(inner_data, "data.frame"))
    expect_true(inherits(outer_data, "data.frame"))
    cases_ermod$inner_n[r] <<- nrow(inner_data)
    cases_ermod$outer_n[r] <<- nrow(outer_data)
  }
})

test_that("extract_data.ermod respects internal_data argument (expsel)", {
  for (r in seq_len(n_cases_ermod)) {
    m <- cases_expsel$mod[[r]]
    expect_no_error(extract_data(m, method = "internal"))
    expect_no_error(extract_data(m, method = "raw"))
    inner_data <- extract_data(m, method = "internal")
    outer_data <- extract_data(m, method = "raw")
    expect_true(inherits(inner_data, "data.frame"))
    expect_true(inherits(outer_data, "data.frame"))
    cases_expsel$inner_n[r] <<- nrow(inner_data)
    cases_expsel$outer_n[r] <<- nrow(outer_data)
  }
})

if (require("projpred")) {
  test_that("extract_data.ermod respects internal_data argument (covsel)", {
    for (r in seq_len(n_cases_covsel)) {
      m <- cases_covsel$mod[[r]]
      expect_no_error(extract_data(m, method = "internal"))
      expect_no_error(extract_data(m, method = "raw"))
      inner_data <- extract_data(m, method = "internal")
      outer_data <- extract_data(m, method = "raw")
      expect_true(inherits(inner_data, "data.frame"))
      expect_true(inherits(outer_data, "data.frame"))
      cases_covsel$inner_n[r] <<- nrow(inner_data)
      cases_covsel$outer_n[r] <<- nrow(outer_data)
    }
  })
}

# tests to examine placebo data handling ---------------------------------

# the inner "stan" data set should only include placebo samples if the
# data set originally contained a placebo group, and the options specify
# that the placebo group is to be used during model fitting
test_that("internal data respects placebo options (basic models)", {
  for (r in seq_len(n_cases_ermod)) {
    if (cases_ermod$dat[r] == "placebo" & cases_ermod$opts[r] == "use_placebo") {
      expect_equal(cases_ermod$inner_n[r], 40L)
    } else {
      expect_equal(cases_ermod$inner_n[r], 30L)
    }
  }
})
test_that("internal data respects placebo options (metric selection)", {
  for (r in seq_len(n_cases_expsel)) {
    if (cases_expsel$dat[r] == "placebo" & cases_expsel$opts[r] == "use_placebo") {
      expect_equal(cases_expsel$inner_n[r], 40L)
    } else {
      expect_equal(cases_expsel$inner_n[r], 30L)
    }
  }
})
if (require("projpred")) {
  test_that("internal data respects placebo options (covariate selection)", {
    for (r in seq_len(n_cases_covsel)) {
      if (cases_covsel$dat[r] == "placebo" & cases_covsel$opts[r] == "use_placebo") {
        expect_equal(cases_covsel$inner_n[r], 40L)
      } else {
        expect_equal(cases_covsel$inner_n[r], 30L)
      }
    }
  })
}

# the "user facing" data set should only include placebo samples if the data set
# originally contained a placebo group, regardless of the placebo handling settings
test_that("outer data preserves all data rows (basic models)", {
  for (r in seq_len(n_cases_ermod)) {
    if (cases_ermod$dat[r] == "placebo") {
      expect_equal(cases_ermod$outer_n[r], 40L)
    } else {
      expect_equal(cases_ermod$outer_n[r], 30L)
    }
  }
})
test_that("outer data preserves all data rows (metric selection)", {
  for (r in seq_len(n_cases_expsel)) {
    if (cases_expsel$dat[r] == "placebo") {
      expect_equal(cases_expsel$outer_n[r], 40L)
    } else {
      expect_equal(cases_expsel$outer_n[r], 30L)
    }
  }
})
if (require("projpred")) {
  test_that("outer data preserves all data rows (covariate selection)", {
    for (r in seq_len(n_cases_covsel)) {
      if (cases_covsel$dat[r] == "placebo") {
        expect_equal(cases_covsel$outer_n[r], 40L)
      } else {
        expect_equal(cases_covsel$outer_n[r], 30L)
      }
    }
  })
}
