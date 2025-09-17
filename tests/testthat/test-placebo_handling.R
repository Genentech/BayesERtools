
# two versions of the data to test
dat <- list(
  placebo = d_sim_emax |> dplyr::slice_sample(n = 10, by = dose),
  noplacebo = d_sim_emax |> dplyr::filter(dose != 0) |> dplyr::slice_sample(n = 10, by = dose)
)

# four versions of the modelling function
dev_ermod <- list(
  dev_ermod_bin = dev_ermod_bin,
  dev_ermod_lin = dev_ermod_lin,
  dev_ermod_emax = dev_ermod_emax,
  dev_ermod_bin_emax = dev_ermod_bin_emax
)

# two versions of the placebo handling options
opts <- list(
  drop_placebo = list(include_placebo = FALSE),
  use_placebo = list(include_placebo = TRUE)
)

# all 16 test cases
cases <- tidyr::expand_grid(
  dat = names(dat),
  dev_ermod = names(dev_ermod),
  opts = names(opts),
) |> dplyr::mutate(
  var_resp = dplyr::case_when(
    dev_ermod == "dev_ermod_bin" ~ "response_2",
    dev_ermod == "dev_ermod_lin" ~ "response_1",
    dev_ermod == "dev_ermod_emax" ~ "response_1",
    dev_ermod == "dev_ermod_bin_emax" ~ "response_2"
  )
)

# fit the model for each case
m <- list()
for(r in 1:nrow(cases)) {
  f <- dev_ermod[[cases$dev_ermod[r]]]
  d <- dat[[cases$dat[r]]]
  o <- opts[[cases$opts[r]]]
  set.seed(123L)
  suppressWarnings(
    m[[r]] <- d |> f(
      var_exposure = "exposure_1",
      var_resp = cases$var_resp[r],
      options_placebo_handling = o,
      chains = 1,
      iter = 100
    )
  ) 
}
cases$mod <- m

# helper function to inspect internal data objects
internal_stan_data_rows <- function(object) {
  if (inherits(object, "stanemax") | inherits(object, "stanemaxbin")) {
    return(object$standata$N)
  } 
  return(nrow(object$data))
}

cases$inner_n <- numeric(nrow(cases))
cases$outer_n <- numeric(nrow(cases))
for(r in 1:nrow(cases)) {
  cases$inner_n[r] <- internal_stan_data_rows(cases$mod[[r]]$mod)
  cases$outer_n[r] <- nrow(cases$mod[[r]]$data)
}

# the "stan" data set should only include placebo samples if the data set
# originally contained a placebo group, and the options specify that the
# placebo group is to be used during model fitting
test_that("internal stan data respects placebo options", {

  for(r in 1:nrow(cases)) {
    if (cases$dat[r] == "placebo" & cases$opts[r] == "use_placebo") {
      expect_equal(cases$inner_n[r], 40L)
    } else {
      expect_equal(cases$inner_n[r], 30L)
    }
  }

})

# the "user facing" data set should only include placebo samples if the data set
# originally contained a placebo group, regardless of the placebo handling settings
test_that("outer data preserves all data rows", {

  for(r in 1:nrow(cases)) {
    if (cases$dat[r] == "placebo") {
      expect_equal(cases$outer_n[r], 40L)
    } else {
      expect_equal(cases$outer_n[r], 30L)
    }
  }

})
