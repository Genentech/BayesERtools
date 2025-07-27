library(BayesERtools)

# a data set that includes a placebo group
dat <- dplyr::bind_rows(
  readr::read_csv("data-raw/d_sim_emax.csv"), # current emax data set in pacakge
  readr::read_csv("other/d_sim_emax_placebo.csv") # placebo data from same model
) |> 
  dplyr::mutate(placebo = dose == 0) # represent the placebo group explicitly

# scenario 1: default is to implicitly drop placebo, assuming that the placebo
# group maps onto the zero exposure rows
mod1 <- dat |> 
  dev_ermod_bin(
    var_resp = "response_2",
    var_exposure = "exposure"
  )

mod1 |> 
  sim_er_curve(output_type = "median_qi") |> 
  plot_er(
    show_orig_data = TRUE, 
    options_orig_data = list(var_group = "dose")
  )


# scenario 2: model should n
mod2 <- dat |> 
  dev_ermod_bin(
    var_resp = "response_2",
    var_exposure = "exposure",
    options_placebo_handling = list(
      include_placebo = TRUE,
      method = "var_placebo",
      var_placebo = "placebo"
    )
  )

mod2 |> 
  sim_er_curve(output_type = "median_qi") |> 
  plot_er(
    show_orig_data = TRUE, 
    options_orig_data = list(var_group = "dose")
  )
