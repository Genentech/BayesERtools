library(BayesERtools)

# a data set that includes a placebo group
dat <- dplyr::bind_rows(
  readr::read_csv("data-raw/d_sim_emax.csv"), # current emax data set in pacakge
  readr::read_csv("other/d_sim_emax_placebo.csv") # placebo data from same model
) |> 
  dplyr::mutate(placebo = dose == 0) # represent the placebo group explicitly

# scenario 1: nothing should change if the user doesn't specify placebo group
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


# scenario 2: model should not pass placebo data to stan if user requests
mod2 <- dat |> 
  dev_ermod_bin(
    var_resp = "response_2",
    var_exposure = "exposure",
    var_placebo = "placebo",
    exclude_placebo = TRUE
  )

mod2 |> 
  sim_er_curve(output_type = "median_qi") |> 
  plot_er(
    show_orig_data = TRUE, 
    options_orig_data = list(var_group = "dose")
  )
