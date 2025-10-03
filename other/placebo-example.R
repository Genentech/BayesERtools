library(BayesERtools)

# logistic regression, no placebo
d_sim_emax |> 
  dev_ermod_bin(
    var_resp = "response_2",
    var_exposure = "exposure_1",
    options_placebo_handling = list(include_placebo = FALSE)
  ) |> 
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
