library(BayesERtools)

# linear regression, no placebo
mod <- d_sim_emax |> 
  dev_ermod_lin(
    var_resp = "response_1",
    var_exposure = "exposure_1",
    options_placebo_handling = list(include_placebo = FALSE)
  ) 
mod |> 
  plot_er(
    show_orig_data = TRUE, 
    options_orig_data = list(var_group = "dose")
  )

# linear regression, with placebo
mod <- d_sim_emax |> 
  dev_ermod_lin(
    var_resp = "response_1",
    var_exposure = "exposure_1",
    options_placebo_handling = list(include_placebo = TRUE)
  ) 
mod |> 
  plot_er(
    show_orig_data = TRUE, 
    options_orig_data = list(var_group = "dose")
  )

# logistic regression, no placebo
mod <- d_sim_emax |> 
  dev_ermod_bin(
    var_resp = "response_2",
    var_exposure = "exposure_1",
    options_placebo_handling = list(include_placebo = FALSE)
  ) 
mod |> 
  plot_er(
    show_orig_data = TRUE, 
    options_orig_data = list(var_group = "dose")
  )

# logistic regression, with placebo
mod <- d_sim_emax |> 
  dev_ermod_bin(
    var_resp = "response_2",
    var_exposure = "exposure_1",
    options_placebo_handling = list(include_placebo = TRUE)
  ) 
mod |> 
  plot_er(
    show_orig_data = TRUE, 
    options_orig_data = list(var_group = "dose")
  )


# emax regression, continuous response, no placebo
mod <- d_sim_emax |> 
  dev_ermod_emax(
    var_resp = "response_1",
    var_exposure = "exposure_1",
    options_placebo_handling = list(include_placebo = FALSE)
  )
mod |> 
  plot_er(
    show_orig_data = TRUE, 
    options_orig_data = list(var_group = "dose")
  )

# emax regression, continuous response, with placebo
mod <- d_sim_emax |> 
  dev_ermod_emax(
    var_resp = "response_1",
    var_exposure = "exposure_1",
    options_placebo_handling = list(include_placebo = TRUE)
  ) 
mod |> 
  plot_er(
    show_orig_data = TRUE, 
    options_orig_data = list(var_group = "dose")
  )

# emax regression, binary response, no placebo
mod <- d_sim_emax |> 
  dev_ermod_bin_emax(
    var_resp = "response_2",
    var_exposure = "exposure_1",
    options_placebo_handling = list(include_placebo = FALSE)
  ) 
mod |> 
  plot_er(
    show_orig_data = TRUE, 
    options_orig_data = list(var_group = "dose")
  )

# emax regression, binary response, with placebo
mod <- d_sim_emax |> 
  dev_ermod_bin_emax(
    var_resp = "response_2",
    var_exposure = "exposure_1",
    options_placebo_handling = list(include_placebo = TRUE)
  ) 
mod |> 
  plot_er(
    show_orig_data = TRUE, 
    options_orig_data = list(var_group = "dose")
  )
