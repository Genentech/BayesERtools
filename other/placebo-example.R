library(BayesERtools)


# fit ER models --------------------------------------------

fit_ermods <- list(

  # linear regression
  lin_drop = d_sim_emax |> 
    dev_ermod_lin(
      var_resp = "response_1",
      var_exposure = "exposure_1",
      options_placebo_handling = list(include_placebo = FALSE)
    ),
  lin_keep = d_sim_emax |> 
    dev_ermod_lin(
      var_resp = "response_1",
      var_exposure = "exposure_1",
      options_placebo_handling = list(include_placebo = TRUE)
    ),   

  # logistic regression
  bin_drop = d_sim_emax |> 
    dev_ermod_bin(
      var_resp = "response_2",
      var_exposure = "exposure_1",
      options_placebo_handling = list(include_placebo = FALSE)
    ),
  bin_keep = d_sim_emax |> 
    dev_ermod_bin(
      var_resp = "response_2",
      var_exposure = "exposure_1",
      options_placebo_handling = list(include_placebo = TRUE)
    ),
    
  # emax continuous
  emax_drop = suppressWarnings(d_sim_emax |> 
    dev_ermod_emax(
      var_resp = "response_1",
      var_exposure = "exposure_1",
      options_placebo_handling = list(include_placebo = FALSE),
      chains = 2,
      iter = 500
    )),
  emax_keep = suppressWarnings(d_sim_emax |> 
    dev_ermod_emax(
      var_resp = "response_1",
      var_exposure = "exposure_1",
      options_placebo_handling = list(include_placebo = TRUE),
      chains = 2,
      iter = 500,
  )),

  # emax binary
  emax_bin_drop = suppressWarnings(d_sim_emax |> 
    dev_ermod_bin_emax(
      var_resp = "response_2",
      var_exposure = "exposure_1",
      options_placebo_handling = list(include_placebo = FALSE),
      chains = 2,
      iter = 500
    )),
  emax_bin_keep = suppressWarnings(d_sim_emax |> 
    dev_ermod_bin_emax(
      var_resp = "response_2",
      var_exposure = "exposure_1",
      options_placebo_handling = list(include_placebo = TRUE),
      chains = 2,
      iter = 500
    ))

)

# plot ER models --------------------------------------------

plt_ermods <- fit_ermods |> 
  purrr::imap(\(m, idx) {
    m |> 
      plot_er(
        show_orig_data = TRUE, 
        options_orig_data = list(var_group = "dose")
      ) + 
      ggplot2::labs(title = idx)
  })

for(p in plt_ermods) print(p)


# fit expsel models ----------------------------------------

fit_expsel <- list(

  # linear regression
  lin_drop = d_sim_emax |> 
    dev_ermod_lin_exp_sel(
      var_resp = "response_1",
      var_exp_candidates = c("exposure_1", "exposure_2"),
      options_placebo_handling = list(include_placebo = FALSE)
    ),
  lin_keep = d_sim_emax |> 
    dev_ermod_lin_exp_sel(
      var_resp = "response_1",
      var_exp_candidates = c("exposure_1", "exposure_2"),
      options_placebo_handling = list(include_placebo = TRUE)
    ),   

  # logistic regression
  bin_drop = d_sim_emax |> 
    dev_ermod_bin_exp_sel(
      var_resp = "response_2",
      var_exp_candidates = c("exposure_1", "exposure_2"),
      options_placebo_handling = list(include_placebo = FALSE)
    ),
  bin_keep = d_sim_emax |> 
    dev_ermod_bin_exp_sel(
      var_resp = "response_2",
      var_exp_candidates = c("exposure_1", "exposure_2"),
      options_placebo_handling = list(include_placebo = TRUE)
    ),
    
  # emax continuous
  emax_drop = suppressWarnings(d_sim_emax |> 
    dev_ermod_emax_exp_sel(
      var_resp = "response_1",
      var_exp_candidates = c("exposure_1", "exposure_2"),
      options_placebo_handling = list(include_placebo = FALSE),
      chains = 2,
      iter = 500
    )),
  emax_keep = suppressWarnings(d_sim_emax |> 
    dev_ermod_emax_exp_sel(
      var_resp = "response_1",
      var_exp_candidates = c("exposure_1", "exposure_2"),
      options_placebo_handling = list(include_placebo = TRUE),
      chains = 2,
      iter = 500,
  )),

  # emax binary
  emax_bin_drop = suppressWarnings(d_sim_emax |> 
    dev_ermod_bin_emax_exp_sel(
      var_resp = "response_2",
      var_exp_candidates = c("exposure_1", "exposure_2"),
      options_placebo_handling = list(include_placebo = FALSE),
      chains = 2,
      iter = 500
    )),
  emax_bin_keep = suppressWarnings(d_sim_emax |> 
    dev_ermod_bin_emax_exp_sel(
      var_resp = "response_2",
      var_exp_candidates = c("exposure_1", "exposure_2"),
      options_placebo_handling = list(include_placebo = TRUE),
      chains = 2,
      iter = 500
    ))

)

# plot expsel models --------------------------------------------

plt_expsel <- fit_expsel |> 
  purrr::imap(\(m, idx) {
    m |> 
      plot_er(
        show_orig_data = TRUE, 
        options_orig_data = list(var_group = "dose")
      ) + 
      ggplot2::labs(title = idx)
  })

for(p in plt_expsel) print(p)


