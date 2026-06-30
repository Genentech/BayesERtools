# Plot exposure metric selection comparison

Plot ER curve for each exposure metric and compare them.

## Usage

``` r
plot_er_exp_sel(x, n_draws_sim = NULL)
```

## Arguments

- x:

  An object of class `ermod_bin_exp_sel`

- n_draws_sim:

  Number of draws to simulate response for each exposure value. Default
  is NULL (use all draws in the model object)

## Value

No return value, called for plotting side effect.

## Examples

``` r
# \donttest{
data(d_sim_binom_cov_hgly2)

ermod_bin_exp_sel <-
  dev_ermod_bin_exp_sel(
    data = d_sim_binom_cov_hgly2,
    var_resp = "AEFLAG",
    var_exp_candidates = c("AUCss_1000", "Cmaxss", "Cminss")
  )
#> ℹ The exposure metric selected was: AUCss_1000

plot_er_exp_sel(ermod_bin_exp_sel) +
  xgxr::xgx_scale_x_log10(guide = ggplot2::guide_axis(minor.ticks = TRUE))
#> Error in tidyr::pivot_longer(extract_data(x), dplyr::all_of(var_exp_order),     names_to = ".exp_metric", values_to = ".exposure"): ℹ In argument: `dplyr::all_of(var_exp_order)`.
#> Caused by error in `dplyr::all_of()`:
#> ! Can't subset elements that don't exist.
#> ✖ Elements `1`, `2`, and `3` don't exist.
# }
```
