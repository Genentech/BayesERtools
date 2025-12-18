# Calculate median and quantile intervals from ersim object

This is useful when you performed simulation with output_type = "draws"
and want to calculate median and quantile intervals without
re-simulating.

## Usage

``` r
calc_ersim_med_qi(x, qi_width = 0.95)
```

## Arguments

- x:

  An object of class `ersim` or `ersim_marg`

- qi_width:

  Width of the quantile interval

## Value

An object of class `ersim_med_qi` or `ersim_marg_med_qi`
