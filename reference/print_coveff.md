# Format the covariate effect simulation results for printing

Format the covariate effect simulation results for printing

## Usage

``` r
print_coveff(
  coveffsim,
  n_sigfig = 3,
  use_seps = TRUE,
  drop_trailing_dec_mark = TRUE
)
```

## Arguments

- coveffsim:

  an object of class `coveffsim`

- n_sigfig:

  Number of significant figures to form value_label of continuous
  variables. See
  [`gt::vec_fmt_number()`](https://gt.rstudio.com/reference/vec_fmt_number.html)
  for details.

- use_seps:

  Whether to use separators for thousands in printing numbers. See
  [`gt::vec_fmt_number()`](https://gt.rstudio.com/reference/vec_fmt_number.html)
  for details.

- drop_trailing_dec_mark:

  Whether to drop the trailing decimal mark (".") in value_label of
  continuous variables. See
  [`gt::vec_fmt_number()`](https://gt.rstudio.com/reference/vec_fmt_number.html)
  for details.

## Value

A data frame with the formatted covariate effect simulation results with
the following columns:

- `var_label`: the label of the covariate

- `value_label`: the label of the covariate value

- `value_annot`: the annotation of the covariate value

- `Odds ratio` or `Response difference`: the odds ratio (for binary
  models) or response difference (for linear models) of the covariate
  effect

- `95% CI`: the 95% credible interval of the covariate effect

## Details

Note that `n_sigfig`, `use_seps`, and `drop_trailing_dec_mark` are only
applied to the response difference/odds ratio and 95% CI columns;
value_label column was already generated in an earlier step in
[`build_spec_coveff()`](https://genentech.github.io/BayesERtools/reference/build_spec_coveff.md)
or
[`sim_coveff()`](https://genentech.github.io/BayesERtools/reference/sim_coveff.md).

## Examples

``` r
# \donttest{
data(d_sim_binom_cov_hgly2)

ermod_bin <- dev_ermod_bin(
  data = d_sim_binom_cov_hgly2,
  var_resp = "AEFLAG",
  var_exposure = "AUCss_1000",
  var_cov = "BHBA1C_5",
)

print_coveff(sim_coveff(ermod_bin))
#> # A tibble: 6 × 5
#>   var_label  value_label value_annot `Odds ratio` `95% CI`        
#>   <chr>      <chr>       <chr>       <chr>        <chr>           
#> 1 AUCss_1000 0.868       5th         0.548        "[0.457, 0.649]"
#> 2 AUCss_1000 2.21        median      1            " "             
#> 3 AUCss_1000 5.30        95th        3.98         "[2.69, 6.03]"  
#> 4 BHBA1C_5   5.75        5th         0.275        "[0.200, 0.374]"
#> 5 BHBA1C_5   7.97        median      1            " "             
#> 6 BHBA1C_5   10.4        95th        4.23         "[2.99, 6.01]"  

# Linear regression model example
data(d_sim_lin)

ermod_lin <- dev_ermod_lin(
  data = d_sim_lin,
  var_resp = "response",
  var_exposure = "AUCss",
  var_cov = c("SEX", "BAGE"),
)

print_coveff(sim_coveff(ermod_lin))
#> # A tibble: 8 × 5
#>   var_label value_label value_annot `Response difference` `95% CI`         
#>   <chr>     <chr>       <chr>       <chr>                 <chr>            
#> 1 AUCss     5.00        5th         −20.5                 "[−23.2, −17.7]" 
#> 2 AUCss     50.0        median      0                     " "              
#> 3 AUCss     95.0        95th        20.5                  "[17.7, 23.2]"   
#> 4 SEX       F           1st freq    0                     " "              
#> 5 SEX       M           2nd freq    −4.07                 "[−7.77, −0.550]"
#> 6 BAGE      33.5        5th         −8.52                 "[−11.4, −5.56]" 
#> 7 BAGE      50.4        median      0                     " "              
#> 8 BAGE      67.0        95th        8.36                  "[5.45, 11.2]"   
# }
```
