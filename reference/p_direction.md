# Probability of Direction (pd)

Compute the **Probability of Direction** (***pd***). Although
differently expressed, this index is fairly similar (*i.e.*, is strongly
correlated) to the frequentist **p-value**. See
[`bayestestR::p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html)
and
[`vignette("overview_of_vignettes", package = "bayestestR")`](https://easystats.github.io/bayestestR/articles/overview_of_vignettes.html)
\> "Probability of Direction (pd)" page for details. For converting
**pd** to a frequentist **p-value**, see
[`bayestestR::pd_to_p()`](https://easystats.github.io/bayestestR/reference/pd_to_p.html).

## Usage

``` r
# S3 method for class 'ermod_bin'
p_direction(
  x,
  null = 0,
  as_p = FALSE,
  as_num = FALSE,
  direction = "two-sided",
  ...
)
```

## Arguments

- x:

  An object of class `ermod_bin_*`

- null:

  The null hypothesis value. Default is 0.

- as_p:

  If `TRUE`, the p-direction (pd) values are converted to a frequentist
  p-value using
  [`bayestestR::pd_to_p()`](https://easystats.github.io/bayestestR/reference/pd_to_p.html).
  Only works when `as_num = TRUE`.

- as_num:

  If `TRUE`, the output is converted to a numeric value.

- direction:

  What type of p-value is requested or provided with as_p = TRUE. Can be
  `"two-sided"` (default, two tailed) or `"one-sided"` (one tailed).

- ...:

  Additional arguments passed to
  [`bayestestR::p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html).

## Value

See
[`bayestestR::p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html)
for details.

## Details

For the class `ermod_bin_*`, it only calculates the **pd** for the
exposure variable.

## Examples

``` r
# \donttest{
library(bayestestR)

df_er_dr2 <-
  d_sim_binom_cov |>
  dplyr::filter(
    AETYPE == "dr2",
    ID %in% seq(1, 500, by = 5)
  ) |>
  dplyr::mutate(AUCss_1000 = AUCss / 1000, BHBA1C_5 = BHBA1C / 5)

ermod_bin <- dev_ermod_bin(
  data = df_er_dr2,
  var_resp = "AEFLAG",
  var_exposure = "AUCss_1000",
  var_cov = "BHBA1C_5"
)

p_direction(ermod_bin, as_num = TRUE, as_p = TRUE)
#> [1] 0.594
# }
```
