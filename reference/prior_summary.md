# Summarize the priors used for linear or linear logistic regression models

See
[`rstanarm::prior_summary()`](https://mc-stan.org/rstanarm/reference/prior_summary.stanreg.html)
for details.

## Usage

``` r
prior_summary(object, ...)

# S3 method for class 'ermod'
prior_summary(object, ...)
```

## Arguments

- object:

  An object of class `ermod`

- ...:

  Additional arguments passed to
  [`rstanarm::prior_summary()`](https://mc-stan.org/rstantools/reference/prior_summary.html)

## Value

An object of class `prior_summary.stanreg`
