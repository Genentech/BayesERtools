# Efficient approximate leave-one-out cross-validation (LOO)

See [`loo::loo()`](https://mc-stan.org/loo/reference/loo.html) for
details.

## Usage

``` r
loo(x, ...)

# S3 method for class 'ermod'
loo(x, ...)

# S3 method for class 'ermod_emax'
loo(x, ...)

# S3 method for class 'ermod_bin_emax'
loo(x, ...)
```

## Arguments

- x:

  An object of class `ermod`

- ...:

  Additional arguments passed to
  [`loo::loo()`](https://mc-stan.org/loo/reference/loo.html)

## Value

An object of class `loo`
