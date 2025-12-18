# Transform to `draws` objects

See
[`posterior::as_draws()`](https://mc-stan.org/posterior/reference/draws.html)
for details.

## Usage

``` r
as_draws(x, ...)

as_draws_list(x, ...)

as_draws_array(x, ...)

as_draws_df(x, ...)

as_draws_matrix(x, ...)

as_draws_rvars(x, ...)

# S3 method for class 'ermod'
as_draws(x, ...)

# S3 method for class 'ermod'
as_draws_list(x, ...)

# S3 method for class 'ermod'
as_draws_array(x, ...)

# S3 method for class 'ermod'
as_draws_df(x, ...)

# S3 method for class 'ermod'
as_draws_matrix(x, ...)

# S3 method for class 'ermod'
as_draws_rvars(x, ...)
```

## Arguments

- x:

  An object of class `ermod`

- ...:

  Arguments passed to individual methods (if applicable).

## Value

A draws object from the `posterior` package.
