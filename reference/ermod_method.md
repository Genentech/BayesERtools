# S3 methods for the classes `ermod_*`

S3 methods for the classes `ermod_*`

## Usage

``` r
# S3 method for class 'ermod'
print(x, digits = 2, ...)

# S3 method for class 'ermod_bin'
plot(x, show_orig_data = FALSE, ...)

# S3 method for class 'ermod'
coef(object, ...)

# S3 method for class 'ermod'
summary(object, ...)
```

## Arguments

- x:

  An object of class `ermod_*`

- digits:

  Number of digits to print

- ...:

  Additional arguments passed to functions

- show_orig_data:

  logical, whether to show the data points in the model development
  dataset. Default is `FALSE`. Only support plotting with data that was
  used in the model development. If you want to use other data, consider
  adding geom_point() to the plot manually.

- object:

  An object of class `ermod_*`

## Value

- [`print()`](https://rdrr.io/r/base/print.html) and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html): No return
  value, called for side effects

- [`coef()`](https://rdrr.io/r/stats/coef.html): Coefficients of the
  model

- [`summary()`](https://rdrr.io/r/base/summary.html): Summary of the
  model
