# S3 methods for the classes `ersim_*` and `ersim_med_qi_*`

S3 methods for the classes `ersim_*` and `ersim_med_qi_*`

## Usage

``` r
# S3 method for class 'ersim'
plot(x, show_orig_data = FALSE, ...)

# S3 method for class 'ersim_med_qi'
plot(x, show_orig_data = FALSE, ...)
```

## Arguments

- x:

  An object of the classes `ersim_*` or `ersim_med_qi_*`

- show_orig_data:

  logical, whether to show the data points in the model development
  dataset. Default is `FALSE`. Only support plotting with data that was
  used in the model development. If you want to use other data, consider
  adding geom_point() to the plot manually.

- ...:

  Additional arguments passed to functions

## Value

No return value, called for print or plot side effects
