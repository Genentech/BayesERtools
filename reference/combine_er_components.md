# Combine ER plot components

Combine ER plot components

## Usage

``` r
combine_er_components(
  components,
  heights = NULL,
  add_caption = !is.null(components$caption),
  ...
)
```

## Arguments

- components:

  An object of class `er_plot_components` returned by
  [`plot_er()`](https://genentech.github.io/BayesERtools/reference/plot_er.md)
  or
  [`plot_er_gof()`](https://genentech.github.io/BayesERtools/reference/plot_er_gof.md)
  with `return_components = TRUE` or
  `options_orig_data = list(return_components = TRUE)`.

- heights:

  Numeric vector of length 2 specifying the relative heights of the main
  plot and boxplot. If `NULL` (default), uses the height from the
  metadata (based on `boxplot_height` parameter).

- add_caption:

  Logical, whether to add the caption to the combined plot. Default is
  `TRUE` if caption is available.

- ...:

  Additional arguments passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).

## Value

A combined ggplot object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get components
comps <- plot_er_gof(ermod_bin, return_components = TRUE)

# Modify components
comps$main <- comps$main + labs(title = "Custom Title")

# Recombine
combine_er_components(comps)
} # }
```
