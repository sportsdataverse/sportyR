# Add Feature to Plot

Add a surface's feature to a `ggplot2` instance

## Usage

``` r
add_feature(
  g,
  x_anchor,
  y_anchor,
  feature_df,
  feature_color,
  feature_outline_color = "#ffffff00",
  reflect_x = FALSE,
  reflect_y = FALSE,
  x_trans = 0,
  y_trans = 0,
  rotation = 0,
  group = NULL
)
```

## Arguments

- g:

  The `ggplot2` instance onto which the feature will be added

- x_anchor:

  The anchor point along the `x` axis for the feature

- y_anchor:

  The anchor point along the `y` axis for the feature

- feature_df:

  The data frame containing the points to add to the feature

- feature_color:

  A hexadecimal string with which to color the feature once added to the
  plot

- feature_outline_color:

  A hexadecimal string with which to color the outline of the feature
  added to the plot. The default value is `"#ffffff00"`, which is white
  with a 0% alpha value. This results in no outline being added, which
  is usually desirable, but may be overwritten to prevent "seams" from
  appearing in the resulting plot

- reflect_x:

  Whether or not to reflect the feature over the `x` axis

- reflect_y:

  Whether or not to reflect the feature over the `y` axis

- group:

  A grouping to pass along to
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  This is used for speed in the NFL and NCAA Football plotting functions

## Value

A `ggplot2` instance with the feature added to it
