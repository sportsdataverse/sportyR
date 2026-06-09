# Baseball Feature Colors

Set the colors to be used for the plot. The values provided in the
arguments are the defaults, and, where specified, are the rule-book
specified values.

## Usage

``` r
baseball_features_set_colors(
  plot_background = "#395d33",
  infield_dirt = "#9b7653",
  infield_grass = "#395d33",
  pitchers_mound = "#9b7653",
  base = "#ffffff",
  pitchers_plate = "#ffffff",
  batters_box = "#ffffff",
  catchers_box = "#ffffff",
  foul_line = "#ffffff",
  running_lane = "#ffffff"
)
```

## Arguments

- plot_background:

  A hexadecimal string representing the color to use for this feature

- infield_dirt:

  A hexadecimal string representing the color to use for this feature

- infield_grass:

  A hexadecimal string representing the color to use for this feature

- pitchers_mound:

  A hexadecimal string representing the color to use for this feature

- base:

  A hexadecimal string representing the color to use for this feature

- pitchers_plate:

  A hexadecimal string representing the color to use for this feature

- batters_box:

  A hexadecimal string representing the color to use for this feature

- catchers_box:

  A hexadecimal string representing the color to use for this feature

- foul_line:

  A hexadecimal string representing the color to use for this feature

- running_lane:

  A hexadecimal string representing the color to use for this feature

## Value

A list of hexadecimal colors to use to color the features on the
resulting plot

## Details

Hexadecimal values are the passed vales to this function by default, but
it is also possible to use string-named values (e.g. `"dodgerblue"`)
when specifying.
