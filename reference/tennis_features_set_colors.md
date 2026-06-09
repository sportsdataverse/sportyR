# Tennis Feature Colors

Set the colors to be used for the plot. The values provided in the
arguments are the defaults, and, where specified, are the rule-book
specified values.

## Usage

``` r
tennis_features_set_colors(
  plot_background = NULL,
  baseline = "#ffffff",
  singles_sideline = "#ffffff",
  doubles_sideline = "#ffffff",
  serviceline = "#ffffff",
  center_serviceline = "#ffffff",
  center_mark = "#ffffff",
  ad_court = "#395d33",
  deuce_court = "#395d33",
  backcourt = "#395d33",
  doubles_alley = "#395d33",
  court_apron = "#395d33",
  net = "#d3d3d3"
)
```

## Arguments

- plot_background:

  A hexadecimal string representing the color to use for this feature

- baseline:

  A hexadecimal string representing the color to use for this feature

- singles_sideline:

  A hexadecimal string representing the color to use for this feature

- doubles_sideline:

  A hexadecimal string representing the color to use for this feature

- serviceline:

  A hexadecimal string representing the color to use for this feature

- center_serviceline:

  A hexadecimal string representing the color to use for this feature

- center_mark:

  A hexadecimal string representing the color to use for this feature

- ad_court:

  A hexadecimal string representing the color to use for this feature

- deuce_court:

  A hexadecimal string representing the color to use for this feature

- backcourt:

  A hexadecimal string representing the color to use for this feature

- doubles_alley:

  A hexadecimal string representing the color to use for this feature

- court_apron:

  A hexadecimal string representing the color to use for this feature

- net:

  A hexadecimal string representing the color to use for this feature

## Value

A list of hexadecimal colors to use to color the features on the
resulting plot

## Details

Hexadecimal values are the passed vales to this function by default, but
it is also possible to use string-named values (e.g. `"dodgerblue"`)
when specifying.
