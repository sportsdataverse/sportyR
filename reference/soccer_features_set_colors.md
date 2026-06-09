# Soccer Feature Colors

Set the colors to be used for the plot. The values provided in the
arguments are the defaults, and, where specified, are the rule-book
specified values.

## Usage

``` r
soccer_features_set_colors(
  plot_background = NULL,
  offensive_half_pitch = "#195f0c",
  defensive_half_pitch = "#195f0c",
  pitch_apron = "#195f0c",
  touchline = "#ffffff",
  goal_line = "#ffffff",
  corner_arc = "#ffffff",
  halfway_line = "#ffffff",
  center_circle = "#ffffff",
  center_mark = "#ffffff",
  penalty_box = "#ffffff",
  goal_box = "#ffffff",
  penalty_mark = "#ffffff",
  corner_defensive_mark = "#ffffff",
  goal = "#ffffff"
)
```

## Arguments

- plot_background:

  A hexadecimal string representing the color to use for this feature

- touchline:

  A hexadecimal string representing the color to use for this feature

- goal_line:

  A hexadecimal string representing the color to use for this feature

- halfway_line:

  A hexadecimal string representing the color to use for this feature

- center_circle:

  A hexadecimal string representing the color to use for this feature

- center_mark:

  A hexadecimal string representing the color to use for this feature

- penalty_box:

  A hexadecimal string representing the color to use for this feature

- goal_box:

  A hexadecimal string representing the color to use for this feature

- penalty_mark:

  A hexadecimal string representing the color to use for this feature

- corner_defensive_mark:

  A hexadecimal string representing the color to use for this feature

- goal:

  A hexadecimal string representing the color to use for this feature

## Value

A list of hexadecimal colors to use to color the features on the
resulting plot

## Details

Hexadecimal values are the passed vales to this function by default, but
it is also possible to use string-named values (e.g. `"dodgerblue"`)
when specifying.
