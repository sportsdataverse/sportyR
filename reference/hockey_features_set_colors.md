# Hockey Feature Colors

Set the colors to be used for the plot. The values provided in the
arguments are the defaults, and, where specified, are the rule-book
specified values.

## Usage

``` r
hockey_features_set_colors(
  plot_background = NULL,
  boards = "#000000",
  ozone_ice = "#ffffff",
  nzone_ice = "#ffffff",
  dzone_ice = "#ffffff",
  center_line = "#c8102e",
  zone_line = "#0033a0",
  goal_line = "#c8102e",
  restricted_trapezoid = "#c8102e",
  goal_crease_outline = "#c8102e",
  goal_crease_fill = "#41b6e6",
  referee_crease = "#c8102e",
  center_faceoff_spot = "#0033a0",
  faceoff_spot_ring = "#c8102e",
  faceoff_spot_stripe = "#c8102e",
  center_faceoff_circle = "#0033a0",
  odzone_faceoff_circle = "#c8102e",
  faceoff_line = "#c8102e",
  goal_frame = "#c8102e",
  goal_fill = "#a5acaf4d",
  team_a_bench = "#ffffff",
  team_b_bench = "#ffffff",
  team_a_penalty_box = "#ffffff",
  team_b_penalty_box = "#ffffff",
  off_ice_officials_box = "#a5acaf"
)
```

## Arguments

- plot_background:

  A hexadecimal string representing the color to use for this feature

- boards:

  A hexadecimal string representing the color to use for this feature

- ozone_ice:

  A hexadecimal string representing the color to use for this feature

- nzone_ice:

  A hexadecimal string representing the color to use for this feature

- dzone_ice:

  A hexadecimal string representing the color to use for this feature

- center_line:

  A hexadecimal string representing the color to use for this feature

- zone_line:

  A hexadecimal string representing the color to use for this feature

- goal_line:

  A hexadecimal string representing the color to use for this feature

- restricted_trapezoid:

  A hexadecimal string representing the color to use for this feature

- goal_crease_outline:

  A hexadecimal string representing the color to use for this feature

- goal_crease_fill:

  A hexadecimal string representing the color to use for this feature

- referee_crease:

  A hexadecimal string representing the color to use for this feature

- center_faceoff_spot:

  A hexadecimal string representing the color to use for this feature

- faceoff_spot_ring:

  A hexadecimal string representing the color to use for this feature

- faceoff_spot_stripe:

  A hexadecimal string representing the color to use for this feature

- center_faceoff_circle:

  A hexadecimal string representing the color to use for this feature

- odzone_faceoff_circle:

  A hexadecimal string representing the color to use for this feature

- faceoff_line:

  A hexadecimal string representing the color to use for this feature

- goal_frame:

  A hexadecimal string representing the color to use for this feature

- goal_fill:

  A hexadecimal string representing the color to use for this feature

- team_a_bench:

  A hexadecimal string representing the color to use for this feature

- team_b_bench:

  A hexadecimal string representing the color to use for this feature

- team_a_penalty_box:

  A hexadecimal string representing the color to use for this feature

- team_b_penalty_box:

  A hexadecimal string representing the color to use for this feature

- off_ice_officials_box:

  A hexadecimal string representing the color to use for this feature

## Value

A list of hexadecimal colors to use to color the features on the
resulting plot

## Details

Hexadecimal values are the passed vales to this function by default, but
it is also possible to use string-named values (e.g. `"dodgerblue"`)
when specifying.
