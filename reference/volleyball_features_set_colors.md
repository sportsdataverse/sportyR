# Volleyball Feature Colors

Set the colors to be used for the plot. The values provided in the
arguments are the defaults, and, where specified, are the rule-book
specified values.

## Usage

``` r
volleyball_features_set_colors(
  plot_background = NULL,
  free_zone = "#d2ab6f",
  front_zone = "#d2ab6f",
  defensive_backcourt = "#d2ab6f",
  offensive_backcourt = "#d2ab6f",
  court_apron = "#d2ab6f",
  end_line = "#000000",
  sideline = "#000000",
  attack_line = "#000000",
  center_line = "#000000",
  service_zone_mark = "#000000",
  substitution_zone = "#000000"
)
```

## Arguments

- plot_background:

  A hexadecimal string representing the color to use for this feature

- free_zone:

  A hexadecimal string representing the color to use for this feature

- front_zone:

  A hexadecimal string representing the color to use for this feature

- defensive_backcourt:

  A hexadecimal string representing the color to use for this feature

- offensive_backcourt:

  A hexadecimal string representing the color to use for this feature

- court_apron:

  A hexadecimal string representing the color to use for this feature

- end_line:

  A hexadecimal string representing the color to use for this feature

- sideline:

  A hexadecimal string representing the color to use for this feature

- attack_line:

  A hexadecimal string representing the color to use for this feature

- center_line:

  A hexadecimal string representing the color to use for this feature

- service_zone_mark:

  A hexadecimal string representing the color to use for this feature

- substitution_zone:

  A hexadecimal string representing the color to use for this feature

## Value

A list of hexadecimal colors to use to color the features on the
resulting plot

## Details

Hexadecimal values are the passed vales to this function by default, but
it is also possible to use string-named values (e.g. `"dodgerblue"`)
when specifying.
