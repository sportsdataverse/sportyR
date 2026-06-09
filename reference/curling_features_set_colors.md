# Curling Feature Colors

Set the colors to be used for the plot. The values provided in the
arguments are the defaults, and, where specified, are the rule-book
specified values.

## Usage

``` r
curling_features_set_colors(
  plot_background = NULL,
  end_1 = "#ffffff",
  centre_zone = "#ffffff",
  end_2 = "#ffffff",
  sheet_apron = "#0033a0",
  centre_line = "#000000",
  tee_line = "#000000",
  back_line = "#000000",
  hog_line = "#c8102e",
  hack_line = "#000000",
  courtesy_line = "#000000",
  hack = "#000000",
  button = "#ffffff",
  house_rings = c("#c8102e", "#ffffff", "#0033a0")
)
```

## Arguments

- plot_background:

  A hexadecimal string representing the color to use for this feature

- end_1:

  A hexadecimal string representing the color to use for this feature

- centre_zone:

  A hexadecimal string representing the color to use for this feature

- end_2:

  A hexadecimal string representing the color to use for this feature

- sheet_apron:

  A hexadecimal string representing the color to use for this feature

- centre_line:

  A hexadecimal string representing the color to use for this feature

- tee_line:

  A hexadecimal string representing the color to use for this feature

- back_line:

  A hexadecimal string representing the color to use for this feature

- hog_line:

  A hexadecimal string representing the color to use for this feature

- hack_line:

  A hexadecimal string representing the color to use for this feature

- courtesy_line:

  A hexadecimal string representing the color to use for this feature

- hack:

  A hexadecimal string representing the color to use for this feature

- button:

  A hexadecimal string representing the color to use for this feature

- house_rings:

  A vector of hexadecimal strings representing the color(s) to use for
  this feature

## Value

A list of hexadecimal colors to use to color the features on the
resulting plot

## Details

Hexadecimal values are the passed vales to this function by default, but
it is also possible to use string-named values (e.g. `"dodgerblue"`)
when specifying.
