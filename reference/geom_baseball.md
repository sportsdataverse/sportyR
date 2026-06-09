# Draw Baseball Field

Generate a `ggplot2` instance containing a baseball field for a
specified league

## Usage

``` r
geom_baseball(
  league,
  display_range = "full",
  field_updates = list(),
  color_updates = list(),
  rotation = 0,
  x_trans = 0,
  y_trans = 0,
  field_units = NULL,
  xlims = NULL,
  ylims = NULL
)
```

## Arguments

- league:

  The league for which to draw the surface. This is case-insensitive

- display_range:

  A case-insensitive string indicating the display range to use for the
  plot. The default is `"full"`, which will be returned when either an
  invalid or no value is passed to the function.

  The possible display ranges are:

  `"full"`

  :   The full field. This is the default

  `"infield"`

  :   The infield on the baseball field

- field_updates:

  A list of updates to the field's parameters. These will overwrite the
  parameters of the league

- color_updates:

  A list of updates to the field's default colors, which are set by
  [`baseball_features_set_colors()`](https://sportyR.sportsdataverse.org/reference/baseball_features_set_colors.md)

- rotation:

  An angle, given in degrees, through which the plot should be rotated

- x_trans:

  The amount that the `x` coordinates are to be shifted. By convention,
  the +`x` axis extends from the back tip of home plate towards the
  left-handed batter's box (the first base side of the field)

- y_trans:

  The amount that the `y` coordinates are to be shifted. By convention,
  the +`y` axis extends from the back tip of home plate towards
  straight-away center field

- field_units:

  The units with which to draw the field. The default is `NULL`, which
  will apply the rule-book specified units

- xlims:

  The limits on the final display in the `x` direction. The default is
  `NULL`, which will utilize the `xlims` specified by the
  `display_range` parameter

- ylims:

  The limits on the final display in the `y` direction. The default is
  `NULL`, which will utilize the `ylims` specified by the
  `display_range` parameter

## Value

A `ggplot2` instance with a full-surface representation of a baseball
field

## Examples

``` r
if (FALSE) { # \dontrun{
  geom_baseball(league = "MLB", rotation = 270, display_range = "infield")
  geom_baseball(league = "little league", field_units = "m")
} # }
```
