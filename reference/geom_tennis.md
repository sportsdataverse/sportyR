# Draw Tennis Court

Generate a `ggplot2` instance containing a tennis court for a specified
league

## Usage

``` r
geom_tennis(
  league,
  display_range = "full",
  court_updates = list(),
  color_updates = list(),
  rotation = 0,
  x_trans = 0,
  y_trans = 0,
  court_units = NULL,
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

  :   The full court. This is the default

  `"in_bounds_only"`

  :   The full in-bounds area of the court

  `"in bounds only"`

  :   The full in-bounds area of the court

  `"serve"`

  :   The serving half of the court

  `"serving"`

  :   The serving half of the court

  `"servicehalf"`

  :   The serving half of the court

  `"service_half"`

  :   The serving half of the court

  `"service half"`

  :   The serving half of the court

  `"servinghalf"`

  :   The serving half of the court

  `"serving_half"`

  :   The serving half of the court

  `"serving half"`

  :   The serving half of the court

  `"receive"`

  :   The receiving half of the court

  `"receiving"`

  :   The receiving half of the court

  `"receivicehalf"`

  :   The receiving half of the court

  `"receivice_half"`

  :   The receiving half of the court

  `"receivice half"`

  :   The receiving half of the court

  `"receivinghalf"`

  :   The receiving half of the court

  `"receiving_half"`

  :   The receiving half of the court

  `"receiving half"`

  :   The receiving half of the court

- court_updates:

  A list of updates to the courts' parameters. These will overwrite the
  parameters of the league

- color_updates:

  A list of updates to the courts' default colors, which are set by
  [`tennis_features_set_colors()`](https://sportyR.sportsdataverse.org/reference/tennis_features_set_colors.md)

- rotation:

  An angle, given in degrees, through which the plot should be rotated

- x_trans:

  The amount that the `x` coordinates are to be shifted. By convention,
  the +`x` axis extends from the center of the court towards the
  right-hand serviceline when viewing the court in TV View

- y_trans:

  The amount that the `y` coordinates are to be shifted. By convention,
  the +`y` axis extends from the center of the court towards the
  sideline when viewing the court in TV view

- court_units:

  The units with which to draw the court. The default is `NULL`, which
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

A `ggplot2` instance with a full-surface representation of a tennis
court

## Examples

``` r
if (FALSE) { # \dontrun{
  geom_tennis(league = "USTA", rotation = 270, display_range = "serving")
  geom_tennis(league = "itf", court_units = "m")
} # }
```
