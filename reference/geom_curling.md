# Draw Curling Sheet

Generate a `ggplot2` instance containing a curling sheet for a specified
league

## Usage

``` r
geom_curling(
  league,
  display_range = "full",
  sheet_updates = list(),
  color_updates = list(),
  rotation = 0,
  x_trans = 0,
  y_trans = 0,
  sheet_units = NULL,
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

  :   The full sheet. This is the default

  `"in_bounds_only"`

  :   The full in-bounds area of the sheet

  `"in bounds only"`

  :   The full in-bounds area of the sheet

  `"house"`

  :   A single house, which defaults to the top house in TV view

- sheet_updates:

  A list of updates to the sheet's parameters. These will overwrite the
  parameters of the league

- color_updates:

  A list of updates to the sheet's default colors, which are set by
  [`curling_features_set_colors()`](https://sportyR.sportsdataverse.org/reference/curling_features_set_colors.md)

- rotation:

  An angle, given in degrees, through which the plot should be rotated

- x_trans:

  The amount that the `x` coordinates are to be shifted. By convention,
  the +`x` axis extends from the center of the sheet towards the
  right-hand goal when viewing the sheet in TV View

- y_trans:

  The amount that the `y` coordinates are to be shifted. By convention,
  the +`y` axis extends from the center of the sheet towards the top of
  the sheet when viewing the sheet in TV view

- sheet_units:

  The units with which to draw the sheet. The default is `NULL`, which
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

A `ggplot2` instance with a full-surface representation of a curling
sheet

## Examples

``` r
if (FALSE) { # \dontrun{
  geom_curling(league = "wcf", rotation = 270, display_range = "house")
  geom_curling(league = "wcf", sheet_units = "ft")
} # }
```
