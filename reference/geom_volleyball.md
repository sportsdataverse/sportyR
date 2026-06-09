# Draw Volleyball Court

Generate a `ggplot2` instance containing a volleyball court for a
specified league

## Usage

``` r
geom_volleyball(
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

  `"offense"`

  :   The offensive half of the court. This is the right half of the
      court in TV view

  `"offence"`

  :   The offensive half of the court. This is the right half of the
      court in TV view

  `"offensivehalfcourt"`

  :   The offensive half of the court. This is the right half of the
      court in TV view

  `"offensive_half_court"`

  :   The offensive half of the court. This is the right half of the
      court in TV view

  `"offensive half court"`

  :   The offensive half of the court. This is the right half of the
      court in TV view

  `"defense"`

  :   The defensive half of the court. This is the left half of the
      court in TV view

  `"defence"`

  :   The defensive half of the court. This is the left half of the
      court in TV view

  `"defensivehalfcourt"`

  :   The defensive half of the court. This is the left half of the
      court in TV view

  `"defensive_half_court"`

  :   The defensive half of the court. This is the left half of the
      court in TV view

  `"defensive half court"`

  :   The defensive half of the court. This is the left half of the
      court in TV view

- court_updates:

  A list of updates to the courts' parameters. These will overwrite the
  parameters of the league

- color_updates:

  A list of updates to the courts' default colors, which are set by
  [`volleyball_features_set_colors()`](https://sportyR.sportsdataverse.org/reference/volleyball_features_set_colors.md)

- rotation:

  An angle, given in degrees, through which the plot should be rotated

- x_trans:

  The amount that the `x` coordinates are to be shifted. By convention,
  the +`x` axis extends from the center of the court towards the
  right-hand basket when viewing the court in TV View

- y_trans:

  The amount that the `y` coordinates are to be shifted. By convention,
  the +`y` axis extends from the center of the court towards the top of
  the court when viewing the court in TV view

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

A `ggplot2` instance with a full-surface representation of a volleyball
court

## Examples

``` r
if (FALSE) { # \dontrun{
  geom_volleyball(league = "NCAA", rotation = 270, display_range = "offense")
  geom_volleyball(league = "FIVB", court_units = "ft")
} # }
```
