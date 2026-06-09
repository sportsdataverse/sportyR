# Draw Basketball Court

Generate a `ggplot2` instance containing a basketball court for a
specified league

## Usage

``` r
geom_basketball(
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

  :   The TV-right half of the court half-court. This is considered the
      offensive half of the court

  `"offence"`

  :   The TV-right half of the court half-court. This is considered the
      offensive half of the court

  `"offensivehalfcourt"`

  :   The TV-right half of the court half-court. This is considered the
      offensive half of the court

  `"offensive_half_court"`

  :   The TV-right half of the court half-court. This is considered the
      offensive half of the court

  `"offensive half court"`

  :   The TV-right half of the court half-court. This is considered the
      offensive half of the court

  `"defense"`

  :   The TV-left half of the court half-court. This is considered the
      defensive half of the court

  `"defence"`

  :   The TV-left half of the court half-court. This is considered the
      defensive half of the court

  `"defensivehalfcourt"`

  :   The TV-left half of the court half-court. This is considered the
      defensive half of the court

  `"defensive_half_court"`

  :   The TV-left half of the court half-court. This is considered the
      defensive half of the court

  `"defensive half court"`

  :   The TV-left half of the court half-court. This is considered the
      defensive half of the court

  `"offensivekey"`

  :   The TV-right offensive key (three-point line and two-point range)

  `"offensive_key"`

  :   The TV-right offensive key (three-point line and two-point range)

  `"offensive key"`

  :   The TV-right offensive key (three-point line and two-point range)

  `"attackingkey"`

  :   The TV-right offensive key (three-point line and two-point range)

  `"attacking_key"`

  :   The TV-right offensive key (three-point line and two-point range)

  `"attacking key"`

  :   The TV-right offensive key (three-point line and two-point range)

  `"defensivekey"`

  :   The TV-left defensive key (three-point line and two-point range)

  `"defensive_key"`

  :   The TV-left defensive key (three-point line and two-point range)

  `"defensive key"`

  :   The TV-left defensive key (three-point line and two-point range)

  `"defendingkey"`

  :   The TV-left defensive key (three-point line and two-point range)

  `"defending_key"`

  :   The TV-left defensive key (three-point line and two-point range)

  `"defending key"`

  :   The TV-left defensive key (three-point line and two-point range)

  `"offensivepaint"`

  :   The TV-right offensive free-throw lane

  `"offensive_paint"`

  :   The TV-right offensive free-throw lane

  `"offensive paint"`

  :   The TV-right offensive free-throw lane

  `"attackingpaint"`

  :   The TV-right offensive free-throw lane

  `"attacking_paint"`

  :   The TV-right offensive free-throw lane

  `"attacking paint"`

  :   The TV-right offensive free-throw lane

  `"offensivelane"`

  :   The TV-right offensive free-throw lane

  `"offensive_lane"`

  :   The TV-right offensive free-throw lane

  `"offensive lane"`

  :   The TV-right offensive free-throw lane

  `"attackinglane"`

  :   The TV-right offensive free-throw lane

  `"attacking_lane"`

  :   The TV-right offensive free-throw lane

  `"attacking lane"`

  :   The TV-right offensive free-throw lane

  `"defensivepaint"`

  :   The TV-left defensive free-throw lane

  `"defensive_paint"`

  :   The TV-left defensive free-throw lane

  `"defensive paint"`

  :   The TV-left defensive free-throw lane

  `"defendingpaint"`

  :   The TV-left defensive free-throw lane

  `"defending_paint"`

  :   The TV-left defensive free-throw lane

  `"defending paint"`

  :   The TV-left defensive free-throw lane

  `"defensivelane"`

  :   The TV-left defensive free-throw lane

  `"defensive_lane"`

  :   The TV-left defensive free-throw lane

  `"defensive lane"`

  :   The TV-left defensive free-throw lane

  `"defendinglane"`

  :   The TV-left defensive free-throw lane

  `"defending_lane"`

  :   The TV-left defensive free-throw lane

  `"defending lane"`

  :   The TV-left defensive free-throw lane

- court_updates:

  A list of updates to the courts' parameters. These will overwrite the
  parameters of the league

- color_updates:

  A list of updates to the courts' default colors, which are set by
  [`basketball_features_set_colors()`](https://sportyR.sportsdataverse.org/reference/basketball_features_set_colors.md)

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

A `ggplot2` instance with a full-surface representation of a basketball
court

## Examples

``` r
if (FALSE) { # \dontrun{
  geom_basketball(league = "NBA", rotation = 270, display_range = "offense")
  geom_basketball(league = "fiba", court_units = "ft")
} # }
```
