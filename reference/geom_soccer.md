# Draw Soccer Pitch

Generate a `ggplot2` instance containing a soccer pitch for a specified
league

## Usage

``` r
geom_soccer(
  league,
  display_range = "full",
  pitch_updates = list(),
  color_updates = list(),
  rotation = 0,
  x_trans = 0,
  y_trans = 0,
  pitch_units = NULL,
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

  :   The full pitch. This is the default

  `"in_bounds_only"`

  :   The full in-bounds area of the pitch

  `"in bounds only"`

  :   The full in-bounds area of the pitch

  `"offense"`

  :   The TV-right half of the pitch

  `"offence"`

  :   The TV-right half of the pitch

  `"offensivehalfpitch"`

  :   The TV-right half of the pitch

  `"offensive_half_pitch"`

  :   The TV-right half of the pitch

  `"offensive half pitch"`

  :   The TV-right half of the pitch

  `"defense"`

  :   The TV-left half of the pitch

  `"defence"`

  :   The TV-left half of the pitch

  `"defensivehalfpitch"`

  :   The TV-left half of the pitch

  `"defensive_half_pitch"`

  :   The TV-left half of the pitch

  `"defensive half pitch"`

  :   The TV-left half of the pitch

- pitch_updates:

  A list of updates to the pitch's parameters. These will overwrite the
  parameters of the league

- color_updates:

  A list of updates to the pitch's default colors, which are set by
  [`soccer_features_set_colors()`](https://sportyR.sportsdataverse.org/reference/soccer_features_set_colors.md)

- rotation:

  An angle, given in degrees, through which the plot should be rotated

- x_trans:

  The amount that the `x` coordinates are to be shifted. By convention,
  the +`x` axis extends from the center of the pitch towards the
  right-hand goal when viewing the pitch in TV View

- y_trans:

  The amount that the `y` coordinates are to be shifted. By convention,
  the +`y` axis extends from the center of the pitch towards the top of
  the pitch when viewing the pitch in TV view

- pitch_units:

  The units with which to draw the pitch. The default is `NULL`, which
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

A `ggplot2` instance with a full-surface representation of a soccer
pitch

## Examples

``` r
if (FALSE) { # \dontrun{
  geom_soccer(league = "EPL", rotation = 270, display_range = "offense")
  geom_soccer(league = "fifa", pitch_units = "ft")
} # }
```
