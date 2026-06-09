# Draw Hockey Rink

Generate a `ggplot2` instance containing an ice rink for a specified
league

## Usage

``` r
geom_hockey(
  league,
  display_range = "full",
  rink_updates = list(),
  color_updates = list(),
  rotation = 0,
  x_trans = 0,
  y_trans = 0,
  rink_units = NULL,
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

  :   The full ice surface. This is the default

  `"in_bounds_only"`

  :   The full in-bounds area of the rink

  `"in bounds only"`

  :   The full in-bounds area of the rink

  `"offense"`

  :   The TV-right half of the rink

  `"offence"`

  :   The TV-right half of the rink

  `"defense"`

  :   The TV-left half of the rink

  `"defence"`

  :   The TV-left half of the rink

  `"ozone"`

  :   The TV-right zone of the rink

  `"offensive_zone"`

  :   The TV-right zone of the rink

  `"offensive zone"`

  :   The TV-right zone of the rink

  `"attacking_zone"`

  :   The TV-right zone of the rink

  `"attacking zone"`

  :   The TV-right zone of the rink

  `"dzone"`

  :   The TV-left zone of the rink

  `"defensive_zone"`

  :   The TV-left zone of the rink

  `"defensive zone"`

  :   The TV-left zone of the rink

  `"defending_zone"`

  :   The TV-left zone of the rink

  `"defending zone"`

  :   The TV-left zone of the rink

  `"nzone"`

  :   The middle zone of the rink

  `"neutral"`

  :   The middle zone of the rink

  `"neutral_zone"`

  :   The middle zone of the rink

  `"neutral zone"`

  :   The middle zone of the rink

- rink_updates:

  A list of updates to the rink's parameters. These will overwrite the
  parameters of the league

- color_updates:

  A list of updates to the courts' default colors, which are set by
  [`hockey_features_set_colors()`](https://sportyR.sportsdataverse.org/reference/hockey_features_set_colors.md)

- rotation:

  An angle, given in degrees, through which the plot should be rotated

- x_trans:

  The amount that the `x` coordinates are to be shifted. By convention,
  the +`x` axis extends from the center of the ice surface towards the
  right-hand goal when viewing the rink in TV View

- y_trans:

  The amount that the `y` coordinates are to be shifted. By convention,
  the +`y` axis extends from the center of the ice surface towards the
  top of the rink when viewing the rink in TV view

- rink_units:

  The units with which to draw the rink. The default is `NULL`, which
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

A `ggplot2` instance with a full-surface representation of an ice hockey
rink

## Examples

``` r
if (FALSE) { # \dontrun{
  geom_hockey(league = "NHL", rotation = 270, display_range = "ozone")
  geom_hockey(league = "iihf", rink_units = "ft")
} # }
```
