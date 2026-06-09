# Draw Football Field

Generate a `ggplot2` instance containing a football field for a
specified league

## Usage

``` r
geom_football(
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

  `"in_bounds_only"`

  :   The full in-bounds area of the field

  `"in bounds only"`

  :   The full in-bounds area of the field

  `"offense"`

  :   The TV-right half of the field

  `"offence"`

  :   The TV-right half of the field

  `"offensivehalffield"`

  :   The TV-right half of the field

  `"offensive_half_field"`

  :   The TV-right half of the field

  `"offensive half field"`

  :   The TV-right half of the field

  `"defense"`

  :   The TV-left half of the field

  `"defence"`

  :   The TV-left half of the field

  `"defensivehalffield"`

  :   The TV-left half of the field

  `"defensive_half_field"`

  :   The TV-left half of the field

  `"defensive half field"`

  :   The TV-left half of the field

  `"redzone"`

  :   The offensive red zone of the field. This is by definition 20
      yards from the goal line

  `"red_zone"`

  :   The offensive red zone of the field. This is by definition 20
      yards from the goal line

  `"red zone"`

  :   The offensive red zone of the field. This is by definition 20
      yards from the goal line

  `"oredzone"`

  :   The offensive red zone of the field. This is by definition 20
      yards from the goal line

  `"offensive_red_zone"`

  :   The offensive red zone of the field. This is by definition 20
      yards from the goal line

  `"offensive red zone"`

  :   The offensive red zone of the field. This is by definition 20
      yards from the goal line

  `"dredzone"`

  :   The defensive red zone of the field. This is by definition 20
      yards from the goal line

  `"defensive_red_zone"`

  :   The defensive red zone of the field. This is by definition 20
      yards from the goal line

  `"defensive red zone"`

  :   The defensive red zone of the field. This is by definition 20
      yards from the goal line

- field_updates:

  A list of updates to the field's parameters. These will overwrite the
  parameters of the league

- color_updates:

  A list of updates to the field's default colors, which are set by
  [`football_features_set_colors()`](https://sportyR.sportsdataverse.org/reference/football_features_set_colors.md)

- rotation:

  An angle, given in degrees, through which the plot should be rotated

- x_trans:

  The amount that the `x` coordinates are to be shifted. By convention,
  the +`x` axis extends from the center of the field towards the
  right-hand endzone when viewing the field in TV View

- y_trans:

  The amount that the `y` coordinates are to be shifted. By convention,
  the +`y` axis extends from the center of the field towards the
  sideline when viewing the field in TV view

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

A `ggplot2` instance with a full-surface representation of a football
field

## Examples

``` r
if (FALSE) { # \dontrun{
  geom_football(league = "NFL", rotation = 270, display_range = "red_zone")
  geom_football(league = "cfl", field_units = "ft")
} # }
```
