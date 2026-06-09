# Draw Lacrosse Field

Generate a `ggplot2` instance containing a lacrosse field for a
specified league

## Usage

``` r
geom_lacrosse(
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

  `"offense"`

  :   The offensive half of the field. This is the right half of the
      field in TV view

  `"offence"`

  :   The offensive half of the field. This is the right half of the
      field in TV view

  `"offensivehalffield"`

  :   The offensive half of the field. This is the right half of the
      field in TV view

  `"offensive_half_field"`

  :   The offensive half of the field. This is the right half of the
      field in TV view

  `"offensive half field"`

  :   The offensive half of the field. This is the right half of the
      field in TV view

  `"defense"`

  :   The defensive half of the field. This is the left half of the
      field in TV view

  `"defence"`

  :   The defensive half of the field. This is the left half of the
      field in TV view

  `"defensivehalffield"`

  :   The defensive half of the field. This is the left half of the
      field in TV view

  `"defensive_half_field"`

  :   The defensive half of the field. This is the left half of the
      field in TV view

  `"defensive half field"`

  :   The defensive half of the field. This is the left half of the
      field in TV view

- field_updates:

  A list of updates to the fields' parameters. These will overwrite the
  parameters of the league

- color_updates:

  A list of updates to the fields' default colors, which are set by
  [`lacrosse_features_set_colors()`](https://sportyR.sportsdataverse.org/reference/lacrosse_features_set_colors.md)

- rotation:

  An angle, given in degrees, through which the plot should be rotated

- x_trans:

  The amount that the `x` coordinates are to be shifted. By convention,
  the +`x` axis extends from the center of the field towards the
  right-hand basket when viewing the field in TV View

- y_trans:

  The amount that the `y` coordinates are to be shifted. By convention,
  the +`y` axis extends from the center of the field towards the top of
  the field when viewing the field in TV view

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

A `ggplot2` instance with a full-surface representation of a lacrosse
field

## Examples

``` r
if (FALSE) { # \dontrun{
  geom_lacrosse(league = "NCAA", rotation = 270, display_range = "offense")
  geom_lacrosse(league = "FIVB", field_units = "ft")
} # }
```
