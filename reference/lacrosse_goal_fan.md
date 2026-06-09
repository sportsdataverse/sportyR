# Lacrosse Goal Fan

The goal arc fan is present on some fields (e.g. NCAAW) as a
quarter-circle located around the goal. The anchor for this feature
should be given as the center of the goal line, but the radius of the
arc actually corresponds to a point on the goal circle (see
[`lacrosse_goal_circle()`](https://sportyR.sportsdataverse.org/reference/lacrosse_goal_circle.md))
that runs through `y = 0`

## Usage

``` r
lacrosse_goal_fan(
  goal_fan_radius = 0,
  goal_circle_radius = 0,
  line_thickness = 0
)
```

## Arguments

- goal_fan_radius:

  The outer radius of the goal fan, measured from the center of the goal
  line to the outer edge of the goal fan

- goal_circle_radius:

  The radius of the goal circle

- line_thickness:

  The thickness of the goal fan line

## Value

A data frame containing the bounding coordinates of the goal fan
