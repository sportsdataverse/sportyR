# Lacrosse Goal Fan

The goal fan is the 8-meter arc present on some fields (e.g. NCAAW). The
anchor for this feature should be given as the center of the goal line.
It is made of two straight lines that start where the goal line extended
crosses the goal circle (see
[`lacrosse_goal_circle()`](https://sportyR.sportsdataverse.org/reference/lacrosse_goal_circle.md))
and run towards the center of the field at 45 degrees to the goal line
extended, joined by an arc of radius `goal_fan_radius` about the center
of the goal line (NCAA Women's Lacrosse Rules, Rule 1, Section 6)

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
