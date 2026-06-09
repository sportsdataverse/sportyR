# Lacrosse Goal Circle (Outline)

The goal circle is a circular feature on the field that houses the goal
line (see
[`lacrosse_goal_line()`](https://sportyR.sportsdataverse.org/reference/lacrosse_goal_line.md))
and the goal. Notably, for fields with a surrounding arc and/or fan
around the goal area, this feature *only* circumscribes the goal. Those
features will be handled separately. This feature may either be the full
circle (e.g. all 360 degrees), or a partial circle that may be greater
than a half-circle

## Usage

``` r
lacrosse_goal_circle(
  goal_circle_radius = 0,
  line_thickness = 0,
  goal_circle_full_360 = TRUE,
  goal_depth = 0,
  goal_depth_to_circle = 0
)
```

## Arguments

- goal_circle_radius:

  The outer radius of the goal circle

- line_thickness:

  The thickness of the goal circle

- goal_circle_full_360:

  A boolean indicating whether the goal circle should be a 360 degree
  circle

- goal_depth:

  The depth of the goal

- goal_depth_to_circle:

  The distance from the back tip of the goal to the outer radius of the
  goal circle

## Value

A data frame containing the bounding coordinates of the goal circle
