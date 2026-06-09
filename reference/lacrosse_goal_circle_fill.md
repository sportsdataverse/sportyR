# Lacrosse Goal Circle (Interior)

This feature is the area enclosed by the goal circle's outline. Please
see
[`lacrosse_goal_circle()`](https://sportyR.sportsdataverse.org/reference/lacrosse_goal_circle.md)
for more information

## Usage

``` r
lacrosse_goal_circle_fill(
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

A data frame containing the bounding coordinates of the goal circle's
enclosed area
