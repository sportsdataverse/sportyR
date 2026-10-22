# Lacrosse Goal Arc

The arc around the goal circle (the PLL two-point arc) is made of two
lines parallel to the sidelines, `goal_arc_parallel_line_to_center` from
the center of the goal line, that run from the end line (see
`goal_arc_extension`) until they are `goal_arc_radius` from the center
of the goal line, joined by an arc of `goal_arc_radius` about the center
of the goal line (2024 PLL Rulebook, II. The Field, G. Two-Point Arc).
When the parallel lines are `goal_arc_radius` from the center, the arc
is a semi-circle. Note: the hash marks are generated via
[`lacrosse_goal_fan_hash_mark()`](https://sportyR.sportsdataverse.org/reference/lacrosse_goal_fan_hash_mark.md)

## Usage

``` r
lacrosse_goal_arc(
  goal_arc_extension = 0,
  goal_arc_radius = 0,
  line_thickness = 0,
  goal_arc_parallel_line_to_center = goal_arc_radius
)
```

## Arguments

- goal_arc_extension:

  The extension from the goal line towards the end line

- goal_arc_radius:

  The outer radius of the goal arc, measured from the center of the goal
  line

- line_thickness:

  The thickness of the goal arc

- goal_arc_parallel_line_to_center:

  The distance from the center of the goal line to the outer edge of
  each line parallel to the sidelines, measured along the goal line
  extended. Defaults to `goal_arc_radius`

## Value

A data frame containing the the bounding coordinates of the goal arc
