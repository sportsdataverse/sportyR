# Lacrosse Goal Arc

The arc around the goal circle is a semi-circular area located around
the goal circle (see
[`lacrosse_goal_circle()`](https://sportyR.sportsdataverse.org/reference/lacrosse_goal_circle.md))
that may extend back to the end line, but also may be cut off at the
goal line extended. The extension should be controlled via the
`goal_arc_extension` parameter. Note: the hash marks are generated via
[`lacrosse_goal_fan_hash_mark()`](https://sportyR.sportsdataverse.org/reference/lacrosse_goal_fan_hash_mark.md)

## Usage

``` r
lacrosse_goal_arc(
  goal_arc_extension = 0,
  goal_arc_radius = 0,
  line_thickness = 0
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

## Value

A data frame containing the the bounding coordinates of the goal arc
