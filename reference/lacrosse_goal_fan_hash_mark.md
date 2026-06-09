# Lacrosse Goal Fan Hash Mark

The hash marks around the goal fan (see
[`lacrosse_goal_fan()`](https://sportyR.sportsdataverse.org/reference/lacrosse_goal_fan.md))
are drawn independently from the goal fan itself. These should just be
rectangles with anchor points along the circle

## Usage

``` r
lacrosse_goal_fan_hash_mark(
  goal_fan_hash_mark_length = 0,
  line_thickness = 0,
  rotational_angle = 0
)
```

## Arguments

- goal_fan_hash_mark_length:

  The length of each hash mark along the goal fan

- line_thickness:

  The thickness of each hash mark along the goal fan

- rotational_angle:

  The angle (in degrees) that the hash mark should be rotated

## Value

A data frame containing the bounding coordinates of a hash mark along
the goal fan
