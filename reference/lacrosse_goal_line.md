# Lacrosse Goal Line

The goal line is where the front edge of the goal sits. It spans the
entire interior dimension of the goal mouth. Its anchoring `x`
coordinate should be its center (e.g. half of the line's width should be
on each side of the `x` anchor)

## Usage

``` r
lacrosse_goal_line(
  goal_frame_width = 0,
  line_thickness = 0,
  goal_line_full_diameter = FALSE,
  goal_circle_radius = 0
)
```

## Arguments

- goal_frame_width:

  The interior width of the goal frame's opening

- line_thickness:

  The thickness of the goal line

- goal_line_full_diameter:

  Whether or not the goal line should extend the full diameter of the
  [`lacrosse_goal_circle()`](https://sportyR.sportsdataverse.org/reference/lacrosse_goal_circle.md)

- goal_circle_radius:

  The outer radius of the goal circle

## Value

A data frame containing the bounding coordinates of the goal line
