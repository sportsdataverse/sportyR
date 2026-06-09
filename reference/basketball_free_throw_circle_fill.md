# Basketball Free Throw Circle (Interior)

The filled-in section of the free throw circle. The circle is the area
where a free throw shooter stands when attempting the free throw. The
outline of this area will be created separately via
[`basketball_free_throw_circle()`](https://sportyR.sportsdataverse.org/reference/basketball_free_throw_circle.md)

## Usage

``` r
basketball_free_throw_circle_fill(
  free_throw_circle_radius = 0,
  line_thickness = 0
)
```

## Arguments

- free_throw_circle_radius:

  The outer radius of the free throw circle, measured from the center of
  the free throw line

- line_thickness:

  The thickness of the outline of the free throw circle

## Value

A data frame containing the bounding coordinates of the free throw
circle's semi-circular filling
