# Basketball Center Circle (Interior)

The center circle is broken into two parts: the
[`basketball_center_circle_outline()`](https://sportyR.sportsdataverse.org/reference/basketball_center_circle_outline.md),
and the fill (this feature), which is the court coloring inside of the
inner edge of this circle

## Usage

``` r
basketball_center_circle_fill(center_circle_radius = 0, line_thickness = 0)
```

## Arguments

- center_circle_radius:

  The outer radius of the center circle

- line_thickness:

  The thickness of the line that comprises the center circle

## Value

A data frame of the boundary of the center circle. The interior of these
coordinates correspond to the filled section
