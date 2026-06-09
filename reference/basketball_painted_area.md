# Basketball Painted Area

The painted area is the area contained by the free throw lane (see
[`basketball_free_throw_lane_boundary()`](https://sportyR.sportsdataverse.org/reference/basketball_free_throw_lane_boundary.md)
for more information on the free throw lane)

## Usage

``` r
basketball_painted_area(
  lane_length = 0,
  lane_width = 0,
  paint_margin = 0,
  line_thickness = 0
)
```

## Arguments

- lane_length:

  The length of the free throw lane

- lane_width:

  The width of the free throw

- paint_margin:

  The distance from the painted area of the lane to the free throw lane
  boundary lines

- line_thickness:

  The thickness of the line of the free throw lane boundary line

## Value

A data frame of the bounding coordinates of the free throw lane's
painted area

## Details

The painted area may be a different color than the rest of the two point
range area (see
[`basketball_two_point_range()`](https://sportyR.sportsdataverse.org/reference/basketball_two_point_range.md)
for more information on two-point range), but may also be the same color
