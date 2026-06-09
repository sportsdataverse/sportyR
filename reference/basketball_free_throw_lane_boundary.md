# Basketball Free Throw Lane (Boundary)

The lines providing the boundary to the free throw lane. When a player
is shooting a free throw, all non-shooting players must be outside of
this boundary

## Usage

``` r
basketball_free_throw_lane_boundary(
  lane_length = 0,
  lane_width = 0,
  line_thickness = 0
)
```

## Arguments

- lane_length:

  The length of the free throw lane

- lane_width:

  The width of the free throw

- line_thickness:

  The thickness of the free throw lane boundary

## Value

A data frame of the bounding coordinates of the free throw lane boundary

## Details

NOTE: This does not include lane space markings (blocks), which will be
created via
[`basketball_lane_space_mark()`](https://sportyR.sportsdataverse.org/reference/basketball_lane_space_mark.md).
