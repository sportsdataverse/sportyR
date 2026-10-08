# Softball Pitcher's Lane

The pitcher's lane. This is the area to which the pitcher is restricted
when delivering a pitch. Per the NCAA softball rules, it is marked by
two lines, each of a fixed length, that extend from the outer edges of
the pitcher's plate toward the inside front corners of the batter's
boxes. The outside edge of each line corresponds with the outside edge
of the pitcher's plate.

## Usage

``` r
softball_pitchers_lane(
  pitchers_lane_length = 0,
  pitchers_plate_length = 0,
  pitchers_plate_front_to_home_plate = 0,
  home_plate_edge_length = 0,
  home_plate_side_to_batters_box = 0,
  batters_box_length = 0,
  batters_box_y_adj = 0,
  pitchers_lane_thickness = 0
)
```

## Arguments

- pitchers_lane_length:

  The length of each pitcher's lane line

- pitchers_plate_length:

  The length (x-direction) of the pitcher's plate

- pitchers_plate_front_to_home_plate:

  The distance from the back tip of home plate to the front edge of the
  pitcher's plate

- home_plate_edge_length:

  The length of a single edge of home plate

- home_plate_side_to_batters_box:

  The distance from the outer edge of the batter's box to the inner edge
  of home plate

- batters_box_length:

  The length of the batter's box (in the y direction) measured from the
  outside of the chalk lines

- batters_box_y_adj:

  The shift off of center in the y direction that the batter's box is to
  be moved to properly align

- pitchers_lane_thickness:

  The thickness of the chalk line that comprises the pitcher's lane

## Value

A data frame of the pitcher's lane line's bounding coordinates

## Details

This function draws the first-base side line. The line starts at the
front edge of the pitcher's plate (the feature's anchor point) and is
reflected over the `y` axis to create the third-base side line
