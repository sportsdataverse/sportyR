# Baseball Batter's Box

The batter's boxes on the field. This is where a batter must stand to
legally hit the ball

## Usage

``` r
baseball_batters_box(
  batters_box_length = 0,
  batters_box_width = 0,
  batters_box_y_adj = 0,
  batters_box_thickness = 0
)
```

## Arguments

- batters_box_length:

  The length of the batter's box (in the y direction) measured from the
  outside of the chalk lines

- batters_box_width:

  The width of the batter's box (in the x direction) measured from the
  outside of the chalk lines

- batters_box_y_adj:

  The shift off of center in the y direction that the batter's box is to
  be moved to properly align

- batters_box_thickness:

  The thickness of the chalk lines that comprise the batter's box

## Value

A data frame of the batter's box
