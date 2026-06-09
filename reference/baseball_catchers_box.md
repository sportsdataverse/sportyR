# Baseball Catcher's Box

The catcher's box. This is where the catcher is located on defense,
usually marked by two white lines and a back line as well. The box may
take various shapes, which are controlled by the `catchers_box_shape`
parameter

## Usage

``` r
baseball_catchers_box(
  catchers_box_depth = 0,
  catchers_box_width = 0,
  batters_box_length = 0,
  batters_box_y_adj = 0,
  catchers_box_shape = "rectangle",
  catchers_box_thickness = 0,
  home_plate_circle_radius = 0
)
```

## Arguments

- catchers_box_depth:

  The distance from the back tip of home plate to the back edge of the
  catcher's box

- catchers_box_width:

  The distance between the outer edges of the catcher's box at the
  widest point

- batters_box_length:

  The length of the batter's box (in the y direction) measured from the
  outside of the chalk lines

- batters_box_y_adj:

  The shift off of center in the y direction that the batter's box is to
  be moved to properly align

- catchers_box_shape:

  A string representing the shape of the catcher's box to draw

- catchers_box_thickness:

  The thickness of the chalk lines that comprise the catcher's box

- home_plate_circle_radius:

  The radius of the circle around home plate

## Value

A data frame containing the bounding box of the catcher's box
