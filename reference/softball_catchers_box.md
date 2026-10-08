# Softball Catcher's Box

The catcher's box. This is where the catcher is located on defense. Per
the NCAA softball rules, the catcher's box is a rectangle that extends 7
feet behind the rear outside corners of the batter's boxes, and is 8
feet, 5 inches wide (including the lines). Its side lines are extensions
of the outer lines of the batter's boxes

## Usage

``` r
softball_catchers_box(
  catchers_box_depth = 0,
  catchers_box_width = 0,
  batters_box_length = 0,
  batters_box_y_adj = 0,
  catchers_box_thickness = 0
)
```

## Arguments

- catchers_box_depth:

  The distance from the rear edge of the batter's boxes to the back edge
  of the catcher's box

- catchers_box_width:

  The distance between the outer edges of the catcher's box

- batters_box_length:

  The length of the batter's box (in the y direction) measured from the
  outside of the chalk lines

- batters_box_y_adj:

  The shift off of center in the y direction that the batter's box is to
  be moved to properly align

- catchers_box_thickness:

  The thickness of the chalk lines that comprise the catcher's box

## Value

A data frame containing the bounding box of the catcher's box
