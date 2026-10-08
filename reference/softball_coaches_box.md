# Softball Coach's Box

The coach's box. Per the NCAA softball rules, each coach's box is marked
by two lines. The first is a line drawn parallel to and a fixed distance
from the first- and third-base lines, extending from the back edge of
the base toward home plate. The second is a shorter line drawn
perpendicular to the end of the first line closest to home plate.

## Usage

``` r
softball_coaches_box(
  baseline_distance = 0,
  coaches_box_depth = 0,
  coaches_box_length = 0,
  coaches_box_width = 0,
  coaches_box_thickness = 0
)
```

## Arguments

- baseline_distance:

  The distance from the back tip of home plate to the back corner of
  either first or third base along the foul line

- coaches_box_depth:

  The distance from the foul-side edge of the foul line to the coach's
  box's line that is parallel to the foul line

- coaches_box_length:

  The length of the coach's box's line that is parallel to the foul line

- coaches_box_width:

  The length of the coach's box's line that is perpendicular to the foul
  line

- coaches_box_thickness:

  The thickness of the chalk lines that comprise the coach's box

## Value

A data frame of the coach's box's bounding coordinates

## Details

This function draws the first-base side coach's box. It should be
reflected over the `y` axis to draw the third-base side coach's box
