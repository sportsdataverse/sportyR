# Hockey Offensive/Defenive Zone Faceoff Line

The offensive/defensive zone faceoff lines are the L-shaped lines where
players on each team line up when taking a faceoff in either the
offensive or defensive zones. There are four of these faceoff lines
around each offensive/defensive faceoff spot

## Usage

``` r
hockey_odzone_faceoff_lines(
  feature_thickness = 0,
  faceoff_line_dist_x = 0,
  faceoff_line_dist_y = 0,
  faceoff_line_length = 0,
  faceoff_line_width = 0
)
```

## Arguments

- feature_thickness:

  The thickness of the faceoff lines

- faceoff_line_dist_x:

  The distance from the center of the faceoff spot to the interior edge
  of the faceoff lines in the x direction

- faceoff_line_dist_y:

  The distance from the center of the faceoff spot to the interior edge
  of the faceoff lines in the y direction

- faceoff_line_length:

  The length of the faceoff lines from the edge nearest the goal line to
  the edge nearest the end boards

- faceoff_line_width:

  The width of the faceoff lines from the edge nearest the center of the
  spot to the edge nearest the side boards

## Value

A data frame containing the bounding coordinates of the
offensive/defensive zone faceoff lines

## Details

These lines are L-shaped, but can be thought of as two rectangles with
thickness given by 'minor_line_thickness', and are usually red in color
