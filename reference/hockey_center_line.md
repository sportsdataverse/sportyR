# Hockey Center Line

The center line is the line that divides the ice surface in half. Its
center should lie directly in the center of the ice surface. Its line
thickness should be given by 'major_line_thickness' as this is a major
line on the ice surface

## Usage

``` r
hockey_center_line(
  feature_thickness = 0,
  rink_width = 0,
  center_faceoff_spot_gap = 0
)
```

## Arguments

- feature_thickness:

  The thickness of the center line

- rink_width:

  The width of the rink

## Value

A data frame of the bounding coordinates of the center line
