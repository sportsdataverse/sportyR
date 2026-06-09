# Hockey Goal Line

The goal lines are the lines over which a puck must cross (within the
goal frame) in order to be considered a goal. Its line thickness should
be given by 'minor_line_thickness' as this is a minor line on the ice
surface.

## Usage

``` r
hockey_goal_line(
  rink_length = 0,
  rink_width = 0,
  feature_radius = 0,
  feature_thickness = 0,
  x_anchor = 0
)
```

## Arguments

- rink_length:

  The length of the rink

- rink_width:

  The width of the rink

- feature_radius:

  The radius of the corner of the rink

- feature_thickness:

  The thickness of the goal line

- x_anchor:

  the `x` coordinate used as the anchor point of the goal line

## Value

A data frame containing the bounding coordinates of the goal line

## Details

This draws the right-side goal line (in TV view), starting with its left
edge. This also accounts for a perfectly rectangular goal line if a user
supplies a value that necessitates one. The line is rectangular in shape
with rounded ends, and usually red in color
