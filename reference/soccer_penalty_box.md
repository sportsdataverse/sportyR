# Soccer Penalty Box

The penalty box on the pitch is the larger of the two boxes that extend
from the goal line. The penalty box is usually 16.5 meters (18 yards)
from the goal line, but may be parameterized via this function

## Usage

``` r
soccer_penalty_box(
  feature_radius = 0,
  feature_thickness = 0,
  box_length = 0,
  penalty_mark_dist = 0,
  goal_width = 0,
  goal_post_to_box_edge = 0
)
```

## Arguments

- feature_radius:

  The radius of the circle at the top of the penalty box

- feature_thickness:

  The thickness of the penalty box

- box_length:

  The length of the penalty box (from the goal line)

- penalty_mark_dist:

  The distance from the back edge of the goal line to the penalty mark

- goal_width:

  The interior width of the goal

- goal_post_to_box_edge:

  The distance from the interior of the goal post to the outer edge of
  the penalty box

## Value

A data frame containing the bounding coordinates of the penalty box

## Details

This draws a half-box, which will include the circular portion at the
top of the box. All dimensions given should be to the outside of the
features

The line thickness will be uniform for all features on the pitch
