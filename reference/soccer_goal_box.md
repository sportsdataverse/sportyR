# Soccer Goal Box

The goal box is the smaller of the two boxes that extend from the goal
line The goal box is usually 5.5 meters (6 yards) from the goal line,
but may be parameterized via this function

## Usage

``` r
soccer_goal_box(
  feature_thickness = 0,
  box_length = 0,
  goal_width = 0,
  goal_post_to_box_edge = 0
)
```

## Arguments

- feature_thickness:

  The thickness of the goal box

- box_length:

  The length of the goal box (from the goal line)

- goal_width:

  The interior width of the goal

- goal_post_to_box_edge:

  The distance from the interior of the goal post to the outer edge of
  the goal box

## Value

A data frame containing the bounding coordinates of the goal box

## Details

The line thickness will be uniform for all features on the pitch
