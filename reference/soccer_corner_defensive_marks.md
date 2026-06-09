# Soccer Corner Defensive Mark

The corner defensive marks on the pitch are typically located 9.15
meters (10 yards) from the corner of the pitch. Defenders should be
beyond these marks (either more towards the goal or more towards the
halfway line) during corner kicks

## Usage

``` r
soccer_corner_defensive_marks(
  feature_thickness = 0,
  is_touchline = FALSE,
  is_goal_line = FALSE,
  depth = 0,
  separation_from_line = 0
)
```

## Arguments

- feature_thickness:

  The thickness of the corner defensive marks

- is_touchline:

  A boolean indicating whether or not the corner defensive marks should
  be along the touchline

- is_goal_line:

  A boolean indicating whether or not the corner defensive marks should
  be along the goal line

- depth:

  The depth that the mark extends out of play

- separation_from_line:

  The distance from the back edge of the goal line to the interior edge
  of the corner defensive mark

## Value

A data frame containing the bounding coordinates of the corner defensive
marks

## Details

The marks should be outside the field of play

The line thickness will be uniform for all features on the pitch
