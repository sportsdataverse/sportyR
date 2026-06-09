# Soccer Pitch Apron

The pitch should have an apron around it to do two things:

## Usage

``` r
soccer_pitch_apron(
  pitch_length = 0,
  pitch_width = 0,
  pitch_apron_touchline = 0,
  pitch_apron_goal_line = 0,
  goal_depth = 0
)
```

## Arguments

- pitch_length:

  The length of the pitch

- pitch_width:

  The width of the pitch

- pitch_apron_touchline:

  The distance beyond the outer edge of the touchline that the pitch's
  apron should extend

- pitch_apron_goal_line:

  The distance beyond the outer edge of the back of the goal that the
  pitch's apron should extend

- goal_depth:

  The depth to which the goal protrudes away from the back edge of the
  goal line

## Value

A data frame containing the bounding coordinates of the pitch's apron

## Details

1.  Replicate the spacing between the goal line/touchline and the
    nearest ad boards

2.  Allow the goal line and touchline to be more clearly visible

This makes practical sense, as there is grass outside of the in-play
area of the pitch
