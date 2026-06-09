# Basketball Inbounding Line

The inbounding line is where the ball is inbounded on the sideline when
necessary. Lines drawn on the top of the court should be drawn in a
top-down direction, and lines on the bottom of the court should be drawn
in the bottom-up direction

## Usage

``` r
basketball_inbounding_line(
  line_thickness = 0,
  in_play_ext = 0,
  out_of_bounds_ext = 0,
  drawn_direction = ""
)
```

## Arguments

- line_thickness:

  The thickness of the inbounding line

- in_play_ext:

  The extension of the inbounding line into the court

- out_of_bounds_ext:

  The extension of the inbounding line away from the court

- drawn_direction:

  A string indicating which way, in an un-rotated plot, the line should
  be drawn when looking at the plot in TV View

## Value

A data frame containing the bounding box of the inbounding line
