# Basketball Substitution Line

The substitution line is where players checking into the game wait for a
stoppage. Lines drawn on the top of the court should be drawn in a
top-down direction, and lines on the bottom of the court should be drawn
in the bottom-up direction

## Usage

``` r
basketball_substitution_line(
  line_thickness = 0,
  substitution_line_width = 0,
  drawn_direction = ""
)
```

## Arguments

- line_thickness:

  The thickness of the substitution line

- substitution_line_width:

  The width of the substitution line, from top to bottom when viewing
  the plot in TV view

- drawn_direction:

  A string indicating which way, in an un-rotated plot, the line should
  be drawn when looking at the plot in TV View

## Value

A data frame containing the bounding coordinates of the substitution
line
