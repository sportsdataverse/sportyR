# Basketball Division Line (Half Court Line)

The division line divides the court into two halves, and is sometimes
referred to as the time line or half-court line. The center of this line
goes through the y axis, with half of the line lying in a team's
offensive half court and the other half in their defensive half court

## Usage

``` r
basketball_division_line(
  court_width = 0,
  line_thickness = 0,
  division_line_extension = 0
)
```

## Arguments

- court_width:

  The width of the court

- line_thickness:

  The thickness of the division line

- division_line_extension:

  The distance that the division line extends beyond the sideline. This
  may be omitted if the value is 0

## Value

A data frame of the bounding box for the division line of the court
