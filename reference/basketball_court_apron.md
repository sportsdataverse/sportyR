# Basketball Court Apron

The apron of the court is the colored boundary around the exterior of
some courts. If no such colored boundary exists, this should take the
same color as the court floor

## Usage

``` r
basketball_court_apron(
  court_length = 0,
  court_width = 0,
  court_apron_endline = 0,
  court_apron_sideline = 0,
  court_apron_to_boundary = 0,
  line_thickness = 0
)
```

## Arguments

- court_length:

  The length of the court

- court_width:

  The width of the court

- court_apron_endline:

  The thickness of the court's apron beyond the endline

- court_apron_sideline:

  The thickness of the court's apron beyond the sideline

- court_apron_to_boundary:

  The distance from the inner edge of the court apron to the outer edge
  of the court's boundary line (sideline and endline will be spaced the
  same)

- line_thickness:

  The thickness of the endline and sideline

## Value

A data frame of the bounding coordinates of the court apron
