# Basketball Endline

The endline on a basketball court, also called the baseline, is located
beyond each basket. In cases where the endline is the court apron, the
endline should still be generated and its color should be set equal to
the court apron's color (see
[`basketball_court_apron()`](https://sportyR.sportsdataverse.org/reference/basketball_court_apron.md)
for more information on the court apron)

## Usage

``` r
basketball_endline(court_width = 0, line_thickness = 0)
```

## Arguments

- court_width:

  The width of the court

- line_thickness:

  The thickness of the endline and sideline

## Value

A data frame of the bounding coordinates of the endline
