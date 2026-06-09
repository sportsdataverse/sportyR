# Basketball Sideline

The sideline on a basketball court run the full length of the court,
typically with the team bench areas and substitution areas on their
exterior. In cases where the sideline is the court apron, the sideline
should still be generated and its color should be set equal to the court
apron's color (see
[`basketball_court_apron()`](https://sportyR.sportsdataverse.org/reference/basketball_court_apron.md)
for more information on the court apron)

## Usage

``` r
basketball_sideline(court_length = 0, line_thickness = 0)
```

## Arguments

- court_length:

  The length of the court

- line_thickness:

  The thickness of the endline and sideline

## Value

A data frame of the bounding coordinates of the sideline
