# Tennis Court Apron

The court apron is referred to as the backstop and sidestop. These areas
are entirely outside of the playing court, but legal shots made here are
considered in play

## Usage

``` r
tennis_court_apron(
  court_length = 0,
  court_width = 0,
  backstop_distance = 0,
  sidestop_distance = 0
)
```

## Arguments

- court_length:

  The length of the court

- court_width:

  The width of the court (usually the doubles width of the court)

- backstop_distance:

  The distance from the back edge of the
  [`tennis_baseline()`](https://sportyR.sportsdataverse.org/reference/tennis_baseline.md)
  to the back boundary

- sidestop_distance:

  The distance from the outer edge of the
  [`tennis_sideline()`](https://sportyR.sportsdataverse.org/reference/tennis_sideline.md)
  to the side boundary

## Value

A data frame containing the bounding coordinates of the court apron
