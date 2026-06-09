# Volleyball Court Apron

The court apron is similar to the
[`basketball_court_apron()`](https://sportyR.sportsdataverse.org/reference/basketball_court_apron.md)
in that it is the area outside the court. It may be the same color as
the interior of the court, but isn't necessarily. Unlike
[`basketball_court_apron()`](https://sportyR.sportsdataverse.org/reference/basketball_court_apron.md)
however, the boundary line thickness doesn't matter since the lines are
considered in-play and therefore are included in the court's length and
width. This is a colored area inside of the free zone (see
[`volleyball_free_zone()`](https://sportyR.sportsdataverse.org/reference/volleyball_free_zone.md)).

## Usage

``` r
volleyball_court_apron(
  court_length = 0,
  court_width = 0,
  court_apron_end_line = 0,
  court_apron_sideline = 0
)
```

## Arguments

- court_length:

  The length of the court, measured from the exterior edges of the end
  lines

- court_width:

  The width of the court, measured from the exterior edges of the
  sidelines

- court_apron_end_line:

  The distance the court apron extends beyond the outer edge of the end
  line

- court_apron_sideline:

  The distance the court apron extends beyond the outer edge of the
  sideline

## Value

A data frame containing the bounding coordinates of the court apron
