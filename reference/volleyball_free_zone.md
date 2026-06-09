# Volleyball Free Zone

The free zone is similar to the
[`basketball_court_apron()`](https://sportyR.sportsdataverse.org/reference/basketball_court_apron.md)
in that it is the area outside the court. It may be the same color as
the interior of the court, but isn't necessarily. Unlike
[`basketball_court_apron()`](https://sportyR.sportsdataverse.org/reference/basketball_court_apron.md)
however, the boundary line thickness doesn't matter since the lines are
considered in-play and therefore are included in the court's length and
width. This is not the same as the
[`volleyball_court_apron()`](https://sportyR.sportsdataverse.org/reference/volleyball_court_apron.md),
as this is the entire area outside of the court's lines, while the court
apron corresponds to a colored apron inside the free zone

## Usage

``` r
volleyball_free_zone(
  court_length = 0,
  court_width = 0,
  free_zone_end_line = 0,
  free_zone_sideline = 0
)
```

## Arguments

- court_length:

  The length of the court, measured from the exterior edges of the end
  lines

- court_width:

  The width of the court, measured from the exterior edges of the
  sidelines

- free_zone_end_line:

  The distance the free zone extends beyond the outer edge of the end
  line

- free_zone_sideline:

  The distance the free zone extends beyond the outer edge of the
  sideline

## Value

A data frame containing the bounding coordinates of the free zone
