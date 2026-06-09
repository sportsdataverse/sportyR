# Volleyball Attack Line

The attack line runs from sideline to sideline separating the court's
backcourt
([`volleyball_backcourt()`](https://sportyR.sportsdataverse.org/reference/volleyball_backcourt.md))
from the front zone
([`volleyball_front_zone()`](https://sportyR.sportsdataverse.org/reference/volleyball_front_zone.md)).
Players in the front row may attack from either side of this line, while
players in the back row must begin their attack from the backcourt side
of the line. The anchor point of this feature should be its outer edge

## Usage

``` r
volleyball_attack_line(court_width = 0, line_thickness = 0)
```

## Arguments

- court_width:

  The width of the court, measured from the exterior edges of the
  sidelines

- line_thickness:

  The thickness of the attack line

## Value

A data frame containing the bounding box of the attack line
