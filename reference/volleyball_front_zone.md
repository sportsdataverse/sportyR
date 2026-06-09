# Volleyball Front Zone

The front zone is the area between the attack line (see
[`volleyball_attack_line()`](https://sportyR.sportsdataverse.org/reference/volleyball_attack_line.md))
and the line running along `x = 0`. If considering the entirety of the
volleyball court as being divided into thirds, this is *half* of the
middle third of the court

## Usage

``` r
volleyball_front_zone(attack_line_edge_to_center_line = 0, court_width = 0)
```

## Arguments

- attack_line_edge_to_center_line:

  The distance from the edge furthest from the attack line to the center
  of the line running along `x = 0`

- court_width:

  The width of the court, measured from the exterior edges of the
  sidelines

## Value

A data frame containing the bounding coordinates of the center zone
