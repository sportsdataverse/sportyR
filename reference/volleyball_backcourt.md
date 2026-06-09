# Volleyball Backcourt

The backcourt is the area between the the attack line (see
[`volleyball_attack_line()`](https://sportyR.sportsdataverse.org/reference/volleyball_attack_line.md))
and the end line (see
[`volleyball_end_line()`](https://sportyR.sportsdataverse.org/reference/volleyball_end_line.md)).
Players playing in the back row of the rotation must take off from this
area before attacking the ball. If considering the entirety of the
volleyball court as being divided into thirds, this is *either* of the
outer thirds of the court

## Usage

``` r
volleyball_backcourt(
  attack_line_edge_to_center_line = 0,
  court_length = 0,
  court_width = 0
)
```

## Arguments

- attack_line_edge_to_center_line:

  The distance from the edge furthest from the attack line to the center
  of the line running along `x = 0`

- court_length:

  The length of the court, measured from the exterior edges of the end
  lines

- court_width:

  The width of the court, measured from the exterior edges of the
  sidelines

## Value

A data frame containing the bounding coordinates of the backcourt
