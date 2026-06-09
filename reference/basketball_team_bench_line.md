# Basketball Team Bench Line

Players not in the game must stay within the team bench lines unless
moving to the substitution area (see
[`basketball_substitution_line()`](https://sportyR.sportsdataverse.org/reference/basketball_substitution_line.md)
class)

## Usage

``` r
basketball_team_bench_line(
  line_thickness = 0,
  extension = 0,
  drawn_direction = ""
)
```

## Arguments

- line_thickness:

  The thickness of the team bench line

- extension:

  The extension of the team bench line out of the court

- drawn_direction:

  A string indicating which way, in an un-rotated plot, the line should
  be drawn when looking at the plot in TV View

## Value

A data frame containing the bounding coordinates of the team bench line
