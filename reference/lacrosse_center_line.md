# Lacrosse Center Line

The center line divides the field of play into two equal halves, which
are generated via
[`lacrosse_offensive_zone()`](https://sportyR.sportsdataverse.org/reference/lacrosse_offensive_zone.md),
[`lacrosse_defensive_zone()`](https://sportyR.sportsdataverse.org/reference/lacrosse_defensive_zone.md),
and
[`lacrosse_neutral_zone()`](https://sportyR.sportsdataverse.org/reference/lacrosse_neutral_zone.md).
This line may not stretch the entire width of the field, so a parameter
is created instead

## Usage

``` r
lacrosse_center_line(center_line_width = 0, line_thickness = 0)
```

## Arguments

- center_line_width:

  The width of the center line (distance in `y`)

- line_thickness:

  The thickness of the center line

## Value

A data frame containing the bounding coordinates of the center line
