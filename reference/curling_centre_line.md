# Curling Centre Line

The centre line is the line that runs the full length of the curling
sheet, or the line `x = 0` in TV view

## Usage

``` r
curling_centre_line(
  line_thickness = 0,
  tee_line_to_center = 0,
  centre_line_extension = 0
)
```

## Arguments

- line_thickness:

  The thickness of the centre line

- tee_line_to_center:

  The distance between the tee lines. (See
  [`curling_tee_line()`](https://sportyR.sportsdataverse.org/reference/curling_tee_line.md)
  for more information)

- centre_line_extension:

  The distance beyond the tee lines that the centre line extends

## Value

A data frame containing the bounding coordinates of the centre line
