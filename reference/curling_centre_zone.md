# Curling Centre Zone

The curling sheet is the entire sheet, with the houses at either the top
or bottom ends. This draws the area between the hog lines

## Usage

``` r
curling_centre_zone(
  sheet_width = 0,
  tee_line_to_center = 0,
  hog_line_to_tee_line = 0
)
```

## Arguments

- sheet_width:

  The width of the curling sheet, from side wall to side wall

- tee_line_to_center:

  The distance from the tee line to the center of the sheet

- hog_line_to_tee_line:

  The distance from the center of the tee line to the interior edge of
  the tee line

## Value

A data frame containing the bounding box of the end of the ice sheet
