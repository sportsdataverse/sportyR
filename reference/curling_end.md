# Curling End

The curling sheet is the entire sheet, with the houses at either the top
or bottom ends. This draws the area of the sheet from the hog line to
the back board

## Usage

``` r
curling_end(
  sheet_length = 0,
  sheet_width = 0,
  tee_line_to_center = 0,
  hog_line_to_tee_line = 0,
  drawn_direction = ""
)
```

## Arguments

- sheet_length:

  The length of the sheet, from back board to back board

- sheet_width:

  The width of the curling sheet, from side wall to side wall

- tee_line_to_center:

  The distance from the tee line to the center of the sheet

- hog_line_to_tee_line:

  The distance from the center of the tee line to the interior edge of
  the tee line

## Value

A data frame containing the bounding box of the end of the ice sheet
