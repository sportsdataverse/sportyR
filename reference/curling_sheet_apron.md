# Curling Apron

The apron of the sheet is what separates adjacent sheets, and in this
context provides a border around the outside of the sheet

## Usage

``` r
curling_sheet_apron(
  sheet_length = 0,
  sheet_width = 0,
  apron_behind_back = 0,
  apron_along_side = 0
)
```

## Arguments

- sheet_length:

  The length of the sheet, from back board to back board

- sheet_width:

  The width of the curling sheet, from side wall to side wall

- apron_behind_back:

  The extension of the apron beyond the back board

- apron_along_side:

  The extension of the apron running along the side walls

## Value

A data frame containing the bounding coordinates of the sheet's apron
