# Convert Units

Convert all units, regardless of starting and ending units

## Usage

``` r
convert_units(meas, from_unit, to_unit, conversion_columns = NULL)
```

## Arguments

- meas:

  A measurement in any unit of length

- from_unit:

  A string containing the original unit of measure to be converted

- to_unit:

  A string containing the ending unit of measure

- conversion_columns:

  A vector containing the columns to convert if `meas` is of type
  `data.frame`

## Value

The measurement in converted units

## Examples

``` r
convert_units(1, "in", "cm")
#> [1] 2.54
convert_units(100, "cm", "m")
#> [1] 1
```
