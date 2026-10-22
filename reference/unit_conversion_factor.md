# Unit Conversion Factor

The factor that converts a length in `from_unit` to `to_unit`

## Usage

``` r
unit_conversion_factor(from_unit, to_unit = NULL, surface = "field")
```

## Arguments

- from_unit:

  The units the surface's parameters are given in

- to_unit:

  The units to draw the surface in. `NULL` keeps the parameters' units

- surface:

  The surface's name in its `geom_*()` arguments (`"field"`, `"court"`,
  `"rink"`, `"pitch"` or `"sheet"`), used to name the arguments in the
  error message

## Value

A single number: 1 when no conversion is needed
