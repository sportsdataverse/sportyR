# Rotate Coordinates

Perform a mathematical rotation about (0, 0) of coordinates. This
rotation is given as x' = x \\ cos(theta) - y \\ sin(theta) y' = x \\
sin(theta) + y \\ cos(theta)

## Usage

``` r
rotate_coords(df, angle = 90)
```

## Arguments

- df:

  The data frame to rotate. It must have `x` and `y` columns

- angle:

  the angle (in degrees) through which to rotate the coordinates

## Value

The rotated data frame

## Examples

``` r
rotate_coords(data.frame(x = 0, y = 1))
#>    x            y
#> 1 -1 6.123234e-17
```
