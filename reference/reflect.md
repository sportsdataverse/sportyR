# Reflect Coordinates

Perform a mathematical reflection of coordinates over a specified axis

## Usage

``` r
reflect(df, over_x = FALSE, over_y = TRUE)
```

## Arguments

- df:

  The data frame to reflect. It must have `x` and `y` columns

- over_x:

  A boolean indicating whether or not to reflect over the x axis.
  Default: FALSE

- over_y:

  A boolean indicating whether or not to reflect over the y axis.
  Default: TRUE

## Value

The reflected data frame

## Examples

``` r
reflect(data.frame(x = 1, y = 0))
#>    x y
#> 1 -1 0
```
