# Check If Two Colors Render the Same

Compare two colors by their RGBA values rather than their spelling, so
that `"white"`, `"#FFFFFF"`, `"#ffffff"` and `"#ffffffff"` all count as
the same color

## Usage

``` r
same_color(a, b)
```

## Arguments

- a:

  The first color, in any form understood by
  [`grDevices::col2rgb()`](https://rdrr.io/r/grDevices/col2rgb.html)

- b:

  The second color, in any form understood by
  [`grDevices::col2rgb()`](https://rdrr.io/r/grDevices/col2rgb.html)

## Value

A boolean indicating if the two colors have identical RGBA values
