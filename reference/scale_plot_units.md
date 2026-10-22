# Scale a Plot's Units

Scale every layer's position columns (`x`, `y`, and box corners) by a
unit conversion factor. The `geom_*()` functions draw a surface in its
rule book's units (so feature anchors, translations and display ranges
are consistent) and then scale the finished surface to the requested
units

## Usage

``` r
scale_plot_units(g, unit_factor = 1)
```

## Arguments

- g:

  A `ggplot2` instance

- unit_factor:

  The factor to scale by (see
  [`unit_conversion_factor()`](https://sportyR.sportsdataverse.org/reference/unit_conversion_factor.md))

## Value

The `ggplot2` instance with its layers scaled
