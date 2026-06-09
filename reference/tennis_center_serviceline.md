# Tennis Center Serviceline

The center serviceline on the court divides the service area into two
parts: the ad court (left) and the deuce court (right)

## Usage

``` r
tennis_center_serviceline(center_serviceline_length = 0, feature_thickness = 0)
```

## Arguments

- center_serviceline_length:

  The length of the center serviceline from the net to the back edge of
  the serviceline (see
  [`tennis_serviceline()`](https://sportyR.sportsdataverse.org/reference/tennis_serviceline.md)
  for more information)

- feature_thickness:

  The thickness of the center serviceline

## Value

A data frame containing the bounding coordinates of the center
serviceline

## Details

This line extends from the net to the back edge of the serviceline, and
is centered on the line `x = 0`
