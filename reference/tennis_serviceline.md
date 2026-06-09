# Tennis Serviceline

The serviceline is the line in front of which (nearest the net) a serve
must land, and be on the proper side of the court to be considered legal
and in play

## Usage

``` r
tennis_serviceline(singles_width = 0, feature_thickness = 0)
```

## Arguments

- singles_width:

  The width of the singles court

- feature_thickness:

  The thickness of the serviceline

## Value

A data frame containing the bounding coordinates of the serviceline

## Details

This line extends completely between the singles sidelines (but not
extend to the doubles sidelines)
