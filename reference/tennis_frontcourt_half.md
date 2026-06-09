# Tennis Front Court

The front court is the area between the
[`tennis_net()`](https://sportyR.sportsdataverse.org/reference/tennis_net.md)
and the
[`tennis_serviceline()`](https://sportyR.sportsdataverse.org/reference/tennis_serviceline.md).
left-hand side of the court when facing the net from the nearest
baseline is the ad court, and the right-hand side is the deuce court.
This is constrained by the singles
[`tennis_sideline()`](https://sportyR.sportsdataverse.org/reference/tennis_sideline.md).

## Usage

``` r
tennis_frontcourt_half(serviceline_distance = 0, singles_width = 0)
```

## Arguments

- serviceline_distance:

  The distance from the net to the serviceline

- singles_width:

  The width of the singles court

## Value

A data frame containing the bounding coordinates of one half of the
frontcourt

## Details

This is one half of the front court (either the ad or deuce court)
