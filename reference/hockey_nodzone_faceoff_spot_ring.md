# Hockey Non-Center Faceoff Spot (Ring)

The non-centered faceoff spots are located in the neutral, offensive and
defensive zones of the ice, with one on each side of the x-axis when
viewing the rink in TV view. These spots differ from the center faceoff
spot because they have a larger diameter, differ in color, and have a
colored stripe that runs through its center.

## Usage

``` r
hockey_nodzone_faceoff_spot_ring(feature_radius = 0, feature_thickness = 0)
```

## Arguments

- feature_radius:

  The outer radius of the non-centered faceoff spot ring

- feature_thickness:

  The thickness of the non-centered faceoff spot ring

## Value

A data frame containing the bounding coordinates of a non-centered
faceoff spot ring

## Details

This function is responsible for creating the outer ring, not the
colored stripe running through it. Please see
[`hockey_nodzone_faceoff_spot_stripe()`](https://sportyR.sportsdataverse.org/reference/hockey_nodzone_faceoff_spot_stripe.md)
for more information on it

The non-centered faceoff spots are where faceoffs are taken after an
icing call or to start a powerplay. They differ from the center ice
faceoff spot in size, color, and form. The thickness should be given by
'minor_line_thickness' as these are minor lines on the ice surface
