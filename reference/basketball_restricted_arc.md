# Basketball Restricted Arc

The arc located in the free throw lane is called the restricted arc. The
interior radius should be specified for this feature.

## Usage

``` r
basketball_restricted_arc(
  feature_radius = 0,
  line_thickness = 0,
  backboard_to_center_of_basket = 0
)
```

## Arguments

- feature_radius:

  The interior radius of the restricted arc

- line_thickness:

  The thickness of the restricted arc line

- backboard_to_center_of_basket:

  The distance from the backboard to the center of the basket

## Value

A data frame containing the bounding coordinates of the restricted arc
