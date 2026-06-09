# Hockey Center Faceoff Circle

The center faceoff circle is where the each period of the game begins.
It differs from the non-centered faceoff circles in that there are no
adjoining hash marks on this circle. It is also a different color than
the non-centered faceoff circles. Its line thickness should be given by
'minor_line_thickness' as this is a minor line on the ice surface

## Usage

``` r
hockey_center_faceoff_circle(feature_radius = 0, feature_thickness = 0)
```

## Arguments

- feature_radius:

  The radius of the center faceoff circle

- feature_thickness:

  The thickness of the line of the center faceoff circle

## Value

A data frame containing the bounding coordinates of the center faceoff
circle

## Details

This draws the line defining the faceoff circle at center ice. The line
is circular in shape, and usually dark blue in color
