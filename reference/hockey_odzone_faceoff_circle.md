# Hockey Non-Center Faceoff Circle

The non-centered faceoff circles are located in the offensive and
defensive zones of the ice, with one on each side of the x-axis when
viewing the rink in TV view. These circles differ from the center
faceoff circle because they have hash marks that extend towards the
boards on each side of the circle

## Usage

``` r
hockey_odzone_faceoff_circle(
  feature_radius = 0,
  feature_thickness = 0,
  hashmark_width = 0,
  hashmark_ext_spacing = 0
)
```

## Arguments

- feature_radius:

  The radius of the faceoff circle

- feature_thickness:

  The thickness of the line of the non-centered faceoff circle

- hashmark_width:

  The width of the hashmarks on the exterior of the non-centered faceoff
  circle

- hashmark_ext_spacing:

  The external spacing between the hashmarks' outer edges

## Value

A data frame containing the bounding coordinates of the non-centered
faceoff circle

## Details

The non-centered faceoff circles are where faceoffs are taken after an
icing call or to start a powerplay. They differ from the center ice
faceoff circle because there are adjoining hash marks on these circles.
It is also a different color than the center ice faceoff circle, and the
spot in the center of it varies in size and form. Its line thickness
should be given by 'minor_line_thickness' as this is a minor line on the
ice surface
