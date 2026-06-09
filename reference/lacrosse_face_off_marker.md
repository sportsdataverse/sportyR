# Lacrosse Faceoff Markers

The face-off markers are where face-offs occur. They may take one of two
forms: an "X" shape or a circle

## Usage

``` r
lacrosse_face_off_marker(
  shape = "O",
  feature_thickness = 0,
  side_length = 0,
  feature_radius = 0
)
```

## Arguments

- shape:

  one of the following strings (case-insensitive):

  `"X"`

  :   An "X"-like shape. This must be specified with the
      `feature_thickness` parameter to specify the width of each bar of
      the "X", and the `side_length` parameter to control the length of
      each bar

  `"O"`

  :   A circle shape. This must be specified with the `feature_radius`
      parameter to determine the size of the circle

- feature_thickness:

  The thickness of a single bar of the "X" shape

- side_length:

  The length of a single bar of the "X" shape

- feature_radius:

  The radius of a circular face-off spot

## Value

A data frame containing the bounding coordinates of a face-off spot
