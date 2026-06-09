# Basketball Free Throw Circle (Dashes)

On some courts, there are a series of dashes that comprise the bottom
half of the free throw circle (e.g. the half closer to the basket). This
function generates a single dash

## Usage

``` r
basketball_free_throw_circle_dash(
  feature_radius = 0,
  line_thickness = 0,
  start_angle = 0,
  end_angle = 0
)
```

## Arguments

- feature_radius:

  The radius of the free throw circle

- line_thickness:

  The thickness of the dash

- start_angle:

  The angle, in `radians / pi`, at which the dash should start

- end_angle:

  The angle, in `radians / pi`, at which the dash should end

## Value

A data frame containing the bounding coordinates of a dash on the free
throw circle
