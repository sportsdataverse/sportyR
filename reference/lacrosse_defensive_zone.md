# Lacrosse Defensive Zone

The defensive zone is the TV-left area of the playing surface. In many
cases, this will correspond to half of the field's length

## Usage

``` r
lacrosse_defensive_zone(
  field_length = 0,
  field_width = 0,
  corner_radius = 0,
  nzone_length = 0,
  field_shape = "rectangle"
)
```

## Arguments

- field_length:

  The interior length of the field

- field_width:

  The interior width of the field

- corner_radius:

  The radius of the corner (assuming `field_shape = "oval"`). Viable
  options are `"rectangle"` or `"oval"`

- nzone_length:

  The length of the neutral zone

- field_shape:

  The shape of the field, passed as a string

## Value

A data frame containing the bounding coordinates of the defensive zone
