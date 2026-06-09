# Lacrosse Offensive Zone

The offensive zone is where a team tries to score a goal. It is the
TV-right area on the field

## Usage

``` r
lacrosse_offensive_zone(
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

A data frame of the bounding coordinates of the offensive zone
