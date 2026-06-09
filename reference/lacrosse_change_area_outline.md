# Lacrosse Change Area (Outline)

The change area is the box-shaped area in front of the team benches
where a substitutions occur. This feature describes its outline; its
interior fill is controlled by
[`lacrosse_change_area_fill()`](https://sportyR.sportsdataverse.org/reference/lacrosse_change_area_fill.md)

## Usage

``` r
lacrosse_change_area_outline(
  change_area_length = 0,
  change_area_width = 0,
  feature_thickness = 0
)
```

## Arguments

- change_area_length:

  The length of the change area's interior

- change_area_width:

  The distance off the boards that the change area extends into the
  playing surface

- feature_thickness:

  The thickness of the outline of the box

## Value

A data frame containing the bounding coordinates of the change area's
outline
