# Football Team Bench Area (Outline)

The outline of the team bench area runs beyond the team bench, but is
inside of any field border that may run behind the team bench area (see
[`football_field_border()`](https://sportyR.sportsdataverse.org/reference/football_field_border.md)
for more information on this feature)

## Usage

``` r
football_team_bench_area_outline(
  restricted_area_length = 0,
  restricted_area_width = 0,
  coaching_box_length = 0,
  coaching_box_width = 0,
  team_bench_length_field_side = 0,
  team_bench_length_back_side = 0,
  team_bench_width = 0,
  feature_thickness = 0
)
```

## Arguments

- restricted_area_length:

  The length of the restricted area

- restricted_area_width:

  The width of the restricted area

- coaching_box_length:

  The length of the coaching box

- coaching_box_width:

  The width of the coaching box

- team_bench_length_field_side:

  The length of the side of the team bench closest to the field

- team_bench_length_back_side:

  The length of the side of the team bench furthest from the field

- team_bench_width:

  The width of the team bench

- feature_thickness:

  The thickness of the outline of the team bench area

## Value

A data frame containing the bounding coordinates of the team bench
area's outline
