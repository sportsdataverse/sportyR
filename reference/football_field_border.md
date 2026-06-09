# Football Field Border

The field border is the border line around the outer edge of the
sideline and end line. They may not be present on every field, but this
is not the same as the sideline or end line (although they may be the
same color)

## Usage

``` r
football_field_border(
  field_length = 0,
  field_width = 0,
  feature_thickness = 0,
  endzone_length = 0,
  boundary_line_thickness = 0,
  restricted_area_length = 0,
  restricted_area_width = 0,
  coaching_box_length = 0,
  coaching_box_width = 0,
  team_bench_length_field_side = 0,
  team_bench_length_back_side = 0,
  team_bench_width = 0,
  team_bench_area_border_thickness = 0,
  surrounds_team_bench_area = FALSE,
  bench_shape = ""
)
```

## Arguments

- field_length:

  The length of the field

- field_width:

  The width of the field

- feature_thickness:

  The thickness of the field border

- endzone_length:

  The length of the endzone

- boundary_line_thickness:

  The thickness of the boundary lines

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

- team_bench_area_border_thickness:

  The thickness of the border around the team bench

- surrounds_team_bench_area:

  A boolean of whether or not the field border should surround the team
  bench

- bench_shape:

  A string of the shape of the bench. Currently, this checks for
  `"rectangle"`

## Value

A data frame of the bounding box of the field border
