# Football Apron

The field should have an apron to appropriately see all out-of-bounds
features. This is typically the same color as the field itself, but will
be created separately so as to allow for more customized plotting

## Usage

``` r
football_field_apron(
  field_length = 0,
  field_width = 0,
  endzone_length = 0,
  boundary_thickness = 0,
  field_border_thickness = 0,
  restricted_area_length = 0,
  restricted_area_width = 0,
  coaching_box_length = 0,
  coaching_box_width = 0,
  team_bench_length_field_side = 0,
  team_bench_length_back_side = 0,
  team_bench_width = 0,
  team_bench_area_border_thickness = 0,
  extra_apron_padding = 0,
  bench_shape = ""
)
```

## Arguments

- field_length:

  The length of the field

- field_width:

  The width of the field

- endzone_length:

  The length of the endzone

- boundary_thickness:

  The thickness of the field boundary

- field_border_thickness:

  The thickness of the field border

- restricted_area_length:

  The length of the restricted area

- restricted_area_width:

  The width of the restricted area

- coaching_box_length:

  The length of the coaching box

- coaching_box_width:

  The width of the coaching box

- team_bench_length_field_side:

  The length of the team bench area nearest the field

- team_bench_length_back_side:

  The length of the team bench area furthest from the field

- team_bench_width:

  The width of the team bench area

- team_bench_area_border_thickness:

  The thickness of the border around the team bench area

- extra_apron_padding:

  Any additional distance to add to the apron of the field

- bench_shape:

  A string of the shape of the bench. Currently, this checks for
  `"rectangle"`

## Value

A data frame of the bounding coordinates of the field apron
