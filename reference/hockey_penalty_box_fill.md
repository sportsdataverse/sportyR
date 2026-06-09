# Hockey Penalty Box (Interior)

The penalty boxes are the areas outside the confines of the rink where
players serve time for a penalty incurred. They are to be on the same
side of the ice surface and separate, as close to center ice as
possible, for each team. This will not include the off-ice officials'
box; see
[`hockey_off_ice_officials_box()`](https://sportyR.sportsdataverse.org/reference/hockey_off_ice_officials_box.md)
for more information

## Usage

``` r
hockey_penalty_box_fill(
  feature_thickness = 0,
  penalty_box_length = 0,
  penalty_box_depth = 0
)
```

## Arguments

- feature_thickness:

  The thickness of the outline of the penalty box

- penalty_box_length:

  The length of the penalty box

- penalty_box_depth:

  The depth at which the penalty box extends from the outer edge of the
  boards

## Value

A data frame containing the bounding coordinates of the penalty box's
inner filling

## Details

This will have the same thickness as the boards, but will be located
outside the ice surface
