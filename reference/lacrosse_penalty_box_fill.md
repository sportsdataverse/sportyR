# Lacrosse Penalty Box (Interior)

The penalty boxes are the areas outside the confines of the field where
players serve time for a penalty incurred. They are to be on the same
side of the field surface and separate, as close to center field as
possible, for each team. This will not include the off-field officials'
box; see
[`lacrosse_off_field_officials_box()`](https://sportyR.sportsdataverse.org/reference/lacrosse_off_field_officials_box.md)
for more information

## Usage

``` r
lacrosse_penalty_box_fill(
  penalty_box_outline_thickness = 0,
  penalty_box_length = 0,
  penalty_box_depth = 0
)
```

## Arguments

- penalty_box_outline_thickness:

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
outside the field surface
