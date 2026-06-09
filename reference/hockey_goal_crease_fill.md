# Hockey Goal Crease (Interior)

The goal crease is the area where a goaltender plays their position. It
is comprised of two components: the outline of the crease (see
[`hockey_goal_crease_outline()`](https://sportyR.sportsdataverse.org/reference/hockey_goal_crease_outline.md)),
and the filling in its boundary. The goal crease may have two notches
(one on each side of the line y = 0)

## Usage

``` r
hockey_goal_crease_fill(
  feature_radius = 0,
  feature_thickness = 0,
  crease_style = "",
  crease_length = 0,
  crease_width = 0,
  notch_dist_x = 0,
  notch_width = 0
)
```

## Arguments

- feature_radius:

  The radius of the goal crease

- feature_thickness:

  The thickness of the line marking the outline of the goal crease

- crease_style:

  The style of the goal crease

- crease_length:

  The length of the goal crease

- crease_width:

  The width of the goal crease

- notch_dist_x:

  The distance from the back edge of the goal line to the further edge
  of the crease notch

- notch_width:

  The width of the notch in the goal crease

## Value

A data frame containing the bounding coordinates of the goal crease's
inner filling

## Details

The filling of the goal crease should have thickness given by
'minor_line_thickness', as this refers to the crease's outline, which is
a minor line on the ice surface. The goal crease's filling is usually
light in color
