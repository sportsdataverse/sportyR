# Basketball Lower Defensive Box Mark

The lower defensive box is an imaginary box on the court extending from
the lines on the baseline to the lines inside the painted area. This box
helps determine when a block/charge call should take place, as an
offensive player is entitled to move outside of (and subsequently enter)
this box without contact

## Usage

``` r
basketball_lower_defensive_box_mark(
  drawn_direction = "",
  extension = 0,
  line_thickness = 0
)
```

## Arguments

- drawn_direction:

  A string indicating which way to draw the lower defensive box mark

- extension:

  The amount that the lower defensive box mark extends in the drawn
  direction

- line_thickness:

  The thickness of the line representing the lower defensive box

## Value

A data frame of the bounding box of a lower defensive box marking
