# Hockey Goaltender's Restricted Area (Trapezoid)

The goaltender's restricted area marks where a goaltender is legally
allowed to handle the puck behind the net. This is often referred to as
"the trapezoid" as it is trapezoidal in shape. Its line thickness should
be given by 'minor_line_thickness' as this is a minor line on the ice
surface

## Usage

``` r
hockey_goaltenders_restricted_area(
  rink_length = 0,
  feature_thickness = 0,
  short_base_width = 0,
  long_base_width = 0,
  x_anchor = 0
)
```

## Arguments

- rink_length:

  The length of the rink

- feature_thickness:

  The thickness of the lines used to draw the goaltender's restricted
  area

- short_base_width:

  The width of the base nearest the center line

- long_base_width:

  The width of the base nearest the boards behind the goal

- x_anchor:

  the `x` coordinate used as the anchor point of the goal line

## Value

A data frame containing the bounding coordinates of the goaltender's
restricted area

## Details

NOTE: This is not a requirement in all leagues, and may be omitted via
the "has_trapezoid" key in the `rink_params` passed to `geom_{league}`

This draws the goaltender's restricted area on the right side (in TV
view) of the ice surface. The figure is composed of lines that outline a
trapezoid in shape, and is usually red in color
