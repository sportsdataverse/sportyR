# Create Circle

Create a set of `x` and `y` coordinates that form a circle (or the arc
of a circle)

## Usage

``` r
create_circle(center = c(0, 0), npoints = 1000, r = 1, start = 0, end = 2)
```

## Arguments

- center:

  The (`x`, `y`) coordinates of the center of the circle. Default:
  `(0, 0)`

- npoints:

  The number of points with which to create the circle. This will also
  be the length of the resulting data frame. Default: 1000

- r:

  The radius of the circle IN THE UNITS OF THE PLOT. This default unit
  will be feet. Default: `1` (unit circle)

- start:

  The angle (in radians, divided by pi) at which to start drawing the
  circle, where zero runs along the +`x` axis. Default: `0`

- end:

  The angle (in radians, divided by pi) at which to stop drawing the
  circle, where zero runs along the +`x` axis. Default: `2`

## Value

A data frame containing the points needed to draw the specified circle
