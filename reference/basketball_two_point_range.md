# Basketball Two-Point Range

If a court has a three-point line (see
[`basketball_three_point_line()`](https://sportyR.sportsdataverse.org/reference/basketball_three_point_line.md)),
then any made basket (not including free throws) made from inside of the
arc are worth two points. The area inside of this arc is therefore
referred to as two point range, which this feature draws. This feature
is enclosed by the three-point line's outer edge and the baseline's
inner edge

## Usage

``` r
basketball_two_point_range(
  basket_center_to_baseline = 0,
  basket_center_to_corner_three = 0,
  line_thickness = 0,
  two_point_range_radius = 0
)
```

## Arguments

- basket_center_to_baseline:

  The distance from the center of the basket ring to the inner edge of
  the baseline

- basket_center_to_corner_three:

  The distance from the center of the basket ring to the outer edge of
  the three-point line in the corner in the court's specified units

- line_thickness:

  The thickness of the three-point line

- two_point_range_radius:

  The radius of the arc portion of the three-point line

## Value

A data frame of the bounding coordinates of two-point range

## Details

It should also be noted that as this corresponds strictly to the area
contained by the three-point line, the interior angle is what's needed.
While utilizing the corner-three distance as the outer edge should work
generally, an issue may arise if the z-order of the feature's plotting
characteristic is changed to be greater than that of the three-point
line itself. This should not happen, but the interior edge is therefore
what is used here

Start by getting the distance from the center of the basket to a corner
three-point shot. This is referred to as `start_y`

Next, get the starting angle with which to trace out the two-point
range. Taking the distance start_y to be a y coordinate, and the (outer)
radius of the arc of the three-point line to be a radius, we the sine of
the starting angle is given as
`start_y / {three_point_arc_radius - three_point_line_thickness}`

As the TV-right angle of the start of the arc is what's drawn here, the
starting and ending angles need to be adjusted relative to 1 radian (the
arc opens to the right, like a `(` character)

The starting angle is therefore given as `1 - angle`, and the ending
angle is `1 + angle`
