# Basketball Basket Ring (Rim)

The hoop through which the ball must pass to score points for a team is
called the basket ring

## Usage

``` r
basketball_basket_ring(
  basket_ring_connector_width = 0,
  backboard_face_to_ring_cent = 0,
  basket_ring_inner_radius = 0,
  basket_ring_thickness = 0
)
```

## Arguments

- basket_ring_connector_width:

  The width of the basket ring connector

- backboard_face_to_ring_cent:

  How far off the face of the backboard the center of the basket ring's
  circle is located

- basket_ring_inner_radius:

  The inner radius of the circular part of the basket ring

- basket_ring_thickness:

  The thickness of the basket ring's circular part

## Value

A data frame of the boundary of the basket ring and connector

## Details

An explanation of the math used to generate the basket ring (and its
connecting portion that attaches the ring to the backboard) is walked
through below using NBA dimensions, but is generalized in the code

The connector has a width of 7", so 3.5" are on each side of the x axis.
The ring has a radius of 9", so the arcsine of these measurements should
give the angle at which point the ring and connector connect
