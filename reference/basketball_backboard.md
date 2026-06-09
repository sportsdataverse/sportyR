# Basketball Backboard

The backboard is the backing onto which the basket ring (created by
[`basketball_basket_ring()`](https://sportyR.sportsdataverse.org/reference/basketball_basket_ring.md))
is affixed. This will be drawn as a rectangle on the court as the court
is drawn from an aerial view

## Usage

``` r
basketball_backboard(backboard_width = 0, backboard_thickness = 0)
```

## Arguments

- backboard_width:

  The width of the backboard when viewed from above. This is the
  x-direction dimension of the backboard when taking the point of view
  of a free throw shooter

- backboard_thickness:

  The thickness of the backboard when viewed from above

## Value

A data frame of the bounding box of the backboard
