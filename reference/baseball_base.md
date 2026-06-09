# Baseball Base

One of the bases on the diamond, or really any base on the field. These
are squares that are rotated 45 degrees

## Usage

``` r
baseball_base(
  base_side_length = 0,
  adjust_x_left = FALSE,
  adjust_x_right = FALSE
)
```

## Arguments

- base_side_length:

  The length of each side of the base

- adjust_x_left:

  Whether or not the base should be adjusted in the -x direction (e.g.
  third base)

- adjust_x_right:

  Whether or not the base should be adjusted in the +x direction (e.g.
  first base)

## Value

A data frame that comprises the boundary of the base
