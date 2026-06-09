# Lacrosse Player Bench (Interior)

The player benches are the areas outside the confines of the field where
players not currently on the field are seated. They are to be on the
same side of the field surface and separate, as close to center field as
possible

## Usage

``` r
lacrosse_player_bench_area_fill(
  bench_area_outline_thickness = 0,
  bench_length = 0,
  bench_depth = 0
)
```

## Arguments

- bench_area_outline_thickness:

  The thickness of the outline of the player bench area

- bench_length:

  The length of the player bench area

- bench_depth:

  The depth of the player bench area

## Value

A data frame containing the bounding coordinates of the player bench
area's inner filling

## Details

This will have the same thickness as the boards, but will be located
outside the field surface
