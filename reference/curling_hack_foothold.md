# Curling Hack (Foothold)

The hack exits on both sides of the curling sheet between the back board
and the back line. This is where a curler pushes off from, and it should
be centered on the centre line (see
[`curling_centre_line()`](https://sportyR.sportsdataverse.org/reference/curling_centre_line.md)).
This function draws one of the footholds of the hack

## Usage

``` r
curling_hack_foothold(foothold_depth = 0, foothold_width = 0)
```

## Arguments

- foothold_depth:

  The depth of each foothold in the hack, from the side nearest the
  house to the side nearest the back board

- foothold_width:

  The width of each foothold in the hack, from the side nearest the
  centre line to the side nearest the nearest side wall

## Value

A data frame containing the bounding box of one foothold of the hack
