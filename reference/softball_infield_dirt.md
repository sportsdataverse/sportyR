# Softball Infield Dirt

The dirt that comprises the infield. Unlike a baseball field, a softball
infield is typically fully skinned (there is no infield grass). This
includes the base paths, the infield arc (the "grass line"), and the
dirt circle around home plate.

## Usage

``` r
softball_infield_dirt(
  home_plate_circle_radius = 0,
  foul_line_to_foul_grass = 0,
  pitchers_plate_distance = 0,
  infield_arc_radius = 0
)
```

## Arguments

- home_plate_circle_radius:

  The radius of the dirt circle around home plate

- foul_line_to_foul_grass:

  The distance from the outer edge of the foul line to the inner edge of
  the grass in foul territory

- pitchers_plate_distance:

  The distance from the back tip of home plate to the front edge of the
  pitcher's plate

- infield_arc_radius:

  The distance from the front edge of the pitcher's plate to the back of
  the infield dirt (the grass line)

## Value

A data frame that comprises the entirety of the infield dirt and dirt
circle around home plate

## Details

The NCAA softball rules recommend that the skinned area be determined by
measuring a 60-foot arc from the front center of the pitcher's plate
