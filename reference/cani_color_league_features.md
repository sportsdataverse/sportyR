# Can I Color a League Feature?

Check to see what features of a surface can be colored

## Usage

``` r
cani_color_league_features(league_code, sport_name = NULL)
```

## Arguments

- league_code:

  The case-insensitive league code to be plotted

- sport_name:

  The name of a sport to use in the event that the `league_code`
  supplied has more than one sport associated with it. Default: `NULL`

## Value

Nothing, but a message is sent to the console

## Examples

``` r
cani_color_league_features("NCAA", "basketball")
#> Here are the viable plotting features to color for NCAA basketball:
#> 
#> plot_background
#> defensive_half_court
#> offensive_half_court
#> court_apron
#> center_circle_outline
#> center_circle_fill
#> division_line
#> endline
#> sideline
#> two_point_range
#> three_point_line
#> painted_area
#> lane_boundary
#> free_throw_circle_outline
#> free_throw_circle_fill
#> free_throw_circle_dash
#> lane_space_mark
#> inbounding_line
#> substitution_line
#> baseline_lower_defensive_box
#> lane_lower_defensive_box
#> team_bench_line
#> restricted_arc
#> backboard
#> basket_ring
#> net
```
