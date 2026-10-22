# Get a Surface's Dimensions

Look up the rule-book parameters that the `geom_*()` functions use to
draw a playing surface, so other packages can read them without reaching
into sportyR's internal data

## Usage

``` r
get_surface_dimensions(sport, league = NULL)
```

## Arguments

- sport:

  The sport, e.g. `"lacrosse"`. Matched case-insensitively

- league:

  The league, e.g. `"NLL"`. Matched case-insensitively, as the
  `geom_*()` functions match their `league` argument. The default `NULL`
  returns every league of the sport

## Value

For a league, a named list of that league's parameters (lengths are in
the units given by its `*_units` entry). With `league = NULL`, a named
list of those lists, one per league

## Examples

``` r
get_surface_dimensions("lacrosse", "NLL")$field_length
#> [1] 200
names(get_surface_dimensions("hockey"))
#>  [1] "custom" "ahl"    "echl"   "iihf"   "phf"    "pwhl"   "ncaa"   "nhl"   
#>  [9] "ohl"    "qmjhl"  "nwhl"   "ushl"  
```
