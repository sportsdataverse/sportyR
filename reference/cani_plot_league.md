# Can I Plot League?

Check to see if a league can be plotted, and alert as to which
function(s) that league will work for

## Usage

``` r
cani_plot_league(league_code)
```

## Arguments

- league_code:

  The case-insensitive league code to be plotted

## Value

Nothing, but a message is sent to the console

## Examples

``` r
cani_plot_league("MLB")
#> A plot for MLB can be created via the geom_baseball() function
```
