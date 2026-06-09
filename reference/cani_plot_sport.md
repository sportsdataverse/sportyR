# Can I Plot Sport?

Check to see if a sport can be plotted, and alert as to which league(s)
are plottable for the sport

## Usage

``` r
cani_plot_sport(sport_code)
```

## Arguments

- sport_code:

  The case-insensitive sport name

## Value

Nothing, but a message is sent to the console

## Examples

``` r
cani_plot_sport("basketball")
#> geom_basketball() can be used to plot for the following leagues: FIBA, NBA, NBA G LEAGUE, NCAA, NFHS, WNBA
```
