sportyR
================

## Installation

You can install `sportyR` from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("rossdrucker/sportyR")
```

## Purpose

As the field of sports analytics evolve, there’s a growing need for
methods to both track and visualize players throughout the game. This
package aims to make this easy regardless of sport needed to be plotted.

## Usage

All functions in this library follow the same general format. They’ll be
called `geom_{sport}`, and take the following arguments:

-   `league`: the league for the sport. In all functions, this will
    ***NOT*** have a default value. Failure to supply will prompt the
    user for a league.

-   `full_surf`: a boolean indicating whether or not to plot the full
    surface. This defaults to `TRUE`

-   `rotate`: a boolean indicating whether or not to rotate the surface.
    All surfaces will be horizontal by nature. This defaults to `FALSE`

-   `rotation_dir`: the direction in which to rotate the plot (if
    `rotate == TRUE`). This defaults to `'ccw'` for counterclockwise

``` r
library(sportyR)
geom_hockey(league = 'NHL')
```

<img src="README_files/figure-gfm/standard_plot-1.png" style="display: block; margin: auto;" />
