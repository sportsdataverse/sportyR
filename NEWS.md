# sportyR 

# sportyR 2.2.3

- Fixed [#38](https://github.com/sportsdataverse/sportyR/issues/38) to natively support PWHL
- Added titles to documentation
- Fixed [#43](https://github.com/sportsdataverse/sportyR/issues/43) to update ggplot2 tests

# sportyR 2.2.2

- Fixed [#32](https://github.com/sportsdataverse/sportyR/issues/32) with plot titles not displaying correctly
- Updating documentation per CRAN notes on `r-devel`

# sportyR 2.2.1

- Faceting in `{ggplot2}` now works as expected (#27, @mrcaseb)
- Fixed link in vignette to point to POSIT, not RStudio

# sportyR 2.2.0

## Patches/Bug Fixes
- Fixed [#24](https://github.com/sportsdataverse/sportyR/issues/24). Invalid league errors should now be more explicit

## Changes to Existing Sports

- MLB and MiLB bases are now 18" by default per the 2023 rules change

## New Sports and Leagues Supported

### Lacrosse
- National Lacrosse League
- NCAA Men's Lacrosse
- NCAA Women's Lacrosse
- Premier Lacrosse League
- USA Men's Lacrosse
- USA Women's Lacrosse
- World Lacrosse

## Internal Changes
- Re-aligned `switch()` statements to have condition and cases with matching indentation to improve readability

- Updated syntax in `geom_football()` to avoid tidy syntax deprecation warning

- Added `load_default_parameters()` function for faster debugging internally. This function allows all defaults of a particular `geom_{sport}()` function to be loaded and set as environment variables rather than having to set each one individually

- Updated [`data-raw/internal-datasets.R`](https://github.com/sportsdataverse/sportyR/blob/main/data-raw/internal-datasets.R) to clear environment and reload current version of package upon sourcing for debugging


# sportyR 2.1.0

## Patches/Bug Fixes
- Fixed [#14](https://github.com/sportsdataverse/sportyR/issues/14). Issue with volleyball documentation

- Fixed [#15](https://github.com/sportsdataverse/sportyR/issues/15). All outline colors are respected

- Added red zone border and border outline to football fields

- Changed default coloring of football field border outline

- Fixed issue with field border thickness when plotting behind the bench

- Fixed [#18](https://github.com/sportsdataverse/sportyR/issues/18). Can now constrain all plots to only display in-bound playing area (plus sidelines)

- Corrected football field layering to work better with hex-alpha color specifications (layering of colors previously caused issues)

## Internal Changes
- Renamed files to use `-` (hyphen) instead of `_` (underscore) across package

- New function, `is_hex()`, checks if a value is valid hexadecimal (relates to [#15](https://github.com/sportsdataverse/sportyR/issues/15))

- [Animating Tracking Data](https://sportyr.sportsdataverse.org/articles/animating-tracking-data.html) vignette now uses images hosted on GitHub to avoid the need to render at run time

## New Sports and Leagues Supported

### Volleyball
- FIVB
- NCAA
- USA Volleyball

### Curling
- Curling Canada

# sportyR 2.0.1

## Patches/Bug Fixes
- Fixed [#8](https://github.com/sportsdataverse/sportyR/issues/8). All display ranges for hockey rinks work as expected

- Fixed [#9](https://github.com/sportsdataverse/sportyR/issues/9). Yardage markers now move responsive to `x_trans` and `y_trans`

- Added `gifski` package as `Suggests`-level dependency so gifs in vignettes may render properly

## Intenal Changes
- Changed logic into how curling ends are drawn/placed on a curling sheet

- Adjusted `xlims` for `geom_hockey()` to include slightly more of the neutral zone when using `display_range %in% c("ozone", "dzone")`

- Updated licensing to GPL >= 3

- Changed website colors

# sportyR 2.0.0

## Internal Changes
- Re-defined all surfaces to be parameterized by the size of their features, rather than being defined individually in a file

- New internal data set (`data-raw/surface_dimensions.json`) to maintain all league dimensions

- Removal of `data-raw/sport_lookup.json` and `data-raw/league_lookup.json` files (replaced by `data-raw/surface_dimensions.json`)

- Updated styling conventions to match [tidyverse style guide](https://style.tidyverse.org/) with the sole exception being the use of explicit `return()` statements

- Updated file naming convention to be easier to group feature files and `geom_{sport}()` files

- Updated documentation

## Additional Capabilities
- Introduction of the `display_range` parameter to allow customized views of plots (e.g. only seeing the red zone of a football field, rather than the entire field)

- Introduction of the `{surface}_updates` parameter to allow customization of a surface starting at a base surface (e.g. `geom_hockey("NHL", surface_updates = list(rink_length = 400)))` will create a regulation NHL ice rink that is twice as long as it should be. This allows the package to better handle edge cases and support more users off the bat with less internal maintenance

- Introduction of the `color_updates` parameter to allow colors of features to change more easily and explicitly internally in the code

- Introduction of `x_trans` and `y_trans` argument for every surface to adjust positioning of origin

## New Sports and Leagues Supported

### Baseball
- MiLB
- Little League
- NCAA
- NFHS (high school)
- Pony

### Basketball
- NBA G League
- NFHS (high school)

### Curling (New Supported Sport)
- WCF (World Curling Federation)

### Hockey
- AHL
- ECHL
- OHL
- QMJHL
- USHL

### Football
- NFHS 11-player
- NFHS 9-player
- NFHS 8-player
- NFHS 6-player

### Tennis
- ATP
- ITA
- USTA
- WTA

## Breaking Changes (and justifications for breaks)
- Removed `full_surf` and `rotation_dir` arguments to allow for `display_range` and more flexible `rotation` arguments to be introduced. The improved argument structure will allow for more granular "zoom" on plots to regions of interest, as well as adjusting coordinates appropriately to match data sets

- Removed `vdiffr` tests as these were becoming unmanageable and didn't scale well to new leagues. Additionally, should a league change its dimension requirements, the case itself is no longer valid despite the league's plot _requiring_ the change. Instead, the individual plotting functions are checked as well as the classes of the resulting plots

# sportyR 1.0.4

- Removed requirement to include a plot caption color (fixes [#3](https://github.com/sportsdataverse/sportyR/issues/3))

- On September 7, 2021, the NWHL changed names to be the PHF. This name change is now reflected in the package, with NWHL still valid so as to be backwards compatible.

# sportyR 1.0.3

- Added tennis plotting capability
- Fixed issue with NCAA football plot hash marks (initially typed as 6, should have been typed as 60)
- Fixed minor bug with all soccer plots (changed `rotation_dir = TRUE` to `rotation_dir = "ccw"`)
- Fixed minor bug in NCAA hockey plots when plotting in units other than `ft` (reordered `geom_ncaa_hockey()` code to adjust units before plotting)
- Restructured code in `tests/testthat/test-plot-functions.R` to more closely resemble rest of repository structure. Plots now tested by sport rather than by plot-specific feature (e.g. units or rotation)
- Fixed minor bug with converting between millimeters and feet (originally was specified as 3048mm per foot, corrected to 304.8mm per foot)

# sportyR 1.0.2

- Added CFL plotting capability

# sportyR 1.0.1

- Introduced new testing infrastructure

# sportyR 1.0.0

Initial release! Ability to plot baseball, basketball ((W)NBA, NCAA, and FIBA), football (NFL and NCAA), hockey ((W)NHL, IIHF, NCAA), and soccer (FIFA, MLS, NCAA, NWSL, and Premier League) is included, with the capability to customize outputs in specific colors.
