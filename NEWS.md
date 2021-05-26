## sportyR 1.0.3

- Added tennis plotting capability
- Fixed issue with NCAA football plot hash marks (initially typed as 6, should have been typed as 60)
- Fixed minor bug with all soccer plots (changed `rotation_dir = TRUE` to `rotation_dir = "ccw"`)
- Fixed minor bug in NCAA hockey plots when plotting in units other than `ft` (reordered `geom_ncaa_hockey()` code to adjust units before plotting)
- Restructured code in `tests/testthat/test-plot-functions.R` to more closely resemble rest of repository structure. Plots now tested by sport rather than by plot-specific feature (e.g. units or rotation)
- Fixed minor bug with converting between millimeters and feet (originally was specified as 3048mm per foot, corrected to 304.8mm per foot)

## sportyR 1.0.2

- Added CFL plotting capability

## sportyR 1.0.1

- Introduced new testing infrastructure

# sportyR 1.0.0

Initial release! Ability to plot baseball, basketball ((W)NBA, NCAA, and FIBA), football (NFL and NCAA), hockey ((W)NHL, IIHF, NCAA), and soccer (FIFA, MLS, NCAA, NWSL, and Premier League) is included, with the capability to customize outputs in specific colors.