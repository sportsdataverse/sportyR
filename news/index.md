# Changelog

## sportyR 2.4.2

### Bug Fixes

- **lacrosse:** end goal mouth stubs on the inner arc
  ([\#89](https://github.com/sportsdataverse/sportyR/issues/89))
  ([1b13ebd](https://github.com/sportsdataverse/sportyR/commit/1b13ebd72295b190697ca0e911d258fc4c8592b4)),
  closes [\#87](https://github.com/sportsdataverse/sportyR/issues/87)
  [\#87](https://github.com/sportsdataverse/sportyR/issues/87)

## sportyR 2.4.1

### Bug Fixes

- make feature polygons simple closed rings
  ([\#79](https://github.com/sportsdataverse/sportyR/issues/79))
  ([\#85](https://github.com/sportsdataverse/sportyR/issues/85))
  ([69e371b](https://github.com/sportsdataverse/sportyR/commit/69e371b5b278c8afa7db2ded1eeeba4083f87647))

## sportyR 2.4.0

### New Functions

- `get_surface_dimensions(sport, league = NULL)` returns the rule-book
  parameters the `geom_*()` functions draw from, for one league or every
  league of a sport, so other packages no longer need to read the
  internal `surface_dimensions`
  ([\#73](https://github.com/sportsdataverse/sportyR/issues/73))

### Patches/Bug Fixes

- [`geom_lacrosse()`](https://sportyR.sportsdataverse.org/reference/geom_lacrosse.md):
  the NLL and World Lacrosse neutral zones now span the 85 ft between
  the restraining lines; the zones read a key the data did not have, so
  the neutral zone was always 0 long
  ([\#60](https://github.com/sportsdataverse/sportyR/issues/60))
- [`geom_lacrosse()`](https://sportyR.sportsdataverse.org/reference/geom_lacrosse.md):
  the NLL and World Lacrosse penalty box fills now start at the inside
  edge of the boards; their anchor read `board_thickness` instead of
  `boards_thickness`
  ([\#61](https://github.com/sportsdataverse/sportyR/issues/61))
- [`geom_lacrosse()`](https://sportyR.sportsdataverse.org/reference/geom_lacrosse.md):
  the goal-fan hash marks (NCAA and USA Lacrosse women’s) are now placed
  at the angle they are rotated by, so each sits on the fan arc,
  perpendicular to it, at its stated distance along the arc; they were
  placed `s / goal_fan_radius` but rotated
  `s / (goal_fan_radius + goal_circle_radius)` radians from the centre
  mark ([\#62](https://github.com/sportsdataverse/sportyR/issues/62))
- [`geom_lacrosse()`](https://sportyR.sportsdataverse.org/reference/geom_lacrosse.md):
  `display_range = "full"` (and the half-field ranges) now take in the
  NLL and World Lacrosse player benches, penalty boxes and off-field
  officials’ box, whose outer 0.5 ft was cut off by a range that stopped
  at the apron
  ([\#63](https://github.com/sportsdataverse/sportyR/issues/63))
- [`geom_lacrosse()`](https://sportyR.sportsdataverse.org/reference/geom_lacrosse.md):
  `goal_arc_line_thickness` now defaults to 0 like every other feature
  argument; a goal arc added through `field_updates` without a thickness
  was drawn as a solid block
  ([\#64](https://github.com/sportsdataverse/sportyR/issues/64))
- [`geom_lacrosse()`](https://sportyR.sportsdataverse.org/reference/geom_lacrosse.md):
  a `center_face_off_marker` colour passed in `color_updates` is kept;
  the contrasting default (`#ffcb05`, NCAA men’s) is applied only when
  the user did not choose a colour and the marker would match the centre
  line. This also means that recolouring only `center_line` now stops
  the yellow contrast default from applying: the marker keeps its
  default white
  ([\#65](https://github.com/sportsdataverse/sportyR/issues/65))
- NCAA football: `restricted_area_width` is now 1.8889 yd, so the
  coaching line sits six feet outside the sideline (measured from its
  inside edge) and the limit line twelve feet out, per NCAA Football
  Rules 1-2-2, 1-2-3-a and 1-2-4-a; it was 1.3333 yd, which put the
  coaching line 4.33 ft out
  ([\#66](https://github.com/sportsdataverse/sportyR/issues/66))
- [`geom_lacrosse()`](https://sportyR.sportsdataverse.org/reference/geom_lacrosse.md):
  the NCAA and USA Lacrosse women’s 8-meter arc now follows NCAA Women’s
  Lacrosse Rule 1, Sections 6-7: an arc of 10.6 m about the centre of
  the goal line, ending on 45-degree lines from where the goal line
  extended crosses the goal circle (it was a 13.2 m arc about a point
  behind the goal line). Its hash marks sit on that circle, the 12 m
  marks past the painted arc as in the rule book’s Diagram 2, and the
  two marks on the goal line extended are 30.5 cm long (were 0.4058 m).
  Leagues without a goal fan no longer draw a hidden, zero-size fan
  polygon (the picture is unchanged), and `geom_lacrosse("custom")` now
  draws instead of failing with “‘from’ must be a finite number”
  ([\#69](https://github.com/sportsdataverse/sportyR/issues/69))
- [`geom_lacrosse()`](https://sportyR.sportsdataverse.org/reference/geom_lacrosse.md):
  the PLL two-point arc now follows the 2024 PLL Rulebook (II. The
  Field, G): its lines run parallel to the sidelines 13 yd from the
  centre of the goal line, from the end line until they are 15 yd from
  it, joined by a 15 yd arc; it was a 15 yd semicircle with its lines 15
  yd out. New `surface_dimensions` key
  `goal_arc_parallel_line_to_center` (PLL: 13 yd); without it the arc
  stays a semicircle
  ([\#70](https://github.com/sportsdataverse/sportyR/issues/70))
- NFL football: where the field border, its outline and the apron wrap
  round the trapezoid team bench, their outer corner now follows the
  bench’s slanted side (19.15 yd from centre, was 19.41 yd), as the 2026
  NFL Rulebook’s bench-area diagram draws the line behind the bench
  parallel to it. The branch for slanted benches had been applied to
  rectangular ones and vice versa; NCAA, NFHS and CFL are unchanged
  ([\#71](https://github.com/sportsdataverse/sportyR/issues/71))
- [`geom_lacrosse()`](https://sportyR.sportsdataverse.org/reference/geom_lacrosse.md):
  each NLL and World Lacrosse penalty box is now `penalty_box_length` (8
  ft) inside, its outer wall moved out by one wall thickness (0.25 ft);
  it was 7.75 ft inside. The 24 ft of NLL Rule 8.1 is read as the sum of
  the three interiors (two penalty boxes and the officials’ box), as for
  the benches
  ([\#72](https://github.com/sportsdataverse/sportyR/issues/72))
- [`geom_baseball()`](https://sportyR.sportsdataverse.org/reference/geom_baseball.md),
  [`geom_basketball()`](https://sportyR.sportsdataverse.org/reference/geom_basketball.md),
  [`geom_curling()`](https://sportyR.sportsdataverse.org/reference/geom_curling.md),
  [`geom_football()`](https://sportyR.sportsdataverse.org/reference/geom_football.md),
  [`geom_hockey()`](https://sportyR.sportsdataverse.org/reference/geom_hockey.md),
  [`geom_lacrosse()`](https://sportyR.sportsdataverse.org/reference/geom_lacrosse.md),
  [`geom_soccer()`](https://sportyR.sportsdataverse.org/reference/geom_soccer.md),
  [`geom_tennis()`](https://sportyR.sportsdataverse.org/reference/geom_tennis.md)
  and
  [`geom_volleyball()`](https://sportyR.sportsdataverse.org/reference/geom_volleyball.md):
  drawing in other units (`field_units`, `court_units`, `rink_units`,
  `pitch_units`, `sheet_units`) now scales the features’ positions
  (including the football yardage numbers) and the display range along
  with their shapes. Before, the shapes were converted but placed with
  unconverted anchors, and the plot limits stayed in the rule book’s
  units. `x_trans`, `y_trans`, `xlims` and `ylims` are in the requested
  units, as before
  ([`geom_softball()`](https://sportyR.sportsdataverse.org/reference/geom_softball.md)
  already handled this)
  ([\#74](https://github.com/sportsdataverse/sportyR/issues/74))
- [`geom_lacrosse()`](https://sportyR.sportsdataverse.org/reference/geom_lacrosse.md):
  the women’s 8-meter-arc hash marks are now centred on the middle of
  the arc’s line (`goal_fan_radius - goal_fan_line_thickness / 2`), so
  they bisect it as NCAA Women’s Lacrosse Rule 1, Section 7 requires;
  they straddled the line’s outer edge
  ([\#75](https://github.com/sportsdataverse/sportyR/issues/75))
- [`geom_football()`](https://sportyR.sportsdataverse.org/reference/geom_football.md):
  the invisible bounding box drawn with each yardage number is no longer
  translated twice by `x_trans` / `y_trans`; it moved by double the
  translation, away from its number
  ([\#76](https://github.com/sportsdataverse/sportyR/issues/76))
- Drawing a `custom` surface in other units
  (e.g. `geom_lacrosse("custom", field_units = "m")`) now explains that
  custom surfaces have no rule-book units and how to set them, instead
  of failing with “’ is not a viable unit at this time.’”
- `geom_basketball("custom")` now draws: the custom template’s empty
  JSON arrays arrive as [`list()`](https://rdrr.io/r/base/list.html),
  which `%or%` could not handle (it now falls back to the default for
  any zero-length value) and which the lane and display-range code read
  directly
  ([\#77](https://github.com/sportsdataverse/sportyR/issues/77))
- The
  [`geom_lacrosse()`](https://sportyR.sportsdataverse.org/reference/geom_lacrosse.md)
  example now uses a lacrosse league (`"NLL"`) instead of the volleyball
  league `"FIVB"`

### Changed output

- football: `field_bordered` is now read; NCAA, NFHS and NFL draw a
  white border outline by default; CFL is unchanged. `field_bordered` is
  now `true` for NCAA, NFHS (11/9/8/6) and NFL, and bordered fields
  default `field_border_outline` and `red_zone_border_outline` to
  `#ffffff` (otherwise transparent `#ffffff00`); a colour passed in
  `color_updates` always wins
  ([\#67](https://github.com/sportsdataverse/sportyR/issues/67))

### Other Changes

- Added a package citation (`citation("sportyR")`) and a softball
  example to the introductory vignette
- Fixed redirecting and broken URLs in the vignette and README
- Documentation regenerated with roxygen2 8.1.0
- Every test file now starts with `skip_on_cran()`

### Features

- lacrosse and football geometry fixes, unit scaling,
  get_surface_dimensions()
  ([\#60](https://github.com/sportsdataverse/sportyR/issues/60)-[\#77](https://github.com/sportsdataverse/sportyR/issues/77))
  ([\#68](https://github.com/sportsdataverse/sportyR/issues/68))
  ([fb7ffe5](https://github.com/sportsdataverse/sportyR/commit/fb7ffe560e697a9fe7758599502fef9433b727f4)),
  closes [\#61](https://github.com/sportsdataverse/sportyR/issues/61)
  [\#62](https://github.com/sportsdataverse/sportyR/issues/62)
  [\#63](https://github.com/sportsdataverse/sportyR/issues/63)
  [\#64](https://github.com/sportsdataverse/sportyR/issues/64)
  [\#65](https://github.com/sportsdataverse/sportyR/issues/65)
  [\#66](https://github.com/sportsdataverse/sportyR/issues/66)
  [\#67](https://github.com/sportsdataverse/sportyR/issues/67)
  [\#67](https://github.com/sportsdataverse/sportyR/issues/67)
  [\#65](https://github.com/sportsdataverse/sportyR/issues/65)
  [\#69](https://github.com/sportsdataverse/sportyR/issues/69)
  [\#70](https://github.com/sportsdataverse/sportyR/issues/70)
  [\#71](https://github.com/sportsdataverse/sportyR/issues/71)
  [\#72](https://github.com/sportsdataverse/sportyR/issues/72)
  [70/#71](https://github.com/70/sportyR/issues/71)
  [\#73](https://github.com/sportsdataverse/sportyR/issues/73)
  [\#74](https://github.com/sportsdataverse/sportyR/issues/74)
  [\#75](https://github.com/sportsdataverse/sportyR/issues/75)
  [\#74](https://github.com/sportsdataverse/sportyR/issues/74)
  [\#76](https://github.com/sportsdataverse/sportyR/issues/76)
  [\#74](https://github.com/sportsdataverse/sportyR/issues/74)
  [\#74](https://github.com/sportsdataverse/sportyR/issues/74)

## sportyR 2.3.0

### New Sports and Leagues Supported

#### Softball

- NCAA (per the NCAA Softball Rules, Rule 2: Field of Play)
- Added
  [`geom_softball()`](https://sportyR.sportsdataverse.org/reference/geom_softball.md),
  along with the internal `softball_*()` feature functions,
  [`softball_features_set_colors()`](https://sportyR.sportsdataverse.org/reference/softball_features_set_colors.md),
  and `"softball"` entries in `surface_dimensions`
- Softball support contributed by Billy Fryer
  ([@billyfryer](https://github.com/billyfryer),
  [\#52](https://github.com/sportsdataverse/sportyR/pull/52))

### Features

- **pkgdown:** add dark-mode toggle + search + a11y labels
  ([417697b](https://github.com/sportsdataverse/sportyR/commit/417697b137d4e4ae2ec953837e06e3a569c56da9))
- **themes:** 1280x640 social cards
  ([\#48](https://github.com/sportsdataverse/sportyR/issues/48))
  ([480fdfe](https://github.com/sportsdataverse/sportyR/commit/480fdfe506fd1d517e953854d2ae4f30f5fa4d69))

### Bug Fixes

- **claude:** limit attribution guard to commit and PR commands
  ([\#56](https://github.com/sportsdataverse/sportyR/issues/56))
  ([2574ee3](https://github.com/sportsdataverse/sportyR/commit/2574ee33b3e42ef1ab7564b86bad209b7178fc1e))
- **claude:** replace stacked attribution guard with the scoped one
  ([\#57](https://github.com/sportsdataverse/sportyR/issues/57))
  ([562e8c4](https://github.com/sportsdataverse/sportyR/commit/562e8c410d6f8ce255b7ead68eb6c6580889ad44))
- **pkgdown:** dark-green dark-mode navbar + legible nav text & version
  badge
  ([a421ad3](https://github.com/sportsdataverse/sportyR/commit/a421ad32a5ffb21204d382d62fe23fb201674dc4))

## sportyR 2.2.3

CRAN release: 2025-07-28

- Fixed [\#38](https://github.com/sportsdataverse/sportyR/issues/38) to
  natively support PWHL
- Added titles to documentation
- Fixed [\#43](https://github.com/sportsdataverse/sportyR/issues/43) to
  update ggplot2 tests

## sportyR 2.2.2

CRAN release: 2024-02-15

- Fixed [\#32](https://github.com/sportsdataverse/sportyR/issues/32)
  with plot titles not displaying correctly
- Updating documentation per CRAN notes on `r-devel`

## sportyR 2.2.1

CRAN release: 2023-06-15

- Faceting in [ggplot2](https://ggplot2.tidyverse.org) now works as
  expected
  ([\#27](https://github.com/sportsdataverse/sportyR/issues/27),
  [@mrcaseb](https://github.com/mrcaseb))
- Fixed link in vignette to point to POSIT, not RStudio

## sportyR 2.2.0

### Patches/Bug Fixes

- Fixed [\#24](https://github.com/sportsdataverse/sportyR/issues/24).
  Invalid league errors should now be more explicit

### Changes to Existing Sports

- MLB and MiLB bases are now 18” by default per the 2023 rules change

### New Sports and Leagues Supported

#### Lacrosse

- National Lacrosse League
- NCAA Men’s Lacrosse
- NCAA Women’s Lacrosse
- Premier Lacrosse League
- USA Men’s Lacrosse
- USA Women’s Lacrosse
- World Lacrosse

### Internal Changes

- Re-aligned [`switch()`](https://rdrr.io/r/base/switch.html) statements
  to have condition and cases with matching indentation to improve
  readability

- Updated syntax in
  [`geom_football()`](https://sportyR.sportsdataverse.org/reference/geom_football.md)
  to avoid tidy syntax deprecation warning

- Added
  [`load_default_parameters()`](https://sportyR.sportsdataverse.org/reference/load_default_parameters.md)
  function for faster debugging internally. This function allows all
  defaults of a particular `geom_{sport}()` function to be loaded and
  set as environment variables rather than having to set each one
  individually

- Updated
  [`data-raw/internal-datasets.R`](https://github.com/sportsdataverse/sportyR/blob/main/data-raw/internal-datasets.R)
  to clear environment and reload current version of package upon
  sourcing for debugging

## sportyR 2.1.0

CRAN release: 2022-10-31

### Patches/Bug Fixes

- Fixed [\#14](https://github.com/sportsdataverse/sportyR/issues/14).
  Issue with volleyball documentation

- Fixed [\#15](https://github.com/sportsdataverse/sportyR/issues/15).
  All outline colors are respected

- Added red zone border and border outline to football fields

- Changed default coloring of football field border outline

- Fixed issue with field border thickness when plotting behind the bench

- Fixed [\#18](https://github.com/sportsdataverse/sportyR/issues/18).
  Can now constrain all plots to only display in-bound playing area
  (plus sidelines)

- Corrected football field layering to work better with hex-alpha color
  specifications (layering of colors previously caused issues)

### Internal Changes

- Renamed files to use `-` (hyphen) instead of `_` (underscore) across
  package

- New function,
  [`is_hex()`](https://sportyR.sportsdataverse.org/reference/is_hex.md),
  checks if a value is valid hexadecimal (relates to
  [\#15](https://github.com/sportsdataverse/sportyR/issues/15))

- [Animating Tracking
  Data](https://sportyr.sportsdataverse.org/articles/animating-tracking-data.html)
  vignette now uses images hosted on GitHub to avoid the need to render
  at run time

### New Sports and Leagues Supported

#### Volleyball

- FIVB
- NCAA
- USA Volleyball

#### Curling

- Curling Canada

## sportyR 2.0.1

CRAN release: 2022-09-24

### Patches/Bug Fixes

- Fixed [\#8](https://github.com/sportsdataverse/sportyR/issues/8). All
  display ranges for hockey rinks work as expected

- Fixed [\#9](https://github.com/sportsdataverse/sportyR/issues/9).
  Yardage markers now move responsive to `x_trans` and `y_trans`

- Added `gifski` package as `Suggests`-level dependency so gifs in
  vignettes may render properly

### Intenal Changes

- Changed logic into how curling ends are drawn/placed on a curling
  sheet

- Adjusted `xlims` for
  [`geom_hockey()`](https://sportyR.sportsdataverse.org/reference/geom_hockey.md)
  to include slightly more of the neutral zone when using
  `display_range %in% c("ozone", "dzone")`

- Updated licensing to GPL \>= 3

- Changed website colors

## sportyR 2.0.0

CRAN release: 2022-08-15

### Internal Changes

- Re-defined all surfaces to be parameterized by the size of their
  features, rather than being defined individually in a file

- New internal data set (`data-raw/surface_dimensions.json`) to maintain
  all league dimensions

- Removal of `data-raw/sport_lookup.json` and
  `data-raw/league_lookup.json` files (replaced by
  `data-raw/surface_dimensions.json`)

- Updated styling conventions to match [tidyverse style
  guide](https://style.tidyverse.org/) with the sole exception being the
  use of explicit [`return()`](https://rdrr.io/r/base/function.html)
  statements

- Updated file naming convention to be easier to group feature files and
  `geom_{sport}()` files

- Updated documentation

### Additional Capabilities

- Introduction of the `display_range` parameter to allow customized
  views of plots (e.g. only seeing the red zone of a football field,
  rather than the entire field)

- Introduction of the `{surface}_updates` parameter to allow
  customization of a surface starting at a base surface
  (e.g. `geom_hockey("NHL", surface_updates = list(rink_length = 400)))`
  will create a regulation NHL ice rink that is twice as long as it
  should be. This allows the package to better handle edge cases and
  support more users off the bat with less internal maintenance

- Introduction of the `color_updates` parameter to allow colors of
  features to change more easily and explicitly internally in the code

- Introduction of `x_trans` and `y_trans` argument for every surface to
  adjust positioning of origin

### New Sports and Leagues Supported

#### Baseball

- MiLB
- Little League
- NCAA
- NFHS (high school)
- Pony

#### Basketball

- NBA G League
- NFHS (high school)

#### Curling (New Supported Sport)

- WCF (World Curling Federation)

#### Hockey

- AHL
- ECHL
- OHL
- QMJHL
- USHL

#### Football

- NFHS 11-player
- NFHS 9-player
- NFHS 8-player
- NFHS 6-player

#### Tennis

- ATP
- ITA
- USTA
- WTA

### Breaking Changes (and justifications for breaks)

- Removed `full_surf` and `rotation_dir` arguments to allow for
  `display_range` and more flexible `rotation` arguments to be
  introduced. The improved argument structure will allow for more
  granular “zoom” on plots to regions of interest, as well as adjusting
  coordinates appropriately to match data sets

- Removed `vdiffr` tests as these were becoming unmanageable and didn’t
  scale well to new leagues. Additionally, should a league change its
  dimension requirements, the case itself is no longer valid despite the
  league’s plot *requiring* the change. Instead, the individual plotting
  functions are checked as well as the classes of the resulting plots

## sportyR 1.0.4

- Removed requirement to include a plot caption color (fixes
  [\#3](https://github.com/sportsdataverse/sportyR/issues/3))

- On September 7, 2021, the NWHL changed names to be the PHF. This name
  change is now reflected in the package, with NWHL still valid so as to
  be backwards compatible.

## sportyR 1.0.3

- Added tennis plotting capability
- Fixed issue with NCAA football plot hash marks (initially typed as 6,
  should have been typed as 60)
- Fixed minor bug with all soccer plots (changed `rotation_dir = TRUE`
  to `rotation_dir = "ccw"`)
- Fixed minor bug in NCAA hockey plots when plotting in units other than
  `ft` (reordered `geom_ncaa_hockey()` code to adjust units before
  plotting)
- Restructured code in `tests/testthat/test-plot-functions.R` to more
  closely resemble rest of repository structure. Plots now tested by
  sport rather than by plot-specific feature (e.g. units or rotation)
- Fixed minor bug with converting between millimeters and feet
  (originally was specified as 3048mm per foot, corrected to 304.8mm per
  foot)

## sportyR 1.0.2

- Added CFL plotting capability

## sportyR 1.0.1

CRAN release: 2021-04-20

- Introduced new testing infrastructure

## sportyR 1.0.0

CRAN release: 2021-04-19

Initial release! Ability to plot baseball, basketball ((W)NBA, NCAA, and
FIBA), football (NFL and NCAA), hockey ((W)NHL, IIHF, NCAA), and soccer
(FIFA, MLS, NCAA, NWSL, and Premier League) is included, with the
capability to customize outputs in specific colors.
