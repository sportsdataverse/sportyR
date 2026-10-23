# CLAUDE.md -- sportyR

Repo-specific rules. The shared commit/PR/safety rules live in `.claude/CLAUDE.md`
(synced across repos -- edit the template in ClaudeCowork, not that file).

`sportyR` draws scaled `ggplot2` playing surfaces per rule-book dimensions. CRAN
maintainer (`cre`) is Ross Drucker: we prepare releases, he runs
`devtools::submit_cran()` and answers CRAN's confirmation email. Never submit.

## Sports (`geom_*()`)

baseball, basketball, curling, football, hockey, lacrosse, soccer, softball
(2.3.0, NCAA), tennis, volleyball. Each has `R/geom-<sport>.R` (exported
`geom_<sport>(league, display_range, field_updates, color_updates, rotation,
x_trans, y_trans, <surface>_units, xlims, ylims)`) and `R/features-<sport>.R`
(internal feature functions). `cani_plot_sport()`, `cani_plot_league()` and
`cani_color_league_features()` read the same data.

## Data pipeline (data-raw -> sysdata)

- Source of truth: `data-raw/surface-dimensions.json` (all dimensions, league
  codes, default colors).
- `data-raw/internal-datasets.R` (working dir = repo root) rebuilds
  `R/sysdata.rda` (`surface_dimensions`, internal). Never hand-edit the `.rda`.
- New league/sport: edit the JSON, re-run the script, add feature functions,
  `geom_*()`, tests, `_pkgdown.yml` reference entry, NEWS, README table.

## Downstream ports (vendor the JSON)

- `sportypy` (Python) and `sdvplot-js` (`packages/sporty`, fixtures under
  `fixtures/sporty/`) vendor `surface-dimensions.json`. A geometry change here
  means re-vendoring there (sdvplot-js: `pnpm vendor`, `pnpm oracle:sporty`).
  `sdvplot` / `sdvplotR` consume surfaces via those ports.

## Conventions

- roxygen2 8.1.0 (`devtools::document()`); never hand-edit `man/` or `NAMESPACE`.
  Every exported fn needs `@return` and `@examples`; existing examples sit in
  `\dontrun{}`.
- `README.Rmd` is the source; `devtools::build_readme()` renders `README.md`
  (revert re-rendered `man/figures/*.png` unless figures changed).
- Tests: `tests/testthat/test-*.R`, **`skip_on_cran()` is the first line of
  every file**; CI sets `NOT_CRAN: true` so they still run (R-hub and covr too).
- Windows R: use the newest install (`C:/Program Files/R/R-4.6.x/bin/Rscript.exe`).
- LF line endings.

## CRAN prep checklist

`devtools::check(args = "--as-cran")` -> 0/0/0; `urlchecker::url_check()`;
`pkgdown::check_pkgdown()`; win-builder (`check_win_devel()` / `check_win_release()`
email the maintainer) and R-hub (`.github/workflows/rhub.yaml`, manual dispatch);
update `NEWS.md` and `cran-comments.md`. `CRAN-SUBMISSION` is written by
`submit_cran()` and stays buildignored -- do not edit it.
