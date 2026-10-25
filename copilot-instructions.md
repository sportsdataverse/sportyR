# GitHub Copilot Instructions – sportyR

These instructions tell GitHub Copilot (and other AI coding assistants)
how to write code that fits this repository. The fuller guide is
[CLAUDE.md](https://sportyR.sportsdataverse.org/../CLAUDE.md);
[CONTRIBUTING.md](https://sportyR.sportsdataverse.org/../CONTRIBUTING.md)
covers the workflow.

`sportyR` draws scaled `ggplot2` representations of sports playing
surfaces (baseball, basketball, curling, football, hockey, lacrosse,
soccer, softball, tennis, volleyball) from rule-book dimensions.

## Golden rules

1.  Dimensions come from `data-raw/surface-dimensions.json` only. Never
    invent a measurement; rebuild `R/sysdata.rda` with
    `data-raw/internal-datasets.R`.
2.  Each sport is `R/geom-<sport>.R` (exported `geom_<sport>()`) plus
    `R/features-<sport>.R` (internal feature functions). Follow the
    existing sport’s shape; do not add a new abstraction.
3.  Never hand-edit `NAMESPACE`, `man/`, `README.md` or `R/sysdata.rda`.
    Edit roxygen / `README.Rmd` / the JSON and regenerate.
4.  Every test file starts with `skip_on_cran()`. CI sets
    `NOT_CRAN: true`.
5.  `sportypy` and `sdvplot-js` vendor `surface-dimensions.json`; a
    geometry change needs a re-vendor there.
6.  The CRAN maintainer submits to CRAN, not us. Never run
    `submit_cran()`.
7.  Never add AI assistants as commit co-authors.

## After changing code

- `devtools::document()`, `devtools::test()`,
  `devtools::check(args = "--as-cran")`.
- Add new exported topics to `_pkgdown.yml` `reference:`
  ([`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html)).
- Update `NEWS.md` for user-visible changes; re-render `README.Rmd` if
  edited.
- Commit with Conventional Commits (`feat(softball):`, `fix(lacrosse):`,
  …).
