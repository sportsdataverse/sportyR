## Release summary

sportyR 2.4.5 (update of 2.2.3, currently on CRAN; 2.3.0 through 2.4.4 were
tagged on GitHub but never submitted, so their changes are included here).
Adds NCAA softball (`geom_softball()`, contributed by Billy Fryer) and
`get_surface_dimensions()`, fixes lacrosse, hockey and football geometry to
match the rule books, makes feature polygons simple closed rings, scales
feature positions and display ranges when drawing in other units, fixes the
`geom_lacrosse()` example, repairs broken/redirecting URLs, and adds
`skip_on_cran()` to every test file. See NEWS.md.

## Test environments

To be run by the maintainer on 2.4.5 before submission: local
`devtools::check(args = "--as-cran")`, win-builder (devel and release), the
R-hub v2 workflow, and GitHub Actions (`R-CMD-check`). Results will be added
here.

## R CMD check results

Pending re-run on 2.4.5. The last local check (Windows 10, R 4.6.1,
`--as-cran`, on the 2.3.0 code) gave 0 errors | 0 warnings | 1 note:

* checking for non-standard things in the check directory ... NOTE
  Found the following files/directories: ''NULL''

## Reverse dependencies

`tools::package_dependencies("sportyR", reverse = TRUE, which = "all")`
against the CRAN package database (2026-10-18) returns no reverse
dependencies, so no reverse dependency checks were needed.
