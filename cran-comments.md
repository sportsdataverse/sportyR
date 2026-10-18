## Release summary

sportyR 2.3.0 (update of 2.2.3, currently on CRAN). Adds NCAA softball
(`geom_softball()`, contributed by Billy Fryer, #52), fixes the
`geom_lacrosse()` example, repairs broken/redirecting URLs, and adds
`skip_on_cran()` to every test file. See NEWS.md.

## Test environments

* Local Windows 10, R 4.6.1 (2026-06-24), `devtools::check(args = "--as-cran")`

To be run by the maintainer before submission: win-builder (devel and
release), the R-hub v2 workflow, and GitHub Actions (`R-CMD-check`) on the PR.
Results will be added here.

## R CMD check results

Local Windows, R 4.6.1, `--as-cran`: 0 errors | 0 warnings | 1 note

* checking for non-standard things in the check directory ... NOTE
  Found the following files/directories: ''NULL''

## Reverse dependencies

`tools::package_dependencies("sportyR", reverse = TRUE, which = "all")`
against the CRAN package database (2026-10-18) returns no reverse
dependencies, so no reverse dependency checks were needed.
