## Release summary

sportyR 2.3.0 (update of 2.2.3, currently on CRAN). Adds NCAA softball
(`geom_softball()`, contributed by Billy Fryer, #52), fixes the
`geom_lacrosse()` example, repairs broken/redirecting URLs, and adds
`skip_on_cran()` to every test file. See NEWS.md.

## Test environments

* Local Windows 10, R 4.6.1 (2026-06-24): `devtools::check(args = "--as-cran")`
* GitHub Actions (`R-CMD-check`): macOS-latest (release), windows-latest
  (release), ubuntu-latest (devel, release, oldrel-1)
* win-builder (devel and release): NOT run for this submission -- the upload
  to win-builder.r-project.org timed out from the preparation machine. The
  maintainer should submit these before sending to CRAN.
* R-hub v2: workflow `.github/workflows/rhub.yaml` is in the repository; no
  R-hub run has been recorded yet.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for non-standard things in the check directory ... NOTE
  Found the following files/directories: ''NULL''

  This note appears with `devtools::check()` on the local Windows setup and is not
  produced by the package; it is a local-environment artifact.

## Reverse dependencies

`tools::package_dependencies("sportyR", reverse = TRUE, which = "all")`
against the CRAN package database (2026-10-18) returns no reverse
dependencies (Depends, Imports, LinkingTo, Suggests, Enhances), so no
reverse dependency checks were needed.
