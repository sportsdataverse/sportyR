## Resubmission

This is a resubmission of sportyR 2.4.5. The previous upload passed the
automated pretests and was returned with:

    Found the following (possibly) invalid URLs:
      URL: https://github.com/70/sportyR/issues/71 (From: NEWS.md, 404)
      URL: https://github.com/billyfryer (From: NEWS.md, 404)

Both are fixed. The first was a spurious duplicate link in the 2.4.0 entry of
NEWS.md and has been removed. The second was a contributor's old GitHub
username and now points to their current profile. The package code is
unchanged.

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

* CRAN incoming pretests on the previous 2.4.5 upload: R Under development
  (2026-10-26 r90766 ucrt), Windows (r-devel-windows-x86_64) and Debian
  (r-devel-linux-x86_64-debian-gcc)
* local macOS (aarch64-apple-darwin20), R 4.2.2, `R CMD check --as-cran`

## R CMD check results

0 errors | 0 warnings | 1 note on both CRAN pretest flavors. The note was
the two invalid URLs above, now fixed.

The local check (R 4.2.2) gave 0 errors | 0 warnings | 4 notes, none caused by
the package:

* The URL https://www.nba.com returns 403 Forbidden to automated requests
  (it loads in a browser).
* "unable to verify current time" and "README.md or NEWS.md cannot be checked
  without pandoc" come from the local machine (no network time check, pandoc
  not installed).
* "Author field differs from that derived from Authors@R" appears because the
  tarball was built with a newer R than the one that checked it; R 4.2.2
  formats the ORCID in the author field differently.

## Reverse dependencies

`tools::package_dependencies("sportyR", reverse = TRUE, which = "all")`
against the CRAN package database (2026-10-18) returns no reverse
dependencies, so no reverse dependency checks were needed.
