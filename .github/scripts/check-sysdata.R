# Fails if R/sysdata.rda is out of date with the files in data-raw/.
#
# Loads the committed R/sysdata.rda, regenerates it by sourcing
# data-raw/internal-datasets.R (which overwrites R/sysdata.rda), and compares
# the two object by object. Objects are compared rather than file bytes so
# differences in compression or serialization alone don't fail the check.
#
# Run from the package root:
#   Rscript .github/scripts/check-sysdata.R

committed <- new.env()
load("R/sysdata.rda", envir = committed)

# Sourced into its own environment because the script ends with
# rm(list = ls()), which would otherwise delete `committed`
source("data-raw/internal-datasets.R", local = new.env())

regenerated <- new.env()
load("R/sysdata.rda", envir = regenerated)

# Objects in only one of the two files, then objects whose contents differ
only_committed <- setdiff(ls(committed), ls(regenerated))
only_regenerated <- setdiff(ls(regenerated), ls(committed))
changed <- Filter(
  function(name) !identical(committed[[name]], regenerated[[name]]),
  intersect(ls(committed), ls(regenerated))
)

if (length(c(only_committed, only_regenerated, changed)) > 0) {
  message("R/sysdata.rda is out of date with data-raw/:")
  for (name in only_committed) message("  - ", name, ": no longer generated")
  for (name in only_regenerated) message("  - ", name, ": new, not committed")
  for (name in changed) message("  - ", name, ": contents differ")
  message(
    "Run source(\"data-raw/internal-datasets.R\") from the package root ",
    "and commit the updated R/sysdata.rda."
  )
  quit(status = 1)
}

message(
  "R/sysdata.rda matches data-raw/ (",
  paste(ls(committed), collapse = ", "),
  ")"
)
