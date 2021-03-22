#' This file processes the league_lookup.json and sport_lookup.json files and
#' saves them to the sysdata.rda file in the R/ subdirectory. A .rda file is
#' more efficient for R to load within the package. Please consult the R/data.R
#' file for descriptions of the datasets.
#'
#' NOTE: When adding datasets, be sure the working directory is set to sportyR/
#' and that this file is sourced on saving.
usethis::use_package('rjson')

league_lookup = rjson::fromJSON(file = "data-raw/league_lookup.json")
sport_lookup = rjson::fromJSON(file = "data-raw/sport_lookup.json")

usethis::use_data(league_lookup, sport_lookup, overwrite = TRUE, internal = TRUE)
