#' Datasets available in this package

#' A list of lists (JSON) that associates a league code with a given sport that
#' it can create. This data is primarily useful in the caniplot_league()
#' function, and is intended to be an internal dataset.
#'
#' @docType data
#' @name league_lookup
#' @details A list of leagues with their associated sports as a list with a
#'   sublist, or a key-value(s) pairing
"league_lookup"

#' A list of lists (JSON) that associates a sport with its various league codes
#' that can be plotted from its geom_{sport}() function. This data is primarily
#' useful in the caniplot_sport() function, and is intended to be an internal
#' dataset.
#'
#' @docType data
#' @name sport_lookup
#' @details A list of sports with their associated leagues as a list with a
#'   sublist, or a key-value(s) pairing
"sport_lookup"
