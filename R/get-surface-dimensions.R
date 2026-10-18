#' Get a Surface's Dimensions
#'
#' Look up the rule-book parameters that the \code{geom_*()} functions use to
#' draw a playing surface, so other packages can read them without reaching
#' into sportyR's internal data
#'
#' @param sport The sport, e.g. \code{"lacrosse"}. Matched case-insensitively
#' @param league The league, e.g. \code{"NLL"}. Matched case-insensitively, as
#'   the \code{geom_*()} functions match their \code{league} argument. The
#'   default \code{NULL} returns every league of the sport
#'
#' @return For a league, a named list of that league's parameters (lengths are
#'   in the units given by its \code{*_units} entry). With \code{league = NULL},
#'   a named list of those lists, one per league
#'
#' @export
#'
#' @examples
#' get_surface_dimensions("lacrosse", "NLL")$field_length
#' names(get_surface_dimensions("hockey"))
get_surface_dimensions <- function(sport, league = NULL) {
  sport <- tolower(sport)
  sport_params <- surface_dimensions[[sport]]

  if (is.null(sport_params)) {
    stop(
      glue::glue(
        "{sport} is not a supported sport. Supported sports are: ",
        "{paste(names(surface_dimensions), collapse = ', ')}"
      )
    )
  }

  if (is.null(league)) {
    return(sport_params)
  }

  league <- tolower(league)
  league_params <- sport_params[[league]]

  if (is.null(league_params)) {
    stop(
      glue::glue(
        "{toupper(league)} is not a supported {sport} league. Supported ",
        "leagues are: {paste(names(sport_params), collapse = ', ')}"
      )
    )
  }

  league_params
}
