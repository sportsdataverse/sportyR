#' Set the default value when not provided in a list
#'
#' @param spec_val The specified value to use (assuming it's provided)
#' @param default_val The value to use as the default
#'
#' @return A value dependent on if the left-hand value is provided, or the
#'   default value specified on the right hand side
#'
#' @keywords internal
`%or%` <- function(spec_val, default_val) {
  cmp <- function(spec_val, default_val) {
      if (is.null(spec_val) ||
          is.na(spec_val) ||
          is.nan(spec_val) ||
          length(spec_val) == 0) {
      return(default_val)
    } else {
      return(spec_val)
    }
  }

  if (length(spec_val) > 1) {
    mapply(cmp, spec_val, default_val)
  } else {
    cmp(spec_val, default_val)
  }
}

#' Identify if a passed color is a hexadecimal string
#'
#' @param col_str The potentially-hexadecimal string to check
#'
#' @return A boolean indicating if the string is in fact hexadecimal
#'
#' @keywords internal
is_hex <- function(col_str = "") {
  col_str <- tolower(col_str)
  # Empty string isn't hex
  if (col_str == "") {
    return(FALSE)
  }

  # Valid hex must be prefixed with the "#" character
  if (substr(col_str, 1, 1) != "#") {
    return(FALSE)
  }

  # Valid hex must use digits 0-9 and a-f
  if (grepl(paste(letters[7:length(letters)], collapse = "|"), col_str)) {
    return(FALSE)
  }

  # Valid hex must be the "#" character plus 3, 6, or 8 characters (alpha value)
  if (!(nchar(col_str) %in% c(4, 7, 9))) {
      return(FALSE)
  }

  return(TRUE)
}

#' Load default parameters for a specified league. This should only be used when
#' debugging the package
#'
#' @param league The league to load into the global environment
#' @param display_range The display range to load into the global environment
#' @param court_updates The default \code{court_updates} to load into the global
#'   environment. This will default to an empty list
#' @param field_updates The default \code{field_updates} to load into the global
#'   environment. This will default to an empty list
#' @param pitch_updates The default \code{pitch_updates} to load into the global
#'   environment. This will default to an empty list
#' @param rink_updates The default \code{rink_updates} to load into the global
#'   environment. This will default to an empty list
#' @param sheet_updates The default \code{sheet_updates} to load into the global
#'   environment. This will default to an empty list
#' @param color_updates The default \code{color_updates} to load into the global
#'   environment. This will default to an empty list
#' @param rotation The default rotation to load into the global environment.
#'   This will default to \code{0}
#' @param x_trans The default translation in the \code{x} direction to load into
#'   the global environment. This will default to \code{0}
#' @param y_trans The default translation in the \code{y} direction to load into
#'   the global environment. This will default to \code{0}
#' @param court_units The default units for a court-like surface. The default
#'   will be \code{NULL}
#' @param field_units The default units for a field-like surface. The default
#'   will be \code{NULL}
#' @param pitch_units The default units for a pitch-like surface. The default
#'   will be \code{NULL}
#' @param rink_units The default units for a rink-like surface. The default will
#'   be \code{NULL}
#' @param sheet_units The default units for a sheet-like surface. The default
#'   will be \code{NULL}
#' @param xlims The default limits on the plot to use in the \code{x} direction.
#'   The default will be \code{NULL}
#' @param ylims The default limits on the plot to use in the \code{y} direction.
#'   The default will be \code{NULL}
#'
#' @return Nothing, but environment variables should be set
#'
#' @keywords internal
load_default_parameters <- function(league = "",
                                    display_range = "full",
                                    court_updates = list(),
                                    field_updates = list(),
                                    pitch_updates = list(),
                                    rink_updates = list(),
                                    sheet_updates = list(),
                                    color_updates = list(),
                                    rotation = 0,
                                    x_trans = 0,
                                    y_trans = 0,
                                    court_units = NULL,
                                    field_units = NULL,
                                    pitch_units = NULL,
                                    rink_units = NULL,
                                    sheet_units = NULL,
                                    xlims = NULL,
                                    ylims = NULL) {
  league <<- league
  display_range <<- display_range
  court_updates <<- court_updates
  field_updates <<- field_updates
  pitch_updates <<- pitch_updates
  rink_updates <<- rink_updates
  sheet_updates <<- sheet_updates
  color_updates <<- color_updates
  rotation <<- rotation
  x_trans <<- x_trans
  y_trans <<- y_trans
  court_units <<- court_units
  field_units <<- field_units
  pitch_units <<- pitch_units
  rink_units <<- rink_units
  sheet_units <<- sheet_units
  xlims <<- xlims
  ylims <<- ylims
}
