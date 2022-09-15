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

  # Valid hex must be the "#" character plus 3, 6, or 8 characters (alpha value)
  if (!(nchar(col_str) %in% c(4, 7, 9))) {
      return(FALSE)
  }

  # Valid hex must be prefixed with the "#" character
  if (substr(col_str, 1, 1) != "#") {
    return(FALSE)
  }

  # Valid hex must use digits 0-9 and a-f
  if (sum(grepl(col_str, letters[7:length(letters)])) != 0) {
    return(FALSE)
  }

  return(TRUE)
}
