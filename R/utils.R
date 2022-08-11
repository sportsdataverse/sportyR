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
