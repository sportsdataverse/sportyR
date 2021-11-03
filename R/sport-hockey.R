#' Generate a ggplot2 instance containing an ice rink for a specified league
#'
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   represenation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not the final rink plot needs
#'   to be rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the final
#'   rink plot Default: \code{'ccw'}
#' @param ... Additional arguments to pass to the function. These should be the
#'   colors to pass to the \code{{league}_features_set_colors()} function,
#'   (although the colors are defined in the rule book) or units with which to
#'   draw the plot
#'
#' @return A ggplot2 instance with a full-surface representation of an ice
#'   hockey rink
#'
#' @export
#'
#' @examples
#' geom_hockey(league = "NHL")
#' geom_hockey(league = "IIHF", full_surf = FALSE)
#' geom_hockey(league = "NCAA", rotate = TRUE, rotation_dir = "ccw")
geom_hockey = function(league,
                       full_surf = TRUE,
                       rotate = FALSE,
                       rotation_dir = "ccw",
                       ...) {
  # Force the league to be all upper case
  league = toupper(league)

  # Call the appropriate plot-generating function
  g = switch(league,
    "NCAA" = geom_ncaa_hockey(full_surf, rotate, rotation_dir, ...),

    "IIHF" = geom_iihf(full_surf, rotate, rotation_dir, ...),

    "NHL" = geom_nhl(full_surf, rotate, rotation_dir, ...),

    "NWHL" = geom_nwhl(full_surf, rotate, rotation_dir, ...),

    "PHF" = geom_phf(full_surf, rotate, rotation_dir, ...),

    stop(glue::glue("{league} is not a valid league at this time."))
  )

  # Return the ggplot2 instance
  return(g)
}
