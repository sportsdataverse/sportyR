#' This draws a football field in its standard coordinate system, with (0, 0)
#' being the bottom left corner of the left-most endzone. Each unit on the
#' coordinate system corresponds to 1 yard

#' Generate a ggplot2 instance containing a regulation football field for a
#' specified league
#'
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not to draw a full-surface
#'   representation of the playing surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param ... Additional arguments to pass to the function. These should be the
#'   colors to pass to the \code{{league}_features_set_colors()} function,
#'   although the colors are defined in the rule book
#'
#' @return A ggplot2 instance with a full-surface representation of a football
#'   field
#'
#' @export
#'
#' @examples
#' geom_football(league = "NFL")
#' geom_football(league = "NCAA", rotate = TRUE, rotation_dir = "ccw")
geom_football = function(league,
                         full_surf = TRUE,
                         rotate = FALSE,
                         rotation_dir = 'ccw',
                         ...
){
  # Force the league to be all upper case
  league = toupper(league)

  # Call the appropriate plot-generating function
  g = switch(
    league,
    'NCAA' = geom_ncaa_football(full_surf, rotate, rotation_dir, ...),

    'NFL' = geom_nfl(full_surf, rotate, rotation_dir, ...),

    stop(glue::glue('{league} is not a valid league at this time.'))
  )

  # Return the ggplot2 instance
  return(g)
}
