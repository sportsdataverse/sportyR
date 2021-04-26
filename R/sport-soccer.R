#' Generate a ggplot2 instance containing a soccer pitch for a specified league
#'
#' @param league The league for which to draw the surface
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the pitch. Default: \code{120}
#' @param goal_line_length The length of the goal line. Default: \code{90}
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param ... Additional arguments to pass to the function. These should be the
#'   colors to pass to the \code{{league}_features_set_colors()} function, or
#'   units with which to draw the plot
#'
#' @return A ggplot2 instance with a full-surface representation of a soccer
#'   pitch
#'
#' @export
#'
#' @examples
#' geom_soccer(league = "MLS")
#' geom_soccer(league = "PREMIER", rotate = TRUE, rotation_dir = "ccw")
geom_soccer = function(league,
                       touchline_length = 120,
                       goal_line_length = 90,
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
    'FIFA' = geom_fifa(touchline_length, goal_line_length, full_surf, rotate, rotation_dir, ...),

    'MLS' = geom_mls(touchline_length, goal_line_length, full_surf, rotate, rotation_dir, ...),

    'NCAA' = geom_ncaa_soccer(touchline_length, goal_line_length, full_surf, rotate, rotation_dir, ...),

    'NWSL' = geom_nwsl(touchline_length, goal_line_length, full_surf, rotate, rotation_dir, ...),

    'PREMIER' = geom_premier_league(touchline_length, goal_line_length, full_surf, rotate, rotation_dir, ...),

    stop(glue::glue('{league} is not a valid league at this time.'))
  )

  # Return the ggplot2 instance
  return(g)
}

