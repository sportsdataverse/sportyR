#' Create a ggplot2 instance of a scale model of a basketball court
#'
#' @param league The league for which to draw the surface
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
#' @return A ggplot2 instance with a full-surface representation of a basketball
#'   court
#'
#' @export
#'
#' @examples
#' geom_basketball(league = "NBA")
#' geom_basketball(league = "NCAA", full_surf = FALSE)
#' geom_basketball(league = "FIBA", rotate = TRUE, rotation_dir = "ccw")
geom_basketball = function(league,
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
    'FIBA' = geom_fiba(full_surf, rotate, rotation_dir, ...),

    'NCAA' = geom_ncaa_bb(full_surf, rotate, rotation_dir, ...),

    'NBA' = geom_nba(full_surf, rotate, rotation_dir, ...),

    'WNBA' = geom_wnba(full_surf, rotate, rotation_dir, ...),

    stop(glue::glue('{league} is not a valid league at this time.'))
  )

  # Return the ggplot2 instance
  return(g)
}
