#' Generate a ggplot2 instance containing a regulation tennis court for a
#' specified league
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
#' @export
#'
#' @examples
#' geom_tennis(league = "itf")
geom_tennis = function(league,
                       full_surf = TRUE,
                       rotate = FALSE,
                       rotation_dir = "ccw",
                       ...) {
  # Force the league to be all upper case
  league = toupper(league)

  # Call the appropriate plot-generating function
  g = switch(league,
    "ITF" = geom_itf(full_surf, rotate, rotation_dir, ...),

    "NCAA" = geom_ncaa_tennis(full_surf, rotate, rotation_dir, ...),

    stop(glue::glue("{league} is not a valid league at this time."))
  )

  # Return the ggplot2 instance
  return(g)
}
