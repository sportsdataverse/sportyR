#' Generate a ggplot2 instance containing a regulation baseball field for a
#' specified league
#'
#' @param league The league for which to draw the surface
#' @param ... Additional arguments to pass to the function. These should be the
#'   colors to pass to the \code{mlb_features_set_colors()} function, or units
#'   with which to draw the plot
#'
#' @return A ggplot2 instance with a full-surface representation of a baseball
#'   field
#'
#' @export
#'
#' @examples
#' geom_baseball(league = "MLB")
geom_baseball = function(league, ...) {
  # Force the league to be all upper case
  league = toupper(league)

  # Call the appropriate plot-generating function
  g = switch(league,
    "MLB" = geom_mlb(...),

    stop(glue::glue("{league} is not a valid league at this time."))
  )

  # Return the ggplot2 instance
  return(g)
}
