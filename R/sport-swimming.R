#' Generate a ggplot2 instance containing a swimming for a specified course
#'
#' @param course The course for which to draw the surface
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist)
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param ... Additional arguments to pass to the function. These should be the
#'   colors to pass to the \code{{league}_features_set_colors()} function, or
#'   units with which to draw the plot
#'
#' @return A ggplot2 instance with a full-surface representation of a swimming pool
#'
#' @export
#'
#' @examples
#' geom_swimming(course = "SCM")
#' geom_swimming(league = "LCM", rotate = TRUE, rotation_dir = "ccw")
geom_swimming = function(course,
                       lane_width = 3,
                       number_of_lanes = 8,
                       overflow_channels = 1.5,
                       rotate = FALSE,
                       rotation_dir = 'ccw',
                       ...
){
  # Force the league to be all upper case
  course = toupper(course)

  # Call the appropriate plot-generating function
  if(course %in% c("SCM", "LCM", "SCY") == FALSE){
    stop(paste0(course, " is not a valid course.  Courses are 'LCM', 'SCM' or 'SCY'."))
  }
  g = geom_swimming_course(course = course, lane_width, number_of_lanes, overflow_channels, rotate, rotation_dir)

  # Return the ggplot2 instance
  return(g)
}

