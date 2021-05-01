#' Generate a ggplot2 instance containing a swimming for a specified course
#'
#' @param leauge The league for which to draw the pool
#' @param course The course for which to draw the pool
#' @param lane_width The width of an individual lane in the same units as \code{course}
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
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
#' geom_swimming(league = "NHFS", course = "SCY", rotate = TRUE, rotation_dir = "ccw")
#' geom_swimming(league = "NCAA", course =  "LCM")
#' geom_swimming(league = "FINA", course =  "LCM")
geom_swimming = function(league,
                         course,
                         lane_width = 3,
                         number_of_lanes = 8,
                         overflow_channels = 1.5,
                         rotate = FALSE,
                         rotation_dir = 'ccw',
                         ...) {

  # Force the league to be all upper case
  course = toupper(course)

  # Call the appropriate plot-generating function
  if(course %in% c("SCM", "LCM", "SCY") == FALSE){
    stop(paste0(course, " is not a valid course.  Courses are 'LCM', 'SCM' or 'SCY'."))
  }

  # FINA requires overflow channels to be at least 0.2m
  if(all(league == "FINA" & overflow_channels < 0.2)){
    stop("FINA requires overflow channels to be at least 0.2m")
  }

  # Call the appropriate plot-generating function
  g = switch(
    league,
    'NCAA' = geom_NCAA_swimming(course, lane_width, number_of_lanes, overflow_channels, ...),

    'NHFS' = geom_NFHS_swimming(course, lane_width, number_of_lanes, overflow_channels, ...),

    'FINA' = geom_FINA_swimming(course, number_of_lanes, overflow_channels, ...),

    stop(glue::glue('{league} is not a valid league at this time.'))
  )

  # Return the ggplot2 instance
  return(g)
}

