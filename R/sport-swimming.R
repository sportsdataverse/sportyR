#' Generate a ggplot2 instance containing a swimming for a specified course
#'
#' @param leauge The league for which to draw the pool
#' @param course The course for which to draw the pool
#' @param lane_width The width of an individual lane in the same units as \code{course}
#' @param number_of_lanes An integar giving the number of lanes in the pool
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
#' geom_swimming(league = "NFHS", course = "SCY", rotate = TRUE, rotation_dir = "ccw")
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
  league = toupper(league)
  course = toupper(course)

  # Require viable course
  if(course %in% c("SCM", "LCM", "SCY") == FALSE){
    stop(paste0(course, " is not a valid course.  Courses are 'LCM', 'SCM' or 'SCY'."))
  }

  # NFHS does not use LCM
  if(all(league == "NFHS" & course == "LCM")){
    stop("Allowed courses for NFHS competition are 'SCM' and 'SCY'")
  }

  # FINA does not use SCY
  if(all(league == "FINA" & course == "SCY")){
    stop("Allowed courses for FINA competition are 'SCM' and 'LCM'")
  }

  # Require lane_width to be numeric
  lane_width = as.numeric(lane_width)
  if(is.na(lane_width)){
    stop("lane_width must be a numeric value, with the same implied units as course")
  }

  # Require lane_width to be positive
  if(lane_width <= 0){
    stop("lane_width must be a postive value, with the same implied units as course")
  }

  # NCAA requires lanes to be at least 6ft (2y) wide
  if(all(league == "NCAA" & lane_width < 2)){
    stop("NCAA requires lanes to be at least 2y (6ft) wide")
  }

  # NFHS requires lanes to be at least 7ft (2.33y) wide
  if(all(league == "NFHS" & lane_width < 2.32)){
    stop("NFHS requires lanes to be at least 2.33y (7ft) wide")
  }

  # Require number_of_lanes to be an integer
  number_of_lanes = as.integer(number_of_lanes)
  if(is.na(number_of_lanes)){
    stop("number_of_lanes must be an integer value")
  }

  # Require number_of_lanes to be positive, greater than zero
  if(number_of_lanes <= 0){
    stop("number_of_lanes must be an integer greater than zero")
  }

  # Require overflow_channels to be numeric
  overflow_channels = as.numeric(overflow_channels)
  if(is.na(overflow_channels)){
    stop("overflow_channels must be a numeric value, with the same implied units as course")
  }

  # FINA requires overflow channels to be at least 0.2m
  if(all(league == "FINA" & overflow_channels < 0.2)){
    stop("FINA requires overflow channels to be at least 0.2m")
  }

  # Call the appropriate plot-generating function
  g = switch(
    league,
    'NCAA' = geom_ncaa_swimming(course, lane_width, number_of_lanes, overflow_channels, ...),

    'NFHS' = geom_nfhs_swimming(course, lane_width, number_of_lanes, overflow_channels, ...),

    'FINA' = geom_fina_swimming(course, number_of_lanes, overflow_channels, ...),

    stop(glue::glue('{league} is not a valid league at this time.'))
  )

  # Return the ggplot2 instance
  return(g)
}

