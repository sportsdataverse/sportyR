# Surface Base Features --------------------------------------------------------

#' The free zone is similar to the [basketball_court_apron()] in that it is the
#' area outside the court. It may be the same color as the interior of the
#' court, but isn't necessarily. Unlike [basketball_court_apron()] however, the
#' boundary line thickness doesn't matter since the lines are considered in-play
#' and therefore are included in the court's length and width. This is not the
#' same as the [volleyball_court_apron()], as this is the entire area outside of
#' the court's lines, while the court apron corresponds to a colored apron
#' inside the free zone
#'
#' @param court_length The length of the court, measured from the exterior edges
#'   of the end lines
#' @param court_width The width of the court, measured from the exterior edges
#'   of the sidelines
#' @param free_zone_end_line The distance the free zone extends beyond
#'   the outer edge of the end line
#' @param free_zone_sideline The distance the free zone extends beyond
#'   the outer edge of the sideline
#'
#' @return A data frame containing the bounding coordinates of the free zone
#'
#' @keywords internal
volleyball_free_zone <- function(court_length = 0,
                                 court_width = 0,
                                 free_zone_end_line = 0,
                                 free_zone_sideline = 0) {
  free_zone_df <- data.frame(
    x = c(
      0,
      court_length / 2,
      court_length / 2,
      0,
      0,
      (court_length / 2) + free_zone_end_line,
      (court_length / 2) + free_zone_end_line,
      0,
      0
    ),

    y = c(
      court_width / 2,
      court_width / 2,
      -court_width / 2,
      -court_width / 2,
      (-court_width / 2) - free_zone_sideline,
      (-court_width / 2) - free_zone_sideline,
      (court_width / 2) + free_zone_sideline,
      (court_width / 2) + free_zone_sideline,
      court_width / 2
    )
  )

  return(free_zone_df)
}

#' The front zone is the area between the attack line (see
#' [volleyball_attack_line()]) and the line running along \code{x = 0}. If
#' considering the entirety of the volleyball court as being divided into
#' thirds, this is _half_ of the middle third of the court
#'
#' @param attack_line_edge_to_center_line The distance from the edge furthest
#'   from the attack line to the center of the line running along \code{x = 0}
#' @param court_width The width of the court, measured from the exterior edges
#'   of the sidelines
#'
#' @return A data frame containing the bounding coordinates of the center zone
#'
#' @keywords internal
volleyball_front_zone <- function(attack_line_edge_to_center_line = 0,
                                  court_width = 0) {
  front_zone <- create_rectangle(
    x_min = 0,
    x_max = attack_line_edge_to_center_line,
    y_min = -court_width / 2,
    y_max = court_width / 2
  )

  return(front_zone)
}

#' The backcourt is the area between the the attack line (see
#' [volleyball_attack_line()]) and the end line (see [volleyball_end_line()]).
#' Players playing in the back row of the rotation must take off from this area
#' before attacking the ball. If considering the entirety of the volleyball
#' court as being divided into thirds, this is _either_ of the outer thirds of
#' the court
#'
#' @param attack_line_edge_to_center_line The distance from the edge furthest
#'   from the attack line to the center of the line running along \code{x = 0}
#' @param court_length The length of the court, measured from the exterior edges
#'   of the end lines
#' @param court_width The width of the court, measured from the exterior edges
#'   of the sidelines
#'
#' @return A data frame containing the bounding coordinates of the backcourt
#'
#' @keywords internal
volleyball_backcourt <- function(attack_line_edge_to_center_line = 0,
                                 court_length = 0,
                                 court_width = 0) {
  # Compute the length of the backcourt as the area remaining beyond the attack
  # line
  backcourt_length <- (court_length / 2) - attack_line_edge_to_center_line

  backcourt_df <- create_rectangle(
    x_min = -backcourt_length / 2,
    x_max = backcourt_length / 2,
    y_min = -court_width / 2,
    y_max = court_width / 2
  )

  return(backcourt_df)
}

# Surface Boundaries -----------------------------------------------------------

#' The free zone is similar to the [basketball_court_apron()] in that it is the
#' area outside the court. It may be the same color as the interior of the
#' court, but isn't necessarily. Unlike [basketball_court_apron()] however, the
#' boundary line thickness doesn't matter since the lines are considered in-play
#' and therefore are included in the court's length and width. This is not the
#' same as the [volleyball_court_apron()], as this is the entire area outside of
#' the court's lines, while the court apron corresponds to a colored apron
#' inside the free zone
#'
#' @param court_length The length of the court, measured from the exterior edges
#'   of the end lines
#' @param court_width The width of the court, measured from the exterior edges
#'   of the sidelines
#' @param court_apron_end_line The distance the court apron extends beyond the
#'   outer edge of the end line
#' @param court_apron_sideline The distance the court apron extends beyond the
#'   outer edge of the sideline
#'
#' @return A data frame containing the bounding coordinates of the court apron
#'
#' @keywords internal
volleyball_court_apron <- function(court_length = 0,
                                   court_width = 0,
                                   court_apron_end_line = 0,
                                   court_apron_sideline = 0) {
  court_apron_df <- data.frame(
    x = c(
      0,
      court_length / 2,
      court_length / 2,
      0,
      0,
      (court_length / 2) + court_apron_end_line,
      (court_length / 2) + court_apron_end_line,
      0,
      0
    ),

    y = c(
      court_width / 2,
      court_width / 2,
      -court_width / 2,
      -court_width / 2,
      (-court_width / 2) - court_apron_sideline,
      (-court_width / 2) - court_apron_sideline,
      (court_width / 2) + court_apron_sideline,
      (court_width / 2) + court_apron_sideline,
      court_width / 2
    )
  )

  return(court_apron_df)
}

#' The lines on the court that run the full width of the court are referred to
#' as the end lines, with the full width of the line being considered in bounds
#'
#' @param court_width The width of the court, measured from the exterior edges
#'   of the sidelines
#' @param line_thickness The thickness of the end line
#'
#' @return A data frame containing the bounding box of the end line
#'
#' @keywords internal
volleyball_end_line <- function(court_width = 0, line_thickness = 0) {
  end_line_df <- create_rectangle(
    x_min = -line_thickness,
    x_max = 0,
    y_min = -court_width / 2,
    y_max = court_width / 2
  )

  return(end_line_df)
}

#' The lines on the court that run the full length of the court are referred to
#' as the sidelines, with the full width of the line being considered in bounds
#'
#' @param court_length The length of the court, measured from the exterior edges
#'   of the end lines
#' @param line_thickness The thickness of the sideline
#'
#' @return A data frame containing the bounding box of the sideline
#'
#' @keywords internal
volleyball_sideline <- function(court_length = 0, line_thickness = 0) {
  sideline_df <- create_rectangle(
    x_min = -court_length / 2,
    x_max = court_length / 2,
    y_min = -line_thickness,
    y_max = 0
  )

  return(sideline_df)
}


# Surface Lines ----------------------------------------------------------------

#' The attack line runs from sideline to sideline separating the court's
#' backcourt ([volleyball_backcourt()]) from the front zone
#' ([volleyball_front_zone()]). Players in the front row may attack from either
#' side of this line, while players in the back row must begin their attack from
#' the backcourt side of the line. The anchor point of this feature should be
#' its outer edge
#'
#' @param court_width The width of the court, measured from the exterior edges
#'   of the sidelines
#' @param line_thickness The thickness of the attack line
#'
#' @return A data frame containing the bounding box of the attack line
#'
#' @keywords internal
volleyball_attack_line <- function(court_width = 0, line_thickness = 0) {
  attack_line_df <- create_rectangle(
    x_min = -line_thickness,
    x_max = 0,
    y_min = -court_width / 2,
    y_max = court_width / 2
  )

  return(attack_line_df)
}

#' The center line's axis runs along \code{x = 0} when viewing the court in TV
#' view, dividing the court into two equal halves
#'
#' @param court_width The width of the court, measured from the exterior edges
#'   of the sidelines
#' @param line_thickness The thickness of the center line
#'
#' @return A data frame containing the bounding box of the center line
#'
#' @keywords internal
volleyball_center_line <- function(court_width = 0, line_thickness = 0) {
  center_line_df <- create_rectangle(
    x_min = -line_thickness / 2,
    x_max = line_thickness / 2,
    y_min = -court_width / 2,
    y_max = court_width / 2
  )

  return(center_line_df)
}

# Surface Features -------------------------------------------------------------

#' The service zone marks are the lines beyond the end lines that denote where a
#' legal serve must take place. These appear as four hash marks that are out of
#' bounds of the court, but contained within the free zone (see
#' [volleyball_free_zone()] for reference)
#'
#' @param service_zone_mark_length The distance the service zone mark extends
#'   away from the outer edge of the end line
#' @param line_thickness The thickness of the service zone marks
#'
#' @return A data frame containing the bounding box of the service zone mark
#'
#' @keywords internal
volleyball_service_zone_mark <- function(service_zone_mark_length = 0,
                                         line_thickness = 0) {
  service_zone_mark_df <- create_rectangle(
    x_min = 0,
    x_max = service_zone_mark_length,
    y_min = -line_thickness,
    y_max = 0
  )

  return(service_zone_mark_df)
}

#' The substitution zone is typically marked by a dashed line extending from the
#' attack lines (see [volleyball_attack_line()] for more). This creates a single
#' dash, and the dashes should be added to the plot accordingly
#'
#' @param dash_length The length of the dash to draw
#' @param line_thickness The thickness of the substitution zone dashes
#'
#' @return A data frame containing the bounding box of a single dash of the
#'   substitution zone
#'
#' @keywords internal
volleyball_substitution_zone_dash <- function(dash_length = 0,
                                              line_thickness = 0) {
  substitution_zone_dash_df <- create_rectangle(
    x_min = -line_thickness,
    x_max = 0,
    y_min = 0,
    y_max = dash_length
  )

  return(substitution_zone_dash_df)
}
