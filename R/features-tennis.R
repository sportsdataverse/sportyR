# Surface Base Features --------------------------------------------------------

#' Tennis Front Court
#' 
#' The front court is the area between the [tennis_net()] and the
#' [tennis_serviceline()]. left-hand side of the court when facing the net from
#' the nearest baseline is the ad court, and the right-hand side is the deuce
#' court. This is constrained by the singles [tennis_sideline()].
#'
#' This is one half of the front court (either the ad or deuce court)
#'
#' @param serviceline_distance The distance from the net to the serviceline
#' @param singles_width The width of the singles court
#'
#' @return A data frame containing the bounding coordinates of one half of the
#'   frontcourt
#'
#' @keywords internal
tennis_frontcourt_half <- function(serviceline_distance = 0,
                                   singles_width = 0) {
  frontcourt_half_df <- create_rectangle(
    x_min = 0,
    x_max = serviceline_distance,
    y_min = -singles_width / 4,
    y_max = singles_width / 4
  )

  return(frontcourt_half_df)
}

#' Tennis Backcourt
#' 
#' The backcourt is the area behind the serviceline on the court, contained
#' within the singles sidelines
#'
#' @param court_length The length of the court
#' @param serviceline_distance The distance from the net to the serviceline
#' @param singles_width The width of the singles court
#'
#' @return A data frame containing the bounding coordinates of the backcourt
#'
#' @keywords internal
tennis_backcourt <- function(court_length = 0,
                             serviceline_distance = 0,
                             singles_width = 0) {
  backcourt_df <- create_rectangle(
    x_min = 0,
    x_max = (court_length / 2) - serviceline_distance,
    y_min = -singles_width / 2,
    y_max = singles_width / 2
  )

  return(backcourt_df)
}

#' Tennis Doubles Alley
#' 
#' The doubles alley is the area between the singles and doubles sideline. It
#' should run the entire length of the court
#'
#' @param court_length The length of the court
#' @param feature_thickness The thickness of the doubles alley (this is the
#'   distance between the singles and doubles sidelines)
#'
#' @return A data frame containing the bounding coordinates of the doubles alley
#'
#' @keywords internal
tennis_doubles_alley <- function(court_length = 0, feature_thickness = 0) {
  doubles_alley_df <- create_rectangle(
    x_min = -court_length / 2,
    x_max = court_length / 2,
    y_min = 0,
    y_max = feature_thickness
  )

  return(doubles_alley_df)
}





# Surface Boundaries -----------------------------------------------------------

#' Tennis Baseline
#' 
#' The baseline is the line behind which a player will serve the ball. It spans
#' the entire width of the court, and its back edge denotes the furthest
#' boundary inside of which a ball can land and be considered in play
#'
#' @param court_width The width of the court
#' @param feature_thickness The thickness of the baseline
#'
#' @return A data frame containing the bounding coordinates of the baseline
#'
#' @keywords internal
tennis_baseline <- function(court_width = 0, feature_thickness = 0) {
  baseline_df <- create_rectangle(
    x_min = -feature_thickness,
    x_max = 0,
    y_min = -court_width / 2,
    y_max = court_width / 2
  )

  return(baseline_df)
}

#' Tennis Sideline
#' 
#' The sideline runs the entire length of the court, and there may be up to four
#' sidelines on the court (this may refer to either the singles or doubles
#' sideline). Lines are considered in play, so the outer edge of the sideline
#' will lie along \code{x = court_width / 2}
#'
#' @param court_length The length of the court
#' @param feature_thickness The thickness of the sideline
#'
#' @return A data frame containing the bounding coordinates of the sideline
#'
#' @keywords internal
tennis_sideline <- function(court_length = 0, feature_thickness = 0) {
  sideline_df <- create_rectangle(
    x_min = -court_length / 2,
    x_max = court_length / 2,
    y_min = -feature_thickness,
    y_max = 0
  )

  return(sideline_df)
}

#' Tennis Court Apron
#' 
#' The court apron is referred to as the backstop and sidestop. These areas are
#' entirely outside of the playing court, but legal shots made here are
#' considered in play
#'
#' @param court_length The length of the court
#' @param court_width The width of the court (usually the doubles width of the
#'   court)
#' @param backstop_distance The distance from the back edge of the
#'   [tennis_baseline()] to the back boundary
#' @param sidestop_distance The distance from the outer edge of the
#'   [tennis_sideline()] to the side boundary
#'
#' @return A data frame containing the bounding coordinates of the court apron
#'
#' @keywords internal
tennis_court_apron <- function(court_length = 0,
                               court_width = 0,
                               backstop_distance = 0,
                               sidestop_distance = 0) {
  court_apron_df <- data.frame(
    x = c(
      0,
      court_length / 2,
      court_length / 2,
      0,
      0,
      (court_length / 2) + backstop_distance,
      (court_length / 2) + backstop_distance,
      0,
      0
    ),
    y = c(
      court_width / 2,
      court_width / 2,
      -court_width / 2,
      -court_width / 2,
      -((court_width / 2) + sidestop_distance),
      -((court_width / 2) + sidestop_distance),
      (court_width / 2) + sidestop_distance,
      (court_width / 2) + sidestop_distance,
      court_width / 2
    )
  )

  return(court_apron_df)
}





# Surface Lines ----------------------------------------------------------------

#' Tennis Serviceline
#' 
#' The serviceline is the line in front of which (nearest the net) a serve must
#' land, and be on the proper side of the court to be considered legal and in
#' play
#'
#' This line extends completely between the singles sidelines (but not extend to
#' the doubles sidelines)
#'
#' @param singles_width The width of the singles court
#' @param feature_thickness The thickness of the serviceline
#'
#' @return A data frame containing the bounding coordinates of the serviceline
#'
#' @keywords internal
tennis_serviceline <- function(singles_width = 0, feature_thickness = 0) {
  serviceline_df <- create_rectangle(
    x_min = -feature_thickness,
    x_max = 0,
    y_min = -singles_width / 2,
    y_max = singles_width / 2
  )

  return(serviceline_df)
}

#' Tennis Center Serviceline
#' 
#' The center serviceline on the court divides the service area into two parts:
#' the ad court (left) and the deuce court (right)
#'
#' This line extends from the net to the back edge of the serviceline, and is
#' centered on the line \code{x = 0}
#'
#' @param center_serviceline_length The length of the center serviceline from
#'   the net to the back edge of the serviceline (see [tennis_serviceline()] for
#'   more information)
#' @param feature_thickness The thickness of the center serviceline
#'
#' @return A data frame containing the bounding coordinates of the center
#'   serviceline
#'
#' @keywords internal
tennis_center_serviceline <- function(center_serviceline_length = 0,
                                      feature_thickness = 0) {
  center_serviceline_df <- create_rectangle(
    x_min = 0,
    x_max = center_serviceline_length,
    y_min = -feature_thickness / 2,
    y_max = feature_thickness / 2
  )

  return(center_serviceline_df)
}





# Surface Features -------------------------------------------------------------

#' Tennis Baseline Center Mark
#' 
#' The center mark identifies the center point of the
#' [tennis_baseline()]. The line should extend towards the net
#'
#' @param center_mark_length The length of the center mark as measured from the
#'   back edge of the baseline
#' @param feature_thickness The thickness of the center mark
#'
#' @return A data frame containing the bounding coordinates of the center mark
#'
#' @keywords internal
tennis_center_mark <- function(center_mark_length = 0, feature_thickness = 0) {
  center_mark_df <- create_rectangle(
    x_min = -center_mark_length,
    x_max = 0,
    y_min = -feature_thickness / 2,
    y_max = feature_thickness / 2
  )

  return(center_mark_df)
}

#' Tennis Net
#' 
#' The net divides the court into two halves, and should run through the line
#' \code{x = 0} when viewing the court in TV view
#'
#' @param feature_thickness The thickness of the net
#' @param net_length The length of the net
#'
#' @return A data frame containing the bounding coordinates of the net
#'
#' @keywords internal
tennis_net <- function(feature_thickness = 0, net_length = 0) {
  net_df <- create_rectangle(
    x_min = -feature_thickness / 2,
    x_max = feature_thickness / 2,
    y_min = -net_length / 2,
    y_max = net_length / 2
  )

  return(net_df)
}
