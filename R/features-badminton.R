# Surface Base Features --------------------------------------------------------
#' Badminton Forecourt
#'
#' The forecourt is the area between the [badminton_net()] and the
#' [badminton_short_serviceline()]. This is constrained by the singles
#' [badminton_sideline()].
#'
#' @param short_serviceline_distance The distance from the net to the short
#' service line
#' @param singles_width The width of the singles court
#'
#' @return A data frame containing the bounding coordinates of one half of the
#'   forecourt
#'
#' @keywords internal
badminton_forecourt_half <- function(short_serviceline_distance = 0,
                                     singles_width = 0) {
  forecourt_half_df <- create_rectangle(
    x_min = 0,
    x_max = short_serviceline_distance,
    y_min = -singles_width / 2,
    y_max = singles_width / 2
  )

  return(forecourt_half_df)
}

#' Badminton Backcourt
#'
#' For singles, the backcourt is the area between the midcourt and the back
#' boundary line, inside the singles sidelines. For doubles, the backcourt is
#' the area between the midcourt and the long service line, inside the doubles
#' sidelines.
#'
#' @param court_length The length of the court
#' @param long_serviceline_distance The distance from the net to the short
#'   service line
#' @param singles_width The width of the singles court
#'
#' @return A data frame containing the bounding coordinates of the backcourt
#'
#' @keywords internal
badminton_backcourt <- function(court_length = 0,
                                long_serviceline_distance = 0,
                                singles_width = 0) {
  backcourt_df <- create_rectangle(
    x_max = court_length / 2,
    x_min = long_serviceline_distance,
    y_min = -singles_width / 2,
    y_max = singles_width / 2
  )

  return(backcourt_df)
}

#' Badminton Doubles Alley
#'
#' The doubles alley is the area between the singles and doubles sidelines. It
#' runs the entire length of the court, along both sides.
#'
#' @param court_length The length of the court
#' @param feature_thickness The thickness of the doubles alley (this is the
#'   distance between the singles and doubles sidelines)
#'
#' @return A data frame containing the bounding coordinates of the doubles alley
#'
#' @keywords internal
badminton_doubles_alley <- function(court_length = 0, feature_thickness = 0) {
  doubles_alley_df <- create_rectangle(
    x_min = -court_length / 2,
    x_max = court_length / 2,
    y_min = -feature_thickness,
    y_max = feature_thickness
  )

  return(doubles_alley_df)
}

# Surface Boundaries -----------------------------------------------------------

#' Back Boundary Line
#'
#' The back boundary line is the line behind which a player will serve the
#' shuttlecock. It spans the entire width of the court, and its back edge
#' denotes the furthest boundary inside of which a shuttlecock can land and be
#' considered in play
#'
#' @param court_width The width of the court
#' @param feature_thickness The thickness of the back boundary line
#'
#' @return A data frame containing the bounding coordinates of the back
#' boundary line
#'
#' @keywords internal
badminton_back_boundary_line <- function(court_width = 0,
                                         feature_thickness = 0) {
  boundary_df <- create_rectangle(
    x_min = -feature_thickness,
    x_max = 0,
    y_min = -court_width / 2,
    y_max = court_width / 2
  )

  return(boundary_df)
}

#' Sideline
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
badminton_sideline <- function(court_length = 0, feature_thickness = 0) {
  sideline_df <- create_rectangle(
    x_min = -court_length / 2,
    x_max = court_length / 2,
    y_min = -feature_thickness,
    y_max = 0
  )

  return(sideline_df)
}

badminton_centerline <- function(court_length = 0,
                                 short_serviceline_distance = 0,
                                 feature_thickness = 0) {
  centerline_df <- create_rectangle(
    x_min = short_serviceline_distance,
    x_max = (court_length)/2,
    y_min = -feature_thickness,
    y_max = 0
  )

  return(centerline_df)
}

#' Court Apron
#'
#' The court apron is referred to as the backstop and sidestop. These areas are
#' entirely outside of the playing court, but legal shots made here are
#' considered in play
#'
#' @param court_length The length of the court
#' @param court_width The width of the court (usually the doubles width of the
#'   court)
#' @param backstop_distance The distance from the back edge of the
#'   [badminton_baseline()] to the back boundary
#' @param sidestop_distance The distance from the outer edge of the
#'   [badminton_sideline()] to the side boundary
#'
#' @return A data frame containing the bounding coordinates of the court apron
#'
#' @keywords internal
badminton_court_apron <- function(court_length = 0,
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

#' There are 2 service lines: Short and Long.

#' The short service line is the line closest to the net that marks the minimum
#' distance a serve must travel. For a serve to be legal, it must land past this
#' line and within the correct service court. Serves that land in front of the
#' short service line are called 'short' and are considered faults.
#'
#' This line extends completely between the singles sidelines (but not extend to
#' the doubles sidelines)
#'
#' #' The long service line marks the maximum distance a serve can travel in
#' doubles. For a serve to be legal in doubles, it must land in front of or on
#' this line, within the correct service court. Serves that land beyond this
#' line are faults. In doubles, this line runs between the doubles sidelines and
#' is shorter than the back boundary line.
#'
#' In singles, the back boundary line acts as the long service line, meaning serves
#' can land anywhere within the full length of the court.

#'
#' @param singles_width The width of the singles service area (usually 5.18m)
#' @param line_thickness The thickness of the short service line
#'
#' @return A data frame containing the bounding coordinates of the short service
#' line
#'
#' @keywords internal
badminton_short_serviceline <- function(doubles_width = 0,
                                        line_thickness = 0,
                                        short_serviceline_distance = 0) {
  # Distance from the net to the short service line (bwf standard is 1.98m)
  x_pos <- short_serviceline_distance

  short_serviceline_df <- create_rectangle(
    x_min = x_pos - (line_thickness / 2),
    x_max = x_pos + (line_thickness / 2),
    y_min = -(doubles_width / 2),
    y_max = (doubles_width / 2)
  )

  return(short_serviceline_df)
}

badminton_long_serviceline <- function(doubles_width = 0,
                                       line_thickness = 0,
                                       long_serviceline_distance = 0) {
  # Distance from the net to long service line (bwf standard is 5.86m)
  x_pos <- long_serviceline_distance

  long_serviceline_df <- create_rectangle(
    x_min = x_pos - (line_thickness / 2),
    x_max = x_pos + (line_thickness / 2),
    y_min = -(doubles_width / 2),
    y_max = (doubles_width / 2)
  )

  return(long_serviceline_df)
}

# Surface Features -------------------------------------------------------------

#' The net divides the court into two halves, and should run through the line
#' \code{x = 0} when viewing the court in TV view
#'
#' @param feature_thickness The thickness of the net
#' @param net_length The length of the net
#'
#' @return A data frame containing the bounding coordinates of the net
#'
#' @keywords internal
badminton_net <- function(feature_thickness = 0, net_length = 0) {
  net_df <- create_rectangle(
    x_min = -feature_thickness / 2,
    x_max = feature_thickness / 2,
    y_min = -net_length / 2,
    y_max = net_length / 2
  )

  return(net_df)
}
