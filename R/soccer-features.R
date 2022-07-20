# Surface base features --------------------------------------------------------

#' Half of the pitch is located on each side of the halfway line (see
#' \code{\link{soccer_halfway_line}} for more information)
#'
#' @param pitch_length The length of the pitch
#' @param pitch_width The width of the pitch
#'
#' @return A data frame containing the bounding coordinates of the half of the
#'   pitch
#'
#' @keywords internal
soccer_half_pitch <- function(pitch_length = 0, pitch_width = 0) {
  half_pitch_df <- create_rectangle(
    x_min = -pitch_length / 4,
    x_max = pitch_length / 4,
    y_min = -pitch_width / 4,
    y_max = pitch_width / 4
  )

  return(half_pitch_df)
}





# Surface boundaries -----------------------------------------------------------

#' The lines that run the full length of the pitch are called the touchlines. In
#' some cases, they may also be referred to as the sidelines, as they comprise
#' the sides of the pitch
#'
#' The line thickness will be uniform for all features on the pitch
#'
#' @param pitch_length The length of the pitch
#' @param feature_thickness The thickness of the touchline
#'
#' @return A data frame containing the bounding coordinates of the touchline
#'
#' @keywords internal
soccer_touchline <- function(pitch_length = 0, feature_thickness = 0) {
  touchline_df <- create_rectangle(
    x_min = -pitch_length / 2,
    x_max = pitch_length / 2,
    y_min = -feature_thickness,
    y_max = 0
  )

  return(touchline_df)
}

#' The goal line is the line that runs the full width of the pitch. The ball
#' must completely cross the goal line to score a goal for the attacking team
#'
#' The line thickness will be uniform for all features on the pitch
#'
#' @param pitch_width The width of the pitch
#' @param feature_thickness The thickness of the goal line
#'
#' @return A data frame containing the bounding coordinates of the goal line
#'
#' @keywords internal
soccer_goal_line <- function(pitch_width = 0, feature_thickness = 0) {
  goal_line_df <- create_rectangle(
    x_min = -feature_thickness,
    x_max = 0,
    y_min = -pitch_width / 2,
    y_max = pitch_width / 2
  )

  return(goal_line_df)
}





# Surface lines ----------------------------------------------------------------

#' The halfway line, aka the midfield line or center line, runs the width of the
#' pitch, dividing it into two equal halves. The left half (in TV view) will be
#' the defensive half, and the right half will be the offensive half
#'
#' The line thickness will be uniform for all features on the pitch
#'
#' @param pitch_width The width of the pitch
#' @param feature_thickness The thickness of the goal line
#'
#' @return A data frame containing the bounding coordinates of the halfway line
#'
#' @keywords internal
soccer_halfway_line <- function(pitch_width = 0, feature_thickness = 0) {
  halfway_line_df <- create_rectangle(
    x_min = -feature_thickness / 2,
    x_max = feature_thickness / 2,
    y_min = -pitch_width / 2,
    y_max = pitch_width / 2
  )

  return(halfway_line_df)
}

#' The penalty box on the pitch is the larger of the two boxes that extend from
#' the goal line. The penalty box is usually 16.5 meters (18 yards) from the
#' goal line, but may be parameterized via this function
#'
#' This draws a half-box, which will include the circular portion at the top of
#' the box. All dimensions given should be to the outside of the features
#'
#' The line thickness will be uniform for all features on the pitch
#'
#' @param feature_radius The radius of the circle at the top of the penalty box
#' @param feature_thickness The thickness of the penalty box
#' @param box_length The length of the penalty box (from the goal line)
#' @param penalty_mark_dist The distance from the back edge of the goal line to
#'   the penalty mark
#' @param goal_width The interior width of the goal
#' @param goal_post_to_box_edge The distance from the interior of the goal post
#'   to the outer edge of the penalty box
#'
#' @return A data frame containing the bounding coordinates of the penalty box
#'
#' @keywords internal
soccer_penalty_box <- function(feature_radius = 0,
                               feature_thickness = 0,
                               box_length = 0,
                               penalty_mark_dist = 0,
                               goal_width = 0,
                               goal_post_to_box_edge = 0) {
  # Compute the half-box width
  half_box_width <- (goal_width / 2) + goal_post_to_box_edge

  # Start by getting the angle at which to start the penalty arc
  x_out <- box_length - penalty_mark_dist
  r_inner <- feature_radius - feature_thickness
  if (feature_radius == 0) {
    start_angle_outer <- 0.5
    start_angle_inner <- 0.5
  } else if (abs(x_out / feature_radius) > 1 || abs(x_out / r_inner) > 1) {
    start_angle_outer <- 0.5
    start_angle_inner <- 0.5
  } else {
    start_angle_outer <- 1 - (acos(x_out / feature_radius) / pi)
    start_angle_inner <- 1 - (acos(x_out / r_inner) / pi)
  }

  penalty_box_df <- rbind(
    data.frame(
      x = c(-feature_thickness, -box_length),
      y = c(half_box_width, half_box_width)
    ),
    create_circle(
      center = c(-penalty_mark_dist, 0),
      start = start_angle_outer,
      end = 1,
      r = feature_radius
    ),
    create_circle(
      center = c(-penalty_mark_dist, 0),
      start = 1,
      end = start_angle_inner,
      r = feature_radius - feature_thickness
    ),
    data.frame(
      x = c(
        -box_length,
        -(box_length - feature_thickness),
        -(box_length - feature_thickness),
        -feature_thickness,
        -feature_thickness
      ),
      y = c(
        0,
        0,
        half_box_width - feature_thickness,
        half_box_width - feature_thickness,
        half_box_width
      )
    )
  )

  return(penalty_box_df)
}

#' The goal box is the smaller of the two boxes that extend from the goal line
#' The goal box is usually 5.5 meters (6 yards) from the goal line, but may be
#' parameterized via this function
#'
#' The line thickness will be uniform for all features on the pitch
#'
#' @param feature_thickness The thickness of the goal box
#' @param box_length The length of the goal box (from the goal line)
#' @param goal_width The interior width of the goal
#' @param goal_post_to_box_edge The distance from the interior of the goal post
#'   to the outer edge of the goal box
#'
#' @return A data frame containing the bounding coordinates of the goal box
#'
#' @keywords internal
soccer_goal_box <- function(feature_thickness = 0,
                            box_length = 0,
                            goal_width = 0,
                            goal_post_to_box_edge = 0) {
  # Compute the half-box width
  half_box_width <- (goal_width / 2) + goal_post_to_box_edge

  goal_box_df <- data.frame(
    x = c(
      -feature_thickness,
      -box_length,
      -box_length,
      -feature_thickness,
      -feature_thickness,
      -(box_length - feature_thickness),
      -(box_length - feature_thickness),
      -feature_thickness,
      -feature_thickness
    ),
    y = c(
      half_box_width,
      half_box_width,
      -half_box_width,
      -half_box_width,
      -(half_box_width - feature_thickness),
      -(half_box_width - feature_thickness),
      half_box_width - feature_thickness,
      half_box_width - feature_thickness,
      half_box_width
    )
  )

  return(goal_box_df)
}





# Surface features -------------------------------------------------------------

#' The corner arcs are the quarter-circles located where the touchline meets the
#' goal line
#'
#' The line thickness will be uniform for all features on the pitch
#'
#' @param feature_radius The outer radius of the corner arc
#' @param feature_thickness The thickness of the corner arc
#'
#' @return A data frame containing the bounding coordinates of the corner arc
#'
#' @keywords internal
soccer_corner_arc <- function(feature_radius = 0, feature_thickness = 0) {
  corner_arc_df <- rbind(
    create_circle(
      center = c(0, 0),
      start = 1,
      end = 1.5,
      r = feature_radius
    ),
    create_circle(
      center = c(0, 0),
      start = 1.5,
      end = 1,
      r = feature_radius - feature_thickness
    )
  )

  return(corner_arc_df)
}

#' The center circle is the circle located at the center of the field. Inside of
#' the center circle is the center mark, where each half begins, as well as
#' where play resumes following a goal
#'
#' The line thickness will be uniform for all features on the pitch
#'
#' @param feature_radius The outer radius of the center circle
#' @param feature_thickness The thickness of the center circle
#'
#' @return A data frame containing the bounding coordinates of the center circle
#'
#' @keywords internal
soccer_center_circle <- function(feature_radius = 0, feature_thickness = 0) {
  center_circle_df <- rbind(
    create_circle(
      center = c(0, 0),
      start = 0.5,
      end = 1.5,
      r = feature_radius
    ),
    create_circle(
      center = c(0, 0),
      start = 1.5,
      end = 0.5,
      r = feature_radius - feature_thickness
    )
  )

  return(center_circle_df)
}

#' The center mark is where kickoffs for each half, as well as following any
#' goal, are taken. The radius should be given to the outside of the mark. This
#' feature is located at midfield
#'
#' @param feature_radius The radius of the center mark on the pitch
#'
#' @return A data frame containing the bounding coordinates of the center mark
#'
#' @keywords internal
soccer_center_mark <- function(feature_radius = 0) {
  center_mark_df <- create_circle(
    center = c(0, 0),
    start = 0,
    end = 2,
    r = feature_radius
  )

  return(center_mark_df)
}

#' The penalty mark is the center point for the arc of the penalty box, as well
#' as where any penalty kick is taken from
#'
#' @param feature_radius The radius of the penalty mark
#'
#' @return A data frame containing the bounding coordinates of the penalty mark
#'
#' @keywords internal
soccer_penalty_mark <- function(feature_radius = 0) {
  penalty_mark_df <- create_circle(
    center = c(0, 0),
    start = 0,
    end = 2,
    r = feature_radius
  )

  return(penalty_mark_df)
}

#' The corner defensive marks on the pitch are typically located 9.15 meters (10
#' yards) from the corner of the pitch. Defenders should be beyond these marks
#' (either more towards the goal or more towards the halfway line) during corner
#' kicks
#'
#' The marks should be outside the field of play
#'
#' The line thickness will be uniform for all features on the pitch
#'
#' @param feature_thickness The thickness of the corner defensive marks
#' @param is_touchline A boolean indicating whether or not the corner defensive
#'   marks should be along the touchline
#' @param is_goal_line A boolean indicating whether or not the corner defensive
#'   marks should be along the goal line
#' @param depth The depth that the mark extends out of play
#' @param separation_from_line The distance from the back edge of the goal line
#'   to the interior edge of the corner defensive mark
#'
#' @return A data frame containing the bounding coordinates of the corner
#'   defensive marks
#'
#' @keywords internal
soccer_corner_defensive_marks <- function(feature_thickness = 0,
                                          is_touchline = FALSE,
                                          is_goal_line = FALSE,
                                          depth = 0,
                                          separation_from_line = 0) {
  if (is_touchline) {
    corner_defensive_mark_df <- create_rectangle(
      x_min = -feature_thickness / 2,
      x_max = feature_thickness / 2,
      y_min = separation_from_line,
      y_max = separation_from_line + depth
    )
  } else if (is_goal_line) {
    corner_defensive_mark_df <- create_rectangle(
      x_min = separation_from_line,
      x_max = separation_from_line + depth,
      y_min = -feature_thickness / 2,
      y_max = feature_thickness / 2
    )
  } else {
    corner_defensive_mark_df <- data.frame(x = c(), y = c())
  }

  return(corner_defensive_mark_df)
}

#' The goal is located beyond each goal line. By rule, the goal posts must be
#' the same thickness as the goal line, and the posts must rest on the front
#' edge of the goal line
#'
#' The line thickness will be uniform for all features on the pitch
#'
#' @param feature_thickness The thickness of the goal
#' @param goal_width The interior width of the goal
#' @param goal_depth The depth to which the goal protrudes away from the back
#'   edge of the goal line
#'
#' @return A data frame containing the bounding coordinates of the goal
#'
#' @keywords internal
soccer_goal <- function(feature_thickness = 0, goal_width = 0, goal_depth = 0) {
  goal_df <- data.frame(
    x = c(
      0,
      goal_depth + feature_thickness,
      goal_depth + feature_thickness,
      0,
      0,
      goal_depth,
      goal_depth,
      0,
      0
    ),
    y = c(
      (goal_width / 2) + feature_thickness,
      (goal_width / 2) + feature_thickness,
      -((goal_width / 2) + feature_thickness),
      -((goal_width / 2) + feature_thickness),
      -goal_width / 2,
      -goal_width / 2,
      goal_width / 2,
      goal_width / 2,
      (goal_width / 2) + feature_thickness
    )
  )

  return(goal_df)
}
