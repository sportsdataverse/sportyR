# Surface Base Features --------------------------------------------------------

#' Lacrosse Defensive Zone
#' 
#' The defensive zone is the TV-left area of the playing surface. In many cases,
#' this will correspond to half of the field's length
#'
#' @param field_length The interior length of the field
#' @param field_width The interior width of the field
#' @param corner_radius The radius of the corner (assuming \code{field_shape =
#'   "oval"}). Viable options are \code{"rectangle"} or \code{"oval"}
#' @param nzone_length The length of the neutral zone
#' @param field_shape The shape of the field, passed as a string
#'
#' @return A data frame containing the bounding coordinates of the defensive
#'   zone
#'
#' @keywords internal
lacrosse_defensive_zone <- function(field_length = 0,
                                    field_width = 0,
                                    corner_radius = 0,
                                    nzone_length = 0,
                                    field_shape = "rectangle") {
  if (tolower(field_shape) == "oval") {
    # Create the defensive zone
    half_length <- field_length / 2
    half_width <- field_width / 2

    # Find where the point to use as the center of the circle with the given
    # radius for the boards' corners' arc
    center_x <- half_length - corner_radius
    center_y <- half_width - corner_radius

    # Calculate the corner arc's inner radius
    arc_inner_upper <- create_circle(
      center = c(-center_x, center_y),
      start = 0.5,
      end = 1,
      r = corner_radius
    )

    arc_inner_lower <- create_circle(
      center = c(-center_x, -center_y),
      start = 1,
      end = 1.5,
      r = corner_radius
    )

    dzone_df <- rbind(
      # Start at the upper right corner of the zone line that is closest to the
      # center of the field
      data.frame(
        x = c(-nzone_length / 2),
        y = c(half_width)
      ),

      # Then draw the upper left arc of the boards
      arc_inner_upper,

      # Then its guaranteed point at half the length of the field
      data.frame(
        x = c(-half_length),
        y = c(0)
      ),

      # Then the lower left arc
      arc_inner_lower,

      # Then go to the bottom of the field in TV view with the boards' inner
      # boundary before closing the path by returning to the starting point
      data.frame(
        x = c(-nzone_length / 2, -nzone_length / 2),
        y = c(-half_width, half_width)
      )
    )
  } else {
    # Create the defensive zone
    dzone_df <- create_rectangle(
      x_min = -field_length / 2,
      x_max = 0,
      y_min = -field_width / 2,
      y_max = field_width / 2
    )
  }

  return(dzone_df)
}

#' Lacrosse Neutral Zone
#' 
#' The neutral zone corresponds to the area between the restraining lines. In
#' cases where there are no restraining lines, this feature will have 0 length
#'
#' @param nzone_length The length of the neutral zone
#' @param field_width The width of the field
#'
#' @return A data frame containing the bounding coordinates of the neutral zone
#'
#' @keywords internal
lacrosse_neutral_zone <- function(nzone_length = 0, field_width = 0) {
  # Create the neutral zone
  nzone_df <- create_rectangle(
    x_min = -nzone_length / 2,
    x_max = nzone_length / 2,
    y_min = -field_width / 2,
    y_max = field_width / 2
  )

  return(nzone_df)
}

#' Lacrosse Offensive Zone
#' 
#' The offensive zone is where a team tries to score a goal. It is the TV-right
#' area on the field
#'
#' @param field_length The interior length of the field
#' @param field_width The interior width of the field
#' @param corner_radius The radius of the corner (assuming \code{field_shape =
#'   "oval"}). Viable options are \code{"rectangle"} or \code{"oval"}
#' @param nzone_length The length of the neutral zone
#' @param field_shape The shape of the field, passed as a string
#'
#' @return A data frame of the bounding coordinates of the offensive zone
#'
#' @keywords internal
lacrosse_offensive_zone <- function(field_length = 0,
                                    field_width = 0,
                                    corner_radius = 0,
                                    nzone_length = 0,
                                    field_shape = "rectangle") {
  if (tolower(field_shape) == "oval") {
    # Specify the dimensions of the field
    half_length <- field_length / 2
    half_width <- field_width / 2

    # Find where the point to use as the center of the circle with the given
    # radius for the boards' corners' arc
    center_x <- half_length - corner_radius
    center_y <- half_width - corner_radius

    # Create the points along the corner arc's inner radii
    arc_inner_upper <- create_circle(
      center = c(center_x, center_y),
      start = 0.5,
      end = 0,
      r = corner_radius
    )

    arc_inner_lower <- create_circle(
      center = c(center_x, -center_y),
      start = 0,
      end = -0.5,
      r = corner_radius
    )

    ozone_df <- rbind(
      # Start at the upper left corner of the zone line that is closest to the
      # center of the field
      data.frame(
        x = c(nzone_length / 2),
        y = c(half_width)
      ),

      # Then draw the upper right corner of the boards
      arc_inner_upper,

      # Then its guaranteed point at half the length of the field
      data.frame(
        x = c(half_length),
        y = c(0)
      ),

      # Then the lower right corner
      arc_inner_lower,

      # Then go to the bottom of the field in TV view with the boards' inner
      # boundary before closing the path by returning to the starting point
      data.frame(
        x = c(nzone_length / 2, nzone_length / 2),
        y = c(-half_width, half_width)
      )
    )
  } else {
    ozone_df <- create_rectangle(
      x_min = 0,
      x_max = field_length / 2,
      y_min = -field_width / 2,
      y_max = field_width / 2
    )
  }

  return(ozone_df)
}

#' Lacrosse Field Apron
#' 
#' Outdoor require should have a field apron so that boundary lines are more
#' visible
#'
#' @param field_length The length of the field
#' @param field_width The width of the field
#' @param field_apron_thickness The thickness of the field apron
#' @param field_shape The shape of the field, passed as a string
#'
#' @return A data frame of the bounding coordinates of the field apron
#'
#' @keywords internal
lacrosse_field_apron <- function(field_length = 0,
                                 field_width = 0,
                                 field_apron_thickness = 0,
                                 field_shape = "rectangle") {
  # If the field is shaped as a rectangle, draw the apron from a series of
  # vertices
  if (tolower(field_shape) == "rectangle") {
    field_apron_df <- data.frame(
      x = c(
        0,
        field_length / 2,
        field_length / 2,
        0,
        0,
        (field_length / 2) + field_apron_thickness,
        (field_length / 2) + field_apron_thickness,
        0,
        0
      ),
      y = c(
        field_width / 2,
        field_width / 2,
        -field_width / 2,
        -field_width / 2,
        (-field_width / 2) - field_apron_thickness,
        (-field_width / 2) - field_apron_thickness,
        field_width / 2 + field_apron_thickness,
        field_width / 2 + field_apron_thickness,
        field_width / 2
      )
    )
  } else {
    # If the field is not a rectangle, return an empty data frame
    field_apron_df <- data.frame(x = c(0), y = c(0))
  }

  return(field_apron_df)
}





# Surface Boundaries -----------------------------------------------------------

#' Lacrosse Sideline
#' 
#' The sidelines run the length of the field, with its interior edge designating
#' the in-bounds area
#'
#' @param field_length The interior length of the field
#' @param line_thickness The thickness of the sideline
#'
#' @return A data frame containing the bounding coordinates of the sideline
#'
#' @keywords internal
lacrosse_sideline <- function(field_length = 0, line_thickness = 0) {
  # Create the sideline
  sideline_df <- create_rectangle(
    x_min = -field_length / 2,
    x_max = field_length / 2,
    y_min = 0,
    y_max = line_thickness
  )

  return(sideline_df)
}

#' Lacrosse End Line
#' 
#' The end lines run the width of the field, with its interior edge designating
#' the in-bounds area
#'
#' @param field_width The interior width of the field
#' @param line_thickness The thickness of the end line
#'
#' @return A data frame containing the bounding coordinates of the sideline
#'
#' @keywords internal
lacrosse_end_line <- function(field_width = 0, line_thickness = 0) {
  # Create the end line
  end_line_df <- create_rectangle(
    x_min = 0,
    x_max = line_thickness,
    y_min = -field_width / 2,
    y_max = field_width / 2
  )

  return(end_line_df)
}

#' Lacrosse Boards
#' 
#' The boards are the wall around the outside of the field that constrain the
#' playing surface. The boards are either typically ovular in shape, or not
#' present if the field is outdoors
#'
#' @param field_length The length of the field
#' @param field_width The width of the field
#' @param corner_radius The radius of the corners of the boards
#' @param boundary_thickness The thickness with which to draw the boards
#'
#' @return A data frame of the bounding coordinates of the boards
#'
#' @keywords internal
lacrosse_boards <- function(field_length = 0,
                            field_width = 0,
                            corner_radius = 0,
                            boundary_thickness = 0) {
  # Specify the half-dimensions of the field
  half_length <- field_length / 2
  half_width <- field_width / 2

  # Find the point to use as the center of the circle with the given radius for
  # the boards' corners' arc
  center_x <- half_length - corner_radius
  center_y <- half_width - corner_radius

  # Create the points along the corner arc's inner radii
  arc_inner_upper <- create_circle(
    center = c(center_x, center_y),
    start = 0.5,
    end = 0,
    r = corner_radius
  )

  arc_inner_lower <- create_circle(
    center = c(center_x, -center_y),
    start = 0,
    end = -0.5,
    r = corner_radius
  )

  # Calculate the corner arc's outer radius
  arc_outer_upper <- create_circle(
    center = c(center_x, center_y),
    start = 0,
    end = 0.5,
    r = corner_radius + boundary_thickness
  )

  arc_outer_lower <- create_circle(
    center = c(center_x, -center_y),
    start = -0.5,
    end = 0,
    r = corner_radius + boundary_thickness
  )

  # Combine the boards' inner and outer arcs with its guaranteed coordinates
  boards_df <- rbind(
    # Start at the top of the field in TV view with the boards' inner boundary
    data.frame(
      x = c(0),
      y = c(half_width)
    ),

    # Then add in its upper inner arc
    arc_inner_upper,

    # Then its guaranteed point at half the length of the field
    data.frame(
      x = c(half_length),
      y = c(0)
    ),

    # Then its lower inner arc
    arc_inner_lower,

    # Then go to the bottom of the field in TV view with the boards' inner
    # boundary before flipping to the outer boundary
    data.frame(
      x = c(0, 0),
      y = c(-half_width, -half_width - boundary_thickness)
    ),

    # Back to the lower arc on the outer boundary
    arc_outer_lower,

    # Then back to the middle
    data.frame(
      x = c(half_length + boundary_thickness),
      y = c(0)
    ),

    # Then back to the upper arc
    arc_outer_upper,

    # Finally back to the top and original starting point
    data.frame(
      x = c(0, 0),
      y = c(half_width + boundary_thickness, half_width)
    )
  )

  return(boards_df)
}





# Surface Lines ----------------------------------------------------------------

#' Lacrosse Center Line
#' 
#' The center line divides the field of play into two equal halves, which are
#' generated via [lacrosse_offensive_zone()], [lacrosse_defensive_zone()], and
#' [lacrosse_neutral_zone()]. This line may not stretch the entire width of the
#' field, so a parameter is created instead
#'
#' @param center_line_width The width of the center line (distance in \code{y})
#' @param line_thickness The thickness of the center line
#'
#' @return A data frame containing the bounding coordinates of the center line
#'
#' @keywords internal
lacrosse_center_line <- function(center_line_width = 0, line_thickness = 0) {
  # Create the center line
  center_line_df <- create_rectangle(
    x_min = -line_thickness / 2,
    x_max = line_thickness / 2,
    y_min = -center_line_width / 2,
    y_max = center_line_width / 2
  )

  return(center_line_df)
}

#' Lacrosse Wing Line
#' 
#' The wing lines run parallel to the sidelines and cross the center line
#'
#' @param wing_line_length The length of the wing line
#' @param line_thickness The thickness of the wing line
#'
#' @return A data frame containing the bounding coordinates of the wing line
#'
#' @keywords internal
lacrosse_wing_line <- function(wing_line_length = 0, line_thickness = 0) {
  # Create the wing line
  wing_line_df <- create_rectangle(
    x_min = -wing_line_length / 2,
    x_max = wing_line_length / 2,
    y_min = 0,
    y_max = line_thickness
  )

  return(wing_line_df)
}

#' Lacrosse Restraining Line
#' 
#' The restraining line spans the entire width of the field (where present) and
#' connects to the defensive area lines (see [lacrosse_defensive_area_line()])
#'
#' @param field_width The width of the field
#' @param line_thickness The thickness of the restraining line
#'
#' @return A data frame containing the bounding coordinates of the restraining
#'   line
#'
#' @keywords internal
lacrosse_restraining_line <- function(field_width = 0, line_thickness = 0) {
  # Create the restraining line
  restraining_line_df <- create_rectangle(
    x_min = -line_thickness,
    x_max = 0,
    y_min = -field_width / 2,
    y_max = field_width / 2
  )

  return(restraining_line_df)
}

#' Lacrosse Defensive Area Line
#' 
#' The defensive-area lines run parallel to the sidelines and are connected to
#' the end line (see [lacrosse_end_line()]) and restraining line (see
#' [lacrosse_restraining_line()]) when these features are present
#'
#' @param defensive_area_line_length The length of the defensive-area line
#'   length
#' @param line_thickness The thickness of the defensive area line
#'
#' @return A data frame containing the bounding coordinates of the defensive
#'   area lines
#'
#' @keywords internal
lacrosse_defensive_area_line <- function(defensive_area_line_length = 0,
                                         line_thickness = 0) {
  # Create the defensive area line
  defensive_area_line_df <- create_rectangle(
    x_min = 0,
    x_max = defensive_area_line_length,
    y_min = 0,
    y_max = line_thickness
  )

  return(defensive_area_line_df)
}

#' Lacrosse Goal Line
#' 
#' The goal line is where the front edge of the goal sits. It spans the entire
#' interior dimension of the goal mouth. Its anchoring \code{x} coordinate
#' should be its center (e.g. half of the line's width should be on each side of
#' the \code{x} anchor)
#'
#' @param goal_frame_width The interior width of the goal frame's opening
#' @param line_thickness The thickness of the goal line
#' @param goal_line_full_diameter Whether or not the goal line should extend the
#'   full diameter of the [lacrosse_goal_circle()]
#' @param goal_circle_radius The outer radius of the goal circle
#'
#' @return A data frame containing the bounding coordinates of the goal line
#'
#' @keywords internal
lacrosse_goal_line <- function(goal_frame_width = 0,
                               line_thickness = 0,
                               goal_line_full_diameter = FALSE,
                               goal_circle_radius = 0) {
  if (!goal_line_full_diameter) {
    # Create the goal line
    goal_line_df <- create_rectangle(
      x_min = -line_thickness / 2,
      x_max = line_thickness / 2,
      y_min = -goal_frame_width / 2,
      y_max = goal_frame_width / 2
    )
  } else {
    angle_dev <- asin((line_thickness / 2) / goal_circle_radius) / pi
    goal_line_df <- rbind(
      create_circle(
        center = c(0, 0),
        start = 0.5 - angle_dev,
        end = 0.5 + angle_dev,
        r = goal_circle_radius
      ),
      create_circle(
        center = c(0, 0),
        start = -0.5 - angle_dev,
        end = -0.5 + angle_dev,
        r = goal_circle_radius
      )
    )
  }

  return(goal_line_df)
}

#' Lacrosse Referee's Crease
#' 
#' The referee's crease is a semi-circle on the "bottom" of the boards (in TV
#' view), centered on the line y = 0 (the center of the center line)
#'
#' @param referee_crease_radius The radius of the referee's crease
#' @param line_thickness The thickness with which to draw the referee's crease
#'
#' @return A data frame of the bounding coordinates of the referee's crease
#'
#' @keywords internal
lacrosse_referee_crease <- function(referee_crease_radius = 0,
                                    line_thickness = 0) {
  referee_crease_df <- rbind(
    data.frame(
      x = c(referee_crease_radius),
      y = c(0)
    ),
    create_circle(
      center = c(0, 0),
      start = 0,
      end = 1,
      r = referee_crease_radius
    ),
    data.frame(
      x = c(
        -referee_crease_radius,
        -referee_crease_radius + line_thickness
      ),
      y = c(
        0,
        0
      )
    ),
    create_circle(
      center = c(0, 0),
      start = 1,
      end = 0,
      r = referee_crease_radius - line_thickness
    ),
    data.frame(
      x = c(referee_crease_radius),
      y = c(0)
    )
  )

  return(referee_crease_df)
}





# Surface Features -------------------------------------------------------------

#' Lacrosse Goal Circle (Outline)
#' 
#' The goal circle is a circular feature on the field that houses the goal line
#' (see [lacrosse_goal_line()]) and the goal. Notably, for fields with a
#' surrounding arc and/or fan around the goal area, this feature _only_
#' circumscribes the goal. Those features will be handled separately. This
#' feature may either be the full circle (e.g. all 360 degrees), or a partial
#' circle that may be greater than a half-circle
#'
#' @param goal_circle_radius The outer radius of the goal circle
#' @param line_thickness The thickness of the goal circle
#' @param goal_circle_full_360 A boolean indicating whether the goal circle
#'   should be a 360 degree circle
#' @param goal_depth The depth of the goal
#' @param goal_depth_to_circle The distance from the back tip of the goal to the
#'   outer radius of the goal circle
#'
#' @return A data frame containing the bounding coordinates of the goal circle
#'
#' @keywords internal
lacrosse_goal_circle <- function(goal_circle_radius = 0,
                                 line_thickness = 0,
                                 goal_circle_full_360 = TRUE,
                                 goal_depth = 0,
                                 goal_depth_to_circle = 0) {
  if (goal_circle_full_360) {
    # Create the full goal circle
    goal_circle_df <- rbind(
      data.frame(
        x = 0,
        y = goal_circle_radius
      ),
      create_circle(
        center = c(0, 0),
        start = 0.5,
        end = -0.5,
        r = goal_circle_radius
      ),
      data.frame(
        x = c(0, 0),
        y = c(-goal_circle_radius, -goal_circle_radius + line_thickness)
      ),
      create_circle(
        center = c(0, 0),
        start = -0.5,
        end = 0.5,
        r = goal_circle_radius - line_thickness
      ),
      data.frame(
        x = c(0, 0),
        y = c(goal_circle_radius - line_thickness, goal_circle_radius)
      )
    )

    goal_circle_df <- rbind(
      goal_circle_df,
      reflect(goal_circle_df, over_x = FALSE, over_y = TRUE)
    )
  } else {
    # To compute the depth to which the goal circle should go, start by
    # calculating the distance to the outer edge of the back of the goal circle
    # from the goal line
    goal_circle_depth <- goal_depth_to_circle + goal_depth

    # Next, compute the angle at which to start drawing the circle
    start_angle <- acos(goal_circle_depth / goal_circle_radius) / pi

    # The ending angle should be 2pi less the starting angle
    end_angle <- 2 - start_angle

    # Get the coordinates of the goal circle
    goal_circle_df <- rbind(
      data.frame(
        x = goal_circle_depth,
        y = 0
      ),
      create_circle(
        center = c(0, 0),
        start = start_angle,
        end = end_angle,
        r = goal_circle_radius
      ),
      data.frame(
        x = c(goal_circle_depth, goal_circle_depth - line_thickness),
        y = c(0, 0)
      ),
      create_circle(
        center = c(0, 0),
        start = 2 - start_angle,
        end = 2 - end_angle,
        r = goal_circle_radius - line_thickness
      ),
      data.frame(
        x = c(goal_circle_depth - line_thickness, goal_circle_depth),
        y = c(0, 0)
      )
    )

  }

  return(goal_circle_df)
}

#' Lacrosse Goal Circle (Interior)
#' 
#' This feature is the area enclosed by the goal circle's outline. Please see
#' [lacrosse_goal_circle()] for more information
#'
#' @param goal_circle_radius The outer radius of the goal circle
#' @param line_thickness The thickness of the goal circle
#' @param goal_circle_full_360 A boolean indicating whether the goal circle
#'   should be a 360 degree circle
#' @param goal_depth The depth of the goal
#' @param goal_depth_to_circle The distance from the back tip of the goal to the
#'   outer radius of the goal circle
#'
#' @return A data frame containing the bounding coordinates of the goal circle's
#'   enclosed area
#'
#' @keywords internal
lacrosse_goal_circle_fill <- function(goal_circle_radius = 0,
                                      line_thickness = 0,
                                      goal_circle_full_360 = TRUE,
                                      goal_depth = 0,
                                      goal_depth_to_circle = 0) {
  if (goal_circle_full_360) {
    # Create the full goal circle
    goal_circle_fill_df <- create_circle(
      center = c(0, 0),
      start = 0,
      end = 2,
      r = goal_circle_radius - line_thickness
    )
  } else {
    # To compute the depth to which the goal circle should go, start by
    # calculating the distance to the outer edge of the back of the goal circle
    # from the goal line
    goal_circle_depth <- goal_depth_to_circle + goal_depth

    # Next, compute the angle at which to start drawing the circle
    start_angle <- acos(goal_circle_depth / goal_circle_radius) / pi

    # The ending angle should be 2pi less the starting angle
    end_angle <- 2 - start_angle

    # Get the coordinates of the goal circle
    goal_circle_fill_df <- create_circle(
      center = c(0, 0),
      start = 2 - start_angle,
      end = 2 - end_angle,
      r = goal_circle_radius - line_thickness
    )
  }

  return(goal_circle_fill_df)
}


#' Lacrosse Goal Arc
#' 
#' The arc around the goal circle (the PLL two-point arc) is made of two lines
#' parallel to the sidelines, \code{goal_arc_parallel_line_to_center} from the
#' center of the goal line, that run from the end line (see
#' \code{goal_arc_extension}) until they are \code{goal_arc_radius} from the
#' center of the goal line, joined by an arc of \code{goal_arc_radius} about the
#' center of the goal line (2024 PLL Rulebook, II. The Field, G. Two-Point
#' Arc). When the parallel lines are \code{goal_arc_radius} from the center,
#' the arc is a semi-circle. Note: the hash marks are generated via
#' [lacrosse_goal_fan_hash_mark()]
#'
#' @param goal_arc_extension The extension from the goal line towards the end
#'   line
#' @param goal_arc_radius The outer radius of the goal arc, measured from the
#'   center of the goal line
#' @param line_thickness The thickness of the goal arc
#' @param goal_arc_parallel_line_to_center The distance from the center of the
#'   goal line to the outer edge of each line parallel to the sidelines,
#'   measured along the goal line extended. Defaults to \code{goal_arc_radius}
#'
#' @return A data frame containing the the bounding coordinates of the goal arc
#'
#' @keywords internal
lacrosse_goal_arc <- function(goal_arc_extension = 0,
                              goal_arc_radius = 0,
                              line_thickness = 0,
                              goal_arc_parallel_line_to_center =
                                goal_arc_radius) {
  # Outer and inner edges of the parallel lines and of the arc
  w_outer <- min(goal_arc_parallel_line_to_center, goal_arc_radius)
  w_inner <- w_outer - line_thickness
  r_inner <- goal_arc_radius - line_thickness

  # Where the parallel lines meet the arc, towards the center of the field
  x_outer <- -sqrt(max((goal_arc_radius^2) - (w_outer^2), 0))
  x_inner <- -sqrt(max((r_inner^2) - (w_inner^2), 0))
  theta_outer <- atan2(w_outer, x_outer) / pi
  theta_inner <- atan2(w_inner, x_inner) / pi

  # Create the goal arc
  goal_arc_df <- rbind(
    data.frame(
      x = c(goal_arc_extension, x_outer),
      y = c(w_outer, w_outer)
    ),
    create_circle(
      center = c(0, 0),
      start = theta_outer,
      end = 2 - theta_outer,
      r = goal_arc_radius
    ),
    data.frame(
      x = c(x_outer, goal_arc_extension, goal_arc_extension, x_inner),
      y = c(-w_outer, -w_outer, -w_inner, -w_inner)
    ),
    create_circle(
      center = c(0, 0),
      start = 2 - theta_inner,
      end = theta_inner,
      r = r_inner
    ),
    data.frame(
      x = c(x_inner, goal_arc_extension, goal_arc_extension),
      y = c(w_inner, w_inner, w_outer)
    )
  )

  return(goal_arc_df)
}

#' Lacrosse Goal Fan
#' 
#' The goal fan is the 8-meter arc present on some fields (e.g. NCAAW). The
#' anchor for this feature should be given as the center of the goal line. It is
#' made of two straight lines that start where the goal line extended crosses the
#' goal circle (see [lacrosse_goal_circle()]) and run towards the center of the
#' field at 45 degrees to the goal line extended, joined by an arc of radius
#' \code{goal_fan_radius} about the center of the goal line (NCAA Women's
#' Lacrosse Rules, Rule 1, Section 6)
#'
#' @param goal_fan_radius The outer radius of the goal fan, measured from the
#'   center of the goal line to the outer edge of the goal fan
#' @param goal_circle_radius The radius of the goal circle
#' @param line_thickness The thickness of the goal fan line
#'
#' @return A data frame containing the bounding coordinates of the goal fan
#'
#' @keywords internal
lacrosse_goal_fan <- function(goal_fan_radius = 0,
                              goal_circle_radius = 0,
                              line_thickness = 0) {
  # Without an arc outside the goal circle there is no goal fan to draw
  if (goal_fan_radius <= goal_circle_radius) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }

  # Direction of the upper 45-degree line, towards the center of the field
  direction <- c(-1, 1) / sqrt(2)

  # Distance along the line from point p (in the direction above) to the circle
  # of the given radius about the center of the goal line
  distance_to_circle <- function(p, radius) {
    b <- sum(p * direction)
    -b + sqrt((b^2) - sum(p^2) + (radius^2))
  }

  # Outer edge of the line: from the goal circle on the goal line extended (B)
  # to the arc (C)
  b_outer <- c(0, goal_circle_radius)
  c_outer <- b_outer + (distance_to_circle(b_outer, goal_fan_radius) * direction)

  # Inner edge of the line: the same line moved towards the goal by the line's
  # thickness, from the goal circle to the inner edge of the arc
  inner_base <- b_outer + (line_thickness * c(-1, -1) / sqrt(2))
  b_inner <- inner_base +
    (distance_to_circle(inner_base, goal_circle_radius) * direction)
  c_inner <- inner_base +
    (distance_to_circle(inner_base, goal_fan_radius - line_thickness) * direction)

  # Angles (in half-turns) at which the lines meet the arc
  theta_outer <- atan2(c_outer[2], c_outer[1]) / pi
  theta_inner <- atan2(c_inner[2], c_inner[1]) / pi

  # Create the goal fan
  goal_fan_df <- rbind(
    data.frame(
      x = c(b_outer[1], c_outer[1]),
      y = c(b_outer[2], c_outer[2])
    ),
    create_circle(
      center = c(0, 0),
      start = theta_outer,
      end = 2 - theta_outer,
      r = goal_fan_radius
    ),
    data.frame(
      x = c(c_outer[1], b_outer[1], b_inner[1], c_inner[1]),
      y = -c(c_outer[2], b_outer[2], b_inner[2], c_inner[2])
    ),
    create_circle(
      center = c(0, 0),
      start = 2 - theta_inner,
      end = theta_inner,
      r = goal_fan_radius - line_thickness
    ),
    data.frame(
      x = c(c_inner[1], b_inner[1], b_outer[1]),
      y = c(c_inner[2], b_inner[2], b_outer[2])
    )
  )

  return(goal_fan_df)
}

#' Lacrosse Goal Fan Hash Mark
#' 
#' The hash marks around the goal fan (see [lacrosse_goal_fan()]) are drawn
#' independently from the goal fan itself. These should just be rectangles with
#' anchor points along the circle
#'
#' @param goal_fan_hash_mark_length The length of each hash mark along the goal
#'   fan
#' @param line_thickness The thickness of each hash mark along the goal fan
#' @param rotational_angle The angle (in degrees) that the hash mark should be
#'   rotated
#'
#' @return A data frame containing the bounding coordinates of a hash mark along
#'   the goal fan
#'
#' @keywords internal
lacrosse_goal_fan_hash_mark <- function(goal_fan_hash_mark_length = 0,
                                        line_thickness = 0,
                                        rotational_angle = 0) {
  # Create the goal fan hash mark
  goal_fan_hash_mark_df <- create_rectangle(
    x_min = -goal_fan_hash_mark_length / 2,
    x_max = goal_fan_hash_mark_length / 2,
    y_min = -line_thickness / 2,
    y_max = line_thickness / 2
  )

  goal_fan_hash_mark_df <- rotate_coords(
    goal_fan_hash_mark_df,
    angle = rotational_angle
  )

  return(goal_fan_hash_mark_df)
}

#' Lacrosse Goal Mouth Hash Mark
#' 
#' The goal mouth may have a hash mark that extends towards midfield from the
#' goal line extended. This is that hash mark, and should be anchored using its
#' outer edge
#'
#' @param goal_mouth_hash_mark_length The length from the goal line extended
#'   towards the center of the field that the hash mark extends
#' @param line_thickness The thickness of the hash mark extending from the goal
#'   mouth
#'
#' @return A data frame containing the bounding coordinates of the goal mouth's
#'   hash mark
#'
#' @keywords internal
lacrosse_goal_mouth_hash_mark <- function(goal_mouth_hash_mark_length = 0,
                                          line_thickness = 0) {
  # Create the goal mouth's hash mark
  goal_mouth_hash_mark_df <- create_rectangle(
    x_min = -goal_mouth_hash_mark_length,
    x_max = 0,
    y_min = -line_thickness,
    y_max = 0
  )

  return(goal_mouth_hash_mark_df)
}

#' Lacrosse Goal Mouth
#' 
#' The goal mouth is similar to the goal fan, except it is plainly a semi-circle
#' located in front of the goal on fields for which it appears. The separation
#' between the ends of it typically correspond to the diameter of the goal
#' circle
#'
#' @param goal_mouth_radius The outer radius of the goal mouth
#' @param line_thickness The thickness of the line marking the goal mouth
#' @param goal_mouth_semi_circle_separation The separation between the ends of
#'   the flat part of the semi-circle that is the goal mouth
#'
#' @return A data frame containing the bounding coordinates of the goal mouth
#'
#' @keywords internal
lacrosse_goal_mouth <- function(goal_mouth_radius = 0,
                                line_thickness = 0,
                                goal_mouth_semi_circle_separation = 0) {
  # The goal mouth is the union of the half annulus and the straight stubs
  # along the goal line extended (x in [-line_thickness, 0], |y| at least the
  # semi-circle separation). The hole inside it is bounded by the inner arc and
  # each stub's edge, and its corners are where the stub's inner edge
  # (x = -line_thickness) or its end (|y| = separation) first meets the inner
  # arc. The inner arc is cut back to start and end at those corners so that
  # the band does not cross itself (#87)
  inner_radius <- goal_mouth_radius - line_thickness
  corner_x <- -min(
    line_thickness,
    sqrt(max(inner_radius^2 - goal_mouth_semi_circle_separation^2, 0))
  )
  corner_y <- sqrt(max(inner_radius^2 - corner_x^2, 0))

  # With no hole left (the line is thicker than the inner radius and the stubs
  # meet), the goal mouth is the solid half disk
  if (inner_radius <= 0 || corner_y == 0) {
    goal_mouth_df <- rbind(
      create_circle(
        center = c(0, 0),
        start = 0.5,
        end = 1.5,
        r = goal_mouth_radius
      ),
      data.frame(
        x = 0,
        y = goal_mouth_radius
      )
    )

    return(goal_mouth_df)
  }

  # The angle, in units of pi, between the bottom of the inner arc and the
  # hole's corner
  inner_arc_offset <- asin(-corner_x / inner_radius) / pi

  # Create the goal mouth
  goal_mouth_df <- rbind(
    data.frame(
      x = c(
        0,
        0
      ),
      y = c(
        goal_mouth_semi_circle_separation,
        goal_mouth_radius
      )
    ),
    create_circle(
      center = c(0, 0),
      start = 0.5,
      end = 1.5,
      r = goal_mouth_radius
    ),
    data.frame(
      x = c(
        0,
        0,
        corner_x
      ),
      y = c(
        -goal_mouth_radius,
        -goal_mouth_semi_circle_separation,
        -goal_mouth_semi_circle_separation
      )
    ),
    create_circle(
      center = c(0, 0),
      start = 1.5 - inner_arc_offset,
      end = 0.5 + inner_arc_offset,
      r = inner_radius
    ),
    data.frame(
      x = c(
        corner_x,
        0
      ),
      y = c(
        goal_mouth_semi_circle_separation,
        goal_mouth_semi_circle_separation
      )
    )
  )

  # Where a corner lies on the inner arc or the goal line extended, it repeats
  # the point next to it, so repeated points are dropped
  is_repeat <- c(FALSE, rowSums(abs(diff(as.matrix(goal_mouth_df)))) < 1e-12)
  goal_mouth_df <- goal_mouth_df[!is_repeat, ]
  rownames(goal_mouth_df) <- NULL

  return(goal_mouth_df)
}

#' Lacrosse Below-Goal Marking
#' 
#' On some fields, there is a below goal marking. These are circles that should
#' mirror each other, with their center points used as anchors
#'
#' @param below_goal_marking_radius The radius of the below goal marking
#'
#' @return A data frame containing the bounding coordinates of the below goal
#'   marking
#'
#' @keywords internal
lacrosse_below_goal_marking <- function(below_goal_marking_radius = 0) {
  # Create the below goal marking
  below_goal_marking_df <- create_circle(
    center = c(0, 0),
    start = 0,
    end = 2,
    r = below_goal_marking_radius
  )

  return(below_goal_marking_df)
}

#' Lacrosse Goal Frame
#' 
#' The goal frame is the posts through which the ball must pass in order to
#' score a goal. It is usually triangular in shape
#'
#' @param goal_frame_opening_interior The interior width of the goal frame's
#'   opening
#' @param goal_post_thickness The thickness of the goal post
#' @param goal_depth The outer depth at which the goal is anchored
#'
#' @return A data frame containing the bounding coordinates of the goal frame
#'
#' @keywords internal
lacrosse_goal_frame <- function(goal_frame_opening_interior = 0,
                                goal_post_thickness = 0,
                                goal_depth = 0) {
  # Create the goal frame
  goal_frame_df <- data.frame(
    x = c(
      0,
      goal_depth - goal_post_thickness,
      0,
      0,
      goal_depth,
      0,
      0
    ),
    y = c(
      goal_frame_opening_interior / 2,
      0,
      -goal_frame_opening_interior / 2,
      (-goal_frame_opening_interior / 2) - goal_post_thickness,
      0,
      (goal_frame_opening_interior / 2) + goal_post_thickness,
      goal_frame_opening_interior / 2
    )
  )

  return(goal_frame_df)
}

#' Lacrosse Goal Net
#' 
#' The goal net is the netting that the ball must hit in order to score a point
#'
#' @param goal_frame_opening_interior The interior width of the goal frame's
#'   opening
#' @param goal_post_thickness The thickness of the goal post
#' @param goal_depth The outer depth at which the goal is anchored
#'
#' @return A data frame containing the bounding coordinates of the goal net
#'
#' @keywords internal
lacrosse_goal_net <- function(goal_frame_opening_interior = 0,
                              goal_post_thickness = 0,
                              goal_depth = 0) {
  # Create the goal net
  goal_net_df <- data.frame(
    x = c(
      0,
      goal_depth - goal_post_thickness,
      0,
      0
    ),
    y = c(
      goal_frame_opening_interior / 2,
      0,
      -goal_frame_opening_interior / 2,
      goal_frame_opening_interior / 2
    )
  )

  return(goal_net_df)
}

#' Lacrosse Referee's Crease (Interior)
#' 
#' The referee's crease is a semi-circle on the "bottom" of the boards (in TV
#' view), centered on the line y = 0 (the center of the center line). This
#' feature corresponds to the section of the field it encloses
#'
#' @param referee_crease_radius The radius of the referee's crease
#' @param line_thickness The thickness with which to draw the referee's crease
#'
#' @return A data frame of the bounding coordinates of the referee's crease's
#'   enclosed area
#'
#' @keywords internal
lacrosse_referee_crease_fill <- function(referee_crease_radius = 0,
                                         line_thickness = 0) {
  referee_crease_fill_df <- create_circle(
    center = c(0, 0),
    start = 0,
    end = 1,
    r = referee_crease_radius
  )

  return(referee_crease_fill_df)
}

#' Lacrosse Center Circle
#' 
#' The center circle is where play begins to start a game. This is located at
#' the center of the field (when present)
#'
#' @param center_circle_radius The outer radius of the center circle
#' @param center_circle_thickness The thickness of the center circle
#'
#' @return A data frame containing the bounding coordinates of the center circle
#'
#' @keywords internal
lacrosse_center_circle <- function(center_circle_radius = 0,
                                   center_circle_thickness = 0) {
  center_circle_df <- rbind(
    create_circle(
      center = c(0, 0),
      start = 0.5,
      end = 1.5,
      r = center_circle_radius
    ),
    data.frame(
      x = c(
        0,
        0
      ),
      y = c(
        -center_circle_radius,
        -center_circle_radius - center_circle_thickness
      )
    ),
    create_circle(
      center = c(0, 0),
      start = 1.5,
      end = 0.5,
      r = center_circle_radius - center_circle_thickness
    )
  )

  return(center_circle_df)
}

#' Lacrosse Player Bench (Outline)
#' 
#' The player benches are the areas outside the confines of the field where
#' players not currently on the field are seated. They are to be on the same
#' side of the field surface and separate, as close to center field as possible
#'
#' This will have the same thickness as the boards, but will be located outside
#' the field surface. Each bench's outline will share the same color as the
#' boards
#'
#' @param bench_area_outline_thickness The thickness of the outline of the
#'   player bench areas
#' @param bench_length The length of the player bench area
#' @param bench_depth The depth of the player bench area
#'
#' @return A data frame containing the bounding coordinates of the player bench
#'   area
#'
#' @keywords internal
lacrosse_player_bench_outline <- function(bench_area_outline_thickness = 0,
                                          bench_length = 0,
                                          bench_depth = 0) {
  bench_outline_df <- data.frame(
    x = c(
      -bench_area_outline_thickness,
      -bench_area_outline_thickness,
      bench_length + bench_area_outline_thickness,
      bench_length + bench_area_outline_thickness,
      bench_length,
      bench_length,
      0,
      0,
      -bench_area_outline_thickness
    ),
    y = c(
      bench_area_outline_thickness,
      (2 * bench_area_outline_thickness) + bench_depth,
      (2 * bench_area_outline_thickness) + bench_depth,
      bench_area_outline_thickness,
      bench_area_outline_thickness,
      bench_area_outline_thickness + bench_depth,
      bench_area_outline_thickness + bench_depth,
      bench_area_outline_thickness,
      bench_area_outline_thickness
    )
  )

  return(bench_outline_df)
}

#' Lacrosse Player Bench (Interior)
#' 
#' The player benches are the areas outside the confines of the field where
#' players not currently on the field are seated. They are to be on the same
#' side of the field surface and separate, as close to center field as possible
#'
#' This will have the same thickness as the boards, but will be located outside
#' the field surface
#'
#' @param bench_area_outline_thickness The thickness of the outline of the
#'   player bench area
#' @param bench_length The length of the player bench area
#' @param bench_depth The depth of the player bench area
#'
#' @return A data frame containing the bounding coordinates of the player bench
#'   area's inner filling
#'
#' @keywords internal
lacrosse_player_bench_area_fill <- function(bench_area_outline_thickness = 0,
                                            bench_length = 0,
                                            bench_depth = 0) {
  bench_fill_df <- create_rectangle(
    x_min = -bench_length / 2,
    x_max = bench_length / 2,
    y_min = bench_area_outline_thickness,
    y_max = bench_area_outline_thickness + bench_depth
  )

  return(bench_fill_df)
}

#' Lacrosse Penalty Box (Outline)
#' 
#' The penalty boxes are the areas outside the confines of the field where
#' players serve time for a penalty incurred. They are to be on the same side of
#' the field surface and separate, as close to center field as possible, for
#' each team. This will also include the off-field officials' box
#'
#' This will have the same thickness as the boards, but will be located outside
#' the field surface. Each penalty box's outline will share the same color as
#' the boards
#'
#' @param penalty_box_outline_thickness The thickness of the outline of the penalty box
#' @param penalty_box_length The interior length of the penalty box
#' @param penalty_box_width The width of the penalty box
#' @param penalty_box_separation The separation between the two penalty boxes
#' @param penalty_box_depth The depth at which the penalty box extends from the
#'   outer edge of the boards
#'
#' @return A data frame containing the bounding coordinates of the penalty box
#'
#' @keywords internal
lacrosse_penalty_box_outline <- function(penalty_box_outline_thickness = 0,
                                         penalty_box_length = 0,
                                         penalty_box_width = 0,
                                         penalty_box_separation = 0,
                                         penalty_box_depth = 0) {
  penalty_box_outline_df <- data.frame(
    x = c(
      0,
      (penalty_box_separation / 2) +
        penalty_box_length + (2 * penalty_box_outline_thickness),
      (penalty_box_separation / 2) +
        penalty_box_length + (2 * penalty_box_outline_thickness),
      (penalty_box_separation / 2) +
        penalty_box_length + penalty_box_outline_thickness,
      (penalty_box_separation / 2) +
        penalty_box_length + penalty_box_outline_thickness,
      (penalty_box_separation / 2) + penalty_box_outline_thickness,
      (penalty_box_separation / 2) + penalty_box_outline_thickness,
      penalty_box_separation / 2,
      penalty_box_separation / 2,
      0,
      0
    ),
    y = c(
      -((2 * penalty_box_outline_thickness) + penalty_box_depth),
      -((2 * penalty_box_outline_thickness) + penalty_box_depth),
      -(penalty_box_outline_thickness),
      -(penalty_box_outline_thickness),
      -(penalty_box_outline_thickness + penalty_box_depth),
      -(penalty_box_outline_thickness + penalty_box_depth),
      -(penalty_box_outline_thickness),
      -(penalty_box_outline_thickness),
      -(penalty_box_outline_thickness + penalty_box_depth),
      -(penalty_box_outline_thickness + penalty_box_depth),
      -((2 * penalty_box_outline_thickness) + penalty_box_depth)
    )
  )

  return(penalty_box_outline_df)
}

#' Lacrosse Penalty Box (Interior)
#' 
#' The penalty boxes are the areas outside the confines of the field where
#' players serve time for a penalty incurred. They are to be on the same side of
#' the field surface and separate, as close to center field as possible, for
#' each team. This will not include the off-field officials' box; see
#' [lacrosse_off_field_officials_box()] for more information
#'
#' This will have the same thickness as the boards, but will be located outside
#' the field surface
#'
#' @param penalty_box_outline_thickness The thickness of the outline of the
#'   penalty box
#' @param penalty_box_length The length of the penalty box
#' @param penalty_box_depth The depth at which the penalty box extends from the
#'   outer edge of the boards
#'
#' @return A data frame containing the bounding coordinates of the penalty box's
#'   inner filling
#'
#' @keywords internal
lacrosse_penalty_box_fill <- function(penalty_box_outline_thickness = 0,
                                      penalty_box_length = 0,
                                      penalty_box_depth = 0) {
  penalty_box_fill_df <- create_rectangle(
    x_min = -penalty_box_length / 2,
    x_max = penalty_box_length / 2,
    y_min = -penalty_box_outline_thickness,
    y_max = -(penalty_box_outline_thickness + penalty_box_depth)
  )

  return(penalty_box_fill_df)
}

#' Lacrosse Off-Field Officials' Box
#' 
#' The off-field officials' box is located between the two penalty boxes,
#' opposite the team bench areas
#'
#' This will have the same thickness as the boards, but will be located outside
#' the field surface
#'
#' @param officials_box_thickness The thickness of the outline of the off-field
#'   officials' box
#' @param officials_box_length The length of the off-field officials' box
#' @param officials_box_depth The depth at which the off-field officials' box
#'   extends from the outer edge of the boards
#'
#' @return A data frame containing the bounding coordinates of the off-field
#'   officials' box's outline
#'
#' @keywords internal
lacrosse_off_field_officials_box <- function(officials_box_thickness = 0,
                                             officials_box_length = 0,
                                             officials_box_depth = 0) {
  off_field_officials_box_df <- create_rectangle(
    x_min = -officials_box_length / 2,
    x_max = officials_box_length / 2,
    y_min = -officials_box_thickness,
    y_max = -(officials_box_thickness + officials_box_depth)
  )

  return(off_field_officials_box_df)
}

#' Lacrosse Faceoff Markers
#' 
#' The face-off markers are where face-offs occur. They may take one of two
#' forms: an "X" shape or a circle
#'
#' @param shape one of the following strings (case-insensitive):
#'
#'  \describe{
#'    \item{\code{"X"}}{
#'      An "X"-like shape. This must be specified with the
#'      \code{feature_thickness} parameter to specify the width of each bar of
#'      the "X", and the \code{side_length} parameter to control the length of
#'      each bar
#'    }
#'    \item{\code{"O"}}{
#'      A circle shape. This must be specified with the \code{feature_radius}
#'      parameter to determine the size of the circle
#'    }
#'  }
#'
#' @param feature_thickness The thickness of a single bar of the "X" shape
#' @param side_length The length of a single bar of the "X" shape
#' @param feature_radius The radius of a circular face-off spot
#'
#' @return A data frame containing the bounding coordinates of a face-off spot
#'
#' @keywords internal
lacrosse_face_off_marker <- function(shape = "O",
                                     feature_thickness = 0,
                                     side_length = 0,
                                     feature_radius = 0) {
  # Create the face-off marker
  face_off_marker <- switch(
    tolower(shape),
    "o" = create_circle(
      center = c(0, 0),
      start = 0,
      end = 2,
      r = feature_radius
    ),
    "x" = create_x_shape(
      bar_length = side_length,
      bar_width = feature_thickness,
      rotation = 45
    ),
    "square" = create_square(
      side_length = side_length
    ),
    data.frame(x = c(), y = c())
  )

  return(face_off_marker)
}

#' Lacrosse Change Area (Outline)
#' 
#' The change area is the box-shaped area in front of the team benches where a
#' substitutions occur. This feature describes its outline; its interior fill
#' is controlled by [lacrosse_change_area_fill()]
#'
#' @param change_area_length The length of the change area's interior
#' @param change_area_width The distance off the boards that the change area
#'  extends into the playing surface
#' @param feature_thickness The thickness of the outline of the box
#'
#' @return A data frame containing the bounding coordinates of the change area's
#'  outline
#'
#' @keywords internal
lacrosse_change_area_outline <- function(change_area_length = 0,
                                         change_area_width = 0,
                                         feature_thickness = 0) {
  change_area_outline_df <- data.frame(
    x = c(
      change_area_length,
      change_area_length,
      0,
      0,
      change_area_length + feature_thickness,
      change_area_length + feature_thickness,
      change_area_length
    ),
    y = c(
      0,
      -change_area_width,
      -change_area_width,
      -(change_area_width + feature_thickness),
      -(change_area_width + feature_thickness),
      0,
      0
    )
  )

  return(change_area_outline_df)
}

#' Lacrosse Change Area (Interior)
#' 
#' The change area is the box-shaped area in front of the team benches where a
#' substitutions occur. This feature describes its interior fill; its outline
#' is controlled by [lacrosse_change_area_outline()]
#'
#' @param change_area_length The length of the change area's interior
#' @param change_area_width The distance off the boards that the change area
#'  extends into the playing surface
#'
#' @return A data frame containing the bounding coordinates of the change area's
#'  interior fill
#'
#' @keywords internal
lacrosse_change_area_fill <- function(change_area_length = 0,
                                      change_area_width = 0) {
  change_area_fill_df <- create_rectangle(
    x_min = 0,
    x_max = change_area_length,
    y_min = -change_area_width,
    y_max = 0
  )

  return(change_area_fill_df)
}
