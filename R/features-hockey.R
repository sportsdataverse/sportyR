# Surface Base Features --------------------------------------------------------

#' The defensive zone is the left "third" of the rink in TV view. This is the
#' area that a team defends when attacking from left to right
#'
#' @param rink_length The length of the rink
#' @param rink_width The width of the rink
#' @param feature_radius The radius of the corners of the boards
#' @param nzone_length The length of the neutral zone
#'
#' @return A data frame of the bounding coordinates of the defensive zone
#'
#' @keywords internal
hockey_defensive_zone <- function(rink_length = 0,
                                  rink_width = 0,
                                  feature_radius = 0,
                                  nzone_length = 0) {
  # Specify the dimensions of the rink
  half_length <- rink_length / 2
  half_width <- rink_width / 2

  # Find where the point to use as the center of the circle with the given
  # radius for the boards' corners' arc
  center_x <- half_length - feature_radius
  center_y <- half_width - feature_radius

  # Calculate the corner arc's inner radius
  arc_inner_upper <- create_circle(
    center = c(-center_x, center_y),
    start = 0.5,
    end = 1,
    r = feature_radius
  )

  arc_inner_lower <- create_circle(
    center = c(-center_x, -center_y),
    start = 1,
    end = 1.5,
    r = feature_radius
  )

  dzone_df <- rbind(
    # Start at the upper right corner of the zone line that is closest to center
    # ice
    data.frame(
      x = c(-nzone_length / 2),
      y = c(half_width)
    ),

    # Then draw the upper left arc of the boards
    arc_inner_upper,

    # Then its guaranteed point at half the length of the rink
    data.frame(
      x = c(-half_length),
      y = c(0)
    ),

    # Then the lower left arc
    arc_inner_lower,

    # Then go to the bottom of the rink in TV view with the boards' inner
    # boundary before closing the path by returning to the starting point
    data.frame(
      x = c(-nzone_length / 2, -nzone_length / 2),
      y = c(-half_width, half_width)
    )
  )

  return(dzone_df)
}

#' The neutral zone is the middle "third" of the rink. This is the area between
#' the two zone (blue) lines. The center of the neutral zone should lie along
#' the line x = 0
#'
#' @param rink_width The width of the rink
#' @param feature_thickness The length of the neutral zone
#'
#' @return A data frame containing the bounding coordinates of the neutral zone
#'
#' @keywords internal
hockey_neutral_zone <- function(rink_width = 0, feature_thickness = 0) {
  # Generate the points of the neutral zone. This is a rectangular region with
  # known dimensions (from the passed parameters), so no reflection is required
  nzone_df <- create_rectangle(
    x_min = -feature_thickness / 2,
    x_max = feature_thickness / 2,
    y_min = -rink_width / 2,
    y_max = rink_width / 2
  )

  return(nzone_df)
}

#' The offensive zone is the right "third" of the rink in TV view. This is the
#' area that a team attacks to try to score a goal when attacking from left to
#' right
#'
#' @param rink_length The length of the rink
#' @param rink_width The width of the rink
#' @param feature_radius The radius of the corners of the boards
#' @param nzone_length The length of the neutral zone
#'
#' @return A data frame of the bounding coordinates of the offensive zone
#'
#' @keywords internal
hockey_offensive_zone <- function(rink_length = 0,
                                  rink_width = 0,
                                  feature_radius = 0,
                                  nzone_length = 0) {
  # Specify the dimensions of the rink
  half_length <- rink_length / 2
  half_width <- rink_width / 2

  # Find where the point to use as the center of the circle with the given
  # radius for the boards' corners' arc
  center_x <- half_length - feature_radius
  center_y <- half_width - feature_radius

  # Create the points along the corner arc's inner radii
  arc_inner_upper <- create_circle(
    center = c(center_x, center_y),
    start = 0.5,
    end = 0,
    r = feature_radius
  )

  arc_inner_lower <- create_circle(
    center = c(center_x, -center_y),
    start = 0,
    end = -0.5,
    r = feature_radius
  )

  ozone_df <- rbind(
    # Start at the upper left corner of the zone line that is closest to center
    # ice
    data.frame(
      x = c(nzone_length / 2),
      y = c(half_width)
    ),

    # Then draw the upper right corner of the boards
    arc_inner_upper,

    # Then its guaranteed point at half the length of the rink
    data.frame(
      x = c(half_length),
      y = c(0)
    ),

    # Then the lower right corner
    arc_inner_lower,

    # Then go to the bottom of the rink in TV view with the boards' inner
    # boundary before closing the path by returning to the starting point
    data.frame(
      x = c(nzone_length / 2, nzone_length / 2),
      y = c(-half_width, half_width)
    )
  )

  return(ozone_df)
}





# Surface Boundaries -----------------------------------------------------------

#' The boards are the wall around the outside of the rink that constrain the
#' playing surface. The boards are typically ovular in shape
#'
#' @param rink_length The length of the rink
#' @param rink_width The width of the rink
#' @param feature_radius The radius of the corners of the boards
#' @param feature_thickness The thickness with which to draw the boards
#'
#' @return A data frame of the bounding coordinates of the boards
#'
#' @keywords internal
hockey_boards <- function(rink_length = 0,
                          rink_width = 0,
                          feature_radius = 0,
                          feature_thickness = 0) {
  # Specify the half-dimensions of the rink
  half_length <- rink_length / 2
  half_width <- rink_width / 2

  # Find the point to use as the center of the circle with the given radius for
  # the boards' corners' arc
  center_x <- half_length - feature_radius
  center_y <- half_width - feature_radius

  # Create the points along the corner arc's inner radii
  arc_inner_upper <- create_circle(
    center = c(center_x, center_y),
    start = 0.5,
    end = 0,
    r = feature_radius
  )

  arc_inner_lower <- create_circle(
    center = c(center_x, -center_y),
    start = 0,
    end = -0.5,
    r = feature_radius
  )

  # Calculate the corner arc's outer radius
  arc_outer_upper <- create_circle(
    center = c(center_x, center_y),
    start = 0,
    end = 0.5,
    r = feature_radius + feature_thickness
  )

  arc_outer_lower <- create_circle(
    center = c(center_x, -center_y),
    start = -0.5,
    end = 0,
    r = feature_radius + feature_thickness
  )

  # Combine the boards' inner and outer arcs with its guaranteed coordinates
  boards_df <- rbind(
    # Start at the top of the rink in TV view with the boards' inner boundary
    data.frame(
      x = c(0),
      y = c(half_width)
    ),

    # Then add in its upper inner arc
    arc_inner_upper,

    # Then its guaranteed point at half the length of the rink
    data.frame(
      x = c(half_length),
      y = c(0)
    ),

    # Then its lower inner arc
    arc_inner_lower,

    # Then go to the bottom of the rink in TV view with the boards' inner
    # boundary before flipping to the outer boundary
    data.frame(
      x = c(0, 0),
      y = c(-half_width, -half_width - feature_thickness)
    ),

    # Back to the lower arc on the outer boundary
    arc_outer_lower,

    # Then back to the middle
    data.frame(
      x = c(half_length + feature_thickness),
      y = c(0)
    ),

    # Then back to the upper arc
    arc_outer_upper,

    # Finally back to the top and original starting point
    data.frame(
      x = c(0, 0),
      y = c(half_width + feature_thickness, half_width)
    )
  )

  return(boards_df)
}





# Surface Lines ----------------------------------------------------------------

#' The center line is the line that divides the ice surface in half. Its center
#' should lie directly in the center of the ice surface. Its line thickness
#' should be given by 'major_line_thickness' as this is a major line on the ice
#' surface
#'
#' @param feature_thickness The thickness of the center line
#' @param rink_width The width of the rink
#'
#' @return A data frame of the bounding coordinates of the center line
#'
#' @keywords internal
hockey_center_line <- function(feature_thickness = 0,
                               rink_width = 0,
                               center_faceoff_spot_gap = 0) {
  center_line_df <- create_rectangle(
    x_min = -feature_thickness / 2,
    x_max = feature_thickness / 2,
    y_min = center_faceoff_spot_gap / 2,
    y_max = rink_width / 2
  )

  return(center_line_df)
}

#' The referee's crease is a semi-circle on the "bottom" of the boards (in TV
#' view), centered on the line y = 0 (the center of the center line)
#'
#' @param feature_radius The radius of the referee's crease
#' @param feature_thickness The thickness with which to draw the referee's
#'   crease
#'
#' @return A data frame of the bounding coordinates of the referee's crease
#'
#' @keywords internal
hockey_referee_crease <- function(feature_radius = 0, feature_thickness = 0) {
  referee_crease_df <- rbind(
    data.frame(
      x = c(feature_radius),
      y = c(0)
    ),
    create_circle(
      center = c(0, 0),
      start = 0,
      end = 1,
      r = feature_radius
    ),
    data.frame(
      x = c(
        -feature_radius,
        -feature_radius + feature_thickness
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
      r = feature_radius - feature_thickness
    ),
    data.frame(
      x = c(feature_radius),
      y = c(0)
    )
  )

  return(referee_crease_df)
}

#' The zone lines are the lines that separate the neutral zone from the
#' offensive and defensive zones. Its line thickness should be given by
#' 'major_line_thickness' as this is a major line on the ice surface
#'
#' @param rink_width The width of the rink
#' @param feature_thickness The thickness of the zone line
#'
#' @return A data frame containing the bounding coordinates of the zone line
#'
#' @keywords internal
hockey_zone_line <- function(rink_width = 0, feature_thickness = 0) {
  zone_line_df <- create_rectangle(
    x_min = 0,
    x_max = feature_thickness,
    y_min = -rink_width / 2,
    y_max = rink_width / 2
  )

  return(zone_line_df)
}

#' The goal lines are the lines over which a puck must cross (within the goal
#' frame) in order to be considered a goal. Its line thickness should be given
#' by 'minor_line_thickness' as this is a minor line on the ice surface.
#'
#' This draws the right-side goal line (in TV view), starting with its left
#' edge. This also accounts for a perfectly rectangular goal line if a user
#' supplies a value that necessitates one. The line is rectangular in shape with
#' rounded ends, and usually red in color
#'
#' @param rink_length The length of the rink
#' @param rink_width The width of the rink
#' @param feature_radius The radius of the corner of the rink
#' @param feature_thickness The thickness of the goal line
#' @param x_anchor the \code{x} coordinate used as the anchor point of the goal
#'   line
#'
#' @return A data frame containing the bounding coordinates of the goal line
#'
#' @keywords internal
hockey_goal_line <- function(rink_length = 0,
                             rink_width = 0,
                             feature_radius = 0,
                             feature_thickness = 0,
                             x_anchor = 0) {
  # Specify the half-dimension of the rink
  half_length <- rink_length / 2
  half_width <- rink_width / 2

  # Find the point to use as the center of the circle with the given radius for
  # the boards' corners' arc
  corner_arc_center_x <- half_length - feature_radius
  corner_arc_center_y <- half_width - feature_radius

  # First, check to see if the goal line will intersect the corner of the rink.
  # Usually, it will, but in case a user supplies a value where this is not the
  # case, this check will accommodate. The absolute value is used here to always
  # force the calculation to be done for the right side of the ice (in TV view),
  # which will be adjusted as necessary in the feature's translate_feature()
  # method
  max_x <- abs(x_anchor) + (feature_thickness / 2)

  # If the maximum value of x is going to be less than the x coordinate of the
  # center of the corner's arc, then the feature should be a rectangle
  if (max_x <= corner_arc_center_x) {
    goal_line_df <- create_rectangle(
      x_min = -feature_thickness / 2,
      x_max = feature_thickness / 2,
      y_min = -half_width,
      y_max = half_width
    )

    return(goal_line_df)
  } else {
    # The starting x position should be the left-hand edge of the right-side
    # goal line
    base_x <- abs(x_anchor) - corner_arc_center_x
    start_x <- base_x - (feature_thickness / 2)
    end_x <- base_x + (feature_thickness / 2)

    # Finally, compute the starting and ending angles by taking the inverse sine
    # of the starting and ending x positions, then dividing by the corner's
    # radius. Divide by pi to ensure that the angles are correctly passed to the
    # create_circle() method
    theta_start <- asin(start_x / feature_radius) / pi
    theta_end <- asin(end_x / feature_radius) / pi

    # Now create the feature's data frame
    goal_line_df <- rbind(
      create_circle(
        center = c(corner_arc_center_x, corner_arc_center_y),
        start = 0.5 - theta_start,
        end = 0.5 - theta_end,
        r = feature_radius
      ),
      create_circle(
        center = c(corner_arc_center_x, -corner_arc_center_y),
        start = -0.5 + theta_end,
        end = -0.5 + theta_start,
        r = feature_radius
      )
    )

    # To properly position the goal line, the x coordinate needs to be brought
    # back to x = 0 so that it can be re-anchored when generated. See note above
    # for an explanation of why the absolute value is used here
    goal_line_df["x"] <- goal_line_df["x"] - abs(x_anchor)

    return(goal_line_df)
  }
}

#' The goaltender's restricted area marks where a goaltender is legally allowed
#' to handle the puck behind the net. This is often referred to as "the
#' trapezoid" as it is trapezoidal in shape. Its line thickness should be given
#' by 'minor_line_thickness' as this is a minor line on the ice surface
#'
#' NOTE: This is not a requirement in all leagues, and may be omitted via the
#' "has_trapezoid" key in the \code{rink_params} passed to \code{geom_{league}}
#'
#' This draws the goaltender's restricted area on the right side (in TV view) of
#' the ice surface. The figure is composed of lines that outline a trapezoid in
#' shape, and is usually red in color
#'
#' @param rink_length The length of the rink
#' @param feature_thickness The thickness of the lines used to draw the
#'   goaltender's restricted area
#' @param short_base_width The width of the base nearest the center line
#' @param long_base_width The width of the base nearest the boards behind the
#'   goal
#' @param x_anchor the \code{x} coordinate used as the anchor point of the goal
#'   line
#'
#' @return A data frame containing the bounding coordinates of the goaltender's
#'   restricted area
#'
#' @keywords internal
hockey_goaltenders_restricted_area <- function(rink_length = 0,
                                               feature_thickness = 0,
                                               short_base_width = 0,
                                               long_base_width = 0,
                                               x_anchor = 0) {
  # Start by defining the half-widths of both the short and long bases of the
  # trapezoid
  half_short_base_width <- short_base_width / 2
  half_long_base_width <- long_base_width / 2

  # Now trace out the trapezoid. NOTE: The absolute value is used here to always
  # force the calculation to be done for the right side of the ice (in TV view),
  # which will be adjusted as necessary in the feature's translate_feature()
  # method
  trapezoid_df <- data.frame(
    x = c(
      abs(x_anchor),
      rink_length / 2,
      rink_length / 2,
      abs(x_anchor) - (feature_thickness / 2),
      abs(x_anchor) - (feature_thickness / 2),
      rink_length / 2,
      rink_length / 2,
      abs(x_anchor),
      abs(x_anchor)
    ),
    y = c(
      half_short_base_width,
      half_long_base_width,
      half_long_base_width - feature_thickness,
      half_short_base_width - feature_thickness,
      -half_short_base_width + feature_thickness,
      -half_long_base_width + feature_thickness,
      -half_long_base_width,
      -half_short_base_width,
      half_short_base_width
    )
  )

  # See note above for an explanation of why the absolute value is used here
  trapezoid_df["x"] <- trapezoid_df["x"] - abs(x_anchor)

  return(trapezoid_df)
}

#' The offensive/defensive zone faceoff lines are the L-shaped lines where
#' players on each team line up when taking a faceoff in either the offensive or
#' defensive zones. There are four of these faceoff lines around each
#' offensive/defensive faceoff spot
#'
#' These lines are L-shaped, but can be thought of as two rectangles with
#' thickness given by 'minor_line_thickness', and are usually red in color
#'
#' @param feature_thickness The thickness of the faceoff lines
#' @param faceoff_line_dist_x The distance from the center of the faceoff spot
#'   to the interior edge of the faceoff lines in the x direction
#' @param faceoff_line_dist_y The distance from the center of the faceoff spot
#'   to the interior edge of the faceoff lines in the y direction
#' @param faceoff_line_length The length of the faceoff lines from the edge
#'   nearest the goal line to the edge nearest the end boards
#' @param faceoff_line_width The width of the faceoff lines from the edge
#'   nearest the center of the spot to the edge nearest the side boards
#'
#' @return A data frame containing the bounding coordinates of the
#'   offensive/defensive zone faceoff lines
#'
#' @keywords internal
hockey_odzone_faceoff_lines <- function(feature_thickness = 0,
                                        faceoff_line_dist_x = 0,
                                        faceoff_line_dist_y = 0,
                                        faceoff_line_length = 0,
                                        faceoff_line_width = 0) {
  faceoff_line_df <- data.frame(
    x = c(
      faceoff_line_dist_x,
      faceoff_line_dist_x + faceoff_line_length,
      faceoff_line_dist_x + faceoff_line_length,
      faceoff_line_dist_x + feature_thickness,
      faceoff_line_dist_x + feature_thickness,
      faceoff_line_dist_x,
      faceoff_line_dist_x
    ),
    y = c(
      faceoff_line_dist_y,
      faceoff_line_dist_y,
      faceoff_line_dist_y + feature_thickness,
      faceoff_line_dist_y + feature_thickness,
      faceoff_line_dist_y + faceoff_line_width,
      faceoff_line_dist_y + faceoff_line_width,
      faceoff_line_dist_y
    )
  )

  faceoff_line_df <- rbind(
    faceoff_line_df,
    reflect(faceoff_line_df, over_x = TRUE, over_y = FALSE)
  )


  return(faceoff_line_df)
}





# Surface Features -------------------------------------------------------------

#' The center faceoff spot is the spot at which the game begins. Its center
#' should lie directly in the center of the ice surface. Its radius is passed as
#' a key in \code{rink_params}
#'
#' @param feature_radius The radius of the center faceoff spot
#'
#' @return A data frame containing the bounding coordinates of the center
#'   faceoff spot
#'
#' @keywords internal
hockey_center_faceoff_spot <- function(feature_radius = 0) {
  center_faceoff_spot_df <- create_circle(
    center = c(0, 0),
    start = 0,
    end = 2,
    r = feature_radius
  )

  return(center_faceoff_spot_df)
}

#' The goal crease is the area where a goaltender plays their position. It is
#' comprised of two components: the outline of the crease, and the filling in
#' its boundary (see [hockey_goal_crease_fill()]). The goal crease may have two
#' notches (one on each side of the line y = 0)
#'
#' The outline of the goal crease should have thickness given by
#' 'minor_line_thickness', as this is a minor line on the ice surface, and the
#' outline is usually red in color
#'
#' @param feature_radius The radius of the goal crease
#' @param feature_thickness The thickness of the line marking the outline of the
#'   goal crease
#' @param crease_style The style of the goal crease
#' @param crease_length The length of the goal crease
#' @param crease_width The width of the goal crease
#' @param notch_dist_x The distance from the back edge of the goal line to the
#'   further edge of the crease notch
#' @param notch_width The width of the notch in the goal crease
#'
#' @return A data frame containing the bounding coordinates of the goal crease's
#'   outline
#'
#' @keywords internal
hockey_goal_crease_outline <- function(feature_radius = 0,
                                       feature_thickness = 0,
                                       crease_style = "",
                                       crease_length = 0,
                                       crease_width = 0,
                                       notch_dist_x = 0,
                                       notch_width = 0) {
  # Convert the crease style to be lower case
  crease_style <- tolower(crease_style)

  # Start by getting the half-width of the crease
  half_crease_width <- crease_width / 2

  # Calculate the starting angle theta of the goal crease's rounded front by
  # taking the inverse cosine of its half-width and dividing it by the radius of
  # the arc
  if (feature_radius == 0 || abs(half_crease_width / feature_radius) > 1) {
    theta <- 0
  } else {
    theta <- acos(half_crease_width / feature_radius) / pi
  }

  goal_crease_outline_df <- switch(
    crease_style,

    # nhl98 crease style: cut-off semi-circle (utilized in most North American
    # leagues, e.g. NHL, AHL)
    "nhl98" = rbind(
      data.frame(
        x = c(0, -crease_length),
        y = c(half_crease_width, half_crease_width)
      ),
      create_circle(
        center = c(0, 0),
        start = 0.5 + theta,
        end = 1.5 - theta,
        r = feature_radius
      ),
      data.frame(
        x = c(
          -crease_length,
          0,
          0,
          -notch_dist_x,
          -notch_dist_x,
          -(notch_dist_x + feature_thickness),
          -(notch_dist_x + feature_thickness),
          -crease_length
        ),
        y = c(
          -half_crease_width,
          -half_crease_width,
          -half_crease_width + feature_thickness,
          -half_crease_width + feature_thickness,
          (
            -half_crease_width +
              feature_thickness +
              notch_width
          ),
          (
            -half_crease_width +
              feature_thickness +
              notch_width
          ),
          -half_crease_width + feature_thickness,
          -half_crease_width + feature_thickness
        )
      ),
      create_circle(
        center = c(0, 0),
        start = 1.5 - theta,
        end = 0.5 + theta,
        r = feature_radius - feature_thickness
      ),
      data.frame(
        x = c(
          -crease_length,
          -(notch_dist_x + feature_thickness),
          -(notch_dist_x + feature_thickness),
          -notch_dist_x,
          -notch_dist_x,
          0,
          0
        ),
        y = c(
          half_crease_width - feature_thickness,
          half_crease_width - feature_thickness,
          (
            half_crease_width -
              feature_thickness -
              notch_width
          ),
          (
            half_crease_width -
              feature_thickness -
              notch_width
          ),
          half_crease_width - feature_thickness,
          half_crease_width - feature_thickness,
          half_crease_width
        )
      )
    ),

    # ushl1 crease style: full semi-circle with NHL-style crease in the
    # interior; only NHL-style crease is painted light blue
    "ushl1" = rbind(
      create_circle(
        center = c(0, 0),
        start = 0.5,
        end = 1.5,
        r = feature_radius
      ),
      create_circle(
        center = c(0, 0),
        start = 1.5,
        end = 1.5 - theta,
        r = feature_radius - feature_thickness
      ),
      data.frame(
        x = c(
          -notch_dist_x - feature_thickness,
          0,
          0,
          -notch_dist_x,
          -notch_dist_x,
          -notch_dist_x - feature_thickness,
          -notch_dist_x - feature_thickness
        ),
        y = c(
          -half_crease_width,
          -half_crease_width,
          -half_crease_width + feature_thickness,
          -half_crease_width + feature_thickness,
          -half_crease_width + feature_thickness + notch_width,
          -half_crease_width + feature_thickness + notch_width,
          -half_crease_width
        )
      ),
      create_circle(
        center = c(0, 0),
        start = 1.5 - theta,
        end = 0.5 + theta,
        r = feature_radius - feature_thickness
      ),
      data.frame(
        x = c(
          -notch_dist_x - feature_thickness,
          -notch_dist_x - feature_thickness,
          -notch_dist_x,
          -notch_dist_x,
          0,
          0,
          -notch_dist_x - feature_thickness
        ),
        y = c(
          half_crease_width,
          half_crease_width - feature_thickness - notch_width,
          half_crease_width - feature_thickness - notch_width,
          half_crease_width - feature_thickness,
          half_crease_width - feature_thickness,
          half_crease_width,
          half_crease_width
        )
      ),
      create_circle(
        center = c(0, 0),
        start = 0.5 + theta,
        end = 0.5,
        r = feature_radius - feature_thickness
      ),
      data.frame(
        x = c(
          0,
          0
        ),
        y = c(
          feature_radius - feature_thickness,
          feature_radius
        )
      )
    ),

    # nhl92 crease style: full semi-circle outline with two L-shaped marks
    # adjoining the semi-circle, but not extending back towards the goal line
    "nhl92" = rbind(
      create_circle(
        center = c(0, 0),
        start = 0.5,
        end = 1.5,
        r = feature_radius
      ),
      create_circle(
        center = c(0, 0),
        start = 1.5,
        end = 1.5 - theta,
        r = feature_radius - feature_thickness
      ),
      data.frame(
        x = c(
          -notch_dist_x,
          -notch_dist_x + notch_width,
          -notch_dist_x + notch_width,
          -notch_dist_x,
          -notch_dist_x,
          -notch_dist_x - feature_thickness,
          -notch_dist_x - feature_thickness,
          -notch_dist_x
        ),
        y = c(
          -half_crease_width,
          -half_crease_width,
          -half_crease_width + feature_thickness,
          -half_crease_width + feature_thickness,
          -half_crease_width + feature_thickness + notch_width,
          -half_crease_width + feature_thickness + notch_width,
          -half_crease_width,
          -half_crease_width
        )
      ),
      create_circle(
        center = c(0, 0),
        start = 1.5 - theta,
        end = 0.5 + theta,
        r = feature_radius - feature_thickness
      ),
      data.frame(
        x = c(
          -notch_dist_x,
          -notch_dist_x + notch_width,
          -notch_dist_x + notch_width,
          -notch_dist_x,
          -notch_dist_x,
          -notch_dist_x - feature_thickness,
          -notch_dist_x - feature_thickness,
          -notch_dist_x
        ),
        y = c(
          half_crease_width,
          half_crease_width,
          half_crease_width - feature_thickness,
          half_crease_width - feature_thickness,
          half_crease_width - feature_thickness - notch_width,
          half_crease_width - feature_thickness - notch_width,
          half_crease_width,
          half_crease_width
        )
      ),
      create_circle(
        center = c(0, 0),
        start = 0.5 + theta,
        end = 0.5,
        r = feature_radius - feature_thickness
      ),
      data.frame(
        x = c(0, 0),
        y = c(feature_radius - feature_thickness, feature_radius)
      )
    ),

    # Default case
    data.frame(
      x = c(0),
      y = c(0)
    )
  )

  return(goal_crease_outline_df)
}

#' The goal crease is the area where a goaltender plays their position. It is
#' comprised of two components: the outline of the crease (see
#' [hockey_goal_crease_outline()]), and the filling in its boundary. The goal
#' crease may have two notches (one on each side of the line y = 0)
#'
#' The filling of the goal crease should have thickness given by
#' 'minor_line_thickness', as this refers to the crease's outline, which is a
#' minor line on the ice surface. The goal crease's filling is usually light in
#' color
#'
#' @param feature_radius The radius of the goal crease
#' @param feature_thickness The thickness of the line marking the outline of the
#'   goal crease
#' @param crease_style The style of the goal crease
#' @param crease_length The length of the goal crease
#' @param crease_width The width of the goal crease
#' @param notch_dist_x The distance from the back edge of the goal line to the
#'   further edge of the crease notch
#' @param notch_width The width of the notch in the goal crease
#'
#' @return A data frame containing the bounding coordinates of the goal crease's
#'   inner filling
#'
#' @keywords internal
hockey_goal_crease_fill <- function(feature_radius = 0,
                                    feature_thickness = 0,
                                    crease_style = "",
                                    crease_length = 0,
                                    crease_width = 0,
                                    notch_dist_x = 0,
                                    notch_width = 0) {
  # Convert the crease style to be lower case
  crease_style <- tolower(crease_style)

  # Start by getting the half-width of the crease
  half_crease_width <- crease_width / 2

  # Calculate the starting angle theta of the goal crease's rounded front by
  # taking the inverse cosine of its half-width and dividing it by the radius of
  # the arc
  if (feature_radius == 0 || abs(half_crease_width / feature_radius) > 1) {
    theta <- 0
  } else {
    theta <- acos(half_crease_width / feature_radius) / pi
  }

  goal_crease_fill_df <- switch(
    crease_style,
    "nhl98" = rbind(
      data.frame(
        x = c(
          0,
          -notch_dist_x,
          -notch_dist_x,
          -(notch_dist_x + feature_thickness),
          -(notch_dist_x + feature_thickness)
        ),
        y = c(
          half_crease_width - feature_thickness,
          half_crease_width - feature_thickness,
          half_crease_width - feature_thickness - notch_width,
          half_crease_width - feature_thickness - notch_width,
          half_crease_width - feature_thickness
        )
      ),
      create_circle(
        center = c(0, 0),
        start = 0.5 + theta,
        end = 1.5 - theta,
        r = feature_radius - feature_thickness
      ),
      data.frame(
        x = c(
          -(notch_dist_x + feature_thickness),
          -(notch_dist_x + feature_thickness),
          -notch_dist_x,
          -notch_dist_x,
          0,
          0
        ),
        y = c(
          -half_crease_width + feature_thickness,
          -half_crease_width + feature_thickness + notch_width,
          -half_crease_width + feature_thickness + notch_width,
          -half_crease_width + feature_thickness,
          -half_crease_width + feature_thickness,
          half_crease_width - feature_thickness
        )
      )
    ),
    "ushl1" = rbind(
      data.frame(
        x = c(
          0,
          -crease_length
        ),
        y = c(
          half_crease_width,
          half_crease_width
        )
      ),
      create_circle(
        center = c(0, 0),
        start = 0.5 + theta,
        end = 1.5 - theta,
        r = feature_radius - feature_thickness
      ),
      data.frame(
        x = c(
          -crease_length,
          0
        ),
        y = c(
          -half_crease_width,
          -half_crease_width
        )
      )
    ),
    "nhl92" = create_circle(
      center = c(0, 0),
      start = 0.5,
      end = 1.5,
      r = feature_radius - feature_thickness
    ),

    # Default case
    data.frame(
      x = c(0),
      y = c(0)
    )
  )

  return(goal_crease_fill_df)
}

#' The center faceoff circle is where the each period of the game begins. It
#' differs from the non-centered faceoff circles in that there are no adjoining
#' hash marks on this circle. It is also a different color than the non-centered
#' faceoff circles. Its line thickness should be given by 'minor_line_thickness'
#' as this is a minor line on the ice surface
#'
#' This draws the line defining the faceoff circle at center ice. The line is
#' circular in shape, and usually dark blue in color
#'
#' @param feature_radius The radius of the center faceoff circle
#' @param feature_thickness The thickness of the line of the center faceoff
#'   circle
#'
#' @return A data frame containing the bounding coordinates of the center
#'   faceoff circle
#'
#' @keywords internal
hockey_center_faceoff_circle <- function(feature_radius = 0,
                                         feature_thickness = 0) {
  center_faceoff_circle_df <- rbind(
    create_circle(
      center = c(0, 0),
      start = 0.5,
      end = 1.5,
      r = feature_radius
    ),
    data.frame(
      x = c(0, 0),
      y = c(-feature_radius, -feature_radius - feature_thickness)
    ),
    create_circle(
      center = c(0, 0),
      start = 1.5,
      end = 0.5,
      r = feature_radius - feature_thickness
    )
  )

  return(center_faceoff_circle_df)
}

#' The non-centered faceoff circles are located in the offensive and defensive
#' zones of the ice, with one on each side of the x-axis when viewing the rink
#' in TV view. These circles differ from the center faceoff circle because they
#' have hash marks that extend towards the boards on each side of the circle
#'
#' The non-centered faceoff circles are where faceoffs are taken after an icing
#' call or to start a powerplay. They differ from the center ice faceoff circle
#' because there are adjoining hash marks on these circles. It is also a
#' different color than the center ice faceoff circle, and the spot in the
#' center of it varies in size and form. Its line thickness should be given by
#' 'minor_line_thickness' as this is a minor line on the ice surface
#'
#' @param feature_radius The radius of the faceoff circle
#' @param feature_thickness The thickness of the line of the non-centered
#'   faceoff circle
#' @param hashmark_width The width of the hashmarks on the exterior of the
#'   non-centered faceoff circle
#' @param hashmark_ext_spacing The external spacing between the hashmarks' outer
#'   edges
#'
#' @return A data frame containing the bounding coordinates of the non-centered
#'   faceoff circle
#'
#' @keywords internal
hockey_odzone_faceoff_circle <- function(feature_radius = 0,
                                         feature_thickness = 0,
                                         hashmark_width = 0,
                                         hashmark_ext_spacing = 0) {
  # To create a faceoff circle, start by finding the angle needed to draw the
  # outer ring of the faceoff circle. This can be computed using some simple
  # trigonometry. The NHL is used to illustrate the trigonometry, however the
  # code is abstracted to allow for variable parameters

  # NHL hash marks are 5' 11" (71") apart on the exterior, with one hash mark on
  # each side of the line that vertically bisects the circle through its center.
  # This means that 35.5" of this distance lies on either side of this line, and
  # thus the arcsine of this over the radius of the circle will give the correct
  # starting angle (after adding pi/2)
  ext_spacing <- hashmark_ext_spacing / 2
  int_spacing <- ext_spacing - feature_thickness

  if (feature_radius == 0 || abs(ext_spacing / feature_radius) > 1) {
    theta1 <- 0
    theta2 <- 0
  } else {
    theta1 <- asin(ext_spacing / feature_radius) / pi
    theta2 <- asin(int_spacing / feature_radius) / pi
  }

  odzone_faceoff_circle_df <- rbind(
    data.frame(
      x = c(0),
      y = c(feature_radius)
    ),
    create_circle(
      center = c(0, 0),
      start = 0.5,
      end = 0.5 + theta2,
      r = feature_radius
    ),
    data.frame(
      x = c(-int_spacing, -ext_spacing),
      y = c(feature_radius + hashmark_width, feature_radius + hashmark_width)
    ),
    create_circle(
      center = c(0, 0),
      start = 0.5 + theta1,
      end = 1.5 - theta1,
      r = feature_radius
    ),
    data.frame(
      x = c(-ext_spacing, -int_spacing),
      y = c(-feature_radius - hashmark_width, -feature_radius - hashmark_width)
    ),
    create_circle(
      center = c(0, 0),
      start = 1.5 - theta2,
      end = 1.5,
      r = feature_radius
    ),
    data.frame(
      x = c(0),
      y = c(-feature_radius + feature_thickness)
    ),
    create_circle(
      center = c(0, 0),
      start = 1.5,
      end = 0.5,
      r = feature_radius - feature_thickness
    ),
    data.frame(
      x = c(0),
      y = c(feature_radius)
    )
  )

  # Reflect the half-circle just created over the y axis
  odzone_faceoff_circle_df <- rbind(
    odzone_faceoff_circle_df,
    reflect(odzone_faceoff_circle_df, over_x = FALSE, over_y = TRUE)
  )

  return(odzone_faceoff_circle_df)
}

#' The non-centered faceoff spots are located in the neutral, offensive and
#' defensive zones of the ice, with one on each side of the x-axis when viewing
#' the rink in TV view. These spots differ from the center faceoff spot because
#' they have a larger diameter, differ in color, and have a colored stripe that
#' runs through its center.
#'
#' This function is responsible for creating the outer ring, not the colored
#' stripe running through it. Please see [hockey_nodzone_faceoff_spot_stripe()]
#' for more information on it
#'
#' The non-centered faceoff spots are where faceoffs are taken after an icing
#' call or to start a powerplay. They differ from the center ice faceoff spot in
#' size, color, and form. The thickness should be given by
#' 'minor_line_thickness' as these are minor lines on the ice surface
#'
#' @param feature_radius The outer radius of the non-centered faceoff spot ring
#' @param feature_thickness The thickness of the non-centered faceoff spot ring
#'
#' @return A data frame containing the bounding coordinates of a non-centered
#'   faceoff spot ring
#'
#' @keywords internal
hockey_nodzone_faceoff_spot_ring <- function(feature_radius = 0,
                                             feature_thickness = 0) {
  # The non-centered faceoff spots are comprised of an outer and inner ring
  nodzone_faceoff_spot_ring_df <- rbind(
    create_circle(
      center = c(0, 0),
      start = 0.5,
      end = 1.5,
      r = feature_radius
    ),
    data.frame(
      x = c(0),
      y = c(-feature_radius + feature_thickness)
    ),
    create_circle(
      center = c(0, 0),
      start = 1.5,
      end = 0.5,
      r = feature_radius - feature_thickness
    ),
    data.frame(
      x = c(0, 0),
      y = c(feature_radius - feature_thickness, feature_radius)
    )
  )

  nodzone_faceoff_spot_ring_df <- rbind(
    nodzone_faceoff_spot_ring_df,
    reflect(nodzone_faceoff_spot_ring_df, over_x = FALSE, over_y = TRUE)
  )

  return(nodzone_faceoff_spot_ring_df)
}

#' The non-centered faceoff spots are located in the neutral, offensive and
#' defensive zones of the ice, with one on each side of the x-axis when viewing
#' the rink in TV view. These spots differ from the center faceoff spot because
#' they have a larger diameter, differ in color, and have a colored stripe that
#' runs through its center.
#'
#' This function is responsible for creating the inner stripe, not the colored
#' outer ring around it. Please see [hockey_nodzone_faceoff_spot_ring()] for
#' more information on it
#'
#' The non-centered faceoff spots are where faceoffs are taken after an icing
#' call or to start a powerplay. They differ from the center ice faceoff spot in
#' size, color, and form. For the faceoff spot's stripe, the 'feature_thickness'
#' parameter should be the thickness of the outer ring, which is
#' 'minor_line_thickness'
#'
#' @param feature_radius The outer radius of the non-centered faceoff spot
#' @param feature_thickness The thickness of the non-centered faceoff spot ring
#' @param gap_width The width of the gap from the inner edge of the non-centered
#'   faceoff spot ring to the outer edge of the stripe
#'
#' @return A data frame containing the bounding coordinates of the non-centered
#'   faceoff spot's stripe
#'
#' @keywords internal
hockey_nodzone_faceoff_spot_stripe <- function(feature_radius = 0,
                                               feature_thickness = 0,
                                               gap_width = 0) {
  # The non-center face-off spots are wider in diameter, with a gap between the
  # top and bottom of the spot and the strip in the center. First, find the
  # angle at which to start the trace for the interior of the spot. The
  # following walk-through uses NHL dimensions for the explanation, but the
  # process is equally applied through all leagues

  # The spot has a radius of 1', and a thickness of 2", so the inner radius is
  # 10". Since there is a 3" gap at theta = 180°, this indicates that the
  # stripe's curve starts at x = -7" from the center. Using trigonometry, the
  # angle can be computed

  # Start by getting the inner radius of the ring
  ring_inner_radius <- feature_radius - feature_thickness

  # Then get the thickness of half of the stripe that runs through the center of
  # the spot
  stripe_thickness <- ring_inner_radius - gap_width

  if (feature_radius == 0 || abs(stripe_thickness / ring_inner_radius) > 1) {
    theta <- 0
  } else {
    theta <- asin(stripe_thickness / ring_inner_radius) / pi
  }

  nodzone_faceoff_spot_stripe_df <- rbind(
    create_circle(
      center = c(0, 0),
      start = 0.5 - theta,
      end = 0.5 + theta,
      r = ring_inner_radius
    ),
    create_circle(
      center = c(0, 0),
      start = 1.5 - theta,
      end = 1.5 + theta,
      r = ring_inner_radius
    )
  )

  return(nodzone_faceoff_spot_stripe_df)
}

#' The goal frame is where the puck enters after crossing the goal line to score
#' a legal goal. The front face of the goal is flush with the goal line, while
#' the back edge features rounded corners and expands outside of the front
#' posts. The goal frame is composed of two pieces: the frame (this method) and
#' the fill (see [hockey_goal_frame_fill()])
#'
#' The goal frame has two thicknesses to be careful of: the outer diameter of
#' the posts, and the outer diameter of the pipe in the back of the goal. The
#' frame of the goal is usually red in color
#'
#' @param feature_radius The radius of the circular part of the goal frame
#' @param goal_mouth_width The width of the goal mouth
#' @param goal_back_width The width of the back of the frame of the goal
#' @param goal_depth The depth of the goal from the front of the goal line to
#'   the back of the goal frame
#' @param goal_post_diameter The diameter of the post of the goal
#'
#' @return A data frame containing the bounding coordinates of the frame of the
#'   goal
#'
#' @keywords internal
hockey_goal_frame <- function(feature_radius = 0,
                              goal_mouth_width = 0,
                              goal_back_width = 0,
                              goal_depth = 0,
                              goal_post_diameter = 0) {
  # Start by getting the half-width of the goal mouth
  half_goal_mouth <- goal_mouth_width / 2

  # Compute the location of the point to use to trace out the rounded corners of
  # the goal
  goal_arc_center_x <- goal_depth - feature_radius - goal_post_diameter
  goal_arc_center_y <- (
    (goal_back_width / 2) - 
      feature_radius + 
      (goal_post_diameter / 2)
  )

  # Trace the path of the goal frame, starting with the exterior
  goal_frame_df <- rbind(
    data.frame(
      x = c(0),
      y = c(half_goal_mouth + goal_post_diameter)
    ),
    create_circle(
      center = c(goal_arc_center_x, goal_arc_center_y),
      start = 0.65,
      end = 0,
      r = feature_radius
    ),
    create_circle(
      center = c(goal_arc_center_x, -goal_arc_center_y),
      start = 0,
      end = -0.65,
      r = feature_radius
    ),
    data.frame(
      x = c(0, 0),
      y = c(-(half_goal_mouth + goal_post_diameter), -half_goal_mouth)
    ),
    create_circle(
      center = c(goal_arc_center_x, -goal_arc_center_y),
      start = -0.65,
      end = 0,
      r = feature_radius - goal_post_diameter
    ),
    create_circle(
      center = c(goal_arc_center_x, goal_arc_center_y),
      start = 0,
      end = 0.65,
      r = feature_radius - goal_post_diameter
    ),
    data.frame(
      x = c(0, 0),
      y = c(half_goal_mouth, half_goal_mouth + goal_post_diameter)
    )
  )

  return(goal_frame_df)
}

#' The goal frame is where the puck enters after crossing the goal line to score
#' a legal goal. The front face of the goal is flush with the goal line, while
#' the back edge features rounded corners and expands outside of the front
#' posts. The goal frame is composed of two pieces: the frame (see
#' [hockey_goal_frame()]) and the fill (this function)
#'
#' The goal frame has two thicknesses to be careful of: the outer diameter of
#' the posts, and the outer diameter of the pipe in the back of the goal. The
#' frame of the goal is usually red in color
#'
#' @param feature_radius The radius of the circular part of the goal frame
#' @param goal_mouth_width The width of the goal mouth
#' @param goal_back_width The width of the back of the frame of the goal
#' @param goal_depth The depth of the goal from the front of the goal line to
#'   the back of the goal frame
#' @param goal_post_diameter The diameter of the post of the goal
#'
#' @return A data frame containing the bounding coordinates of the frame of the
#'   goal
#'
#' @keywords internal
hockey_goal_frame_fill <- function(feature_radius = 0,
                                   goal_mouth_width = 0,
                                   goal_back_width = 0,
                                   goal_depth = 0,
                                   goal_post_diameter = 0) {
  # Start by getting the half-width of the goal mouth
  half_goal_mouth <- goal_mouth_width / 2

  # Compute the location of the point to use to trace out the rounded corners of
  # the goal
  goal_arc_center_x <- goal_depth - feature_radius - goal_post_diameter
  goal_arc_center_y <- (
  (goal_back_width / 2) -
    feature_radius +
    (goal_post_diameter / 2)
  )

  # Trace the path of the goal frame's interior
  goal_frame_fill_df <- rbind(
    data.frame(
      x = c(0),
      y = c(-half_goal_mouth)
    ),
    create_circle(
      center = c(goal_arc_center_x, -goal_arc_center_y),
      start = -0.65,
      end = 0,
      r = feature_radius - goal_post_diameter
    ),
    create_circle(
      center = c(goal_arc_center_x, goal_arc_center_y),
      start = 0,
      end = 0.65,
      r = feature_radius - goal_post_diameter
    ),
    data.frame(
      x = c(0),
      y = c(half_goal_mouth)
    )
  )

  return(goal_frame_fill_df)
}

#' The player benches are the areas outside the confines of the rink where
#' players not currently on the ice are seated. They are to be on the same side
#' of the ice surface and separate, as close to center ice as possible
#'
#' This will have the same thickness as the boards, but will be located outside
#' the ice surface. Each bench's outline will share the same color as the boards
#'
#' @param feature_thickness The thickness of the outline of the player bench
#'   areas
#' @param bench_length The length of the player bench area
#' @param bench_depth The depth of the player bench area
#'
#' @return A data frame containing the bounding coordinates of the player bench
#'   area
#'
#' @keywords internal
hockey_player_bench_outline <- function(feature_thickness = 0,
                                        bench_length = 0,
                                        bench_depth = 0) {
  bench_outline_df <- data.frame(
    x = c(
      -feature_thickness,
      -feature_thickness,
      bench_length + feature_thickness,
      bench_length + feature_thickness,
      bench_length,
      bench_length,
      0,
      0,
      feature_thickness
    ),
    y = c(
      feature_thickness,
      (2 * feature_thickness) + bench_depth,
      (2 * feature_thickness) + bench_depth,
      feature_thickness,
      feature_thickness,
      feature_thickness + bench_depth,
      feature_thickness + bench_depth,
      feature_thickness,
      feature_thickness
    )
  )

  return(bench_outline_df)
}

#' The player benches are the areas outside the confines of the rink where
#' players not currently on the ice are seated. They are to be on the same side
#' of the ice surface and separate, as close to center ice as possible
#'
#' This will have the same thickness as the boards, but will be located outside
#' the ice surface
#'
#' @param feature_thickness The thickness of the outline of the player bench
#'   area
#' @param bench_length The length of the player bench area
#' @param bench_depth The depth of the player bench area
#'
#' @return A data frame containing the bounding coordinates of the player bench
#'   area's inner filling
#'
#' @keywords internal
hockey_player_bench_area_fill <- function(feature_thickness = 0,
                                          bench_length = 0,
                                          bench_depth = 0) {
  bench_fill_df <- create_rectangle(
    x_min = -bench_length / 2,
    x_max = bench_length / 2,
    y_min = feature_thickness,
    y_max = feature_thickness + bench_depth
  )

  return(bench_fill_df)
}

#' The penalty boxes are the areas outside the confines of the rink where
#' players serve time for a penalty incurred. They are to be on the same side of
#' the ice surface and separate, as close to center ice as possible, for each
#' team. This will also include the off-ice officials' box
#'
#' This will have the same thickness as the boards, but will be located outside
#' the ice surface. Each penalty box's outline will share the same color as the
#' boards
#'
#' @param feature_thickness The thickness of the outline of the penalty box
#' @param penalty_box_length The length of the penalty box
#' @param penalty_box_depth The depth at which the penalty box extends from the
#'   outer edge of the boards
#' @param penalty_box_separation The separation between the two penalty boxes
#'
#' @return A data frame containing the bounding coordinates of the penalty box
#'
#' @keywords internal
hockey_penalty_box_outline <- function(feature_thickness = 0,
                                       penalty_box_length = 0,
                                       penalty_box_width = 0,
                                       penalty_box_separation = 0,
                                       penalty_box_depth = 0) {
  penalty_box_outline_df <- data.frame(
    x = c(
      0,
      (penalty_box_separation / 2) + penalty_box_length + feature_thickness,
      (penalty_box_separation / 2) + penalty_box_length + feature_thickness,
      (penalty_box_separation / 2) + penalty_box_length,
      (penalty_box_separation / 2) + penalty_box_length,
      (penalty_box_separation / 2) + feature_thickness,
      (penalty_box_separation / 2) + feature_thickness,
      penalty_box_separation / 2,
      penalty_box_separation / 2,
      0,
      0
    ),
    y = c(
      -((2 * feature_thickness) + penalty_box_depth),
      -((2 * feature_thickness) + penalty_box_depth),
      -(feature_thickness),
      -(feature_thickness),
      -(feature_thickness + penalty_box_depth),
      -(feature_thickness + penalty_box_depth),
      -(feature_thickness),
      -(feature_thickness),
      -(feature_thickness + penalty_box_depth),
      -(feature_thickness + penalty_box_depth),
      -((2 * feature_thickness) + penalty_box_depth)
    )
  )

  return(penalty_box_outline_df)
}

#' The penalty boxes are the areas outside the confines of the rink where
#' players serve time for a penalty incurred. They are to be on the same side of
#' the ice surface and separate, as close to center ice as possible, for each
#' team. This will not include the off-ice officials' box; see
#' [hockey_off_ice_officials_box()] for more information
#'
#' This will have the same thickness as the boards, but will be located outside
#' the ice surface
#'
#' @param feature_thickness The thickness of the outline of the penalty box
#' @param penalty_box_length The length of the penalty box
#' @param penalty_box_depth The depth at which the penalty box extends from the
#'   outer edge of the boards
#'
#' @return A data frame containing the bounding coordinates of the penalty box's
#'   inner filling
#'
#' @keywords internal
hockey_penalty_box_fill <- function(feature_thickness = 0,
                                    penalty_box_length = 0,
                                    penalty_box_depth = 0) {
  penalty_box_fill_df <- create_rectangle(
    x_min = -penalty_box_length / 2,
    x_max = penalty_box_length / 2,
    y_min = -feature_thickness,
    y_max = -(feature_thickness + penalty_box_depth)
  )

  return(penalty_box_fill_df)
}

#' The off-ice officials' box is located between the two penalty boxes, opposite
#' the team bench areas
#'
#' This will have the same thickness as the boards, but will be located outside
#' the ice surface
#'
#' @param feature_thickness The thickness of the outline of the off-ice
#'   officials' box
#' @param officials_box_length The length of the off-ice officials' box
#' @param officials_box_depth The depth at which the off-ice officials' box
#'   extends from the outer edge of the boards
#'
#' @return A data frame containing the bounding coordinates of the off-ice
#'   officials' box's outline
#'
#' @keywords internal
hockey_off_ice_officials_box <- function(feature_thickness = 0,
                                         officials_box_length = 0,
                                         officials_box_depth = 0) {
  off_ice_officials_box_df <- create_rectangle(
    x_min = -officials_box_length / 2,
    x_max = officials_box_length / 2,
    y_min = -feature_thickness,
    y_max = -(feature_thickness + officials_box_depth)
  )

  return(off_ice_officials_box_df)
}
