#' Generate the data frame for the points that comprise the boards
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the boards
iihf_feature_boards = function(full_surf = TRUE,
                               rotate = FALSE,
                               rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # IIHF boards are 60m long by 30m wide, with corners rounded at an arc of 8.5m
  corner_1_in = create_circle(
    center = c(-21.5, 6.5),
    start = .5,
    end = 1,
    d = 17
  )

  corner_2_in = create_circle(
    center = c(-21.5, -6.5),
    start = 1,
    end = 1.5,
    d = 17
  )

  corner_2_out = create_circle(
    center = c(-21.5, -6.5),
    start = 1.5,
    end = 1,
    d = 17.1
  )

  corner_1_out = create_circle(
    center = c(-21.5, 6.5),
    start = 1,
    end = .5,
    d = 17.1
  )

  boards = rbind(
    data.frame(
      x = 0,
      y = 15
    ),

    corner_1_in,

    data.frame(
      x = -30,
      y = 0
    ),

    corner_2_in,

    data.frame(
      x = c(0, 0),
      y = c(-15, -15.05)
    ),

    corner_2_out,
    data.frame(
      x = -30 - 0.05,
      y = 0
    ),

    corner_1_out,

    data.frame(
      x = c(0, 0),
      y = c(15.05, 15)
    )
  )

  if (full_surf) {
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    boards = rbind(
      boards,
      reflect(
        boards,
        over_y = TRUE
      )
    )
  }

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    boards = rotate_coords(
      boards,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(boards)
}

#' Generate the data frame for the points that comprise the center line
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the center line
iihf_feature_center_line = function(full_surf = TRUE,
                                    rotate = FALSE,
                                    rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  center_line = create_rectangle(
    x_min = .15,
    x_max = 0,
    y_min = -15,
    y_max = 15
  )

  if (full_surf) {
    # If the surface being drawn is a full-surface representation, reflect
    # over the y axis
    center_line = rbind(
      center_line,
      reflect(
        center_line,
        over_y = TRUE
      )
    )
  }

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    center_line = rotate_coords(
      center_line,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(center_line)
}

#' Generate the data frame for the points that comprise the blue line(s)
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the blue line
iihf_feature_blue_line = function(full_surf = TRUE,
                                  rotate = FALSE,
                                  rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The blue line is 30cm thick with the edge closest to the center line lying
  # 7.14m from the center of the ice. It spans the entire width of the ice
  blue_line = create_rectangle(
    x_min = -7.14 - .3,
    x_max = -7.14,
    y_min = -15,
    y_max = 15
  )

  if (full_surf) {
    # If the surface being drawn is a full-surface representation, reflect
    # over the y axis
    blue_line = rbind(
      blue_line,
      reflect(
        blue_line,
        over_y = TRUE
      )
    )
  }

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    blue_line = rotate_coords(
      blue_line,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(blue_line)
}

#' Generate the data frame for the points that comprise the goal line
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the goal line
iihf_feature_goal_line = function(full_surf = TRUE,
                                  rotate = FALSE,
                                  rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The edge of the goal line closest to the center line is 4m away from the
  # boards (or  26m from the center), but follows the curvature of the boards
  # in the corner. To get the curvature, a similar calculation to that of the
  # face-off spot interior can be performed
  theta1 = asin(4.5 / 8.5) / pi
  theta2 = asin(4.55 / 8.5) / pi

  goal_line = rbind(
    create_circle(
      center = c(-21.5, 6.5),
      start = .5 + theta1,
      end = .5 + theta2,
      d = 17
    ),

    create_circle(
      center = c(-21.5, -6.5),
      start = 1.5 - theta2,
      end = 1.5 - theta1,
      d = 17
    ),

    create_circle(
      center = c(-21.5, 6.5),
      start = .5 + theta1,
      end = .5 + theta2,
      d = 17
    )[1, ]
  )

  if (full_surf) {
    # If the surface being drawn is a full-surface representation, reflect
    # over the y axis
    goal_line = rbind(
      goal_line,
      reflect(
        goal_line,
        over_y = TRUE
      )
    )
  }

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    goal_line = rotate_coords(
      goal_line,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(goal_line)
}

#' Generate the data frame for the points that comprise the goal crease
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the goal crease
iihf_feature_goal_crease = function(full_surf = TRUE,
                                    rotate = FALSE,
                                    rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The angle through which to trace the outer radius of the goal crease
  theta_out = asin(122 / 183) / pi

  # The angle through which to trace the inner radius of the goal crease
  theta_in = asin(117 / 178) / pi

  # The outer arc of the crease semi-circle
  crease_outer_arc = create_circle(
    center = c(-26, 0),
    start = theta_out,
    end = -theta_out,
    d = 3.66
  )

  # The inner arc of the crease semi-circle
  crease_fill_arc = create_circle(
    center = c(-26, 0),
    start = -theta_in,
    end = theta_in,
    d = 3.56
  )

  # Goal crease outline (red)
  goal_crease_outline = rbind(
    data.frame(
      x = c(
        -26,
        -24.63
      ),

      y = c(
        1.22,
        1.22
      )
    ),

    crease_outer_arc,

    data.frame(
      x = c(
        -26,
        -26,
        -24.78,
        -24.78,
        -24.73,
        -24.73
      ),

      y = c(
        -1.22,
        -1.17,
        -1.17,
        -1.04,
        -1.04,
        -1.17
      )
    ),

    crease_fill_arc,

    data.frame(
      x = c(
        -24.73,
        -24.73,
        -24.78,
        -24.78,
        -26,
        -26
      ),

      y = c(
        1.17,
        1.04,
        1.04,
        1.17,
        1.17,
        1.22
      )
    )
  )

  # Goal crease filling (light blue)
  goal_crease_fill = rbind(
    data.frame(
      x = c(
        -26,
        -26,
        -24.78,
        -24.78,
        -24.73,
        -24.73
      ),

      y = c(
        -1.22,
        -1.17,
        -1.17,
        -1.04,
        -1.04,
        -1.17
      )
    ),

    crease_fill_arc,

    data.frame(
      x = c(
        -24.73,
        -24.73,
        -24.78,
        -24.78,
        -26,
        -26,
        -26
      ),

      y = c(
        1.17,
        1.04,
        1.04,
        1.17,
        1.17,
        1.22,
        -1.22
      )
    )
  )

  if (full_surf) {
    # If the surface being drawn is a full-surface representation, reflect
    # over the y axis
    goal_crease_outline = rbind(
      goal_crease_outline,
      reflect(
        goal_crease_outline,
        over_y = TRUE
      )
    )

    goal_crease_fill = rbind(
      goal_crease_fill,
      reflect(
        goal_crease_fill,
        over_y = TRUE
      )
    )
  }

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    goal_crease_outline = rotate_coords(
      goal_crease_outline,
      rotation_dir
    )

    goal_crease_fill = rotate_coords(
      goal_crease_fill,
      rotation_dir
    )
  }

  # Return the feature's data frames as a list
  goal_crease = list(
    goal_crease_outline = goal_crease_outline,
    goal_crease_fill = goal_crease_fill
  )

  return(goal_crease)
}

#' Generate the data frame for the points that comprise the referee's crease
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the referee's crease
iihf_feature_referee_crease = function(full_surf = TRUE,
                                       rotate = FALSE,
                                       rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The referee's crease
  referee_crease = rbind(
    create_circle(
      center = c(0, -15),
      start = .5,
      end = 1,
      d = 6
    ),

    data.frame(
      x = -2.95,
      y = -15
    ),

    create_circle(
      center = c(0, -15),
      start = 1,
      end = .5,
      d = 5.9
    ),

    data.frame(
      x = 0,
      y = -12
    )
  )

  if (full_surf) {
    # If the surface being drawn is a full-surface representation, reflect
    # over the y axis
    referee_crease = rbind(
      referee_crease,
      reflect(
        referee_crease,
        over_y = TRUE
      )
    )
  }

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    referee_crease = rotate_coords(
      referee_crease,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(referee_crease)
}

#' Generate the data frame for the points that comprise the faceoff spots
#'
#' @param center The center coordinates of the faceoff spot
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise a faceoff spot
iihf_feature_faceoff_spot = function(center = c(0, 0),
                                     full_surf = TRUE,
                                     rotate = FALSE,
                                     rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The center dot on an IIHF ice rink is 30cm in diameter
  if (identical(center, c(0, 0))) {
    center_spot = create_circle(
      center = c(0, 0),
      start = .5,
      end = 1.5,
      d = .3
    )

    if (full_surf) {
      # If the surface being drawn is a full-surface representation, reflect
      # over the y axis
      center_spot = rbind(
        center_spot,
        reflect(
          center_spot,
          over_y = TRUE
        )
      )
    }

    if (rotate) {
      # If the desired output needs to be rotated, rotate the coordinates
      center_spot = rotate_coords(
        center_spot,
        rotation_dir
      )
    }

    # Return the feature's data frame
    return(center_spot)
  }

  else {
    # If the spot is NOT the center circle, it should be drawn as shown in the
    # rule book on page 26 in red

    # The spot is comprised of two pieces: an outer ring (with outer diameter
    # of 60cm, thickness 5cm), and an inner filling
    spot_outer_ring = rbind(
      create_circle(
        center = c(0, 0),
        start = .5,
        end = 1.5,
        d = .6
      ),
      create_circle(
        center = c(0, 0),
        start = 1.5,
        end = .5,
        d = .5
      )
    )

    # Since the entire spot needs to be drawn, reflect the outer-ring
    # coordinates over the y axis
    spot_outer_ring = rbind(
      spot_outer_ring,
      reflect(
        spot_outer_ring,
        over_y = TRUE
      )
    )

    # Move the outer ring's center to its correct location
    spot_outer_ring = translate(
      spot_outer_ring,
      translate_x = center[1],
      translate_y = center[2]
    )

    # The non-center face-off spots are 60cm in diameter, with a 12.5cm gap
    # between the top and bottom of the spot and the strip in the center.
    # First, find the angle at which to start the trace for the interior of
    # the spot.

    # The spot has a radius of 30cm, and a thickness of 5cm, so the inner
    # radius is 15cm. Since there is a 7.5cm gap at theta = 180deg, this
    # indicates that the stripe's curve starts at x = -17.5cm from the center.
    # Using trigonometry, the angle can be computed
    theta = asin(17.5 / 25) / pi

    # The inner filling can then be created
    spot_fill = rbind(
      create_circle(
        center = c(0, 0),
        start = .5 - theta,
        end = .5 + theta,
        d = .5
      ),

      create_circle(
        center = c(0, 0),
        start = 1.5 - theta,
        end = 1.5 + theta,
        d = .5
      )
    )

    # Move the inner filling's center to the correct location
    spot_fill = translate(
      spot_fill,
      translate_x = center[1],
      translate_y = center[2]
    )

    if (rotate) {
      # If the desired output needs to be rotated, rotate the coordinates
      spot_outer_ring = rotate_coords(
        spot_outer_ring,
        rotation_dir
      )

      spot_fill = rotate_coords(
        spot_fill,
        rotation_dir
      )
    }

    # Return the feature's data frames as a list
    faceoff_spot = list(
      spot_outer_ring = spot_outer_ring,
      spot_fill = spot_fill
    )

    return(faceoff_spot)
  }
}

#' Generate the data frame for the points that comprise the faceoff circles
#'
#' @param center The center coordinates of the faceoff spot
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the faceoff circle
iihf_feature_faceoff_circle = function(center = c(0, 0),
                                       full_surf = TRUE,
                                       rotate = FALSE,
                                       rotation_dir = "ccw") {
  if (identical(center, c(0, 0))) {
    # The center circle on an IIHF ice rink is 9m in diameter
    faceoff_circle = rbind(
      create_circle(
        center = c(0, 0),
        start = .5,
        end = 1.5,
        d = 9
      ),

      data.frame(
        x = c(0, 0),
        y = c(-4.5, -4.45)
      ),

      create_circle(
        center = c(0, 0),
        start = 1.5,
        end = .5,
        d = 8.9
      ),

      data.frame(
        x = c(0, 0),
        y = c(8.9, 9)
      )
    )

    if (full_surf) {
      # If the surface being drawn is a full-surface representation, reflect
      # over the y axis
      faceoff_circle = rbind(
        faceoff_circle,
        reflect(
          faceoff_circle,
          over_y = TRUE
        )
      )
    }

    if (rotate) {
      # If the desired output needs to be rotated, rotate the coordinates
      faceoff_circle = rotate_coords(
        faceoff_circle,
        rotation_dir
      )
    }

    # Return the feature's data frame
    return(faceoff_circle)
  }

  else {
    # Similar to the method described above, the starting angle to draw the
    # outer ring can be computed. The hash marks are 180cm apart on the
    # exterior, so taking where this hash mark meets the circle to be the
    # center, the starting angle is computed as follows
    theta1 = asin(90 / 450) / pi

    # The same process gives the angle to find the point on the interior of
    # the hash mark, which are 170cm apart
    theta2 = asin(85 / 450) / pi

    faceoff_circle = rbind(
      create_circle(
        center = c(0, 0),
        start = .5 + theta1,
        end = 1.5 - theta1,
        d = 9
      ),

      data.frame(
        x = c(-.9, -.85),
        y = c(-5.1, -5.1)
      ),

      create_circle(
        center = c(0, 0),
        start = 1.5 - theta2,
        end = 1.5,
        d = 9
      ),

      data.frame(
        x = 0,
        y = 8.9
      ),

      create_circle(
        center = c(0, 0),
        start = 1.5,
        end = .5,
        d = 8.9
      ),

      data.frame(
        x = 0,
        y = 9
      ),

      create_circle(
        center = c(0, 0),
        start = .5,
        end = .5 + theta2,
        d = 9
      ),

      data.frame(
        x = c(-.85, -.9),
        y = c(5.1, 5.1)
      ),

      create_circle(
        center = c(0, 0),
        start = .5 + theta1,
        end = 1.5 - theta1,
        d = 9
      )[1, ]
    )

    # Reflect the half-circle over the y axis
    faceoff_circle = rbind(
      faceoff_circle,
      reflect(
        faceoff_circle,
        over_y = TRUE
      )
    )

    # Move the faceoff circle to the correct position on the ice
    faceoff_circle = translate(
      faceoff_circle,
      translate_x = center[1],
      translate_y = center[2]
    )

    if (rotate) {
      # If the desired output needs to be rotated, rotate the coordinates
      faceoff_circle = rotate_coords(
        faceoff_circle,
        rotation_dir
      )
    }

    # Return the feature's data frame
    return(faceoff_circle)
  }
}

#' Generate the data frame for the points that comprise the faceoff lines
#'
#' @param center The center coordinates of the faceoff spot
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the faceoff lines
iihf_feature_faceoff_lines = function(center = c(0, 0),
                                      full_surf = TRUE,
                                      rotate = FALSE,
                                      rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  if (!identical(center, c(0, 0))) {
    # Only the four end-zone faceoff circles need these features. They measure
    # .9m tall, 1.20 long, and all lines are 5cm in width. The lines begin
    # (outer edges) .225m above the center of the faceoff spot
    faceoff_line_ul = data.frame(
      x = c(
        -.6,
        -1.8,
        -1.8,
        -.65,
        -.65,
        -.6,
        -.6
      ),

      y = c(
        .225,
        .225,
        .275,
        .275,
        1.125,
        1.125,
        .225
      )
    )

    # Since the line-details are all the same dimensions but appear in all
    # four quadrants relative to the center of the spot, the line can be
    # reflected over the x and y axes accordingly to create the rest of the
    # lines
    faceoff_line_ur = reflect(
      faceoff_line_ul,
      over_x = FALSE,
      over_y = TRUE
    )

    faceoff_line_ll = reflect(
      faceoff_line_ul,
      over_x = TRUE,
      over_y = FALSE
    )

    faceoff_line_lr = reflect(
      faceoff_line_ul,
      over_x = TRUE,
      over_y = TRUE
    )

    # Now all four detail lines must be moved to the correct positions on the
    # ice
    faceoff_line_ul = translate(
      faceoff_line_ul,
      translate_x = center[1],
      translate_y = center[2]
    )

    faceoff_line_ur = translate(
      faceoff_line_ur,
      translate_x = center[1],
      translate_y = center[2]
    )

    faceoff_line_ll = translate(
      faceoff_line_ll,
      translate_x = center[1],
      translate_y = center[2]
    )

    faceoff_line_lr = translate(
      faceoff_line_lr,
      translate_x = center[1],
      translate_y = center[2]
    )

    if (rotate) {
      # If the desired output needs to be rotated, rotate the coordinates
      faceoff_line_ul = rotate_coords(
        faceoff_line_ul,
        rotation_dir
      )

      faceoff_line_ur = rotate_coords(
        faceoff_line_ur,
        rotation_dir
      )

      faceoff_line_ll = rotate_coords(
        faceoff_line_ll,
        rotation_dir
      )

      faceoff_line_lr = rotate_coords(
        faceoff_line_lr,
        rotation_dir
      )
    }

    # Return the feature's data frames as a list
    faceoff_lines = list(
      faceoff_line_ul = faceoff_line_ul,
      faceoff_line_ur = faceoff_line_ur,
      faceoff_line_ll = faceoff_line_ll,
      faceoff_line_lr = faceoff_line_lr
    )

    return(faceoff_lines)
  }

  else {
    # Return an empty data frame
    return(data.frame(x = c(), y = c()))
  }
}

#' Generate the data frame for the points that comprise the goal
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the goal
iihf_feature_goal = function(full_surf = TRUE,
                             rotate = FALSE,
                             rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # A goal is 6' (interior) between the posts, and the posts have thickness of 2
  # 3/8". The goal is 40" (exterior) deep, and the back rail of the goal has
  # thickness 1.9" (outer diameter)
  goal = rbind(
    data.frame(
      x = c(-26 - convert_units(2.5, "in", "m")),
      y = c(convert_units(3, "ft", "m") + convert_units(2.375, "in", "m"))
    ),

    create_circle(
      center = c(-26 - convert_units(20, "in", "m"), convert_units(2, "ft", "m")),
      start = 1 / 3 + (1 / 16),
      end = 1,
      d = convert_units(40, "in", "m")
    ),

    create_circle(
      center = c(-26 - convert_units(20, "in", "m"), convert_units(-2, "ft", "m")),
      start = -1,
      end = -1 / 3 - (1 / 16),
      d = convert_units(40, "in", "m")
    ),

    data.frame(
      x = c(-26 - convert_units(2.5, "in", "m"), -26 - convert_units(2.5, "in", "m")),
      y = c(convert_units(-3, "ft", "m") - convert_units(2.375, "in", "m"), convert_units(-3, "ft", "m"))
    ),

    create_circle(
      center = c(-26 - convert_units(20, "in", "m"), convert_units(-2, "ft", "m")),
      start = -1 / 3 - (1 / 16),
      end = -1,
      d = convert_units(36.2, "in", "m")
    ),

    create_circle(
      center = c(-26 - convert_units(20, "in", "m"), convert_units(2, "ft", "m")),
      start = 1,
      end = 1 / 3 + (1 / 16),
      d = convert_units(36.2, "in", "m")
    ),

    data.frame(
      x = c(-26 - convert_units(2.5, "in", "m"), -26 - convert_units(2.5, "in", "m")),
      y = c(convert_units(3, "ft", "m"), convert_units(3, "ft", "m") + convert_units(2.375, "in", "m"))
    )
  )

  goal_fill = rbind(
    data.frame(
      x = -26 - convert_units(2.5, "in", "m"),
      y = convert_units(-3, "ft", "m")
    ),

    create_circle(
      center = c(-26 - convert_units(20, "in", "m"), convert_units(-2, "ft", "m")),
      start = -1 / 3 - (1 / 16),
      end = -1,
      d = convert_units(36.2, "in", "m")
    ),

    create_circle(
      center = c(-26 - convert_units(20, "in", "m"), convert_units(2, "ft", "m")),
      start = 1,
      end = 1 / 3 + (1 / 16),
      d = convert_units(36.2, "in", "m")
    ),

    data.frame(
      x = c(-26 - convert_units(2.5, "in", "m"), -26 - convert_units(2.5, "in", "m")),
      y = c(convert_units(3, "ft", "m"), convert_units(-3, "ft", "m"))
    )
  )

  if (full_surf) {
    # If the surface being drawn is a full-surface representation, reflect
    # over the y axis
    goal = rbind(
      goal,
      reflect(
        goal,
        over_y = TRUE
      )
    )

    goal_fill = rbind(
      goal_fill,
      reflect(
        goal_fill,
        over_y = TRUE
      )
    )
  }

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    goal = rotate_coords(
      goal,
      rotation_dir
    )

    goal_fill = rotate_coords(
      goal_fill,
      rotation_dir
    )
  }

  # Return the feature's data frames as a list
  goal_and_fill = list(
    goal = goal,
    goal_fill = goal_fill
  )

  return(goal_and_fill)
}

#' Generate the list of colors for an IIHF rink plot. The defaults can be
#' overwritten by supplying the names of the list elements to the
#' \code{geom_iihf()} function (or its wrapper \code{geom_hockey()})
#'
#' @param boards_color A hexadecimal string representing the color to use for
#'   this feature
#' @param center_line_color A hexadecimal string representing the color to use
#'   for this feature
#' @param blue_line_color A hexadecimal string representing the color to use for
#'   this feature
#' @param goal_line_color A hexadecimal string representing the color to use for
#'   this feature
#' @param goal_crease_outline_color A hexadecimal string representing the color to
#'   use for this feature
#' @param goal_crease_fill_color A hexadecimal string representing the color to
#'   use for this feature
#' @param referee_crease_color A hexadecimal string representing the color to
#'   use for this feature
#' @param center_faceoff_spot_color A hexadecimal string representing the color
#'   to use for this feature
#' @param faceoff_spot_outer_ring_color A hexadecimal string representing the
#'   color to use for this feature
#' @param faceoff_spot_fill_color A hexadecimal string representing the color to
#'   use for this feature
#' @param center_faceoff_circle_color A hexadecimal string representing the
#'   color to use for this feature
#' @param non_center_faceoff_circle_color A hexadecimal string representing the
#'   color to use for this feature
#' @param faceoff_line_color A hexadecimal string representing the color to use
#'   for this feature
#' @param goal_color A hexadecimal string representing the color to use for this
#'   feature
#' @param goal_fill_color A hexadecimal string representing the color to use for
#'   this feature
#'
#' @return A list of hexadecimal colors to use to color the features on the
#'   resulting plot
iihf_features_set_colors = function(boards_color = "#000000",
                                    center_line_color = "#c8102e",
                                    blue_line_color = "#0033a0",
                                    goal_line_color = "#c8102e",
                                    goal_crease_outline_color = "#c8102e",
                                    goal_crease_fill_color = "#41b6e6",
                                    referee_crease_color = "#c8102e",
                                    center_faceoff_spot_color = "#0033a0",
                                    faceoff_spot_outer_ring_color = "#c8102e",
                                    faceoff_spot_fill_color = "#c8102e",
                                    center_faceoff_circle_color = "#0033a0",
                                    non_center_faceoff_circle_color = "#c8102e",
                                    faceoff_line_color = "#c8102e",
                                    goal_color = "#c8102e",
                                    goal_fill_color = "#a5acaf") {

  # Create the colors to use for the plot
  feature_colors = list(
    boards_color = boards_color,
    center_line_color = center_line_color,
    blue_line_color = blue_line_color,
    goal_line_color = goal_line_color,
    goal_crease_outline_color = goal_crease_outline_color,
    goal_crease_fill_color = goal_crease_fill_color,
    referee_crease_color = referee_crease_color,
    center_faceoff_spot_color = center_faceoff_spot_color,
    faceoff_spot_outer_ring_color = faceoff_spot_outer_ring_color,
    faceoff_spot_fill_color = faceoff_spot_fill_color,
    center_faceoff_circle_color = center_faceoff_circle_color,
    non_center_faceoff_circle_color = non_center_faceoff_circle_color,
    faceoff_line_color = faceoff_line_color,
    goal_color = goal_color,
    goal_fill_color = goal_fill_color
  )

  # Return the list of colors
  return(feature_colors)
}

#' Create a ggplot2 instance that represents a regulation IIHF rink, with the
#' center of the rink corresponding to (0, 0)
#'
#' @param full_surf A boolean indicating whether or not to draw a full-surface
#'   representation of the playing surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not the surface representation
#'   needs to be rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the surface
#'   representation. Default: \code{'ccw'}
#' @param unit A string indicating the units with which to make the plot.
#'   Default: \code{'m'}
#' @param background_color A hexadecimal string representing the color to use
#'   for the plot's background. Default: \code{NULL}
#' @param ... Additional arguments to pass to the function. These should be the
#'   colors to pass to the \code{iihf_features_set_colors()} function
#'
#' @return A ggplot2 instance that represents a regulation IIHF rink
geom_iihf = function(full_surf = TRUE,
                     rotate = FALSE,
                     rotation_dir = "ccw",
                     unit = "m",
                     background_color = NULL,
                     ...) {
  # Faceoff spot centers for a half-sheets. These can be reflected over the y
  # axis for full-surface representations
  faceoff_spots = list(
    center = c(0, 0),
    spot_1 = c(-20, -8),
    spot_2 = c(-20, 8),
    spot_3 = c(-5.64, -8),
    spot_4 = c(-5.64, 8)
  )

  if (full_surf) {
    # If a full-surface representation is needed, reflect the spots over the y
    # axis
    faceoff_spots$spot_5 = c(
      -1 * faceoff_spots$spot_4[1],
      faceoff_spots$spot_4[2]
    )

    faceoff_spots$spot_6 = c(
      -1 * faceoff_spots$spot_3[1],
      faceoff_spots$spot_3[2]
    )

    faceoff_spots$spot_7 = c(
      -1 * faceoff_spots$spot_2[1],
      faceoff_spots$spot_2[2]
    )

    faceoff_spots$spot_8 = c(
      -1 * faceoff_spots$spot_1[1],
      faceoff_spots$spot_1[2]
    )
  }

  # Create the colors to use for the plot
  color_list = iihf_features_set_colors(...)

  # Generate the data frames for the features of an IIHF rink
  boards = iihf_feature_boards(full_surf, rotate, rotation_dir)
  center_line = iihf_feature_center_line(full_surf, rotate, rotation_dir)
  blue_line = iihf_feature_blue_line(full_surf, rotate, rotation_dir)
  goal_line = iihf_feature_goal_line(full_surf, rotate, rotation_dir)
  goal_crease = iihf_feature_goal_crease(full_surf, rotate, rotation_dir)
  referee_crease = iihf_feature_referee_crease(full_surf, rotate, rotation_dir)
  goal = iihf_feature_goal(full_surf, rotate, rotation_dir)

  # Convert between units as necessary
  if (!(unit %in% c("m", "meters"))) {
    boards = convert_units(boards, "m", unit, conversion_columns = c("x", "y"))
    center_line = convert_units(center_line, "m", unit, conversion_columns = c("x", "y"))
    blue_line = convert_units(blue_line, "m", unit, conversion_columns = c("x", "y"))
    goal$goal = convert_units(goal$goal, "m", unit, conversion_columns = c("x", "y"))
    goal$goal_fill = convert_units(goal$goal_fill, "m", unit, conversion_columns = c("x", "y"))
    goal_line = convert_units(goal_line, "m", unit, conversion_columns = c("x", "y"))
    goal_crease$goal_crease_outline = convert_units(goal_crease$goal_crease_outline, "m", unit, conversion_columns = c("x", "y"))
    goal_crease$goal_crease_fill = convert_units(goal_crease$goal_crease_fill, "m", unit, conversion_columns = c("x", "y"))
    referee_crease = convert_units(referee_crease, "m", unit, conversion_columns = c("x", "y"))
  }

  # Create the initial ggplot2 instance onto which the features will be added
  g = create_plot_base(rotate, background_color)

  # Add the features to the ggplot2 instance
  g = add_feature(g, boards, color_list$boards_color)
  g = add_feature(g, center_line, color_list$center_line_color)
  g = add_feature(g, blue_line, color_list$blue_line_color)
  g = add_feature(g, goal$goal, color_list$goal_color)
  g = add_feature(g, goal$goal_fill, color_list$goal_fill_color)
  g = add_feature(g, goal_line, color_list$goal_line_color)
  g = add_feature(g, goal_crease$goal_crease_outline, color_list$goal_crease_outline_color)
  g = add_feature(g, goal_crease$goal_crease_fill, color_list$goal_crease_fill_color)
  g = add_feature(g, referee_crease, color_list$referee_crease_color)

  # Handle the faceoff spots and circles
  for (spot in 1:length(faceoff_spots)) {
    spot_name = names(faceoff_spots[spot])
    center = faceoff_spots[[spot]]

    # Get the faceoff spot's data frames
    faceoff_spot = iihf_feature_faceoff_spot(center, full_surf, rotate, rotation_dir)
    faceoff_circle = iihf_feature_faceoff_circle(center, full_surf, rotate, rotation_dir)
    faceoff_lines = iihf_feature_faceoff_lines(center, full_surf, rotate, rotation_dir)

    # Draw the faceoff spot
    if (identical(center, c(0, 0))) {
      # Convert between units as necessary
      if (!(unit %in% c("m", "meters"))) {
        faceoff_spot = convert_units(faceoff_spot, "m", unit, conversion_columns = c("x", "y"))
        faceoff_circle = convert_units(faceoff_circle, "m", unit, conversion_columns = c("x", "y"))
      }

      g = add_feature(g, faceoff_spot, color_list$center_faceoff_spot_color)
      g = add_feature(g, faceoff_circle, color_list$center_faceoff_circle_color)
    }

    else if (spot_name %in% c("spot_1", "spot_2", "spot_7", "spot_8")) {
      # Convert between units as necessary
      if (!(unit %in% c("m", "meters"))) {
        faceoff_spot$spot_outer_ring = convert_units(faceoff_spot$spot_outer_ring, "m", unit, conversion_columns = c("x", "y"))
        faceoff_spot$spot_fill = convert_units(faceoff_spot$spot_fill, "m", unit, conversion_columns = c("x", "y"))
        faceoff_circle = convert_units(faceoff_circle, "m", unit, conversion_columns = c("x", "y"))
        faceoff_lines$faceoff_line_ul = convert_units(faceoff_lines$faceoff_line_ul, "m", unit, conversion_columns = c("x", "y"))
        faceoff_lines$faceoff_line_ur = convert_units(faceoff_lines$faceoff_line_ur, "m", unit, conversion_columns = c("x", "y"))
        faceoff_lines$faceoff_line_ll = convert_units(faceoff_lines$faceoff_line_ll, "m", unit, conversion_columns = c("x", "y"))
        faceoff_lines$faceoff_line_lr = convert_units(faceoff_lines$faceoff_line_lr, "m", unit, conversion_columns = c("x", "y"))
      }

      g = add_feature(g, faceoff_spot$spot_outer_ring, color_list$faceoff_spot_outer_ring_color)
      g = add_feature(g, faceoff_spot$spot_fill, color_list$faceoff_spot_fill_color)
      g = add_feature(g, faceoff_circle, color_list$non_center_faceoff_circle_color)
      g = add_feature(g, faceoff_lines$faceoff_line_ul, color_list$non_center_faceoff_circle_color)
      g = add_feature(g, faceoff_lines$faceoff_line_ur, color_list$non_center_faceoff_circle_color)
      g = add_feature(g, faceoff_lines$faceoff_line_ll, color_list$non_center_faceoff_circle_color)
      g = add_feature(g, faceoff_lines$faceoff_line_lr, color_list$non_center_faceoff_circle_color)
    }

    else {
      # Convert between units as necessary
      if (!(unit %in% c("m", "meters"))) {
        faceoff_spot$spot_outer_ring = convert_units(faceoff_spot$spot_outer_ring, "m", unit, conversion_columns = c("x", "y"))
        faceoff_spot$spot_fill = convert_units(faceoff_spot$spot_fill, "m", unit, conversion_columns = c("x", "y"))
      }

      g = add_feature(g, faceoff_spot$spot_outer_ring, color_list$faceoff_spot_outer_ring_color)
      g = add_feature(g, faceoff_spot$spot_fill, color_list$faceoff_spot_fill_color)
    }
  }

  # Return the ggplot2 instance that contains the rink plot
  return(g)
}
