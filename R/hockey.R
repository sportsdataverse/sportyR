usethis::use_package("ggplot2")
usethis::use_package("glue")

#' Generate the dataframe for the points that comprise the boards
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the boards added to it
hockey_boards = function(g, league = 'NHL', full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NHL', 'NWHL')){
    # NHL boards are 200' long and 85' wide, with corners rounded at an arc of
    # 28 feet
    corner_1_in = create_circle(
      center = c(-72, 14.5),
      start = .5,
      end = 1,
      d = 56
    )

    corner_2_in = create_circle(
      center = c(-72, -14.5),
      start = 1,
      end = 1.5,
      d = 56
    )

    corner_2_out = create_circle(
      center = c(-72, -14.5),
      start = 1.5,
      end = 1,
      d = 56 + (4/12)
    )

    corner_1_out = create_circle(
      center = c(-72, 14.5),
      start = 1,
      end = .5,
      d = 56 + (4/12)
    )

    boards = rbind(
      data.frame(
        x = 0,
        y = 42.5
      ),
      corner_1_in,
      data.frame(
        x = -100,
        y = 0
      ),
      corner_2_in,
      data.frame(
        x = c(0, 0),
        y = c(-42.5, -42.5 - (2/12))
      ),
      corner_2_out,
      data.frame(
        x = -100 - (2/12),
        y = 0
      ),
      corner_1_out,
      data.frame(
        x = c(0, 0),
        y = c(42.5 + (2/12), 42.5)
      )
    )
  }

  else if(league == 'IIHF'){
    # IIHF boards are 60m long by 30m wide, with corners rounded at an arc of
    # 8.5m
    corner_1_in = create_circle(
      center = c(m_to_ft(-21.5), m_to_ft(6.5)),
      start = .5,
      end = 1,
      d = m_to_ft(17)
    )

    corner_2_in = create_circle(
      center = c(m_to_ft(-21.5), m_to_ft(-6.5)),
      start = 1,
      end = 1.5,
      d = m_to_ft(17)
    )

    corner_2_out = create_circle(
      center = c(m_to_ft(-21.5), m_to_ft(-6.5)),
      start = 1.5,
      end = 1,
      d = m_to_ft(17.1)
    )

    corner_1_out = create_circle(
      center = c(m_to_ft(-21.5), m_to_ft(6.5)),
      start = 1,
      end = .5,
      d = m_to_ft(17.1)
    )

    boards = rbind(
      data.frame(
        x = 0,
        y = m_to_ft(15)
      ),
      corner_1_in,
      data.frame(
        x = m_to_ft(-30),
        y = 0
      ),
      corner_2_in,
      data.frame(
        x = c(0, 0),
        y = c(m_to_ft(-15), m_to_ft(-15.05))
      ),
      corner_2_out,
      data.frame(
        x = m_to_ft(-30) - (2/12),
        y = 0
      ),
      corner_1_out,
      data.frame(
        x = c(0, 0),
        y = c(m_to_ft(15.05), m_to_ft(15))
      )
    )
  }

  else if(league == 'NCAA'){
    # NCAA boards are 200' long and 85' wide, with corners rounded at an arc of
    # 20 feet
    corner_1_in = create_circle(
      center = c(-80, 22.5),
      start = .5,
      end = 1,
      d = 40
    )

    corner_2_in = create_circle(
      center = c(-80, -22.5),
      start = 1,
      end = 1.5,
      d = 40
    )

    corner_2_out = create_circle(
      center = c(-80, -22.5),
      start = 1.5,
      end = 1,
      d = 40 + (4/12)
    )

    corner_1_out = create_circle(
      center = c(-80, 22.5),
      start = 1,
      end = .5,
      d = 40 + (4/12)
    )

    boards = rbind(
      data.frame(
        x = 0,
        y = 42.5
      ),
      corner_1_in,
      data.frame(
        x = -100,
        y = 0
      ),
      corner_2_in,
      data.frame(
        x = c(0, 0),
        y = c(-42.5, -42.5 - (2/12))
      ),
      corner_2_out,
      data.frame(
        x = -100 - (2/12),
        y = 0
      ),
      corner_1_out,
      data.frame(
        x = c(0, 0),
        y = c(42.5 + (2/12), 42.5)
      )
    )
  }

  else{
    # If the league isn't valid (i.e. either NHL, NWHL, NCAA, or IIHF), return
    # the ggplot2 instance
    return(g)
  }

  if(full_surf){
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

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    boards = rotate_coords(
      boards,
      rotation_dir
    )
  }

  # Add the boards to the ggplot2 instance. The boards will be black in color to
  # make them easier to see on the plot
  g = g +
    ggplot2::geom_polygon(data = boards, ggplot2::aes(x, y), fill = '#000000')

  # Return the ggplot2 instance
  return(g)
}

#' Generate the dataframe for the points that comprise the center line
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the center line added to it
hockey_center_line = function(g, league = 'NHL', full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NHL', 'NWHL')){
    # The center line is 12" thick, so 6" are on each side of 0. It spans the
    # entire width of the ice
    center_line = create_rectangle(
      x_min = -.5,
      x_max = 0,
      y_min = -42.5,
      y_max = 42.5
    )

    if(full_surf){
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

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      center_line = rotate_coords(
        center_line,
        rotation_dir
      )
    }

    # Add the center line to the ggplot2 instance
    g = g +
      ggplot2::geom_polygon(data = center_line, ggplot2::aes(x, y), fill = '#c8102e')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'IIHF'){
    center_line = create_rectangle(
      x_min = m_to_ft(.15),
      x_max = m_to_ft(0),
      y_min = m_to_ft(-15),
      y_max = m_to_ft(15)
    )

    if(full_surf){
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

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      center_line = rotate_coords(
        center_line,
        rotation_dir
      )
    }

    # Add the center line to the ggplot2 instance. It will be red in color
    g = g +
      ggplot2::geom_polygon(data = center_line, ggplot2::aes(x, y), fill = '#c8102e')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'NCAA'){
    # The center line is 12" thick, so 6" are on each side of 0. It spans the
    # entire width of the ice
    center_line = create_rectangle(
      x_min = -.5,
      x_max = 0,
      y_min = -42.5,
      y_max = 42.5
    )

    if(full_surf){
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

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      center_line = rotate_coords(
        center_line,
        rotation_dir
      )
    }

    # Add the center line to the ggplot2 instance. It will be red in color
    g = g +
      ggplot2::geom_polygon(data = center_line, ggplot2::aes(x, y), fill = '#c8102e')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NHL, NWHL, NCAA, or IIHF), return
    # the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the blue line(s)
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the blue line(s) added to it
hockey_blue_line = function(g, league = 'NHL', full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NHL', 'NWHL')){
    # The blue line is 12" thick with the edge closest to the center line lying
    # 25' from the center of the ice. It spans the entire width of the ice
    blue_line = create_rectangle(
      x_min = -26,
      x_max = -25,
      y_min = -42.5,
      y_max = 42.5
    )

    if(full_surf){
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

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      blue_line = rotate_coords(
        blue_line,
        rotation_dir
      )
    }

    # Add the blue line(s) to the ggplot2 instance. They will be blue in color
    g = g +
      ggplot2::geom_polygon(data = blue_line, ggplot2::aes(x, y), fill = '#0033a0')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'IIHF'){
    # The blue line is 30cm thick with the edge closest to the center line lying
    # 7.14m from the center of the ice. It spans the entire width of the ice
    blue_line = create_rectangle(
      x_min = m_to_ft(-7.14 - .3),
      x_max = m_to_ft(-7.14),
      y_min = m_to_ft(-15),
      y_max = m_to_ft(15)
    )

    if(full_surf){
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

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      blue_line = rotate_coords(
        blue_line,
        rotation_dir
      )
    }

    # Add the blue line(s) to the ggplot2 instance. They will be blue in color
    g = g +
      ggplot2::geom_polygon(data = blue_line, ggplot2::aes(x, y), fill = '#0033a0')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'NCAA'){
    # The blue line is 12" thick with the edge closest to the center line lying
    # 25' from the center of the ice. It spans the entire width of the ice
    blue_line = create_rectangle(
      x_min = -26,
      x_max = -25,
      y_min = -42.5,
      y_max = 42.5
    )

    if(full_surf){
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

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      blue_line = rotate_coords(
        blue_line,
        rotation_dir
      )
    }

    # Add the blue line(s) to the ggplot2 instance. They will be blue in color
    g = g +
      ggplot2::geom_polygon(data = blue_line, ggplot2::aes(x, y), fill = '#0033a0')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NHL, NWHL, NCAA, or IIHF), return
    # the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the goal line
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A dataframe containing the points necessary to draw the goal line
hockey_goal_line = function(g, league = 'NHL', full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NHL', 'NWHL')){
    # The center of the goal line is 11' away from the boards (or 89' from the
    # center), but follows the curvature of the boards in the corner. To get the
    # curvature, a similar calculation to that of the face-off spot interior can
    # be performed
    theta1 = asin((17 - (1/12))/28) / pi
    theta2 = asin((17 + (1/12))/28) / pi

    goal_line = rbind(
      create_circle(
        center = c(-72, 14.5),
        start = .5 + theta1,
        end = .5 + theta2,
        d = 56
      ),
      create_circle(
        center = c(-72, -14.5),
        start = 1.5 - theta2,
        end = 1.5 - theta1,
        d = 56
      ),
      create_circle(
        center = c(-72, 14.5),
        start = .5 + theta1,
        end = .5 + theta2,
        d = 56
      )[1, ]
    )

    if(full_surf){
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

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      goal_line = rotate_coords(
        goal_line,
        rotation_dir
      )
    }

    # Add the goal line(s) to the ggplot2 instance. They will be red in color
    g = g +
      ggplot2::geom_polygon(data = goal_line, ggplot2::aes(x, y), fill = '#c8102e')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'IIHF'){
    # The edge of the goal line closest to the center line is 4m away from the
    # boards (or  26m from the center), but follows the curvature of the boards
    # in the corner. To get the curvature, a similar calculation to that of the
    # face-off spot interior can be performed
    theta1 = asin(m_to_ft(4.5)/m_to_ft(8.5)) / pi
    theta2 = asin(m_to_ft(4.55)/m_to_ft(8.5)) / pi

    goal_line = rbind(
      create_circle(
        center = c(m_to_ft(-21.5), m_to_ft(6.5)),
        start = .5 + theta1,
        end = .5 + theta2,
        d = m_to_ft(17)
      ),
      create_circle(
        center = c(m_to_ft(-21.5), m_to_ft(-6.5)),
        start = 1.5 - theta2,
        end = 1.5 - theta1,
        d = m_to_ft(17)
      ),
      create_circle(
        center = c(m_to_ft(-21.5), m_to_ft(6.5)),
        start = .5 + theta1,
        end = .5 + theta2,
        d = m_to_ft(17)
      )[1, ]
    )

    if(full_surf){
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

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      goal_line = rotate_coords(
        goal_line,
        rotation_dir
      )
    }

    # Add the goal line(s) to the ggplot2 instance. They will be red in color
    g = g +
      ggplot2::geom_polygon(data = goal_line, ggplot2::aes(x, y), fill = '#c8102e')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'NCAA'){
    # The edge of the goal line closest to the center line is 11' away from the
    # boards (or 89' from the center), but follows the curvature of the boards
    # in the corner. To get the curvature, a similar calculation to that of the
    # face-off spot interior can be performed
    theta1 = asin(9/20) / pi
    theta2 = asin((9 + (2/12))/20) / pi

    goal_line = rbind(
      create_circle(
        center = c(-80, 22.5),
        start = .5 + theta1,
        end = .5 + theta2,
        d = 40
      ),
      create_circle(
        center = c(-80, -22.5),
        start = 1.5 - theta2,
        end = 1.5 - theta1,
        d = 40
      ),
      create_circle(
        center = c(-80, 22.5),
        start = .5 + theta1,
        end = .5 + theta2,
        d = 40
      )[1, ]
    )

    if(full_surf){
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

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      goal_line = rotate_coords(
        goal_line,
        rotation_dir
      )
    }

    # Add the goal line(s) to the ggplot2 instance. They will be red in color
    g = g +
      ggplot2::geom_polygon(data = goal_line, ggplot2::aes(x, y), fill = '#c8102e')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NHL, NWHL, NCAA, or IIHF), return
    # the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the goalkeeper's
#' restricted area
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A dataframe containing the points necessary to draw the goalkeeper's
#'   restricted area
hockey_goalkeepers_restricted_area = function(g, league = 'NHL', full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NHL', 'NWHL')){
    # The restricted area is the trapezoid area located behind each goal
    goalkeepers_restricted_area = data.frame(
      x = c(
        -100,
        -89 + 1/12,
        -89 + 1/12,
        -100,
        -100,
        -89 - (1/12),
        -89 - (1/12),
        -100,
        -100
      ),

      y = c(
        14,
        11,
        -11,
        -14,
        -14 + (2/12),
        -11 + (2/12),
        11 - (2/12),
        14 - (2/12),
        14
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect
      # over the y axis
      goalkeepers_restricted_area = rbind(
        goalkeepers_restricted_area,
        reflect(
          goalkeepers_restricted_area,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      goalkeepers_restricted_area = rotate_coords(
        goalkeepers_restricted_area,
        rotation_dir
      )
    }

    # Add the goalkeeper's restricted area to the ggplot2 instance. It will be
    # red in color
    g = g +
      ggplot2::geom_polygon(data = goalkeepers_restricted_area, ggplot2::aes(x, y), fill = '#c8102e')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # The NHL is the only league that requires this feature
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the goal crease
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A dataframe containing the points necessary to draw the goal crease
hockey_goal_crease = function(g, league = 'NHL', full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NHL', 'NWHL')){
    # The angle through which to trace the outer radius of the goal crease
    theta_out = asin(4/6) / pi

    # The angle through which to trace the inner radius of the goal crease
    theta_in = asin((4 - (2/12))/(6 - (2/12))) / pi

    # The outer arc of the crease semi-circle
    crease_outer_arc = create_circle(
      center = c(-89, 0),
      start = theta_out,
      end = -theta_out,
      d = 12
    )

    # The inner arc of the crease semi-circle
    crease_inner_arc = create_circle(
      center = c(-89, 0),
      start = -theta_in,
      end = theta_in,
      d = 12 - (4/12)
    )

    # Goal crease outline (red)
    goal_crease_outline = rbind(
      data.frame(
        x = c(
          -89 + (1/12),
          -89 + 4.5 + (1/12)
        ),

        y = c(
          4,
          4
        )
      ),
      crease_outer_arc,
      data.frame(
        x = c(
          -89 + (1/12),
          -89 + (1/12),
          -85 + (1/12),
          -85 + (1/12),
          -85 + (3/12),
          -85 + (3/12)
        ),

        y = c(
          -4,
          -4 + (2/12),
          -4 + (2/12),
          -4 + (7/12),
          -4 + (7/12),
          -4 + (2/12)
        )
      ),
      crease_inner_arc,
      data.frame(
        x = c(
          -85 + (3/12),
          -85 + (3/12),
          -85 + (1/12),
          -85 + (1/12),
          -89 + (1/12),
          -89 + (1/12)
        ),

        y = c(
          4 - (2/12),
          4 - (7/12),
          4 - (7/12),
          4 - (2/12),
          4 - (2/12),
          4
        )
      )
    )

    # Goal crease fill (light blue)
    goal_crease_inner = rbind(
      data.frame(
        x = c(
          -89 + (1/12),
          -85 + (1/12),
          -85 + (1/12),
          -85 + (3/12),
          -85 + (3/12)
        ),

        y = c(
          -4 + (2/12),
          -4 + (2/12),
          -4 + (7/12),
          -4 + (7/12),
          -4 + (2/12)
        )
      ),
      crease_inner_arc,
      data.frame(
        x = c(
          -85 + (3/12),
          -85 + (3/12),
          -85 + (1/12),
          -85 + (1/12),
          -89 + (1/12),
          -89 + (1/12)
        ),

        y = c(
          4 - (2/12),
          4 - (7/12),
          4 - (7/12),
          4 - (2/12),
          4 - (2/12),
          -4 + (2/12)
        )
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect
      # over the y axis
      goal_crease_outline = rbind(
        goal_crease_outline,
        reflect(
          goal_crease_outline,
          over_y = TRUE
        )
      )

      goal_crease_inner = rbind(
        goal_crease_inner,
        reflect(
          goal_crease_inner,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      goal_crease_outline = rotate_coords(
        goal_crease_outline,
        rotation_dir
      )

      goal_crease_inner = rotate_coords(
        goal_crease_inner,
        rotation_dir
      )
    }

    # Add the goal crease to the ggplot2 instance. The outline will be red, and
    # the inner fill will be blue in color
    g = g +
      ggplot2::geom_polygon(data = goal_crease_outline, ggplot2::aes(x, y), fill = '#c8102e') +
      ggplot2::geom_polygon(data = goal_crease_inner, ggplot2::aes(x, y), fill = '#41b6e6')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'IIHF') {
    # The angle through which to trace the outer radius of the goal crease
    theta_out = asin(122/183) / pi

    # The angle through which to trace the inner radius of the goal crease
    theta_in = asin(117/178) / pi

    # The outer arc of the crease semi-circle
    crease_outer_arc = create_circle(
      center = c(m_to_ft(-26), m_to_ft(0)),
      start = theta_out,
      end = -theta_out,
      d = m_to_ft(3.66)
    )

    # The inner arc of the crease semi-circle
    crease_inner_arc = create_circle(
      center = c(m_to_ft(-26), m_to_ft(0)),
      start = -theta_in,
      end = theta_in,
      d = m_to_ft(3.56)
    )

    # Goal crease outline (red)
    goal_crease_outline = rbind(
      data.frame(
        x = c(
          m_to_ft(-26),
          m_to_ft(-24.63)
        ),

        y = c(
          m_to_ft(1.22),
          m_to_ft(1.22)
        )
      ),
      crease_outer_arc,
      data.frame(
        x = c(
          m_to_ft(-26),
          m_to_ft(-26),
          m_to_ft(-24.78),
          m_to_ft(-24.78),
          m_to_ft(-24.73),
          m_to_ft(-24.73)
        ),

        y = c(
          m_to_ft(-1.22),
          m_to_ft(-1.17),
          m_to_ft(-1.17),
          m_to_ft(-1.04),
          m_to_ft(-1.04),
          m_to_ft(-1.17)
        )
      ),
      crease_inner_arc,
      data.frame(
        x = c(
          m_to_ft(-24.73),
          m_to_ft(-24.73),
          m_to_ft(-24.78),
          m_to_ft(-24.78),
          m_to_ft(-26),
          m_to_ft(-26)
        ),

        y = c(
          m_to_ft(1.17),
          m_to_ft(1.04),
          m_to_ft(1.04),
          m_to_ft(1.17),
          m_to_ft(1.17),
          m_to_ft(1.22)
        )
      )
    )

    # Goal crease filling (light blue)
    goal_crease_inner = rbind(
      data.frame(
        x = c(
          m_to_ft(-26),
          m_to_ft(-26),
          m_to_ft(-24.78),
          m_to_ft(-24.78),
          m_to_ft(-24.73),
          m_to_ft(-24.73)
        ),

        y = c(
          m_to_ft(-1.22),
          m_to_ft(-1.17),
          m_to_ft(-1.17),
          m_to_ft(-1.04),
          m_to_ft(-1.04),
          m_to_ft(-1.17)
        )
      ),
      crease_inner_arc,
      data.frame(
        x = c(
          m_to_ft(-24.73),
          m_to_ft(-24.73),
          m_to_ft(-24.78),
          m_to_ft(-24.78),
          m_to_ft(-26),
          m_to_ft(-26),
          m_to_ft(-26)
        ),

        y = c(
          m_to_ft(1.17),
          m_to_ft(1.04),
          m_to_ft(1.04),
          m_to_ft(1.17),
          m_to_ft(1.17),
          m_to_ft(1.22),
          m_to_ft(-1.22)
        )
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect
      # over the y axis
      goal_crease_outline = rbind(
        goal_crease_outline,
        reflect(
          goal_crease_outline,
          over_y = TRUE
        )
      )

      goal_crease_inner = rbind(
        goal_crease_inner,
        reflect(
          goal_crease_inner,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      goal_crease_outline = rotate_coords(
        goal_crease_outline,
        rotation_dir
      )

      goal_crease_inner = rotate_coords(
        goal_crease_inner,
        rotation_dir
      )
    }

    # Add the goal crease to the ggplot2 instance. The outline will be red, and
    # the inner fill will be blue in color
    g = g +
      ggplot2::geom_polygon(data = goal_crease_outline, ggplot2::aes(x, y), fill = '#c8102e') +
      ggplot2::geom_polygon(data = goal_crease_inner, ggplot2::aes(x, y), fill = '#41b6e6')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'NCAA'){
    # The angle through which to trace the outer radius of the goal crease
    theta_out = asin(4/6) / pi

    # The angle through which to trace the inner radius of the goal crease
    theta_in = asin((4 - (2/12))/(6 - (2/12))) / pi

    # The outer arc of the crease semi-circle
    crease_outer_arc = create_circle(
      center = c(-89, 0),
      start = theta_out,
      end = -theta_out,
      d = 12
    )

    # The inner arc of the crease semi-circle
    crease_inner_arc = create_circle(
      center = c(-89, 0),
      start = -theta_in,
      end = theta_in,
      d = 12 - (4/12)
    )

    # Goal crease outline (red)
    goal_crease_outline = rbind(
      data.frame(
        x = c(
          -89 + (1/12),
          -89 + 4.5 + (1/12)
        ),

        y = c(
          4,
          4
        )
      ),
      crease_outer_arc,
      data.frame(
        x = c(
          -89 + (1/12),
          -89 + (1/12),
          -85 + (1/12),
          -85 + (1/12),
          -85 + (3/12),
          -85 + (3/12)
        ),

        y = c(
          -4,
          -4 + (2/12),
          -4 + (2/12),
          -4 + (7/12),
          -4 + (7/12),
          -4 + (2/12)
        )
      ),
      crease_inner_arc,
      data.frame(
        x = c(
          -85 + (3/12),
          -85 + (3/12),
          -85 + (1/12),
          -85 + (1/12),
          -89 + (1/12),
          -89 + (1/12)
        ),

        y = c(
          4 - (2/12),
          4 - (7/12),
          4 - (7/12),
          4 - (2/12),
          4 - (2/12),
          4
        )
      )
    )

    # Goal crease fill (light blue)
    goal_crease_inner = rbind(
      data.frame(
        x = c(
          -89 + (1/12),
          -85 + (1/12),
          -85 + (1/12),
          -85 + (3/12),
          -85 + (3/12)
        ),

        y = c(
          -4 + (2/12),
          -4 + (2/12),
          -4 + (7/12),
          -4 + (7/12),
          -4 + (2/12)
        )
      ),
      crease_inner_arc,
      data.frame(
        x = c(
          -85 + (3/12),
          -85 + (3/12),
          -85 + (1/12),
          -85 + (1/12),
          -89 + (1/12),
          -89 + (1/12)
        ),

        y = c(
          4 - (2/12),
          4 - (7/12),
          4 - (7/12),
          4 - (2/12),
          4 - (2/12),
          -4 + (2/12)
        )
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect
      # over the y axis
      goal_crease_outline = rbind(
        goal_crease_outline,
        reflect(
          goal_crease_outline,
          over_y = TRUE
        )
      )

      goal_crease_inner = rbind(
        goal_crease_inner,
        reflect(
          goal_crease_inner,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      goal_crease_outline = rotate_coords(
        goal_crease_outline,
        rotation_dir
      )

      goal_crease_inner = rotate_coords(
        goal_crease_inner,
        rotation_dir
      )
    }

    # Add the goal crease to the ggplot2 instance. The outline will be red, and
    # the inner fill will be blue in color
    g = g +
      ggplot2::geom_polygon(data = goal_crease_outline, ggplot2::aes(x, y), fill = '#c8102e') +
      ggplot2::geom_polygon(data = goal_crease_inner, ggplot2::aes(x, y), fill = '#41b6e6')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NHL, NWHL, NCAA, or IIHF), return
    # the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the referee's crease
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A dataframe containing the points necessary to draw the referee's
#'   crease
hockey_referee_crease = function(g, league = 'NHL', full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NHL', 'NWHL')){
    # The referee's crease
    referee_crease = rbind(
      create_circle(
        center = c(0, -42.5),
        start = .5,
        end = 1,
        d = 20
      ),
      data.frame(
        x = c(-10 + (2/12)),
        y = c(-42.5)
      ),
      create_circle(
        center = c(0, -42.5),
        start = 1,
        end = .5,
        d = 20 - (4/12)
      ),
      data.frame(
        x = 0,
        y = -32.5
      )
    )

    if(full_surf){
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

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      referee_crease = rotate_coords(
        referee_crease,
        rotation_dir
      )
    }

    # Add the referee's crease to the ggplot2 instance. It will be red in color
    g = g +
      ggplot2::geom_polygon(data = referee_crease, ggplot2::aes(x, y), fill = '#c8102e')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'IIHF'){
    # The referee's crease
    referee_crease = rbind(
      create_circle(
        center = c(m_to_ft(0), m_to_ft(-15)),
        start = .5,
        end = 1,
        d = m_to_ft(6)
      ),
      data.frame(
        x = m_to_ft(-2.95),
        y = m_to_ft(-15)
      ),
      create_circle(
        center = c(m_to_ft(0), m_to_ft(-15)),
        start = 1,
        end = .5,
        d = m_to_ft(5.9)
      ),
      data.frame(
        x = 0,
        y = m_to_ft(-12)
      )
    )

    if(full_surf){
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

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      referee_crease = rotate_coords(
        referee_crease,
        rotation_dir
      )
    }

    # Add the referee's crease to the ggplot2 instance. It will be red in color
    g = g +
      ggplot2::geom_polygon(data = referee_crease, ggplot2::aes(x, y), fill = '#c8102e')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'NCAA'){
    # The referee's crease
    referee_crease = rbind(
      create_circle(
        center = c(0, -42.5),
        start = .5,
        end = 1,
        d = 20
      ),
      data.frame(
        x = c(-10 + (2/12)),
        y = c(-42.5)
      ),
      create_circle(
        center = c(0, -42.5),
        start = 1,
        end = .5,
        d = 20 - (4/12)
      ),
      data.frame(
        x = 0,
        y = -32.5
      )
    )

    if(full_surf){
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

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      referee_crease = rotate_coords(
        referee_crease,
        rotation_dir
      )
    }

    # Add the referee's crease to the ggplot2 instance. It will be red in color
    g = g +
      ggplot2::geom_polygon(data = referee_crease, ggplot2::aes(x, y), fill = '#c8102e')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NHL, NWHL, NCAA, or IIHF), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the faceoff spots
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param center The center coordinates of the faceoff spot
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the faceoff spot added to it
hockey_faceoff_spot = function(g, league, center = c(0, 0), full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NHL', 'NWHL')){
    # The center dot on an NHL ice rink is 1' in diameter
    if(identical(center, c(0, 0))){
      center_spot = create_circle(
        center = c(0, 0),
        start = .5,
        end = 1.5,
        d = 1
      )

      if(full_surf){
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

      if(rotate){
        # If the desired output needs to be rotated, rotate the coordinates
        center_spot = rotate_coords(
          center_spot,
          rotation_dir
        )
      }

      # Add the center spot to the ggplot2 instance
      g = g +
        ggplot2::geom_polygon(data = center_spot, ggplot2::aes(x, y), fill = '#0033a0')

      # Return the ggplot2 instance
      return(g)

    }

    else {
      # If the spot is NOT the center circle, it should be drawn as shown in
      # the rule book on page v in red

      # The spot is comprised of two pieces: an outer ring (with outer diameter
      # of 2', thickness 2"), and an inner filling
      spot_outer = rbind(
        create_circle(
          center = c(0, 0),
          start = .5,
          end = 1.5,
          d = 2
        ),
        create_circle(
          center = c(0, 0),
          start = 1.5,
          end = .5,
          d = 2 - (4/12)
        )
      )

      # Since the entire spot needs to be drawn, reflect the outer-ring
      # coordinates over the y axis
      spot_outer = rbind(
        spot_outer,
        reflect(
          spot_outer,
          over_y = TRUE
        )
      )

      # Move the outer ring's center to its correct location
      spot_outer = translate(
        spot_outer,
        translate_x = center[1],
        translate_y = center[2]
      )

      # The non-center face-off spots are 2' in diameter, with a 3" gap between
      # the top and bottom of the spot and the strip in the center. First, find
      # the angle at which to start the trace for the interior of the spot.

      # The spot has a radius of 1', and a thickness of 2", so the inner radius
      # is 10". Since there is a 3" gap at theta = 180deg, this indicates that
      # the stripe's curve starts at x = -7" from the center. Using
      # trigonometry, the angle can be computed
      theta = asin(7/10) / pi

      # The inner filling can then be created
      spot_inner = rbind(
        create_circle(
          center = c(0, 0),
          start = .5 - theta,
          end = .5 + theta,
          d = 2 - (4/12)
        ),
        create_circle(
          center = c(0, 0),
          start = 1.5 - theta,
          end = 1.5 + theta,
          d = 2 - (4/12)
        )
      )

      # Move the inner filling's center to the correct location
      spot_inner = translate(
        spot_inner,
        translate_x = center[1],
        translate_y = center[2]
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      spot_outer = rotate_coords(
        spot_outer,
        rotation_dir
      )

      spot_inner = rotate_coords(
        spot_inner,
        rotation_dir
      )
    }

    # Add the faceoff spot to the ggplot2 instance. The non-center spots will be
    # red in color
    g = g +
      ggplot2::geom_polygon(data = spot_outer, ggplot2::aes(x, y), fill = '#c8102e') +
      ggplot2::geom_polygon(data = spot_inner, ggplot2::aes(x, y), fill = '#c8102e')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'IIHF'){
    # The center dot on an IIHF ice rink is 30cm in diameter
    if(identical(center, c(0, 0))){
      center_spot = create_circle(
        center = c(0, 0),
        start = .5,
        end = 1.5,
        d = m_to_ft(.3)
      )

      if(full_surf){
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

      if(rotate){
        # If the desired output needs to be rotated, rotate the coordinates
        center_spot = rotate_coords(
          center_spot,
          rotation_dir
        )
      }

      # Add the center spot to the ggplot2 instance. It will be blue in color
      g = g +
        ggplot2::geom_polygon(data = center_spot, ggplot2::aes(x, y), fill = '#0033a0')

      # Return the ggplot2 instance
      return(g)

    }

    else {
      # If the spot is NOT the center circle, it should be drawn as shown in the
      # rule book on page 26 in red

      # The spot is comprised of two pieces: an outer ring (with outer diameter
      # of 60cm, thickness 5cm), and an inner filling
      spot_outer = rbind(
        create_circle(
          center = c(0, 0),
          start = .5,
          end = 1.5,
          d = m_to_ft(.6)
        ),
        create_circle(
          center = c(0, 0),
          start = 1.5,
          end = .5,
          d = m_to_ft(.5)
        )
      )

      # Since the entire spot needs to be drawn, reflect the outer-ring
      # coordinates over the y axis
      spot_outer = rbind(
        spot_outer,
        reflect(
          spot_outer,
          over_y = TRUE
        )
      )

      # Move the outer ring's center to its correct location
      spot_outer = translate(
        spot_outer,
        translate_x = m_to_ft(center[1]),
        translate_y = m_to_ft(center[2])
      )

      # The non-center face-off spots are 60cm in diameter, with a 12.5cm gap
      # between the top and bottom of the spot and the strip in the center.
      # First, find the angle at which to start the trace for the interior of
      # the spot.

      # The spot has a radius of 30cm, and a thickness of 5cm, so the inner
      # radius is 15cm. Since there is a 7.5cm gap at theta = 180deg, this
      # indicates that the stripe's curve starts at x = -17.5cm from the center.
      # Using trigonometry, the angle can be computed
      theta = asin(17.5/25) / pi

      # The inner filling can then be created
      spot_inner = rbind(
        create_circle(
          center = c(0, 0),
          start = .5 - theta,
          end = .5 + theta,
          d = m_to_ft(.5)
        ),
        create_circle(
          center = c(0, 0),
          start = 1.5 - theta,
          end = 1.5 + theta,
          d = m_to_ft(.5)
        )
      )

      # Move the inner filling's center to the correct location
      spot_inner = translate(
        spot_inner,
        translate_x = m_to_ft(center[1]),
        translate_y = m_to_ft(center[2])
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      spot_outer = rotate_coords(
        spot_outer,
        rotation_dir
      )

      spot_inner = rotate_coords(
        spot_inner,
        rotation_dir
      )
    }

    # Add the faceoff spot to the ggplot2 instance. The non-center spots will be
    # red in color
    g = g +
      ggplot2::geom_polygon(data = spot_outer, ggplot2::aes(x, y), fill = '#c8102e') +
      ggplot2::geom_polygon(data = spot_inner, ggplot2::aes(x, y), fill = '#c8102e')

    # Return the ggplot2 instance
    return(g)

  }

  else if(league == 'NCAA'){
    # The center dot on an ncaa ice rink is 1' in diameter
    if(identical(center, c(0, 0))){
      center_spot = create_circle(
        center = c(0, 0),
        start = .5,
        end = 1.5,
        d = 1
      )

      if(full_surf){
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

      if(rotate){
        # If the desired output needs to be rotated, rotate the coordinates
        center_spot = rotate_coords(
          center_spot,
          rotation_dir
        )
      }

      # Add the center spot to the ggplot2 instance
      g = g +
        ggplot2::geom_polygon(data = center_spot, ggplot2::aes(x, y), fill = '#0033a0')

      # Return the ggplot2 instance
      return(g)

    }

    else {
      # If the spot is NOT the center circle, it should be drawn as shown in the
      # rule book on page 16 in red

      # The spot is comprised of two pieces: an outer ring (with outer diameter
      # of 2', thickness 2"), and an inner filling
      spot_outer = rbind(
        create_circle(
          center = c(0, 0),
          start = .5,
          end = 1.5,
          d = 2
        ),
        create_circle(
          center = c(0, 0),
          start = 1.5,
          end = .5,
          d = 2 - (4/12)
        )
      )

      # Since the entire spot needs to be drawn, reflect the outer-ring
      # coordinates over the y axis
      spot_outer = rbind(
        spot_outer,
        reflect(
          spot_outer,
          over_y = TRUE
        )
      )

      # Move the outer ring's center to its correct location
      spot_outer = translate(
        spot_outer,
        translate_x = center[1],
        translate_y = center[2]
      )

      # The non-center face-off spots are 2' in diameter, with a 3" gap between
      # the top and bottom of the spot and the strip in the center. First, find
      # the angle at which to start the trace for the interior of the spot.

      # The spot has a radius of 1', and a thickness of 2", so the inner radius
      # is 10". The width of the solid red inside is 16", wchich indicates that
      # the stripe's curve starts at x = -8" from the center. Using
      # trigonometry, the angle can be computed
      theta = asin(8/10) / pi

      # The inner filling can then be created
      spot_inner = rbind(
        create_circle(
          center = c(0, 0),
          start = .5 - theta,
          end = .5 + theta,
          d = 2 - (4/12)
        ),
        create_circle(
          center = c(0, 0),
          start = 1.5 - theta,
          end = 1.5 + theta,
          d = 2 - (4/12)
        )
      )

      # Move the inner filling's center to the correct location
      spot_inner = translate(
        spot_inner,
        translate_x = center[1],
        translate_y = center[2]
      )

      if(rotate){
        # If the desired output needs to be rotated, rotate the coordinates
        spot_outer = rotate_coords(
          spot_outer,
          rotation_dir
        )

        spot_inner = rotate_coords(
          spot_inner,
          rotation_dir
        )
      }

      # Add the faceoff spot to the ggplot2 instance. The non-center spots will
      # be red in color
      g = g +
        ggplot2::geom_polygon(data = spot_outer, ggplot2::aes(x, y), fill = '#c8102e') +
        ggplot2::geom_polygon(data = spot_inner, ggplot2::aes(x, y), fill = '#c8102e')

      # Return the ggplot2 instance
      return(g)
    }

  }

  else {
    # If the league isn't valid (i.e. either NHL, NWHL, NCAA, or IIHF), return
    # the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the faceoff circles
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param center The center coordinates of the faceoff spot
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the faceoff circle added to it
hockey_faceoff_circle = function(g, league, center = c(0, 0), full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NHL', 'NWHL')){
    if(identical(center, c(0, 0))){
      # The center circle on an NHL ice rink is 15' in diameter
      faceoff_circle = rbind(
        create_circle(
          center = c(0, 0),
          start = .5,
          end = 1.5,
          d = 30
        ),
        data.frame(
          x = c(0, 0),
          y = c(-15, -15 + (2/12))
        ),
        create_circle(
          center = c(0, 0),
          start = 1.5,
          end = .5,
          d = 30 - (4/12)
        ),
        data.frame(
          x = c(0, 0),
          y = c(-15 - (2/12), -15)
        )
      )

      if(full_surf){
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

      if(rotate){
        # If the desired output needs to be rotated, rotate the coordinates
        faceoff_circle = rotate_coords(
          faceoff_circle,
          rotation_dir
        )
      }

      # Add the center spot to the ggplot2 instance. It will be blue in color
      g = g +
        ggplot2::geom_polygon(data = faceoff_circle, ggplot2::aes(x, y), fill = '#0033a0')

      # Return the ggplot2 instance
      return(g)
    }

    else {
      # Similar to the method described above, the starting angle to draw the
      # outer ring can be computed. The hash marks are 5' 11" (71") apart on the
      # exterior, so taking where this hash mark meets the circle to be the
      # center, the starting angle is computed as follows
      theta1 = asin((35.5/12)/15) / pi

      # The same process gives the angle to find the point on the interior of
      # the hash mark, which are 5' 7" (67") apart
      theta2 = asin((33.5/12)/15) / pi

      faceoff_circle = rbind(
        create_circle(
          center = c(0, 0),
          start = .5 + theta1,
          end = 1.5 - theta1,
          d = 30
        ),
        data.frame(
          x = c(-35.5/12, -33.5/12),
          y = c(-17, -17)
        ),
        create_circle(
          center = c(0, 0),
          start = 1.5 - theta2,
          end = 1.5,
          d = 30
        ),
        data.frame(
          x = 0,
          y = -15 + (2/12)
        ),
        create_circle(
          center = c(0, 0),
          start = 1.5,
          end = .5,
          d = 30 - (4/12)
        ),
        data.frame(
          x = 0,
          y = 15
        ),
        create_circle(
          center = c(0, 0),
          start = .5,
          end = .5 + theta2,
          d = 30
        ),
        data.frame(
          x = c(-33.5/12, -35.5/12),
          y = c(17, 17)
        ),
        create_circle(
          center = c(0, 0),
          start = .5 + theta1,
          end = 1.5 - theta1,
          d = 30
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

      if(rotate){
        # If the desired output needs to be rotated, rotate the coordinates
        faceoff_circle = rotate_coords(
          faceoff_circle,
          rotation_dir
        )
      }

      # Add the faceoff circle to the ggplot2 instance. It will be red in color
      g = g +
        ggplot2::geom_polygon(data = faceoff_circle, ggplot2::aes(x, y), fill = '#c8102e')

      # Return the ggplot2 instance
      return(g)
    }
  }

  else if(league == 'IIHF'){
    if(identical(center, c(0, 0))){
      # The center circle on an NHL ice rink is 9m in diameter
      faceoff_circle = rbind(
        create_circle(
          center = c(0, 0),
          start = .5,
          end = 1.5,
          d = m_to_ft(9)
        ),
        data.frame(
          x = c(0, 0),
          y = c(m_to_ft(-4.5), m_to_ft(-4.45))
        ),
        create_circle(
          center = c(0, 0),
          start = 1.5,
          end = .5,
          d = m_to_ft(8.9)
        ),
        data.frame(
          x = c(0, 0),
          y = c(m_to_ft(8.9), m_to_ft(9))
        )
      )

      if(full_surf){
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

      if(rotate){
        # If the desired output needs to be rotated, rotate the coordinates
        faceoff_circle = rotate_coords(
          faceoff_circle,
          rotation_dir
        )
      }

      # Add the faceoff circle to the ggplot2 instance. It will be blue in color
      g = g +
        ggplot2::geom_polygon(data = faceoff_circle, ggplot2::aes(x, y), fill = '#0033a0')

      # Return the ggplot2 instance
      return(g)
    }

    else {
      # Similar to the method described above, the starting angle to draw the
      # outer ring can be computed. The hash marks are 180cm apart on the
      # exterior, so taking where this hash mark meets the circle to be the
      # center, the starting angle is computed as follows
      theta1 = asin(90/450) / pi

      # The same process gives the angle to find the point on the interior of
      # the hash mark, which are 170cm apart
      theta2 = asin(85/450) / pi

      faceoff_circle = rbind(
        create_circle(
          center = c(0, 0),
          start = .5 + theta1,
          end = 1.5 - theta1,
          d = m_to_ft(9)
        ),
        data.frame(
          x = c(m_to_ft(-.9), m_to_ft(-.85)),
          y = c(m_to_ft(-5.1), m_to_ft(-5.1))
        ),
        create_circle(
          center = c(0, 0),
          start = 1.5 - theta2,
          end = 1.5,
          d = m_to_ft(9)
        ),
        data.frame(
          x = 0,
          y = m_to_ft(8.9)
        ),
        create_circle(
          center = c(0, 0),
          start = 1.5,
          end = .5,
          d = m_to_ft(8.9)
        ),
        data.frame(
          x = 0,
          y = m_to_ft(9)
        ),
        create_circle(
          center = c(0, 0),
          start = .5,
          end = .5 + theta2,
          d = m_to_ft(9)
        ),
        data.frame(
          x = c(m_to_ft(-.85), m_to_ft(-.9)),
          y = c(m_to_ft(5.1), m_to_ft(5.1))
        ),
        create_circle(
          center = c(0, 0),
          start = .5 + theta1,
          end = 1.5 - theta1,
          d = m_to_ft(9)
        )[1, ]
      )
    }

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
      translate_x = m_to_ft(center[1]),
      translate_y = m_to_ft(center[2])
    )

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      faceoff_circle = rotate_coords(
        faceoff_circle,
        rotation_dir
      )
    }

    # Add the faceoff circle to the ggplot2 instance. It will be red in color
    g = g +
      ggplot2::geom_polygon(data = faceoff_circle, ggplot2::aes(x, y), fill = '#c8102e')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'NCAA'){
    if(identical(center, c(0, 0))){
      # The center circle on an NCAA ice rink is 15' in diameter
      faceoff_circle = rbind(
        create_circle(
          center = c(0, 0),
          start = .5,
          end = 1.5,
          d = 30
        ),

        data.frame(
          x = c(0, 0),
          y = c(-15, -15 + (2/12))
        ),

        create_circle(
          center = c(0, 0),
          start = 1.5,
          end = .5,
          d = 30 - (4/12)
        ),

        data.frame(
          x = c(0, 0),
          y = c(-15 - (2/12), -15)
        )

      )

      if(full_surf){
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

      if(rotate){
        # If the desired output needs to be rotated, rotate the coordinates
        faceoff_circle = rotate_coords(
          faceoff_circle,
          rotation_dir
        )
      }

      # Add the faceoff circle to the ggplot2 instance. It will be blue in color
      g = g +
        ggplot2::geom_polygon(data = faceoff_circle, ggplot2::aes(x, y), fill = '#0033a0')

      # Return the ggplot2 instance
      return(g)
    }

    else {
      # Similar to the method described above, the starting angle to draw the
      # outer ring can be computed. The hash marks are 5' 11" (71") apart on the
      # exterior, so taking where this hash mark meets the circle to be the
      # center, the starting angle is computed as follows
      theta1 = asin((35.5/12)/15) / pi

      # The same process gives the angle to find the point on the interior of
      # the hash mark, which are 5' 7" (67") apart
      theta2 = asin((33.5/12)/15) / pi

      faceoff_circle = rbind(
        create_circle(
          center = c(0, 0),
          start = .5 + theta1,
          end = 1.5 - theta1,
          d = 30
        ),
        data.frame(
          x = c(-35.5/12, -33.5/12),
          y = c(-17, -17)
        ),
        create_circle(
          center = c(0, 0),
          start = 1.5 - theta2,
          end = 1.5,
          d = 30
        ),
        data.frame(
          x = 0,
          y = -15 + (2/12)
        ),
        create_circle(
          center = c(0, 0),
          start = 1.5,
          end = .5,
          d = 30 - (4/12)
        ),
        data.frame(
          x = 0,
          y = 15
        ),
        create_circle(
          center = c(0, 0),
          start = .5,
          end = .5 + theta2,
          d = 30
        ),
        data.frame(
          x = c(-33.5/12, -35.5/12),
          y = c(17, 17)
        ),
        create_circle(
          center = c(0, 0),
          start = .5 + theta1,
          end = 1.5 - theta1,
          d = 30
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

      if(rotate){
        # If the desired output needs to be rotated, rotate the coordinates
        faceoff_circle = rotate_coords(
          faceoff_circle,
          rotation_dir
        )
      }

      # Add the faceoff circle to the ggplot2 instance. It will be red in color
      g = g +
        ggplot2::geom_polygon(data = faceoff_circle, ggplot2::aes(x, y), fill = '#c8102e')

      # Return the ggplot2 instance
      return(g)
    }
  }

  else {
    # If the league isn't valid (i.e. either NHL, NWHL, NCAA, or IIHF), return
    # the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the faceoff lines
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param center The center coordinates of the faceoff spot
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the boards added to it
hockey_faceoff_lines = function(g, league, center = c(0, 0), full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NHL', 'NWHL')){
    if(!identical(center, c(0, 0))){
      # Only the four end-zone faceoff circles need these features. They measure
      # 3' tall, 4' long, and all lines are 2" in width. The lines begin (outer
      # edges) 9" above the center of the faceoff spot
      faceoff_line_ul = data.frame(
        x = c(-2, -6, -6, -2 - (2/12), -2 - (2/12), -2, -2),
        y = c(.75, .75, .75 + (2/12), .75 + (2/12), 3.75, 3.75, .75)
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

      if(rotate){
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

      # Add the faceoff lines to the ggplot2 instance. They will be red in color
      g = g +
        ggplot2::geom_polygon(data = faceoff_line_ul, ggplot2::aes(x, y), fill = '#c8102e') +
        ggplot2::geom_polygon(data = faceoff_line_ur, ggplot2::aes(x, y), fill = '#c8102e') +
        ggplot2::geom_polygon(data = faceoff_line_ll, ggplot2::aes(x, y), fill = '#c8102e') +
        ggplot2::geom_polygon(data = faceoff_line_lr, ggplot2::aes(x, y), fill = '#c8102e')

      # Return the ggplot2 instance
      return(g)
    }

    else {
      # Return the ggplot2 instance
      return(g)
    }
  }

  else if(league == 'IIHF'){
    if(!identical(center, c(0, 0))){
      # Only the four end-zone faceoff circles need these features. They measure
      # .9m tall, 1.20 long, and all lines are 5cm in width. The lines begin
      # (outer edges) .225m above the center of the faceoff spot
      faceoff_line_ul = data.frame(
        x = c(
          m_to_ft(-.6),
          m_to_ft(-1.8),
          m_to_ft(-1.8),
          m_to_ft(-.65),
          m_to_ft(-.65),
          m_to_ft(-.6),
          m_to_ft(-.6)
        ),

        y = c(
          m_to_ft(.225),
          m_to_ft(.225),
          m_to_ft(.275),
          m_to_ft(.275),
          m_to_ft(1.125),
          m_to_ft(1.125),
          m_to_ft(.225)
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
        translate_x = m_to_ft(center[1]),
        translate_y = m_to_ft(center[2])
      )

      faceoff_line_ur = translate(
        faceoff_line_ur,
        translate_x = m_to_ft(center[1]),
        translate_y = m_to_ft(center[2])
      )

      faceoff_line_ll = translate(
        faceoff_line_ll,
        translate_x = m_to_ft(center[1]),
        translate_y = m_to_ft(center[2])
      )

      faceoff_line_lr = translate(
        faceoff_line_lr,
        translate_x = m_to_ft(center[1]),
        translate_y = m_to_ft(center[2])
      )

      if(rotate){
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

      # Add the faceoff lines to the ggplot2 instance. They will be red in color
      g = g +
        ggplot2::geom_polygon(data = faceoff_line_ul, ggplot2::aes(x, y), fill = '#c8102e') +
        ggplot2::geom_polygon(data = faceoff_line_ur, ggplot2::aes(x, y), fill = '#c8102e') +
        ggplot2::geom_polygon(data = faceoff_line_ll, ggplot2::aes(x, y), fill = '#c8102e') +
        ggplot2::geom_polygon(data = faceoff_line_lr, ggplot2::aes(x, y), fill = '#c8102e')

      # Return the ggplot2 instance
      return(g)
    }

    else {
      # Return the ggplot2 instance
      return(g)
    }
  }

  else if(league == 'NCAA'){
    if(!identical(center, c(0, 0))){
      # Only the four end-zone faceoff circles need these features. They measure
      # 3' tall, 4' long, and all lines are 2" in width. The lines begin (outer
      # edges) 9" above the center of the faceoff spot
      faceoff_line_ul = data.frame(
        x = c(-2, -6, -6, -2 - (2/12), -2 - (2/12), -2, -2),
        y = c(.75, .75, .75 + (2/12), .75 + (2/12), 3.75, 3.75, .75)
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

      if(rotate){
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

      # Add the faceoff lines to the ggplot2 instance. They will be red in color
      g = g +
        ggplot2::geom_polygon(data = faceoff_line_ul, ggplot2::aes(x, y), fill = '#c8102e') +
        ggplot2::geom_polygon(data = faceoff_line_ur, ggplot2::aes(x, y), fill = '#c8102e') +
        ggplot2::geom_polygon(data = faceoff_line_ll, ggplot2::aes(x, y), fill = '#c8102e') +
        ggplot2::geom_polygon(data = faceoff_line_lr, ggplot2::aes(x, y), fill = '#c8102e')

      # Return the ggplot2 instance
      return(g)
    }

    else {
      # Return the ggplot2 instance
      return(g)
    }
  }

  else {
    # If the league isn't valid (i.e. either NHL, NWHL, NCAA, or IIHF), return
    # the ggplot2 instance
    return(g)
  }
}

#' Generate a ggplot2 instance containing an ice rink for a specified league
#'
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   represenation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not the final rink plot needs
#'   to be rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the final
#'   rink plot Default: 'ccw'
#'
#' @return A ggplot2 instance with a full-surface representation of an ice
#'   hockey rink
#'
#' @export
#'
#' @examples
#' geom_hockey(league = "NHL")
#' geom_hockey(league = "IIHF", full_surf = FALSE)
#' geom_hockey(league = "NCAA", rotate = TRUE, rotation_dir = "ccw")
geom_hockey = function(league, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Force the league to be all upper case
  league = toupper(league)

  # Faceoff spot centers for half-sheets. These can be reflected over the y axis
  # for full-surface representations
  if(league %in% c('NHL', 'NWHL')){
    faceoff_spots = list(
      center = c(0, 0),
      spot_1 = c(-69, -22),
      spot_2 = c(-69, 22),
      spot_3 = c(-20, -22),
      spot_4 = c(-20, 22)
    )
  }

  else if(league == 'IIHF'){
    faceoff_spots = list(
      center = c(0, 0),
      spot_1 = c(-20, -8),
      spot_2 = c(-20, 8),
      spot_3 = c(-5.64, -8),
      spot_4 = c(-5.64, 8)
    )
  }

  else if(league == 'NCAA'){
    faceoff_spots = list(
      center = c(0, 0),
      spot_1 = c(-69, -22),
      spot_2 = c(-69, 22),
      spot_3 = c(-20, -22),
      spot_4 = c(-20, 22)
    )
  }

  else {
    stop(message(glue::glue("Sorry, {league} is not a valid league.")))
  }

  if(full_surf){
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

  # Create the initial ggplot2 instance onto which the features will be added
  if(rotate){
    g = ggplot2::ggplot() +
      ggplot2::coord_fixed() +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(color = '#707372'),
        plot.margin = ggplot2::margin(0, -1, 0, -1, "cm"),
        panel.border = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
      ) +
      ggplot2::labs(
        caption = "Plot made via sportyR"
      )
  }

  else {
    g = ggplot2::ggplot() +
      ggplot2::coord_fixed() +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(color = '#707372'),
        plot.margin = ggplot2::margin(-1, 0, -1, 0, "cm"),
        panel.border = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
      ) +
      ggplot2::labs(
        caption = "Plot made via sportyR"
      )
  }

  # Add the boards
  g = hockey_boards(g, league, full_surf, rotate, rotation_dir)

  # Add the center line
  g = hockey_center_line(g, league, full_surf, rotate, rotation_dir)

  # Add the blue line(s)
  g = hockey_blue_line(g, league, full_surf, rotate, rotation_dir)

  # Add the goal line(s)
  g = hockey_goal_line(g, league, full_surf, rotate, rotation_dir)

  # Add the goalkeeper's restricted area (NHL only)
  g = hockey_goalkeepers_restricted_area(g, league, full_surf, rotate, rotation_dir)

  # Add the goal crease(s)
  g = hockey_goal_crease(g, league, full_surf, rotate, rotation_dir)

  # Add the referee's crease
  g = hockey_referee_crease(g, league, full_surf, rotate, rotation_dir)

  # Loop over the faceoff spot centers and add the faceoff spots, their
  # enclosing circles, and their detail lines (when applicable)
  for(spot in 1:length(faceoff_spots)){
    spot_name = names(faceoff_spots[spot])
    center = faceoff_spots[[spot]]

    # Draw all faceoff spots
    g = hockey_faceoff_spot(g, league, center, full_surf, rotate, rotation_dir)

    # Only faceoff spots 1, 2, 7, 8, and center above need circles and details
    if(spot_name %in% c('center', 'spot_1', 'spot_2', 'spot_7', 'spot_8')){
      g = hockey_faceoff_circle(g, league, center, full_surf, rotate, rotation_dir)
      g = hockey_faceoff_lines(g, league, center, full_surf, rotate, rotation_dir)
    }
  }

  # Return the ggplot2 instance that contains the rink plot
  return(g)
}
