#' Generate the dataframe for the points that comprise the grass background
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the field
#' @param goal_line_length The length of the goal line
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the grass added to it
soccer_grass = function(g, league, touchline_length, goal_line_length, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('FIFA', 'MLS', 'PREMIER', 'NWSL')){
    # This gives the green background of the field
    grass = create_rectangle(
      x_min = (-touchline_length / 2) - 6,
      x_max = (touchline_length / 2) + 6,
      y_min = (-goal_line_length / 2) - 3,
      y_max = (goal_line_length / 2) + 3
    )

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      grass = rotate_coords(
        grass,
        rotation_dir
      )
    }

    # Add the field to the ggplot2 instance. It will be green in color
    g = g +
      ggplot2::geom_polygon(data = grass, ggplot2::aes(x, y), fill = '#196f0c')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either FIFA, MLS, or PREMIER), return the
    # ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the touchlines
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the field
#' @param goal_line_length The length of the goal line
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the touchlines added to it
soccer_touchlines = function(g, league, touchline_length, goal_line_length, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('FIFA', 'MLS', 'PREMIER', 'NWSL')){
    # The touchlines are 12cm wide, and the entire line lies within the field of
    # play
    touchline_lower = create_rectangle(
      x_min = -touchline_length / 2,
      x_max = touchline_length / 2,
      y_min = -goal_line_length / 2,
      y_max = (-goal_line_length / 2) + .12
    )

    touchline_upper = create_rectangle(
      x_min = -touchline_length / 2,
      x_max = touchline_length / 2,
      y_min = goal_line_length / 2,
      y_max = (goal_line_length / 2) - .12
    )

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      touchline_lower = rotate_coords(
        touchline_lower,
        rotation_dir
      )

      touchline_upper = rotate_coords(
        touchline_upper,
        rotation_dir
      )
    }

    # Add the touchlines to the ggplot2 instance. They will be white in color
    g = g +
      ggplot2::geom_polygon(data = touchline_lower, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = touchline_upper, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either FIFA, MLS, or PREMIER), return the
    # ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the goal lines
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the field
#' @param goal_line_length The length of the goal line
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the goal lines added to it
soccer_goal_lines = function(g, league, touchline_length, goal_line_length, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('FIFA', 'MLS', 'PREMIER', 'NWSL')){
    # The goal lines are 12cm wide, and the entire line lies within the field of
    # play
    goal_line_left = create_rectangle(
      x_min = -touchline_length / 2,
      x_max = -touchline_length / 2 + .12,
      y_min = -goal_line_length / 2,
      y_max = goal_line_length / 2
    )

    goal_line_right = create_rectangle(
      x_min = touchline_length / 2,
      x_max = touchline_length / 2 - .12,
      y_min = -goal_line_length / 2,
      y_max = goal_line_length / 2
    )

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      goal_line_left = rotate_coords(
        goal_line_left,
        rotation_dir
      )

      goal_line_right = rotate_coords(
        goal_line_right,
        rotation_dir
      )
    }

    # Add the goal lines to the ggplot2 instance. They will be white in color
    g = g +
      ggplot2::geom_polygon(data = goal_line_left, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = goal_line_right, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either FIFA, MLS, or PREMIER), return the
    # ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the halfway line
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param goal_line_length The length of the goal line
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the halfway line added to it
soccer_halfway_line = function(g, league, goal_line_length, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('FIFA', 'MLS', 'PREMIER', 'NWSL')){
    # The halfway line is 12cm wide and spans the entire width of the field. Its
    # center lies along the line x = 0
    halfway_line = create_rectangle(
      x_min = -.06,
      x_max = .06,
      y_min = -goal_line_length / 2,
      y_max = goal_line_length / 2
    )

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      halfway_line = rotate_coords(
        halfway_line,
        rotation_dir
      )
    }

    # Add the halfway line to the ggplot2 instance. It will be white in color
    g = g +
      ggplot2::geom_polygon(data = halfway_line, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either FIFA, MLS, or PREMIER), return the
    # ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the center circle
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the center circle added to it
soccer_center_circle = function(g, league, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('FIFA', 'MLS', 'PREMIER', 'NWSL')){
    # The center circle is a circle of outer radius 9.15m centered at the center
    # of the pitch. It is 12cm thick
    center_circle = rbind(
      create_circle(
        center = c(0, 0),
        start = .5,
        end = 1.5,
        d = 18.3
      ),

      create_circle(
        center = c(0, 0),
        start = 1.5,
        end = .5,
        d = 18.06
      )
    )

    center_circle = rbind(
      center_circle,
      reflect(
        center_circle,
        over_y = TRUE
      )
    )

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      center_circle = rotate_coords(
        center_circle,
        rotation_dir
      )
    }

    # Add the center circle to the ggplot2 instance. It will be white in color
    g = g +
      ggplot2::geom_polygon(data = center_circle, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either FIFA, MLS, or PREMIER), return the
    # ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the corner circle
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the field
#' @param goal_line_length The length of the goal line
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the corner circle added to it
soccer_corner_circle = function(g, league, touchline_length, goal_line_length, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('FIFA', 'MLS', 'PREMIER', 'NWSL')){
    # The corners of the pitch have a quarter circle of radius 1m from the
    # center point of the corner flag. This point is the intersection of the
    # midpoints of the touchline and the goal line. This quarter circle has a
    # width of 12cm
    corner_circle_ll = rbind(
      create_circle(
        center = c((-touchline_length / 2) + .06, (-goal_line_length / 2) + .06),
        start = 0,
        end = .5,
        d = 2
      ),
      create_circle(
        center = c((-touchline_length / 2) + .06, (-goal_line_length / 2) + .06),
        start = .5,
        end = 0,
        d = 1.76
      )
    )

    corner_circle_ul = rbind(
      create_circle(
        center = c((-touchline_length / 2) + .06, (goal_line_length / 2) - .06),
        start = -.5,
        end = 0,
        d = 2
      ),
      create_circle(
        center = c((-touchline_length / 2) + .06, (goal_line_length / 2) - .06),
        start = 0,
        end = -.5,
        d = 1.76
      )
    )

    corner_circle_lr = rbind(
      create_circle(
        center = c((touchline_length / 2) - .06, (-goal_line_length / 2) + .06),
        start = .5,
        end = 1,
        d = 2
      ),
      create_circle(
        center = c((touchline_length / 2) - .06, (-goal_line_length / 2) + .06),
        start = 1,
        end = .5,
        d = 1.76
      )
    )

    corner_circle_ur = rbind(
      create_circle(
        center = c((touchline_length / 2) - .06, (goal_line_length / 2) - .06),
        start = 1,
        end = 1.5,
        d = 2
      ),
      create_circle(
        center = c((touchline_length / 2) - .06, (goal_line_length / 2) - .06),
        start = 1.5,
        end = 1,
        d = 1.76
      )
    )

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      corner_circle_ll = rotate_coords(
        corner_circle_ll,
        rotation_dir
      )

      corner_circle_ul = rotate_coords(
        corner_circle_ul,
        rotation_dir
      )

      corner_circle_lr = rotate_coords(
        corner_circle_lr,
        rotation_dir
      )

      corner_circle_ur = rotate_coords(
        corner_circle_ur,
        rotation_dir
      )
    }

    # Add the corner quarter circles to the ggplot2 instance. It will be white
    # in color
    g = g +
      ggplot2::geom_polygon(data = corner_circle_ll, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = corner_circle_ul, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = corner_circle_lr, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = corner_circle_ur, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either FIFA, MLS, or PREMIER), return the
    # ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the 6-yard boxes
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the field
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the 6-yard boxes added to it
soccer_box_6yd = function(g, league, touchline_length, rotate = FALSE, rotation_dir = TRUE){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('FIFA', 'MLS', 'PREMIER', 'NWSL')){
    # The 6-yard box extends 5.5m into the field from the outer edge of the goal
    # line. It is 12cm in width
    box_6yd_left = data.frame(
      x = c(
        -touchline_length / 2,
        (-touchline_length / 2) + 5.5,
        (-touchline_length / 2) + 5.5,
        -touchline_length / 2,
        -touchline_length / 2,
        (-touchline_length / 2) + 5.38,
        (-touchline_length / 2) + 5.38,
        -touchline_length / 2,
        -touchline_length / 2
      ),

      y = c(
        9.16,
        9.16,
        -9.16,
        -9.16,
        -9.04,
        -9.04,
        9.04,
        9.04,
        9.16
      )
    )

    box_6yd_right = reflect(
      box_6yd_left,
      over_y = TRUE
    )

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      box_6yd_left = rotate_coords(
        box_6yd_left,
        rotation_dir
      )

      box_6yd_right = rotate_coords(
        box_6yd_right,
        rotation_dir
      )
    }

    # Add the 6-yard boxes to the ggplot2 instance. They will be white in color
    g = g +
      ggplot2::geom_polygon(data = box_6yd_left, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = box_6yd_right, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either FIFA, MLS, or PREMIER), return the
    # ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the 18-yard boxes
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the field
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the 18-yard boxes added to it
soccer_box_18yd = function(g, league, touchline_length, rotate = FALSE, rotation_dir = TRUE){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('FIFA', 'MLS', 'PREMIER', 'NWSL')){
    # The 18-yard box extends 5.5m into the field from the outer edge of the
    # goal line. It is 12cm in width
    box_18yd_left = data.frame(
      x = c(
        -touchline_length / 2,
        (-touchline_length / 2) + 16.5,
        (-touchline_length / 2) + 16.5,
        -touchline_length / 2,
        -touchline_length / 2,
        (-touchline_length / 2) + 16.38,
        (-touchline_length / 2) + 16.38,
        -touchline_length / 2,
        -touchline_length / 2
      ),

      y = c(
        20.16,
        20.16,
        -20.16,
        -20.16,
        -20.04,
        -20.04,
        20.04,
        20.04,
        20.16
      )
    )

    box_18yd_right = reflect(
      box_18yd_left,
      over_y = TRUE
    )

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      box_18yd_left = rotate_coords(
        box_18yd_left,
        rotation_dir
      )

      box_18yd_right = rotate_coords(
        box_18yd_right,
        rotation_dir
      )
    }

    # Add the 18-yard boxes to the ggplot2 instance. They will be white in color
    g = g +
      ggplot2::geom_polygon(data = box_18yd_left, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = box_18yd_right, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either FIFA, MLS, or PREMIER), return the
    # ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the penalty arc
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the field
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the penalty arcs added to it
soccer_penalty_arc = function(g, league, touchline_length, rotate = FALSE, rotation_dir = TRUE){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('FIFA', 'MLS', 'PREMIER', 'NWSL')){
    # The penalty arc are the points that lie outside the 18-yard box along a
    # circle of width 12cm that have an outer radius of 9.15m from the center of
    # the penalty mark. The penalty mark is 11m from the outer edge of the goal
    # line.
    penalty_arc_left = rbind(
      create_circle(
        center = c((-touchline_length / 2) + 11, 0),
        start = .5,
        end = -.5,
        d = 18.3
      ),
      create_circle(
        center = c((-touchline_length / 2) + 11, 0),
        start = -.5,
        end = .5,
        d = 18.06
      )
    )

    # Remove points that are inside of the 18-yard box
    penalty_arc_left = penalty_arc_left[penalty_arc_left$x >= (-touchline_length / 2) + 16.5, ]

    penalty_arc_right = reflect(
      penalty_arc_left,
      over_y = TRUE
    )

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      penalty_arc_left = rotate_coords(
        penalty_arc_left,
        rotation_dir
      )

      penalty_arc_right = rotate_coords(
        penalty_arc_right,
        rotation_dir
      )
    }

    # Add the penalty arcs to the ggplot2 instance. They will be white in color
    g = g +
      ggplot2::geom_polygon(data = penalty_arc_left, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = penalty_arc_right, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either FIFA, MLS, or PREMIER), return the
    # ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the penalty mark
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the field
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the penalty mark added to it
soccer_penalty_mark = function(g, league, touchline_length, rotate = FALSE, rotation_dir = TRUE){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('FIFA', 'MLS', 'PREMIER', 'NWSL')){
    # The penalty mark is 11m from the outside edge of the goal line. It is
    # 30.48cm (12") in diameter
    penalty_mark_left = create_circle(
      center = c((-touchline_length / 2) + 11, 0),
      start = 0,
      end = 2,
      d = in_to_m(12)
    )

    penalty_mark_right = reflect(
      penalty_mark_left,
      over_y = TRUE
    )

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      penalty_mark_left = rotate_coords(
        penalty_mark_left,
        rotation_dir
      )

      penalty_mark_right = rotate_coords(
        penalty_mark_right,
        rotation_dir
      )
    }

    # Add the penalty marks to the ggplot2 instance. They will be white in color
    g = g +
      ggplot2::geom_polygon(data = penalty_mark_left, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = penalty_mark_right, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either FIFA, MLS, or PREMIER), return the
    # ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the center mark
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the field
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the center mark added to it
soccer_center_mark = function(g, league, touchline_length, rotate = FALSE, rotation_dir = TRUE){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('FIFA', 'MLS', 'PREMIER', 'NWSL')){
    # The center mark is located at the center of the pitch, with its center at
    # (0, 0). It is 30.48cm (12") in diameter
    center_mark = create_circle(
      center = c(0, 0),
      start = 0,
      end = 2,
      d = in_to_m(12)
    )

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      center_mark = rotate_coords(
        center_mark,
        rotation_dir
      )
    }

    # Add the center mark to the ggplot2 instance. It will be white in color
    g = g +
      ggplot2::geom_polygon(data = center_mark, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either FIFA, MLS, or PREMIER), return the
    # ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the goal
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param touchline_length The length of touchline. This should be the entire
#'   length (both halves) of the field
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the goal added to it
soccer_goal = function(g, league, touchline_length, rotate = FALSE, rotation_dir = TRUE){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('FIFA', 'MLS', 'PREMIER', 'NWSL')){
    # The goal is centered on the goal line, with the inner edge of the posts
    # being 7.32m apart. The posts are each 12cm in width, and the goal extends
    # backwards a depth of 1.7m
    goal_left = data.frame(
      x = c(
        -touchline_length / 2,
        (-touchline_length / 2) - 1.7,
        (-touchline_length / 2) - 1.7,
        -touchline_length / 2,
        -touchline_length / 2,
        (-touchline_length / 2) - 1.82,
        (-touchline_length / 2) - 1.82,
        -touchline_length / 2,
        -touchline_length / 2
      ),

      y = c(
        3.66,
        3.66,
        -3.66,
        -3.66,
        -3.78,
        -3.78,
        3.78,
        3.78,
        3.66
      )
    )

    goal_right = reflect(
      goal_left,
      over_y = TRUE
    )

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      goal_left = rotate_coords(
        goal_left,
        rotation_dir
      )

      goal_right = rotate_coords(
        goal_right,
        rotation_dir
      )
    }

    # Add the goals to the ggplot2 instance. They will be white in color
    g = g +
      ggplot2::geom_polygon(data = goal_left, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = goal_right, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either FIFA, MLS, or PREMIER), return the
    # ggplot2 instance
    return(g)
  }
}

#' Generate a ggplot2 instance containing a regulation soccer pitch for a
#' specified league. Since soccer pitches can vary in dimension, the dimensions
#' must be supplied. These values will default to the maximum values according
#' to the Laws of the Game
#'
#' @param league The league for which to draw the surface
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the field
#' @param goal_line_length The length of the goal line
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#'
#' @return A ggplot2 instance with a full-surface representation of a soccer
#'   pitch
#'
#' @export
#'
#' @examples
#' geom_soccer(league = "MLS")
#' geom_soccer(league = "PREMIER", rotate = TRUE, rotation_dir = "ccw")
geom_soccer = function(league, touchline_length = 120, goal_line_length = 90, rotate = FALSE, rotation_dir = 'ccw'){
  # Force the league to be all upper case
  league = toupper(league)

  if(league %in% c('FIFA', 'MLS', 'PREMIER', 'NWSL')){
    # Create the initial ggplot2 instance onto which the features will be added
    g = ggplot2::ggplot() +
      ggplot2::coord_fixed() +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(color = '#707372'),
        plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
        plot.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
      ) +
      ggplot2::labs(
        caption = "Plot made via sportyR"
      )

    # Add the grass
    g = soccer_grass(g, league, touchline_length, goal_line_length, rotate, rotation_dir)

    # Add the touchlines
    g = soccer_touchlines(g, league, touchline_length, goal_line_length, rotate, rotation_dir)

    # Add the goal lines
    g = soccer_goal_lines(g, league, touchline_length, goal_line_length, rotate, rotation_dir)

    # Add the halfway line
    g = soccer_halfway_line(g, league, goal_line_length, rotate, rotation_dir)

    # Add the corner quarter circles
    g = soccer_corner_circle(g, league, touchline_length, goal_line_length, rotate, rotation_dir)

    # Add the center circle
    g = soccer_center_circle(g, league, rotate, rotation_dir)

    # Add the 6-yard boxes
    g = soccer_box_6yd(g, league, touchline_length, rotate, rotation_dir)

    # Add the 18-yard boxes
    g = soccer_box_18yd(g, league, touchline_length, rotate, rotation_dir)

    # Add the penalty arcs
    g = soccer_penalty_arc(g, league, touchline_length, rotate, rotation_dir)

    # Add the penalty marks
    g = soccer_penalty_mark(g, league, touchline_length, rotate, rotation_dir)

    # Add the center mark
    g = soccer_center_mark(g, league, touchline_length, rotate, rotation_dir)

    # Add the goals
    g = soccer_goal(g, league, touchline_length, rotate, rotation_dir)

    return(g)
  }

  else {
    stop(message(glue::glue("Sorry, {league} is not a valid league.")))
  }
}

