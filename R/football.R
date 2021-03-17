usethis::use_package("ggplot2")

#' This draws a football field in its standard coordinate system, with (0, 0)
#' being the bottom left corner of the left-most endzone. Each unit on the
#' coordinate system corresponds to 1 yard. The entire field will always be
#' provided, although this may change in a future iteration

#' Generate the dataframe for the points that comprise the grass background
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the grass added to it
football_grass = function(g, league, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NFL', 'NCAA')){
    # This gives the green background of the field
    grass = create_rectangle(
      x_min = -4,
      x_max = 124,
      y_min = -4,
      y_max = 57.3
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
    # If the league isn't valid (i.e. either NFL or NCAA), return the ggplot2
    # instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the sidelines
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the sidelines added to it
football_sideline = function(g, league, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NFL', 'NCAA')){
    # The sidelines are solid white and 6' (2 yards) in width. Their interior
    # edges form the boundary line of the field
    sideline_lower = create_rectangle(
      x_min = -2,
      x_max = 120,
      y_min = -2,
      y_max = 0
    )

    sideline_upper = create_rectangle(
      x_min = -2,
      x_max = 120,
      y_min = 53.3,
      y_max = 55.3
    )

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      sideline_lower = rotate_coords(
        sideline_lower,
        rotation_dir
      )

      sideline_upper = rotate_coords(
        sideline_upper,
        rotation_dir
      )
    }

    # Add the sidelines to the ggplot2 instance. They will be white in color
    g = g +
      ggplot2::geom_polygon(data = sideline_lower, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = sideline_upper, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NFL or NCAA), return the ggplot2
    # instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the endline
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the endline added to it
football_endline = function(g, league, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NFL', 'NCAA')){
    # The endlines are solid white and 6' (2 yards) in width. Their interior
    # edges form the boundary line of the field
    endline_left = create_rectangle(
      x_min = -2,
      x_max = 0,
      y_min = -2,
      y_max = 55.3
    )

    endline_right = create_rectangle(
      x_min = 120,
      x_max = 122,
      y_min = -2,
      y_max = 55.3
    )

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      endline_left = rotate_coords(
        endline_left,
        rotation_dir
      )

      endline_right = rotate_coords(
        endline_right,
        rotation_dir
      )
    }

    # Add the endlines to the ggplot2 instance. They will be white in color
    g = g +
      ggplot2::geom_polygon(data = endline_left, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = endline_right, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NFL or NCAA), return the ggplot2
    # instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the goal line
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the goal line added to it
football_goal_line = function(g, league, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NFL', 'NCAA')){
    # The goal lines are solid white and 8" in width and span the entire width
    # of the field. Their interior edges form the boundary line of the field
    goal_line_left = create_rectangle(
      x_min = 10 - inches_to_yd(8),
      x_max = 10,
      y_min = 0,
      y_max = 55.3
    )

    goal_line_right = create_rectangle(
      x_min = 110,
      x_max = 110 + inches_to_yd(8),
      y_min = 0,
      y_max = 55.3
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
    # If the league isn't valid (i.e. either NFL or NCAA), return the ggplot2
    # instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the yard lines
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the yard lines added to it
football_yard_markings = function(g, league, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league == 'NFL'){
    # The lines are to be placed 8" from the interior of the sidelines, and be
    # 4" wide. At 5-yard intervals across the field, the lines should stretch
    # the width of the field, with a 2' long by 4" wide hash 70' 9" from the
    # interior of the nearest boundary. At 1-yard intervals between these
    # markings at 5-yard intervals, a 2' tall by 4" wide marker should be placed
    # 8" from the interior of the sideline as well as 70' 9" from the interior
    # of the sideline (and extending back towards the sideline)
    for(yardage in 11:109){
      if(yardage %% 5 == 0){
        # At 5-yard intervals, the line should stretch the width of the field,
        # with the inbound line marker 70'9" from the interior of the sideline
        # boundary
        yard_marking = data.frame(
          x = c(
            # Start 8" from the sideline
            yardage - inches_to_yd(2),

            # Lower inbound line marker
            yardage - inches_to_yd(2),
            yardage - inches_to_yd(2) - inches_to_yd(10),
            yardage - inches_to_yd(2) - inches_to_yd(10),
            yardage - inches_to_yd(2),

            # Upper inbound line marker
            yardage - inches_to_yd(2),
            yardage - inches_to_yd(2) - inches_to_yd(10),
            yardage - inches_to_yd(2) - inches_to_yd(10),
            yardage - inches_to_yd(2),

            # Top
            yardage - inches_to_yd(2),

            # Crossover
            yardage + inches_to_yd(2),

            # Upper inbound line marker
            yardage + inches_to_yd(2),
            yardage + inches_to_yd(2) + inches_to_yd(10),
            yardage + inches_to_yd(2) + inches_to_yd(10),
            yardage + inches_to_yd(2),

            # Lower inbound line marker
            yardage + inches_to_yd(2),
            yardage + inches_to_yd(2) + inches_to_yd(10),
            yardage + inches_to_yd(2) + inches_to_yd(10),
            yardage + inches_to_yd(2),

            # Return to bottom
            yardage + inches_to_yd(2),

            # Return to start
            yardage - inches_to_yd(2)
          ),

          y = c(
            # Start 8" from the sideline
            inches_to_yd(8),

            # Lower inbound line marker
            ft_to_yd(70) + inches_to_yd(5),
            ft_to_yd(70) + inches_to_yd(5),
            ft_to_yd(70) + inches_to_yd(9),
            ft_to_yd(70) + inches_to_yd(9),

            # Upper inbound line marker
            53.3 - ft_to_yd(70) - inches_to_yd(9),
            53.3 - ft_to_yd(70) - inches_to_yd(9),
            53.3 - ft_to_yd(70) - inches_to_yd(5),
            53.3 - ft_to_yd(70) - inches_to_yd(5),

            # Top
            53.3 - inches_to_yd(8),

            # Crossover
            53.3 - inches_to_yd(8),

            # Upper inbound line marker
            53.3 - ft_to_yd(70) - inches_to_yd(9),
            53.3 - ft_to_yd(70) - inches_to_yd(9),
            53.3 - ft_to_yd(70) - inches_to_yd(5),
            53.3 - ft_to_yd(70) - inches_to_yd(5),

            # Lower inbound line marker
            ft_to_yd(70) + inches_to_yd(5),
            ft_to_yd(70) + inches_to_yd(5),
            ft_to_yd(70) + inches_to_yd(9),
            ft_to_yd(70) + inches_to_yd(9),

            # Return to bottom
            inches_to_yd(8),

            # Return to start
            inches_to_yd(8)
          )
        )

        if(rotate){
          # If the desired output needs to be rotated, rotate the coordinates
          yard_marking = rotate_coords(
            yard_marking,
            rotation_dir
          )
        }

        # Add the yard marking to the plot. They will be white in color
        g = g +
          ggplot2::geom_polygon(data = yard_marking, ggplot2::aes(x, y), fill = '#ffffff')
      }

      else {
        # At 1-yard intervals, the line should be 2' long. The line should
        # appear at the bottom (b) and top (t) of the field inside the 6' wide
        # boundary, and also appear at 70'9" from the nearest boundary and
        # extending from this point towards that boundary (l and u)
        yard_marking_b = create_rectangle(
          x_min = yardage - inches_to_yd(2),
          x_max = yardage + inches_to_yd(2),
          y_min = inches_to_yd(8),
          y_max = ft_to_yd(2) + inches_to_yd(8)
        )

        yard_marking_l = create_rectangle(
          x_min = yardage - inches_to_yd(2),
          x_max = yardage + inches_to_yd(2),
          y_min = ft_to_yd(68) + inches_to_yd(9),
          y_max = ft_to_yd(70) + inches_to_yd(9)
        )

        yard_marking_u = create_rectangle(
          x_min = yardage - inches_to_yd(2),
          x_max = yardage + inches_to_yd(2),
          y_min = 53.3 - ft_to_yd(70) - inches_to_yd(9),
          y_max = 53.3 - ft_to_yd(68) - inches_to_yd(9)
        )

        yard_marking_t = create_rectangle(
          x_min = yardage - inches_to_yd(2),
          x_max = yardage + inches_to_yd(2),
          y_min = 53.3 - ft_to_yd(2) - inches_to_yd(8),
          y_max = 53.3 - inches_to_yd(8)
        )

        if(rotate){
          # If the desired output needs to be rotated, rotate the coordinates
          yard_marking_b = rotate_coords(
            yard_marking_b,
            rotation_dir
          )

          yard_marking_l = rotate_coords(
            yard_marking_l,
            rotation_dir
          )

          yard_marking_u = rotate_coords(
            yard_marking_u,
            rotation_dir
          )

          yard_marking_t = rotate_coords(
            yard_marking_t,
            rotation_dir
          )
        }

        # Add the yard markings to the plot. They will be white in color
        g = g +
          ggplot2::geom_polygon(data = yard_marking_b, ggplot2::aes(x, y), fill = '#ffffff') +
          ggplot2::geom_polygon(data = yard_marking_l, ggplot2::aes(x, y), fill = '#ffffff') +
          ggplot2::geom_polygon(data = yard_marking_u, ggplot2::aes(x, y), fill = '#ffffff') +
          ggplot2::geom_polygon(data = yard_marking_t, ggplot2::aes(x, y), fill = '#ffffff')
      }
    }

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'NCAA'){
    # The lines are to be placed 4" from the interior of the sidelines, and be
    # 4" wide. At 5-yard intervals across the field, the lines should stretch
    # the width of the field, with a 2' long by 4" wide hash 60' from the
    # interior of the nearest boundary. At 1-yard intervals between these
    # markings at 5-yard intervals, a 2' tall by 4" wide marker should be placed
    # 4" from the interior of the sideline as well as 60' from the interior of
    # the sideline (and extending back towards the sideline)
    for(yardage in 11:109){
      if(yardage %% 5 == 0){
        # At 5-yard intervals, the line should stretch the width of the field,
        # with the inbound line marker 70'9" from the interior of the sideline
        # boundary
        yard_marking = data.frame(
          x = c(
            # Start 4" from the sideline
            yardage - inches_to_yd(2),

            # Lower inbound line marker
            yardage - inches_to_yd(2),
            yardage - inches_to_yd(2) - inches_to_yd(10),
            yardage - inches_to_yd(2) - inches_to_yd(10),
            yardage - inches_to_yd(2),

            # Upper inbound line marker
            yardage - inches_to_yd(2),
            yardage - inches_to_yd(2) - inches_to_yd(10),
            yardage - inches_to_yd(2) - inches_to_yd(10),
            yardage - inches_to_yd(2),

            # Top
            yardage - inches_to_yd(2),

            # Crossover
            yardage + inches_to_yd(2),

            # Upper inbound line marker
            yardage + inches_to_yd(2),
            yardage + inches_to_yd(2) + inches_to_yd(10),
            yardage + inches_to_yd(2) + inches_to_yd(10),
            yardage + inches_to_yd(2),

            # Lower inbound line marker
            yardage + inches_to_yd(2),
            yardage + inches_to_yd(2) + inches_to_yd(10),
            yardage + inches_to_yd(2) + inches_to_yd(10),
            yardage + inches_to_yd(2),

            # Return to bottom
            yardage + inches_to_yd(2),

            # Return to start
            yardage - inches_to_yd(2)
          ),

          y = c(
            # Start 4" from the sideline
            inches_to_yd(4),

            # Lower inbound line marker
            ft_to_yd(60),
            ft_to_yd(60),
            ft_to_yd(60) + inches_to_yd(4),
            ft_to_yd(60) + inches_to_yd(4),

            # Upper inbound line marker
            53.3 - ft_to_yd(60) - inches_to_yd(4),
            53.3 - ft_to_yd(60) - inches_to_yd(4),
            53.3 - ft_to_yd(60),
            53.3 - ft_to_yd(60),

            # Top
            53.3 - inches_to_yd(4),

            # Crossover
            53.3 - inches_to_yd(4),

            # Upper inbound line marker
            53.3 - ft_to_yd(60) - inches_to_yd(4),
            53.3 - ft_to_yd(60) - inches_to_yd(4),
            53.3 - ft_to_yd(60),
            53.3 - ft_to_yd(60),

            # Lower inbound line marker
            ft_to_yd(60) + inches_to_yd(4),
            ft_to_yd(60) + inches_to_yd(4),
            ft_to_yd(60),
            ft_to_yd(60),

            # Return to bottom
            inches_to_yd(4),

            # Return to start
            inches_to_yd(4)
          )
        )

        if(rotate){
          # If the desired output needs to be rotated, rotate the coordinates
          yard_marking = rotate_coords(
            yard_marking,
            rotation_dir
          )
        }

        # Add the yard marking to the plot. They will be white in color
        g = g +
          ggplot2::geom_polygon(data = yard_marking, ggplot2::aes(x, y), fill = '#ffffff')
      }

      else {
        # At 1-yard intervals, the line should be 2' long. The line should
        # appear at the bottom (b) and top (t) of the field inside the 6' wide
        # boundary, and also appear at 70'9" from the nearest boundary and
        # extending from this point towards that boundary (l and u)
        yard_marking_b = create_rectangle(
          x_min = yardage - inches_to_yd(2),
          x_max = yardage + inches_to_yd(2),
          y_min = inches_to_yd(4),
          y_max = ft_to_yd(2) + inches_to_yd(4)
        )

        yard_marking_l = create_rectangle(
          x_min = yardage - inches_to_yd(2),
          x_max = yardage + inches_to_yd(2),
          y_min = ft_to_yd(58),
          y_max = ft_to_yd(60)
        )

        yard_marking_u = create_rectangle(
          x_min = yardage - inches_to_yd(2),
          x_max = yardage + inches_to_yd(2),
          y_min = 53.3 - ft_to_yd(60),
          y_max = 53.3 - ft_to_yd(58)
        )

        yard_marking_t = create_rectangle(
          x_min = yardage - inches_to_yd(2),
          x_max = yardage + inches_to_yd(2),
          y_min = 53.3 - ft_to_yd(2) - inches_to_yd(4),
          y_max = 53.3 - inches_to_yd(4)
        )

        if(rotate){
          # If the desired output needs to be rotated, rotate the coordinates
          yard_marking_b = rotate_coords(
            yard_marking_b,
            rotation_dir
          )

          yard_marking_l = rotate_coords(
            yard_marking_l,
            rotation_dir
          )

          yard_marking_u = rotate_coords(
            yard_marking_u,
            rotation_dir
          )

          yard_marking_t = rotate_coords(
            yard_marking_t,
            rotation_dir
          )
        }

        # Add the yard markings to the plot. They will be white in color
        g = g +
          ggplot2::geom_polygon(data = yard_marking_b, ggplot2::aes(x, y), fill = '#ffffff') +
          ggplot2::geom_polygon(data = yard_marking_l, ggplot2::aes(x, y), fill = '#ffffff') +
          ggplot2::geom_polygon(data = yard_marking_u, ggplot2::aes(x, y), fill = '#ffffff') +
          ggplot2::geom_polygon(data = yard_marking_t, ggplot2::aes(x, y), fill = '#ffffff')
      }
    }

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NFL or NCAA), return the ggplot2
    # instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the try lines
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the try lines added to it
football_try_markings = function(g, league, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league == 'NFL'){
    # The NFL try line is at the 2-yard line, is 1-yard in length (centered at
    # the midpoint of the goal line), and has a 2" width
    try_line_left = create_rectangle(
      x_min = 12 - inches_to_yd(2),
      x_max = 12 + inches_to_yd(2),
      y_min = ft_to_yd(70) + inches_to_yd(9) + ft_to_yd(9) + inches_to_yd(3) - .5,
      y_max = ft_to_yd(70) + inches_to_yd(9) + ft_to_yd(9) + inches_to_yd(3) + .5
    )

    try_line_right = create_rectangle(
      x_min = 108 - inches_to_yd(2),
      x_max = 108 + inches_to_yd(2),
      y_min = ft_to_yd(70) + inches_to_yd(9) + ft_to_yd(9) + inches_to_yd(3) - .5,
      y_max = ft_to_yd(70) + inches_to_yd(9) + ft_to_yd(9) + inches_to_yd(3) + .5
    )

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      try_line_left = rotate_coords(
        try_line_left,
        rotation_dir
      )

      try_line_right = rotate_coords(
        try_line_right,
        rotation_dir
      )
    }

    # Add the goal lines to the ggplot2 instance. They will be white in color
    g = g +
      ggplot2::geom_polygon(data = try_line_left, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = try_line_right, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'NCAA'){
    # The NCAA try line is at the 3-yard line, is 1-yard in length (centered at
    # the midpoint of the goal line), and has a 2" width
    try_line_left = create_rectangle(
      x_min = 13 - inches_to_yd(2),
      x_max = 13 + inches_to_yd(2),
      y_min = ft_to_yd(80) - .5,
      y_max = ft_to_yd(80) + .5
    )

    try_line_right = create_rectangle(
      x_min = 107 - inches_to_yd(2),
      x_max = 107 + inches_to_yd(2),
      y_min = ft_to_yd(80) - .5,
      y_max = ft_to_yd(80) + .5
    )

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      try_line_left = rotate_coords(
        try_line_left,
        rotation_dir
      )

      try_line_right = rotate_coords(
        try_line_right,
        rotation_dir
      )
    }

    # Add the goal lines to the ggplot2 instance. They will be white in color
    g = g +
      ggplot2::geom_polygon(data = try_line_left, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = try_line_right, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NFL or NCAA), return the ggplot2
    # instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the directional arrows
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the directional arrows added to it
football_directional_arrows = function(g, league, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The arrow has two sides of 36", and one side of 18". The Pythagorean Theorem
  # can be used to determine the height (using half the length of the base,
  # which in this case is 18")
  arrow_width = sqrt((inches_to_yd(36) ** 2) - (inches_to_yd(9) ** 2))

  if(league == 'NFL'){
    for(yardage in seq(20, 100, 10)){
      # The directional arrows should not be drawn at the 50-yard line. Other
      # than that, an arrow should be drawn every 10 yards

      if(yardage < 60){
        # Draw the directional arrow that's below the middle point of the field
        directional_arrow_lower = data.frame(
          x = c(
            # The numbers are 1' from the outer edge of the yard line, which is
            # 2" wide. The number itself is 4' wide, and the number is 6" off
            # the outside edge of the number
            yardage - inches_to_yd(2) - ft_to_yd(5.5),
            yardage - inches_to_yd(2) - ft_to_yd(5.5),
            yardage - inches_to_yd(2) - ft_to_yd(5.5) - arrow_width,
            yardage - inches_to_yd(2) - ft_to_yd(5.5)
          ),

          y = c(
            # The bottom of the numbers must be 12 yards (36') off the interior
            # of the sideline. The number itself is then 6' tall, and the top
            # tip of the arrow is 15" below this line
            14 - inches_to_yd(15),
            14 - inches_to_yd(15) - inches_to_yd(18),
            14 - inches_to_yd(15) - inches_to_yd(9),
            14 - inches_to_yd(15)
          )
        )

        # Draw the directional arrow that's above the middle point of the field
        directional_arrow_upper = data.frame(
          x = c(
            # The numbers are 1' from the outer edge of the yard line, which is
            # 2" wide. The number itself is 4' wide, and the number is 6" off
            # the outside edge of the number
            yardage - inches_to_yd(2) - ft_to_yd(5.5),
            yardage - inches_to_yd(2) - ft_to_yd(5.5),
            yardage - inches_to_yd(2) - ft_to_yd(5.5) - arrow_width,
            yardage - inches_to_yd(2) - ft_to_yd(5.5)
          ),

          y = c(
            # The bottom of the numbers must be 12 yards (36') off the interior
            # of the sideline. The number itself is then 6' tall, and the top
            # tip of the arrow is 15" below this line
            53.3 - 14 + inches_to_yd(15),
            53.3 - 14 + inches_to_yd(15) + inches_to_yd(18),
            53.3 - 14 + inches_to_yd(15) + inches_to_yd(9),
            53.3 - 14 + inches_to_yd(15)
          )
        )
      }

      else if(yardage > 60){
        # Draw the directional arrow that's below the middle point of the field
        directional_arrow_lower = data.frame(
          x = c(
            # The numbers are 1' from the outer edge of the yard line, which is
            # 2" wide. The number itself is 4' wide, and the number is 6" off
            # the outside edge of the number
            yardage + inches_to_yd(2) + ft_to_yd(5.5),
            yardage + inches_to_yd(2) + ft_to_yd(5.5),
            yardage + inches_to_yd(2) + ft_to_yd(5.5) + arrow_width,
            yardage + inches_to_yd(2) + ft_to_yd(5.5)
          ),

          y = c(
            # The bottom of the numbers must be 12 yards (36') off the interior
            # of the sideline. The number itself is then 6' tall, and the top
            # tip of the arrow is 15" below this line
            14 - inches_to_yd(15),
            14 - inches_to_yd(15) - inches_to_yd(18),
            14 - inches_to_yd(15) - inches_to_yd(9),
            14 - inches_to_yd(15)
          )
        )

        # Draw the directional arrow that's above the middle point of the field
        directional_arrow_upper = data.frame(
          x = c(
            # The numbers are 1' from the outer edge of the yard line, which is
            # 2" wide. The number itself is 4' wide, and the number is 6" off
            # the outside edge of the number
            yardage + inches_to_yd(2) + ft_to_yd(5.5),
            yardage + inches_to_yd(2) + ft_to_yd(5.5),
            yardage + inches_to_yd(2) + ft_to_yd(5.5) + arrow_width,
            yardage + inches_to_yd(2) + ft_to_yd(5.5)
          ),

          y = c(
            # The bottom of the numbers must be 12 yards (36') off the interior
            # of the sideline. The number itself is then 6' tall, and the top
            # tip of the arrow is 15" below this line
            53.3 - 14 + inches_to_yd(15),
            53.3 - 14 + inches_to_yd(15) + inches_to_yd(18),
            53.3 - 14 + inches_to_yd(15) + inches_to_yd(9),
            53.3 - 14 + inches_to_yd(15)
          )
        )
      }

      if(rotate){
        # If the desired output needs to be rotated, rotate the coordinates
        directional_arrow_lower = rotate_coords(
          directional_arrow_lower,
          rotation_dir
        )

        directional_arrow_upper = rotate_coords(
          directional_arrow_upper,
          rotation_dir
        )
      }

      # Add the directional arrow to the plot. They will be white in color
      g = g +
        ggplot2::geom_polygon(data = directional_arrow_lower, ggplot2::aes(x, y), fill = '#ffffff') +
        ggplot2::geom_polygon(data = directional_arrow_upper, ggplot2::aes(x, y), fill = '#ffffff')
    }

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'NCAA'){
    for(yardage in seq(20, 100, 10)){
      # The directional arrows should not be drawn at the 50-yard line. Other
      # than that, an arrow should be drawn every 10 yards

      if(yardage < 60){
        # Draw the directional arrow that's below the middle point of the field
        directional_arrow_lower = data.frame(
          x = c(
            # The numbers are 1' from the outer edge of the yard line, which is
            # 2" wide. The number itself is 4' wide, and the number is 6" off
            # the outside edge of the number
            yardage - inches_to_yd(2) - ft_to_yd(5.5),
            yardage - inches_to_yd(2) - ft_to_yd(5.5),
            yardage - inches_to_yd(2) - ft_to_yd(5.5) - arrow_width,
            yardage - inches_to_yd(2) - ft_to_yd(5.5)
          ),

          y = c(
            # The top of the numbers must be 9 yards (27') off the interior of
            # the sideline. The number itself is 6' tall, and the top tip of the
            # arrow is 15" below this line
            9 - inches_to_yd(15),
            9 - inches_to_yd(15) - inches_to_yd(18),
            9 - inches_to_yd(15) - inches_to_yd(9),
            9 - inches_to_yd(15)
          )
        )

        # Draw the directional arrow that's above the middle point of the field
        directional_arrow_upper = data.frame(
          x = c(
            # The numbers are 1' from the outer edge of the yard line, which is
            # 2" wide. The number itself is 4' wide, and the number is 6" off
            # the outside edge of the number
            yardage - inches_to_yd(2) - ft_to_yd(5.5),
            yardage - inches_to_yd(2) - ft_to_yd(5.5),
            yardage - inches_to_yd(2) - ft_to_yd(5.5) - arrow_width,
            yardage - inches_to_yd(2) - ft_to_yd(5.5)
          ),

          y = c(
            # The top of the numbers must be 9 yards (27') off the interior of
            # the sideline. The number itself is 6' tall, and the top tip of the
            # arrow is 15" below this line
            53.3 - 9 + inches_to_yd(15),
            53.3 - 9 + inches_to_yd(15) + inches_to_yd(18),
            53.3 - 9 + inches_to_yd(15) + inches_to_yd(9),
            53.3 - 9 + inches_to_yd(15)
          )
        )
      }

      else if(yardage > 60){
        # Draw the directional arrow that's below the middle point of the field
        directional_arrow_lower = data.frame(
          x = c(
            # The numbers are 1' from the outer edge of the yard line, which is
            # 2" wide. The number itself is 4' wide, and the number is 6" off
            # the outside edge of the number
            yardage + inches_to_yd(2) + ft_to_yd(5.5),
            yardage + inches_to_yd(2) + ft_to_yd(5.5),
            yardage + inches_to_yd(2) + ft_to_yd(5.5) + arrow_width,
            yardage + inches_to_yd(2) + ft_to_yd(5.5)
          ),

          y = c(
            # The top of the numbers must be 9 yards (27') off the interior of
            # the sideline. The number itself is 6' tall, and the top tip of the
            # arrow is 15" below this line
            9 - inches_to_yd(15),
            9 - inches_to_yd(15) - inches_to_yd(18),
            9 - inches_to_yd(15) - inches_to_yd(9),
            9 - inches_to_yd(15)
          )
        )

        # Draw the directional arrow that's above the middle point of the field
        directional_arrow_upper = data.frame(
          x = c(
            # The numbers are 1' from the outer edge of the yard line, which is
            # 2" wide. The number itself is 4' wide, and the number is 6" off
            # the outside edge of the number
            yardage + inches_to_yd(2) + ft_to_yd(5.5),
            yardage + inches_to_yd(2) + ft_to_yd(5.5),
            yardage + inches_to_yd(2) + ft_to_yd(5.5) + arrow_width,
            yardage + inches_to_yd(2) + ft_to_yd(5.5)
          ),

          y = c(
            # The top of the numbers must be 9 yards (27') off the interior of
            # the sideline. The number itself is 6' tall, and the top tip of the
            # arrow is 15" below this line
            53.3 - 9 + inches_to_yd(15),
            53.3 - 9 + inches_to_yd(15) + inches_to_yd(18),
            53.3 - 9 + inches_to_yd(15) + inches_to_yd(9),
            53.3 - 9 + inches_to_yd(15)
          )
        )
      }

      if(rotate){
        # If the desired output needs to be rotated, rotate the coordinates
        directional_arrow_lower = rotate_coords(
          directional_arrow_lower,
          rotation_dir
        )

        directional_arrow_upper = rotate_coords(
          directional_arrow_upper,
          rotation_dir
        )
      }

      # Add the directional arrow to the plot. It will be white in color
      g = g +
        ggplot2::geom_polygon(data = directional_arrow_lower, ggplot2::aes(x, y), fill = '#ffffff') +
        ggplot2::geom_polygon(data = directional_arrow_upper, ggplot2::aes(x, y), fill = '#ffffff')
    }

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NFL or NCAA), return the ggplot2
    # instance
    return(g)
  }

}

#' Add the yardline-marking numbers at 10-yard intervals
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#' @return A ggplot2 instance with the yardline-marking numbers added to it
football_yard_numbers = function(g, league, rotate = FALSE, rotation_dir = 'ccw'){
  # Set the default text orientation for the lower and upper yardline-marking
  # numbers
  angle_l = 0
  angle_u = 180

  if(rotate){
    if(rotation_dir == 'ccw'){
      # If the rotation is counter-clockwise, rotate the numbers 90 degrees
      # counter-clockwise
      angle_l = angle_l + 90
      angle_u = angle_u + 90
    }

    else {
      # Otherwise, rotate the numbers 90 degrees clockwise
      angle_l = angle_l - 90
      angle_u = angle_u - 90
    }
  }

  if(league == 'NFL'){
    # Add the field numbers to the plot
    g = g +
      ggplot2::annotate('text', label = '1', x = 20 - ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '2', x = 30 - ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '3', x = 40 - ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '4', x = 50 - ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '5', x = 60 - ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '4', x = 70 - ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '3', x = 80 - ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '2', x = 90 - ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '1', x = 100 - ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 20 + ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 30 + ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 40 + ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 50 + ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 60 + ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 70 + ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 80 + ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 90 + ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 100 + ft_to_yd(3), y = 13, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 20 - ft_to_yd(3), y = 53.3 - 13, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 30 - ft_to_yd(3), y = 53.3 - 13, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 40 - ft_to_yd(3), y = 53.3 - 13, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 50 - ft_to_yd(3), y = 53.3 - 13, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 60 - ft_to_yd(3), y = 53.3 - 13, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 70 - ft_to_yd(3), y = 53.3 - 13, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 80 - ft_to_yd(3), y = 53.3 - 13, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 90 - ft_to_yd(3), y = 53.3 - 13, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 100 - ft_to_yd(3), y =53.3 -  13, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '1', x = 20 + ft_to_yd(3), y = 53.3 - 13, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '2', x = 30 + ft_to_yd(3), y = 53.3 - 13, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '3', x = 40 + ft_to_yd(3), y = 53.3 - 13, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '4', x = 50 + ft_to_yd(3), y = 53.3 - 13, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '5', x = 60 + ft_to_yd(3), y = 53.3 - 13, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '4', x = 70 + ft_to_yd(3), y = 53.3 - 13, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '3', x = 80 + ft_to_yd(3), y = 53.3 - 13, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '2', x = 90 + ft_to_yd(3), y = 53.3 - 13, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '1', x = 100 + ft_to_yd(3), y = 53.3 - 13, angle = angle_u, size = 2, color = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'NCAA'){
    # Add the field numbers to the plot
    g = g +
      ggplot2::annotate('text', label = '1', x = 20 - ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '2', x = 30 - ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '3', x = 40 - ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '4', x = 50 - ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '5', x = 60 - ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '4', x = 70 - ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '3', x = 80 - ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '2', x = 90 - ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '1', x = 100 - ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 20 + ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 30 + ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 40 + ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 50 + ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 60 + ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 70 + ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 80 + ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 90 + ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 100 + ft_to_yd(3), y = 8, angle = angle_l, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 20 - ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 30 - ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 40 - ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 50 - ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 60 - ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 70 - ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 80 - ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 90 - ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '0', x = 100 - ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '1', x = 20 + ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '2', x = 30 + ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '3', x = 40 + ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '4', x = 50 + ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '5', x = 60 + ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '4', x = 70 + ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '3', x = 80 + ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '2', x = 90 + ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff') +
      ggplot2::annotate('text', label = '1', x = 100 + ft_to_yd(3), y = 53.3 - 8, angle = angle_u, size = 2, color = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NFL or NCAA), return the ggplot2
    # instance
    return(g)
  }
}

#' Generate a ggplot2 instance containing a regulation football field for a
#' specified league
#'
#' @param league The league for which to draw the surface
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: 'ccw'
#'
#' @return A ggplot2 instance with a full-surface representation of a football
#'   field
#'
#' @export
#'
#' @examples
#' geom_football(league = "NFL")
#' geom_football(league = "NCAA", rotate = TRUE, rotation_dir = "ccw")
geom_football = function(league, rotate = FALSE, rotation_dir = 'ccw'){
  # Force the league to be all upper case
  league = toupper(league)

  if(league %in% c('NFL', 'NCAA')){
    # Create the initial ggplot2 instance onto which the features will be added
    g = ggplot2::ggplot() +
      ggplot2::coord_fixed() +
      ggplot2::theme(
        plot.margin = ggplot2::margin(0, 0, 0, 0),
        plot.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )

    # Add the grass
    g = football_grass(g, league, rotate, rotation_dir)

    # Add the sidelines
    g = football_sideline(g, league, rotate, rotation_dir)

    # Add the endlines
    g = football_endline(g, league, rotate, rotation_dir)

    # Add the goal lines
    g = football_goal_line(g, league, rotate, rotation_dir)

    # Add the yard markings
    g = football_yard_markings(g, league, rotate, rotation_dir)

    # Add the try markings
    g = football_try_markings(g, league, rotate, rotation_dir)

    # Add the directional arrows
    g = football_directional_arrows(g, league, rotate, rotation_dir)

    # Add the numbers
    g = football_yard_numbers(g, league, rotate, rotation_dir)

    # Return the ggplot2 instance that contains the field plot
    return(g)
  }

  else {
    message(paste('Sorry,', league, 'does not exist yet.'))
    return(0)
  }
}
