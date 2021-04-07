#' Generate the data frame for the points that comprise the grass background
#'
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the pitch
#' @param goal_line_length The length of the goal line
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the grass
nwsl_feature_grass = function(touchline_length, goal_line_length, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # This gives the green background of the pitch
  grass = create_rectangle(
    x_min = (-touchline_length / 2) - 6,
    x_max = 0,
    y_min = (-goal_line_length / 2) - 3,
    y_max = (goal_line_length / 2) + 3
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    grass = rbind(
      grass,
      reflect(
        grass,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    grass = rotate_coords(
      grass,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(grass)
}

#' Generate the data frame for the points that comprise the touchlines
#'
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the pitch
#' @param goal_line_length The length of the goal line
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A list of data frames containing the points that comprise the
#'   touchlines
nwsl_feature_touchlines = function(touchline_length, goal_line_length, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The touchlines are 12cm wide, and the entire line lies within the pitch of
  # play
  touchline_1 = create_rectangle(
    x_min = -touchline_length / 2,
    x_max = 0,
    y_min = -goal_line_length / 2,
    y_max = (-goal_line_length / 2) + .12
  )

  touchline_2 = create_rectangle(
    x_min = -touchline_length / 2,
    x_max = 0,
    y_min = goal_line_length / 2,
    y_max = (goal_line_length / 2) - .12
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    touchline_1 = rbind(
      touchline_1,
      reflect(
        touchline_1,
        over_y = TRUE
      )
    )

    touchline_2 = rbind(
      touchline_2,
      reflect(
        touchline_2,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    touchline_1 = rotate_coords(
      touchline_1,
      rotation_dir
    )

    touchline_2 = rotate_coords(
      touchline_2,
      rotation_dir
    )
  }

  # Return the feature's data frames as a list
  touchlines = list(
    touchline_1 = touchline_1,
    touchline_2 = touchline_2
  )

  return(touchlines)
}

#' Generate the data frame for the points that comprise the goal lines
#'
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the pitch
#' @param goal_line_length The length of the goal line
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the goal line(s)
nwsl_feature_goal_line = function(touchline_length, goal_line_length, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The goal lines are 12cm wide, and the entire line lies within the pitch of
  # play
  goal_line = create_rectangle(
    x_min = -touchline_length / 2,
    x_max = -touchline_length / 2 + .12,
    y_min = -goal_line_length / 2,
    y_max = goal_line_length / 2
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
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

  # Return the feature's data frame
  return(goal_line)
}

#' Generate the data frame for the points that comprise the halfway line
#'
#' @param goal_line_length The length of the goal line
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the halfway line
nwsl_feature_halfway_line = function(goal_line_length, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The halfway line is 12cm wide and spans the entire width of the pitch. Its
  # center lies along the line x = 0
  halfway_line = create_rectangle(
    x_min = -.06,
    x_max = 0,
    y_min = -goal_line_length / 2,
    y_max = goal_line_length / 2
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    halfway_line = rbind(
      halfway_line,
      reflect(
        halfway_line,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    halfway_line = rotate_coords(
      halfway_line,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(halfway_line)
}

#' Generate the data frame for the points that comprise the center circle
#'
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the center circle
nwsl_feature_center_circle = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

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

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    center_circle = rbind(
      center_circle,
      reflect(
        center_circle,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    center_circle = rotate_coords(
      center_circle,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(center_circle)
}

#' Generate the data frame for the points that comprise the corner circle
#'
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the pitch
#' @param goal_line_length The length of the goal line
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A list of data frames containing the points that comprise the corner
#'   quarter-circles
nwsl_feature_corner_circle = function(touchline_length, goal_line_length, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The corners of the pitch have a quarter circle of radius 1m from the
  # center point of the corner flag. This point is the intersection of the
  # midpoints of the touchline and the goal line. This quarter circle has a
  # width of 12cm
  corner_circle_1 = rbind(
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

  corner_circle_2 = rbind(
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

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    corner_circle_3 = reflect(
      corner_circle_1,
      over_y = TRUE
    )

    corner_circle_4 = reflect(
      corner_circle_2,
      over_y = TRUE
    )
  }

  else {
    corner_circle_3 = data.frame(x = c(), y = c())
    corner_circle_4 = data.frame(x = c(), y = c())
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    corner_circle_1 = rotate_coords(
      corner_circle_1,
      rotation_dir
    )

    corner_circle_2 = rotate_coords(
      corner_circle_2,
      rotation_dir
    )

    corner_circle_3 = rotate_coords(
      corner_circle_3,
      rotation_dir
    )

    corner_circle_4 = rotate_coords(
      corner_circle_4,
      rotation_dir
    )
  }

  # Return the feature's data frames as a list
  corner_circles = list(
    corner_circle_1 = corner_circle_1,
    corner_circle_2 = corner_circle_2,
    corner_circle_3 = corner_circle_3,
    corner_circle_4 = corner_circle_4
  )

  return(corner_circles)
}

#' Generate the data frame for the points that comprise the 6-yard box
#'
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the pitch
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the 6-yard box
nwsl_feature_box_5.5m = function(touchline_length, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The 5.5m box extends 5.5m into the pitch from the outer edge of the goal
  # line. It is 12cm in width
  box_5.5m = data.frame(
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

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    box_5.5m = rbind(
      box_5.5m,
      reflect(
        box_5.5m,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    box_5.5m = rotate_coords(
      box_5.5m,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(box_5.5m)
}

#' Generate the data frame for the points that comprise the 18-yard box
#'
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the pitch
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the 18-yard box
nwsl_feature_box_9.15m = function(touchline_length, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The 9.15m box extends 9.15m into the pitch from the outer edge of the goal
  # line. It is 12cm in width
  box_9.15m = data.frame(
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

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    box_9.15m = rbind(
      box_9.15m,
      reflect(
        box_9.15m,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    box_9.15m = rotate_coords(
      box_9.15m,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(box_9.15m)
}

#' Generate the data frame for the points that comprise the penalty arc
#'
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the pitch
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the penalty arc
nwsl_feature_penalty_arc = function(touchline_length, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The penalty arc are the points that lie outside the 18-yard box along a
  # circle of width 12cm that have an outer radius of 9.15m from the center of
  # the penalty mark. The penalty mark is 11m from the outer edge of the goal
  # line.
  penalty_arc = rbind(
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
  penalty_arc = penalty_arc[penalty_arc$x >= (-touchline_length / 2) + 16.5, ]

  # Add on the first point of the arc to the end of the data frame to avoid plot
  # issues
  penalty_arc = rbind(
    penalty_arc,
    penalty_arc[1, ]
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    penalty_arc = rbind(
      penalty_arc,
      reflect(
        penalty_arc,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    penalty_arc = rotate_coords(
      penalty_arc,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(penalty_arc)
}

#' Generate the data frame for the points that comprise the penalty mark
#'
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the pitch
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the penalty mark
nwsl_feature_penalty_mark = function(touchline_length, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The penalty mark is 11m from the outside edge of the goal line. It is
  # 30.48cm (12") in diameter
  penalty_mark = create_circle(
    center = c((-touchline_length / 2) + 11, 0),
    start = 0,
    end = 2,
    d = .3
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    penalty_mark = rbind(
      penalty_mark,
      reflect(
        penalty_mark,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    penalty_mark = rotate_coords(
      penalty_mark,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(penalty_mark)
}

#' Generate the data frame for the points that comprise the center mark
#'
#' @param touchline_length The length of the touchline. This should be the
#'   entire length (both halves) of the pitch
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the center mark
nwsl_feature_center_mark = function(touchline_length, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The center mark is located at the center of the pitch, with its center at
  # (0, 0). It is 30.48cm (12") in diameter
  center_mark = create_circle(
    center = c(0, 0),
    start = .5,
    end = 1.5,
    d = .3
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    center_mark = rbind(
      center_mark,
      reflect(
        center_mark,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    center_mark = rotate_coords(
      center_mark,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(center_mark)
}

#' Generate the data frame for the points that comprise the goal
#'
#' @param touchline_length The length of touchline. This should be the entire
#'   length (both halves) of the pitch
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the goal
nwsl_feature_goal = function(touchline_length, full_surf = TRUE, rotate = FALSE, rotation_dir = TRUE){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The goal is centered on the goal line, with the inner edge of the posts
  # being 7.32m apart. The posts are each 12cm in width, and the goal extends
  # backwards a depth of 1.7m
  goal = data.frame(
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

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    goal = rbind(
      goal,
      reflect(
        goal,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    goal = rotate_coords(
      goal,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(goal)
}

#' Generate the list of colors for an NWSL pitch plot. The defaults can be
#' overwritten by supplying the names of the list elements to the
#' \code{geom_nwsl()} function (or its wrapper \code{geom_soccer()})
#'
#' @param grass_color A hexadecimal string representing the color to use for
#'   this feature
#' @param touchline_1_color A hexadecimal string representing the color to use
#'   for this feature
#' @param touchline_2_color A hexadecimal string representing the color to use
#'   for this feature
#' @param goal_line_color A hexadecimal string representing the color to use for
#'   this feature
#' @param halfway_line_color A hexadecimal string representing the color to use
#'   for this feature
#' @param center_circle_color A hexadecimal string representing the color to use
#'   for this feature
#' @param corner_circle_1_color A hexadecimal string representing the color to
#'   use for this feature
#' @param corner_circle_2_color A hexadecimal string representing the color to
#'   use for this feature
#' @param corner_circle_3_color A hexadecimal string representing the color to
#'   use for this feature
#' @param corner_circle_4_color A hexadecimal string representing the color to
#'   use for this feature
#' @param box_5.5m_color A hexadecimal string representing the color to use for
#'   this feature
#' @param box_9.15m_color A hexadecimal string representing the color to use for
#'   this feature
#' @param penalty_arc_color A hexadecimal string representing the color to use
#'   for this feature
#' @param penalty_mark_color A hexadecimal string representing the color to use
#'   for this feature
#' @param center_mark_color A hexadecimal string representing the color to use
#'   for this feature
#' @param goal_color A hexadecimal string representing the color to use for this
#'   feature
#'
#' @return A list of hexadecimal colors to use to color the features on the
#'   resulting plot
nwsl_features_set_colors = function(grass_color = '#196f0c',
                                    touchline_1_color = '#ffffff',
                                    touchline_2_color = '#ffffff',
                                    goal_line_color = '#ffffff',
                                    halfway_line_color = '#ffffff',
                                    center_circle_color = '#ffffff',
                                    corner_circle_1_color = '#ffffff',
                                    corner_circle_2_color = '#ffffff',
                                    corner_circle_3_color = '#ffffff',
                                    corner_circle_4_color = '#ffffff',
                                    box_5.5m_color = '#ffffff',
                                    box_9.15m_color = '#ffffff',
                                    penalty_arc_color = '#ffffff',
                                    penalty_mark_color = '#ffffff',
                                    center_mark_color = '#ffffff',
                                    goal_color = '#ffffff'
){
  # Create the colors to use for the plot
  feature_colors = list(
    grass_color = grass_color,
    touchline_1_color = touchline_1_color,
    touchline_2_color = touchline_2_color,
    goal_line_color = goal_line_color,
    halfway_line_color = halfway_line_color,
    center_circle_color = center_circle_color,
    corner_circle_1_color = corner_circle_1_color,
    corner_circle_2_color = corner_circle_2_color,
    corner_circle_3_color = corner_circle_3_color,
    corner_circle_4_color = corner_circle_4_color,
    box_5.5m_color = box_5.5m_color,
    box_9.15m_color = box_9.15m_color,
    penalty_arc_color = penalty_arc_color,
    penalty_mark_color = penalty_mark_color,
    center_mark_color = center_mark_color,
    goal_color = goal_color
  )

  # Return the list of colors
  return(feature_colors)
}

#' Create a ggplot2 instance that represents a regulation NWSL pitch, with the
#' center of the pitch corresponding to (0, 0)
#'
#' @param touchline_length The entire length of the touchline (both halves).
#'   This can be in any units, but will default to meters. Default: \code{120}
#' @param goal_line_length The length of the goal line. This can be in any
#'   units, but will default to meters. Default: \code{90}
#' @param full_surf A boolean indicating whether or not to draw a full-surface
#'   representation of the playing surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not the surface representation
#'   needs to be rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the surface
#'   representation. Default: \code{'ccw'}
#' @param unit A string indicating the units with which to make the plot.
#'   Default: \code{'m'}
#' @param caption_color A hexadecimal string representing the color to use for
#'   the plot's caption. Default: '#707372' (grey)
#' @param background_color A hexadecimal string representing the color to use
#'   for the plot's background. Default: \code{NULL}
#' @param ... Additional arguments to pass to the function. These should be the
#'   colors to pass to the \code{nwsl_features_set_colors()} function
#'
#' @return A ggplot2 instance that represents a regulation NWSL pitch
geom_nwsl = function(touchline_length = 120,
                     goal_line_length = 90,
                     full_surf = TRUE,
                     rotate = FALSE,
                     rotation_dir = 'ccw',
                     unit = 'm',
                     caption_color = '#707372',
                     background_color = NULL,
                     ...
){
  # Create the colors to use for the plot
  color_list = nwsl_features_set_colors(...)

  # Generate the data frames for the features of an NWSL pitch
  grass = nwsl_feature_grass(touchline_length, goal_line_length, full_surf, rotate, rotation_dir)
  touchlines = nwsl_feature_touchlines(touchline_length, goal_line_length, full_surf, rotate, rotation_dir)
  goal_line = nwsl_feature_goal_line(touchline_length, goal_line_length, full_surf, rotate, rotation_dir)
  halfway_line = nwsl_feature_halfway_line(goal_line_length, full_surf, rotate, rotation_dir)
  center_circle = nwsl_feature_center_circle(full_surf, rotate, rotation_dir)
  corner_circle = nwsl_feature_corner_circle(touchline_length, goal_line_length, full_surf, rotate, rotation_dir)
  box_5.5m = nwsl_feature_box_5.5m(touchline_length, full_surf, rotate, rotation_dir)
  box_9.15m = nwsl_feature_box_9.15m(touchline_length, full_surf, rotate, rotation_dir)
  penalty_arc = nwsl_feature_penalty_arc(touchline_length, full_surf, rotate, rotation_dir)
  penalty_mark = nwsl_feature_penalty_mark(touchline_length, full_surf, rotate, rotation_dir)
  center_mark = nwsl_feature_center_mark(touchline_length, full_surf, rotate, rotation_dir)
  goal = nwsl_feature_goal(touchline_length, full_surf, rotate, rotation_dir)

  # Convert units as necessary
  if(!(unit %in% c('m', 'meters'))){
    grass = convert_units(grass, 'm', unit, conversion_columns = c('x', 'y'))
    touchlines$touchline_1 = convert_units(touchlines$touchline_1, 'm', unit, conversion_columns = c('x', 'y'))
    touchlines$touchline_2 = convert_units(touchlines$touchline_2, 'm', unit, conversion_columns = c('x', 'y'))
    goal_line = convert_units(goal_line, 'm', unit, conversion_columns = c('x', 'y'))
    halfway_line = convert_units(halfway_line, 'm', unit, conversion_columns = c('x', 'y'))
    center_circle = convert_units(center_circle, 'm', unit, conversion_columns = c('x', 'y'))
    corner_circle$corner_circle_1 = convert_units(corner_circle$corner_circle_1, 'm', unit, conversion_columns = c('x', 'y'))
    corner_circle$corner_circle_2 = convert_units(corner_circle$corner_circle_2, 'm', unit, conversion_columns = c('x', 'y'))
    corner_circle$corner_circle_3 = convert_units(corner_circle$corner_circle_3, 'm', unit, conversion_columns = c('x', 'y'))
    corner_circle$corner_circle_4 = convert_units(corner_circle$corner_circle_4, 'm', unit, conversion_columns = c('x', 'y'))
    box_5.5m = convert_units(box_5.5m, 'm', unit, conversion_columns = c('x', 'y'))
    box_9.15m = convert_units(box_9.15m, 'm', unit, conversion_columns = c('x', 'y'))
    penalty_arc = convert_units(penalty_arc, 'm', unit, conversion_columns = c('x', 'y'))
    penalty_mark = convert_units(penalty_mark, 'm', unit, conversion_columns = c('x', 'y'))
    center_mark = convert_units(center_mark, 'm', unit, conversion_columns = c('x', 'y'))
    goal = convert_units(goal, 'm', unit, conversion_columns = c('x', 'y'))
  }

  # Create the initial ggplot2 instance onto which the features will be added
  g = create_plot_base(rotate, caption_color, background_color)

  # Add the features to the ggplot2 instance
  g = add_feature(g, grass, color_list$grass_color)
  g = add_feature(g, touchlines$touchline_1, color_list$touchline_1_color)
  g = add_feature(g, touchlines$touchline_2, color_list$touchline_2_color)
  g = add_feature(g, goal_line, color_list$goal_line_color)
  g = add_feature(g, halfway_line, color_list$halfway_line_color)
  g = add_feature(g, center_circle, color_list$center_circle_color)
  g = add_feature(g, corner_circle$corner_circle_1, color_list$corner_circle_1_color)
  g = add_feature(g, corner_circle$corner_circle_2, color_list$corner_circle_2_color)
  g = add_feature(g, corner_circle$corner_circle_3, color_list$corner_circle_3_color)
  g = add_feature(g, corner_circle$corner_circle_4, color_list$corner_circle_4_color)
  g = add_feature(g, box_5.5m, color_list$box_5.5m_color)
  g = add_feature(g, box_9.15m, color_list$box_9.15m_color)
  g = add_feature(g, penalty_arc, color_list$penalty_arc_color)
  g = add_feature(g, penalty_mark, color_list$penalty_mark_color)
  g = add_feature(g, center_mark, color_list$center_mark_color)
  g = add_feature(g, goal, color_list$goal_color)

  # Return the ggplot2 instance that contains the pitch plot
  return(g)
}
