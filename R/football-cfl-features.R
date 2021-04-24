#' Generate the data frame for the points that comprise the grass background
#'
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the grass
cfl_feature_grass = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # This gives the green background of the field
  grass = create_rectangle(
    x_min = -84,
    x_max = 0,
    y_min = -36.5,
    y_max = 36.5
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

#' Generate the data frame for the points that comprise the sidelines
#'
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A list of data frames containing the points that comprise the
#'   sidelines
cfl_feature_sideline = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The sidelines are solid white and 6' (2 yards) in width. Their interior
  # edges form the boundary line of the field
  sideline_1 = create_rectangle(
    x_min = -77,
    x_max = 0,
    y_min = -34.5,
    y_max = -32.5
  )

  sideline_2 = create_rectangle(
    x_min = -77,
    x_max = 0,
    y_min = 32.5,
    y_max = 34.5
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    sideline_1 = rbind(
      sideline_1,
      reflect(
        sideline_1,
        over_y = TRUE
      )
    )

    sideline_2 = rbind(
      sideline_2,
      reflect(
        sideline_2,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    sideline_1 = rotate_coords(
      sideline_1,
      rotation_dir
    )

    sideline_2 = rotate_coords(
      sideline_2,
      rotation_dir
    )
  }

  # Return the feature's data frame as a list
  sidelines = list(
    sideline_1 = sideline_1,
    sideline_2 = sideline_2
  )

  return(sidelines)
}

#' Generate the data frame for the points that comprise the dead line
#'
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the dead lines
cfl_feature_dead_line = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The dead lines are solid white and 6' (2 yards) in width. Their interior
  # edges form the boundary line of the field
  dead_line = create_rectangle(
    x_min = -77,
    x_max = -75,
    y_min = -34.5,
    y_max = 34.5
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    dead_line = rbind(
      dead_line,
      reflect(
        dead_line,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    dead_line = rotate_coords(
      dead_line,
      rotation_dir
    )

  }

  # Return the feature's data frame
  return(dead_line)
}

#' Generate the data frame for the points that comprise the goal line
#'
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the goal lines
cfl_feature_goal_line = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The goal lines are solid white and 8" in width and span the entire width
  # of the field. Their interior edges form the boundary line of the field
  goal_line = create_rectangle(
    x_min = -55 - convert_units(8, 'in', 'yd'),
    x_max = -55,
    y_min = -34.5,
    y_max = 34.5
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

#' Generate the data frames for the points that comprise the yard lines. This
#' function is fed into the cfl_feature_yard_markings() function below and helps to
#' optimize performance
#'
#' @param yardage The yardage for which to create the yard markings
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame (or list of data frames) that contain the coordinates for
#'   the yard lines
cfl_feature_yard_markings_df_maker = function(yardage, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # The lines are to be placed 8" from the interior of the sidelines, and be
  # 4" wide. At 5-yard intervals across the field, the lines should stretch
  # the width of the field, with a 2' long by 4" wide hash 70' 9" from the
  # interior of the nearest boundary. At 1-yard intervals between these
  # markings at 5-yard intervals, a 2' tall by 4" wide marker should be placed
  # 8" from the interior of the sideline as well as 70' 9" from the interior
  # of the sideline (and extending back towards the sideline)

  if(yardage %% 5 == 0){
    if(yardage == 0){
      # Only draw the left half of the 50 yard line. The rest can be created via
      # reflection
      yard_marking = data.frame(
        x = c(
          # Start 8" from the sideline
          yardage - convert_units(2, 'in', 'yd'),

          # Lower inbound line marker
          yardage - convert_units(2, 'in', 'yd'),
          yardage - convert_units(2, 'in', 'yd') - convert_units(10, 'in', 'yd'),
          yardage - convert_units(2, 'in', 'yd') - convert_units(10, 'in', 'yd'),
          yardage - convert_units(2, 'in', 'yd'),

          # Upper inbound line marker
          yardage - convert_units(2, 'in', 'yd'),
          yardage - convert_units(2, 'in', 'yd') - convert_units(10, 'in', 'yd'),
          yardage - convert_units(2, 'in', 'yd') - convert_units(10, 'in', 'yd'),
          yardage - convert_units(2, 'in', 'yd'),

          # Top
          yardage - convert_units(2, 'in', 'yd'),

          # Crossover
          0,

          # Return to bottom
          0,

          # Return to start
          yardage - convert_units(2, 'in', 'yd')
        ),

        y = c(
          # Start 8" from the sideline
          -32.5 + convert_units(8, 'in', 'yd'),

          # Lower inbound line marker
          -32.5 + 24 - convert_units(4, 'in', 'yd'),
          -32.5 + 24 - convert_units(4, 'in', 'yd'),
          -32.5 + 24,
          -32.5 + 24,

          # Upper inbound line marker
          32.5 - 24,
          32.5 - 24,
          32.5 - 24 + convert_units(4, 'in', 'yd'),
          32.5 - 24 + convert_units(4, 'in', 'yd'),

          # Top
          32.5 - convert_units(8, 'in', 'yd'),

          # Crossover
          32.5 - convert_units(8, 'in', 'yd'),

          # Return to bottom
          -32.5 + convert_units(8, 'in', 'yd'),

          # Return to start
          -32.5 + convert_units(8, 'in', 'yd')
        )
      )

      if(full_surf){
        # If the surface being drawn is a full-surface representation, reflect
        # over the y axis
        yard_marking = rbind(
          yard_marking,
          reflect(
            yard_marking,
            over_y = TRUE
          )
        )
      }

      if(rotate){
        # If the desired output needs to be rotated, rotate the coordinates
        yard_marking = rotate_coords(
          yard_marking,
          rotation_dir
        )
      }

      # Return the yard marking
      return(yard_marking)
    }

    else{
      # At 5-yard intervals, the line should stretch the width of the field,
      # with the inbound line marker 24 yards from the interior of the sideline
      # boundary
      yard_marking = data.frame(
        x = c(
          # Start 8" from the sideline
          yardage - convert_units(2, 'in', 'yd'),

          # Lower inbound line marker
          yardage - convert_units(2, 'in', 'yd'),
          yardage - convert_units(2, 'in', 'yd') - convert_units(10, 'in', 'yd'),
          yardage - convert_units(2, 'in', 'yd') - convert_units(10, 'in', 'yd'),
          yardage - convert_units(2, 'in', 'yd'),

          # Upper inbound line marker
          yardage - convert_units(2, 'in', 'yd'),
          yardage - convert_units(2, 'in', 'yd') - convert_units(10, 'in', 'yd'),
          yardage - convert_units(2, 'in', 'yd') - convert_units(10, 'in', 'yd'),
          yardage - convert_units(2, 'in', 'yd'),

          # Top
          yardage - convert_units(2, 'in', 'yd'),

          # Crossover
          yardage + convert_units(2, 'in', 'yd'),

          # Upper inbound line marker
          yardage + convert_units(2, 'in', 'yd'),
          yardage + convert_units(2, 'in', 'yd') + convert_units(10, 'in', 'yd'),
          yardage + convert_units(2, 'in', 'yd') + convert_units(10, 'in', 'yd'),
          yardage + convert_units(2, 'in', 'yd'),

          # Lower inbound line marker
          yardage + convert_units(2, 'in', 'yd'),
          yardage + convert_units(2, 'in', 'yd') + convert_units(10, 'in', 'yd'),
          yardage + convert_units(2, 'in', 'yd') + convert_units(10, 'in', 'yd'),
          yardage + convert_units(2, 'in', 'yd'),

          # Return to bottom
          yardage + convert_units(2, 'in', 'yd'),

          # Return to start
          yardage - convert_units(2, 'in', 'yd')
        ),

        y = c(
          # Start 8" from the sideline
          -32.5 + convert_units(8, 'in', 'yd'),

          # Lower inbound line marker
          -32.5 + 24 - convert_units(4, 'in', 'yd'),
          -32.5 + 24 - convert_units(4, 'in', 'yd'),
          -32.5 + 24,
          -32.5 + 24,

          # Upper inbound line marker
          32.5 - 24,
          32.5 - 24,
          32.5 - 24 + convert_units(4, 'in', 'yd'),
          32.5 - 24 + convert_units(4, 'in', 'yd'),

          # Top
          32.5 - convert_units(8, 'in', 'yd'),

          # Crossover
          32.5 - convert_units(8, 'in', 'yd'),

          # Upper inbound line marker
          32.5 - 24 + convert_units(4, 'in', 'yd'),
          32.5 - 24 + convert_units(4, 'in', 'yd'),
          32.5 - 24,
          32.5 - 24,

          # Lower inbound line marker
          -32.5 + 24,
          -32.5 + 24,
          -32.5 + 24 - convert_units(4, 'in', 'yd'),
          -32.5 + 24 - convert_units(4, 'in', 'yd'),

          # Return to bottom
          -32.5 + convert_units(8, 'in', 'yd'),

          # Return to start
          -32.5 + convert_units(8, 'in', 'yd')
        )
      )

      if(full_surf){
        # If the surface being drawn is a full-surface representation, reflect
        # over the y axis
        yard_marking = rbind(
          yard_marking,
          reflect(
            yard_marking,
            over_y = TRUE
          )
        )
      }

      if(rotate){
        # If the desired output needs to be rotated, rotate the coordinates
        yard_marking = rotate_coords(
          yard_marking,
          rotation_dir
        )
      }

      # Return the yard marking
      return(yard_marking)}
  }

  else {
    # At 1-yard intervals, the line should be 2' long. The line should
    # appear at the bottom (b) and top (t) of the field inside the 6' wide
    # boundary, and also appear at 24 yards from the nearest boundary and
    # extending from this point towards that boundary (l and u)
    yard_marking_b = create_rectangle(
      x_min = yardage - convert_units(2, 'in', 'yd'),
      x_max = yardage + convert_units(2, 'in', 'yd'),
      y_min = -32.5 + convert_units(8, 'in', 'yd'),
      y_max = -32.5 + convert_units(2, 'ft', 'yd') + convert_units(8, 'in', 'yd')
    )

    yard_marking_l = create_rectangle(
      x_min = yardage - convert_units(2, 'in', 'yd'),
      x_max = yardage + convert_units(2, 'in', 'yd'),
      y_min = -32.5 + 24 - convert_units(2, 'ft', 'yd'),
      y_max = -32.5 + 24
    )

    yard_marking_u = create_rectangle(
      x_min = yardage - convert_units(2, 'in', 'yd'),
      x_max = yardage + convert_units(2, 'in', 'yd'),
      y_min = 32.5 - 24,
      y_max = 32.5 - 24 + convert_units(2, 'ft', 'yd')
    )

    yard_marking_t = create_rectangle(
      x_min = yardage - convert_units(2, 'in', 'yd'),
      x_max = yardage + convert_units(2, 'in', 'yd'),
      y_min = 32.5 - convert_units(2, 'ft', 'yd') - convert_units(8, 'in', 'yd'),
      y_max = 32.5 - convert_units(8, 'in', 'yd')
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect
      # over the y axis
      yard_marking_b = rbind(
        yard_marking_b,
        reflect(
          yard_marking_b,
          over_y = TRUE
        )
      )

      yard_marking_l = rbind(
        yard_marking_l,
        reflect(
          yard_marking_l,
          over_y = TRUE
        )
      )

      yard_marking_u = rbind(
        yard_marking_u,
        reflect(
          yard_marking_u,
          over_y = TRUE
        )
      )

      yard_marking_t = rbind(
        yard_marking_t,
        reflect(
          yard_marking_t,
          over_y = TRUE
        )
      )
    }

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

    # Return the list of yard markings
    return(list(yard_marking_b, yard_marking_l, yard_marking_u, yard_marking_t))
  }
}

#' Generate the data frame for the points that comprise the yard lines
#'
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the yard lines
cfl_feature_yard_markings = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x, y, and yardage (to pass checks)
  x = y = NULL
  yardage = NULL

  # The yard lines start at the left-side 1-yard mark, which is 11 yards from
  # the back of the endzone. They end at the right-side 1-yard mark, which is 11
  # yards from the back of the right endzone
  yardages = -54:0

  # Use lapply for speed to create the yard markings data frames. NOTE: the
  # result is a list, and will be stacked into a single data frame
  yard_markings_list = lapply(yardages, cfl_feature_yard_markings_df_maker, full_surf, rotate, rotation_dir)

  # Reshape the list of data frames into a single data frame
  yard_markings = dplyr::bind_rows(yard_markings_list, .id = 'yardage')

  # Return the feature's data frame
  return(yard_markings)
}

#' Generate the data frames for the points that comprise the directional arrows. This
#' function is fed into the cfl_feature_directional_arrows() function below and helps to
#' optimize performance
#'
#' @param yardage The yardage for which to create the yard markings
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param lower A boolean indicating whether or not to return the lower
#'   directional arrows
#'
#' @return A data frame (or list of data frames) that contain the coordinates for
#'   the directional arrows
cfl_feature_directional_arrows_df_maker = function(yardage, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw', lower = TRUE){
  # The arrow has two sides of 36", and one side of 18". The Pythagorean Theorem
  # can be used to determine the height (using half the length of the base,
  # which in this case is 18")
  arrow_width = sqrt((convert_units(36, 'in', 'yd') ** 2) - (convert_units(9, 'in', 'yd') ** 2))

  # The directional arrows should not be drawn at the 50-yard line. Other
  # than that, an arrow should be drawn every 10 yards
  if(yardage < 0){
    # Draw the directional arrow that's below the middle point of the field
    directional_arrow_1 = data.frame(
      x = c(
        # The numbers are 1' from the outer edge of the yard line, which is
        # 2" wide. The number itself is 4' wide, and the number is 6" off
        # the outside edge of the number
        yardage - convert_units(2, 'in', 'yd') - convert_units(5.5, 'ft', 'yd'),
        yardage - convert_units(2, 'in', 'yd') - convert_units(5.5, 'ft', 'yd'),
        yardage - convert_units(2, 'in', 'yd') - convert_units(5.5, 'ft', 'yd') - arrow_width,
        yardage - convert_units(2, 'in', 'yd') - convert_units(5.5, 'ft', 'yd')
      ),

      y = c(
        # The bottom of the numbers must be 12 yards (36') off the interior
        # of the sideline. The number itself is then 6' tall, and the top
        # tip of the arrow is 15" below this line
        -32.5 + 14 - convert_units(15, 'in', 'yd'),
        -32.5 + 14 - convert_units(15, 'in', 'yd') - convert_units(18, 'in', 'yd'),
        -32.5 + 14 - convert_units(15, 'in', 'yd') - convert_units(9, 'in', 'yd'),
        -32.5 + 14 - convert_units(15, 'in', 'yd')
      )
    )

    # Draw the directional arrow that's above the middle point of the field
    directional_arrow_2 = data.frame(
      x = c(
        # The numbers are 1' from the outer edge of the yard line, which is
        # 2" wide. The number itself is 4' wide, and the number is 6" off
        # the outside edge of the number
        yardage - convert_units(2, 'in', 'yd') - convert_units(5.5, 'ft', 'yd'),
        yardage - convert_units(2, 'in', 'yd') - convert_units(5.5, 'ft', 'yd'),
        yardage - convert_units(2, 'in', 'yd') - convert_units(5.5, 'ft', 'yd') - arrow_width,
        yardage - convert_units(2, 'in', 'yd') - convert_units(5.5, 'ft', 'yd')
      ),

      y = c(
        # The bottom of the numbers must be 12 yards (36') off the interior
        # of the sideline. The number itself is then 6' tall, and the top
        # tip of the arrow is 15" below this line
        32.5 - 14 + convert_units(15, 'in', 'yd'),
        32.5 - 14 + convert_units(15, 'in', 'yd') + convert_units(18, 'in', 'yd'),
        32.5 - 14 + convert_units(15, 'in', 'yd') + convert_units(9, 'in', 'yd'),
        32.5 - 14 + convert_units(15, 'in', 'yd')
      )
    )
  }

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    directional_arrow_1 = rbind(
      directional_arrow_1,
      reflect(
        directional_arrow_1,
        over_y = TRUE
      )
    )

    directional_arrow_2 = rbind(
      directional_arrow_2,
      reflect(
        directional_arrow_2,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    directional_arrow_1 = rotate_coords(
      directional_arrow_1,
      rotation_dir
    )

    directional_arrow_2 = rotate_coords(
      directional_arrow_2,
      rotation_dir
    )
  }

  # Combine the feature's data frames as a list
  directional_arrows = list(
    directional_arrow_1 = directional_arrow_1,
    directional_arrow_2 = directional_arrow_2
  )

  # Return the correct set of arrows
  if(lower){
    return(directional_arrows$directional_arrow_1)
  }

  else{
    return(directional_arrows$directional_arrow_2)
  }
}

#' Generate the data frame for the points that comprise the directional arrows
#'
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame that contains the points that comprise the directional
#'   arrows
cfl_feature_directional_arrows = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x, y, and yardage (to pass checks)
  x = y = NULL
  yardage = NULL

  # There should be a directional arrow every 10 yards, except for the 55 yard
  # line. They start at the 10 yard line, which is 20 yards from the back of
  # the endzone
  yardages = seq(-45, 0, 10)

  # Use lapply for speed to create the directional arrows' data frames. NOTE: the
  # result is a list, and will be stacked into a single data frame
  directional_arrows_1_list = lapply(yardages, cfl_feature_directional_arrows_df_maker, full_surf, rotate, rotation_dir, lower = TRUE)
  directional_arrows_2_list = lapply(yardages, cfl_feature_directional_arrows_df_maker, full_surf, rotate, rotation_dir, lower = FALSE)

  # Reshape the list of data frames into a single data frame
  directional_arrows_1 = dplyr::bind_rows(directional_arrows_1_list, .id = 'yardage')
  directional_arrows_2 = dplyr::bind_rows(directional_arrows_2_list, .id = 'yardage')

  # Return the feature's data frames as a list
  directional_arrows = list(
    directional_arrows_1 = directional_arrows_1,
    directional_arrows_2 = directional_arrows_2
  )
  return(directional_arrows)
}

#' Generate the data frame for the points that will become the yard marking
#' numbers
#'
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return a data frame that contains the information needed to add the yard
#'   marking numbers
cfl_feature_yard_numbers = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Set the initial rotation angle for the text
  angle_1 = 0
  angle_2 = 180

  if(rotate){
    if(tolower(rotation_dir) %in% c('ccw', 'pos', 'positive', 'counterclockwise', 'anticlockwise')){
      # If the rotation is counter-clockwise, rotate the numbers 90 degrees
      # counter-clockwise
      angle_1 = angle_1 + 90
      angle_2 = angle_2 + 90
    }

    else {
      # Otherwise, rotate the numbers 90 degrees clockwise
      angle_1 = angle_1 - 90
      angle_2 = angle_2 - 90
    }
  }

  # Create the yard marking numbers' data frame
  yard_marking_numbers = rbind(
    data.frame(
      x = seq(-45, 0, 10) - convert_units(3, 'ft', 'yd'),
      y = rep(-32.5 + 13, 5),
      label = c('1', '2', '3', '4', '5'),
      angle = angle_1
    ),

    data.frame(
      x = seq(-45, 0, 10) + convert_units(3, 'ft', 'yd'),
      y = rep(-32.5 + 13, 5),
      label = rep('0', 5),
      angle = angle_1
    ),

    data.frame(
      x = seq(-45, 0, 10) + convert_units(3, 'ft', 'yd'),
      y = rep(32.5 - 13, 5),
      label = c('1', '2', '3', '4', '5'),
      angle = angle_2
    ),

    data.frame(
      x = seq(-45, 0, 10) - convert_units(3, 'ft', 'yd'),
      y = rep(32.5 - 13, 5),
      label = rep('0', 5),
      angle = angle_2
    )
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    yard_marking_numbers = rbind(
      yard_marking_numbers,
      data.frame(
        x = seq(5, 45, 10) - convert_units(3, 'ft', 'yd'),
        y = rep(-32.5 + 13, 5),
        label = c('5', '4', '3', '2', '1'),
        angle = angle_1
      ),

      data.frame(
        x = seq(5, 45, 10) + convert_units(3, 'ft', 'yd'),
        y = rep(-32.5 + 13, 5),
        label = rep('0', 5),
        angle = angle_1
      ),

      data.frame(
        x = seq(5, 45, 10) + convert_units(3, 'ft', 'yd'),
        y = rep(32.5 - 13, 5),
        label = c('5', '4', '3', '2', '1'),
        angle = angle_2
      ),

      data.frame(
        x = seq(5, 45, 10) - convert_units(3, 'ft', 'yd'),
        y = rep(32.5 - 13, 5),
        label = rep('0', 5),
        angle = angle_2
      ),

      data.frame(
        x = c(0, 0),
        y = c(-32.5 + 13, 32.5 - 13),
        label = rep('C', 2),
        angle = c(angle_1, angle_2)
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    yard_marking_numbers = rotate_coords(
      yard_marking_numbers,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(yard_marking_numbers)
}

#' Generate the list of colors for an NFL field plot. The defaults can be
#' overwritten by supplying the names of the list elements to the
#' \code{geom_nfl()} function (or its wrapper \code{geom_football()})
#'
#' @param grass_color A hexadecimal string representing the color to use for
#'   this feature
#' @param sideline_1_color A hexadecimal string representing the color to use
#'   for this feature
#' @param sideline_2_color A hexadecimal string representing the color to use
#'   for this feature
#' @param dead_line_color A hexadecimal string representing the color to use for
#'   this feature
#' @param goal_line_color A hexadecimal string representing the color to use for
#'   this feature
#' @param yard_markings_color A hexadecimal string representing the color to use
#'   for this feature
#' @param directional_arrows_color A hexadecimal string representing the color
#'   to use for this feature
#' @param yard_numbers_color A hexadecimal string representing the color to use
#'   for this feature
#'
#' @return A list of hexadecimal colors to use to color the features on the
#'   resulting plot
cfl_features_set_colors = function(grass_color = '#196f0c',
                                   sideline_1_color = '#ffffff',
                                   sideline_2_color = '#ffffff',
                                   dead_line_color = '#ffffff',
                                   goal_line_color = '#ffffff',
                                   yard_markings_color = '#ffffff',
                                   directional_arrows_color = '#ffffff',
                                   yard_numbers_color = '#ffffff'
){

  # Create the colors to use for the plot
  feature_colors = list(
    grass_color = grass_color,
    sideline_1_color = sideline_1_color,
    sideline_2_color = sideline_2_color,
    dead_line_color = dead_line_color,
    goal_line_color = goal_line_color,
    yard_markings_color = yard_markings_color,
    directional_arrows_color = directional_arrows_color,
    yard_numbers_color = yard_numbers_color
  )

  # Return the list of colors
  return(feature_colors)
}

#' Create a ggplot2 instance that represents a regulation NFL field, with the
#' bottom left corner of the left-most endzone corresponding to  (0, 0)
#'
#' @param full_surf A boolean indicating whether or not to draw a full-surface
#'   representation of the playing surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not the surface representation
#'   needs to be rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the surface
#'   representation. Default: \code{'ccw'}
#' @param unit A string indicating the units with which to make the plot.
#'   Default: \code{'yd'}
#' @param origin_bottom_left A boolean indicating whether or not to place the
#'   origin at the bottom-left corner of the plot (where the dead_line meets the
#'   sideline). Default: \code{TRUE}
#' @param caption_color A hexadecimal string representing the color to use for
#'   the plot's caption. Default: '#707372' (grey)
#' @param background_color A hexadecimal string representing the color to use
#'   for the plot's background. Default: \code{NULL}
#' @param ... Additional arguments to pass to the function. These should be the
#'   colors to pass to the \code{cfl_features_set_colors()} function
#'
#' @return A ggplot2 instance that represents a regulation NFL field
geom_cfl = function(full_surf = TRUE,
                    rotate = FALSE,
                    rotation_dir = 'ccw',
                    unit = 'yd',
                    origin_bottom_left = TRUE,
                    caption_color = '#707372',
                    background_color = NULL,
                    ...
){
  # Force the plot unit to be lower case
  unit = tolower(unit)

  # Create the colors to use for the plot
  color_list = cfl_features_set_colors(...)

  # Generate the data frames for the features of an NFL field
  grass = cfl_feature_grass(full_surf, rotate, rotation_dir)
  sideline = cfl_feature_sideline(full_surf, rotate, rotation_dir)
  dead_line = cfl_feature_dead_line(full_surf, rotate, rotation_dir)
  goal_line = cfl_feature_goal_line(full_surf, rotate, rotation_dir)
  yard_markings = cfl_feature_yard_markings(full_surf, rotate, rotation_dir)
  directional_arrows = cfl_feature_directional_arrows(full_surf, rotate, rotation_dir)
  yard_numbers = cfl_feature_yard_numbers(full_surf, rotate, rotation_dir)

  # Translate the plot to the correct position on the axes as necessary. NOTE:
  # All translations are in units of yards
  if(origin_bottom_left){
    if(rotate){
      grass = translate(grass, translate_x = 32.5, translate_y = 75)
      sideline$sideline_1 = translate(sideline$sideline_1, translate_x = 32.5, translate_y = 75)
      sideline$sideline_2 = translate(sideline$sideline_2, translate_x = 32.5, translate_y = 75)
      dead_line = translate(dead_line, translate_x = 32.5, translate_y = 75)
      goal_line = translate(goal_line, translate_x = 32.5, translate_y = 75)
      yard_markings = translate(yard_markings, translate_x = 32.5, translate_y = 75)
      directional_arrows$directional_arrows_1 = translate(directional_arrows$directional_arrows_1, translate_x = 32.5, translate_y = 75)
      directional_arrows$directional_arrows_2 = translate(directional_arrows$directional_arrows_2, translate_x = 32.5, translate_y = 75)
      yard_numbers = translate(yard_numbers, translate_x = 32.5, translate_y = 75)
    }

    else {
      grass = translate(grass, translate_x = 75, translate_y = 32.5)
      sideline$sideline_1 = translate(sideline$sideline_1, translate_x = 75, translate_y = 32.5)
      sideline$sideline_2 = translate(sideline$sideline_2, translate_x = 75, translate_y = 32.5)
      dead_line = translate(dead_line, translate_x = 75, translate_y = 32.5)
      goal_line = translate(goal_line, translate_x = 75, translate_y = 32.5)
      yard_markings = translate(yard_markings, translate_x = 75, translate_y = 32.5)
      directional_arrows$directional_arrows_1 = translate(directional_arrows$directional_arrows_1, translate_x = 75, translate_y = 32.5)
      directional_arrows$directional_arrows_2 = translate(directional_arrows$directional_arrows_2, translate_x = 75, translate_y = 32.5)
      yard_numbers = translate(yard_numbers, translate_x = 75, translate_y = 32.5)
    }
  }

  # Convert between units as necessary
  if(!(unit %in% c('yd', 'yards'))){
    grass = convert_units(grass, 'yd', unit, conversion_columns = c('x', 'y'))
    sideline$sideline_1 = convert_units(sideline$sideline_1, 'yd', unit, conversion_columns = c('x', 'y'))
    sideline$sideline_2 = convert_units(sideline$sideline_2, 'yd', unit, conversion_columns = c('x', 'y'))
    dead_line = convert_units(dead_line, 'yd', unit, conversion_columns = c('x', 'y'))
    goal_line = convert_units(goal_line, 'yd', unit, conversion_columns = c('x', 'y'))
    yard_markings = convert_units(yard_markings, 'yd', unit, conversion_columns = c('x', 'y'))
    directional_arrows$directional_arrows_1 = convert_units(directional_arrows$directional_arrows_1, 'yd', unit, conversion_columns = c('x', 'y'))
    directional_arrows$directional_arrows_2 = convert_units(directional_arrows$directional_arrows_2, 'yd', unit, conversion_columns = c('x', 'y'))
    yard_numbers = convert_units(yard_numbers, 'yd', unit, conversion_columns = c('x', 'y'))
  }

  # Create the initial ggplot2 instance onto which the features will be added
  g = create_plot_base(rotate, caption_color, background_color)

  # Add the features to the ggplot2 instance
  g = add_feature(g, grass, color_list$grass_color)
  g = add_feature(g, sideline$sideline_1, color_list$sideline_1_color)
  g = add_feature(g, sideline$sideline_2, color_list$sideline_2_color)
  g = add_feature(g, dead_line, color_list$dead_line_color)
  g = add_feature(g, goal_line, color_list$goal_line_color)
  g = add_feature(g, yard_markings, color_list$yard_markings_color, yard_markings$yardage)
  g = add_feature(g, directional_arrows$directional_arrows_1, color_list$directional_arrows_color, directional_arrows$yardage)
  g = add_feature(g, directional_arrows$directional_arrows_2, color_list$directional_arrows_color, directional_arrows$yardage)

  # Add yardage numbers at 10-yard intervals
  g = g +
    ggplot2::annotate(
      'text',
      x = yard_numbers$x,
      y = yard_numbers$y,
      label = yard_numbers$label,
      angle = yard_numbers$angle,
      color = color_list$yard_numbers_color,
      size = 5
    )

  # Return the ggplot2 instance that contains the field plot
  return(g)
}
