#' Generate the data frame for the points that comprise a regulation FIBA
#' basketball court
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param unit A string indicating the unit with which to make the plot.
#'   Default: \code{'ft'}
#'
#' @return A data frame containing the points that comprise the court
fiba_feature_court_bground = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw', unit = 'ft'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # A regulation college court is 28m long and 15m wide, but this will only draw
  # the half-court, so it will measure 14m long and 7.5m wide
  court = switch(
    unit,
    'ft' = {
      create_rectangle(
        x_min = m_to_ft(-14.0),
        x_max = m_to_ft( 0.00),
        y_min = m_to_ft(-7.50),
        y_max = m_to_ft( 7.50)
      )
    },

    'm' = {
      create_rectangle(
        x_min = -14.0,
        x_max =  0.00,
        y_min = -7.50,
        y_max =  7.50
      )
    }
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    court = rbind(
      court,
      reflect(
        court,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    court = rotate_coords(
      court,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(court)
}

#' Generate the data frame for the points that comprise the center circles
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param unit A string indicating the unit with which to make the plot.
#'   Default: \code{'ft'}
#'
#' @return A list of data frames containing the points that comprise the center
#'   circles
fiba_feature_center_circle = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw', unit = 'ft'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # A regulation college court is 28m long and 15m wide, but this will only draw
  # the half-court, so it will measure 14m long and 7.5m wide
  center_circle = switch(
    unit,
    'ft' = {
      rbind(
        create_circle(
          center = c(0, 0),
          start  = 0.5,
          end    = 1.5,
          d      = m_to_ft(3.6)
        ),
        data.frame(
          x = m_to_ft( 0.0),
          y = m_to_ft(-3.5)
        ),
        create_circle(
          center = c(0, 0),
          start  = 1.5,
          end    = 0.5,
          d      = m_to_ft(3.5)
        )
      )
    },

    'm' = {
      rbind(
        create_circle(
          center = c(0, 0),
          start  = 0.5,
          end    = 1.5,
          d      = 3.6
        ),
        data.frame(
          x =  0.0,
          y = -3.5
        ),
        create_circle(
          center = c(0, 0),
          start  = 1.5,
          end    = 0.5,
          d      = 3.5
        )
      )
    }
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

#' Generate the data frame for the points that comprise the division line
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param unit A string indicating the unit with which to make the plot.
#'   Default: \code{'ft'}
#'
#' @return A data frame containing the points that comprise the division line
fiba_feature_division_line = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw', unit = 'ft'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The line is 5cm thick, goes right through the middle of the court (2.5cm of
  # its width on each side of it), and extends .15m beyond each sideline
  division_line = switch(
    unit,
    'ft' = {
      create_rectangle(
        x_min = m_to_ft(-0.025),
        x_max = m_to_ft( 0.000),
        y_min = m_to_ft(-7.650),
        y_max = m_to_ft( 7.650)
      )
    },

    'm' = {
      create_rectangle(
        x_min = -0.025,
        x_max =  0.000,
        y_min = -7.650,
        y_max =  7.650
      )
    }
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    division_line = rbind(
      division_line,
      reflect(
        division_line,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    division_line = rotate_coords(
      division_line,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(division_line)
}

#' Generate the data frame for the points that comprise the end lines
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param unit A string indicating the unit with which to make the plot.
#'   Default: \code{'ft'}
#'
#' @return A data frame containing the points that comprise the end lines
fiba_feature_endline = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw', unit = 'ft'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The endline is 14m (interior) from the center of the court, spans the width
  # of the court, and is 5cm in thickness
  endline = switch(
    unit,
    'ft' = {
      create_rectangle(
        x_min = m_to_ft(-14.05),
        x_max = m_to_ft(-14.00),
        y_min = m_to_ft(- 7.50),
        y_max = m_to_ft(  7.50)
      )
    },

    'm' = {
      create_rectangle(
        x_min = -14.05,
        x_max = -14.00,
        y_min = - 7.50,
        y_max =   7.50
      )
    }
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    endline = rbind(
      endline,
      reflect(
        endline,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    endline = rotate_coords(
      endline,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(endline)
}

#' Generate the data frame for the points that comprise the sidelines
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param unit A string indicating the unit with which to make the plot.
#'   Default: \code{'ft'}
#'
#' @return A list of data frames containing the points that comprise the
#'   sidelines
fiba_feature_sideline = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw', unit = 'ft'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The sideline is 25' (interior) from the center of the court, spans the
  # length of the court, and is 2" in thickness

  # First sideline
  sideline_1 = switch(
    unit,
    'ft' = {
      create_rectangle(
        x_min = m_to_ft(-14.05),
        x_max = m_to_ft(  0.00),
        y_min = m_to_ft(- 7.55),
        y_max = m_to_ft(- 7.50)
      )
    },

    'm' = {
      create_rectangle(
        x_min = -14.05,
        x_max =   0.00,
        y_min = - 7.55,
        y_max = - 7.50
      )
    }
  )

  # Second sideline
  sideline_2 = switch(
    unit,
    'ft' = {
      create_rectangle(
        x_min = m_to_ft(-14.05),
        x_max = m_to_ft(  0.00),
        y_min = m_to_ft(  7.50),
        y_max = m_to_ft(  7.55)
      )
    },

    'm' = {
      create_rectangle(
        x_min = -14.05,
        x_max =   0.00,
        y_min =   7.50,
        y_max =   7.55
      )
    }
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

  # Return the feature's data frames as a list
  sidelines = list(
    sideline_1 = sideline_1,
    sideline_2 = sideline_2
  )

  return(sidelines)
}

#' Generate the data frames for the points that comprise the team bench
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param unit A string indicating the unit with which to make the plot.
#'   Default: \code{'ft'}
#'
#' @return A list of data frames containing the points that comprise the team
#'   bench
fiba_feature_team_bench = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw', unit = 'ft'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # On a FIBA court, a throw-in line is .15m in length, with its outer edge
  # 8.325m from the inner edge of the nearest endline, and with thickness of
  # 5cm. They only exist on one side of the court. Team benches are 5m from the
  # center of the division line, extend 2m towards the bench, and are 5cm in
  # width

  # Throw-in line
  throw_in_line = switch(
    unit,
    'ft' = {
      create_rectangle(
        x_min = m_to_ft(-5.725),
        x_max = m_to_ft(-5.675),
        y_min = m_to_ft(-7.700),
        y_max = m_to_ft(-7.550)
      )
    },

    'm' = {
      create_rectangle(
        x_min = -5.725,
        x_max = -5.675,
        y_min = -7.700,
        y_max = -7.550
      )
    }
  )

  # Team bench
  team_bench = switch(
    unit,
    'ft' = {
      create_rectangle(
        x_min = m_to_ft(-5.05),
        x_max = m_to_ft(-5.00),
        y_min = m_to_ft( 7.55),
        y_max = m_to_ft( 9.55)
      )
    },

    'm' = {
      create_rectangle(
        x_min = -5.05,
        x_max = -5.00,
        y_min =  7.55,
        y_max =  9.55
      )
    }
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    throw_in_line = rbind(
      throw_in_line,
      reflect(
        throw_in_line,
        over_y = TRUE
      )
    )

    team_bench = rbind(
      team_bench,
      reflect(
        team_bench,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    throw_in_line = rotate_coords(
      throw_in_line,
      rotation_dir
    )

    team_bench = rotate_coords(
      team_bench,
      rotation_dir
    )
  }

  # Return the feature's data frames as a list
  throw_in_line_team_benches = list(
    team_bench_1 = throw_in_line,
    team_bench_2  = team_bench
  )

  return(throw_in_line_team_benches)
}

#' Generate the data frame for the points that comprise the court apron
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param unit A string indicating the unit with which to make the plot.
#'   Default: \code{'ft'}
#'
#' @return A data frame containing the points that comprise the court apron
fiba_feature_court_apron = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw', unit = 'ft'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The court apron is the boundary on the court, which may be made of a
  # contrasting color. No distance is specified in the official FIBA rule book,
  # so a distance of 3m on all sides will be used
  court_apron = switch(
    unit,
    'ft' = {
      data.frame(
        x = c(
          m_to_ft(  0.000),
          m_to_ft(- 0.025),
          m_to_ft(- 0.025),
          m_to_ft(- 5.675),
          m_to_ft(- 5.675),
          m_to_ft(- 5.725),
          m_to_ft(- 5.725),
          m_to_ft(-14.050),
          m_to_ft(-14.050),
          m_to_ft(- 5.050),
          m_to_ft(- 5.050),
          m_to_ft(- 5.000),
          m_to_ft(- 5.000),
          m_to_ft(- 0.025),
          m_to_ft(- 0.025),
          m_to_ft(  0.000),
          m_to_ft(  0.000),
          m_to_ft(-17.050),
          m_to_ft(-17.050),
          m_to_ft(  0.000),
          m_to_ft(  0.000)
        ),

        y = c(
          m_to_ft(- 7.65),
          m_to_ft(- 7.65),
          m_to_ft(- 7.55),
          m_to_ft(- 7.55),
          m_to_ft(- 7.65),
          m_to_ft(- 7.65),
          m_to_ft(- 7.55),
          m_to_ft(- 7.55),
          m_to_ft(  7.55),
          m_to_ft(  7.55),
          m_to_ft(  9.55),
          m_to_ft(  9.55),
          m_to_ft(  7.55),
          m_to_ft(  7.55),
          m_to_ft(  7.65),
          m_to_ft(  7.65),
          m_to_ft( 10.55),
          m_to_ft( 10.55),
          m_to_ft(-10.55),
          m_to_ft(-10.55),
          m_to_ft(- 7.65)
        )
      )
    },

    'm' = {
      data.frame(
        x = c(
            0.000,
          - 0.025,
          - 0.025,
          - 5.675,
          - 5.675,
          - 5.725,
          - 5.725,
          -14.050,
          -14.050,
          - 5.050,
          - 5.050,
          - 5.000,
          - 5.000,
          - 0.025,
          - 0.025,
            0.000,
            0.000,
          -17.050,
          -17.050,
            0.000,
            0.000
        ),

        y = c(
          - 7.65,
          - 7.65,
          - 7.55,
          - 7.55,
          - 7.65,
          - 7.65,
          - 7.55,
          - 7.55,
            7.55,
            7.55,
            9.55,
            9.55,
            7.55,
            7.55,
            7.65,
            7.65,
           10.55,
           10.55,
          -10.55,
          -10.55,
          - 7.65
        )
      )
    }
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    court_apron = rbind(
      court_apron,
      reflect(
        court_apron,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    court_apron = rotate_coords(
      court_apron,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(court_apron)
}

#' Generate the data frame for the points that comprise the three-point line and
#' two-point range
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param unit A string indicating the unit with which to make the plot.
#'   Default: \code{'ft'}
#'
#' @return A list of data frames containing the points that comprise the
#'   three-point line and two-point range
fiba_feature_three_point_line = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw', unit = 'ft'){
  # First, a bit of math is needed to determine the starting and ending angles
  # of the three-point arc, relative to 0 radians. Since in the end, the angle
  # is what matters, the units of measure do not. The angle begins .90m from the
  # interior edge of the sideline
  start_y = switch(
    unit,
    'ft' = m_to_ft(6.6),
    'm' = 6.6
  )

  # The rule book describes the arc as having a radius of 6.75m
  radius_outer = switch(
    unit,
    'ft' = m_to_ft(6.75),
    'm' = 6.75
  )

  # From here, the calculation is relatively straightforward. To determine the
  # angle, the inverse sine is needed. It will be multiplied by pi so that it
  # can be passed to the create_circle() function
  start_angle_outer = asin(start_y / radius_outer) / pi
  end_angle_outer = -start_angle_outer

  # The same method can be used for the inner angles, however, since the inner
  # radius will be traced from bottom to top, the angle must be negative to
  # start
  radius_inner = switch(
    unit,
    'ft' = m_to_ft(6.70),
    'm' = 6.70
  )

  start_angle_inner = switch(
    unit,
    'ft' = -asin((start_y - m_to_ft(0.05)) / radius_inner) / pi,
    'm' = -asin((start_y - 0.05) / radius_inner) / pi
  )

  end_angle_inner = -start_angle_inner

  # FIBA three-point line
  three_point_line = switch(
    unit,
    'ft' = {
      rbind(
        data.frame(
          x = m_to_ft(-14),
          y = m_to_ft(6.6)
        ),

        create_circle(
          center = c(m_to_ft(-12.425), m_to_ft(0)),
          start = start_angle_outer,
          end = end_angle_outer,
          d = 2 * radius_outer
        ),

        data.frame(
          x = c(m_to_ft(-14), m_to_ft(-14)),
          y = c(m_to_ft(-6.6), m_to_ft(-6.55))
        ),

        create_circle(
          center = c(m_to_ft(-12.425), m_to_ft(0)),
          start = start_angle_inner,
          end = end_angle_inner,
          d = 2 * radius_inner
        ),

        data.frame(
          x = c(m_to_ft(-14), m_to_ft(-14)),
          y = c(m_to_ft(6.55), m_to_ft(6.6))
        )
      )
    },

    'm' = {
      rbind(
        data.frame(
          x = -14,
          y = 6.6
        ),

        create_circle(
          center = c(-12.425, 0),
          start = start_angle_outer,
          end = end_angle_outer,
          d = 2 * radius_outer
        ),

        data.frame(
          x = c(-14, -14),
          y = c(-6.6, -6.55)
        ),

        create_circle(
          center = c(-12.425, 0),
          start = start_angle_inner,
          end = end_angle_inner,
          d = 2 * radius_inner
        ),

        data.frame(
          x = c(-14, -14),
          y = c(6.55, 6.6)
        )
      )
    }
  )

  # Sometimes, the inside of the three-point arc (aka 2-point range) will be a
  # different color than the floor. This section allows for this to happen in a
  # future iteration
  two_point_range = switch(
    unit,
    'ft' = {
      rbind(
        data.frame(
          x = m_to_ft(-14),
          y =  m_to_ft(-6.55)
        ),

        create_circle(
          center = c(m_to_ft(-12.425), m_to_ft(0)),
          start = start_angle_inner,
          end = end_angle_inner,
          d = 2 * radius_inner
        ),

        data.frame(
          x = c(m_to_ft(-14), m_to_ft(-14)),
          y = c(m_to_ft(6.55), m_to_ft(-6.55))
        )
      )
    },

    'm' = {
      rbind(
        data.frame(
          x = -14,
          y =  -6.55
        ),

        create_circle(
          center = c(-12.425, 0),
          start = start_angle_inner,
          end = end_angle_inner,
          d = 2 * radius_inner
        ),

        data.frame(
          x = c(-14, -14),
          y = c(6.55, -6.55)
        )
      )
    }
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    three_point_line = rbind(
      three_point_line,
      reflect(
        three_point_line,
        over_y = TRUE
      )
    )

    two_point_range = rbind(
      two_point_range,
      reflect(
        two_point_range,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    three_point_line = rotate_coords(
      three_point_line,
      rotation_dir
    )

    two_point_range = rotate_coords(
      two_point_range,
      rotation_dir
    )
  }

  # Return the feature's data frames as a list
  three_point_arc_two_point_range = list(
    three_point_line = three_point_line,
    two_point_range = two_point_range
  )

  return(three_point_arc_two_point_range)

}

#' Generate the data frames for the points that comprise both the professional
#' and amateur free-throw lanes
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param unit A string indicating the unit with which to make the plot.
#'   Default: \code{'ft'}
#'
#' @return A list of data frames containing the points that comprise both the
#'   professional and amateur free-throw lanes
fiba_feature_free_throw_lane = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw', unit = 'ft'){
  # The free-throw lane measures 5.8m from the inside edge of the endline to the
  # outside edge of the free-throw line, has a width of 4.9m, and has lines of
  # width of 5cm
  free_throw_lane = switch(
    unit,
    'ft' = {
      data.frame(
        x = c(
          m_to_ft(-14),
          m_to_ft(-8.2),
          m_to_ft(-8.2),
          m_to_ft(-14),

          m_to_ft(-14),
          m_to_ft(-8.25),
          m_to_ft(-8.25),
          m_to_ft(-14),

          m_to_ft(-14)
        ),

        y = c(
          m_to_ft(-2.45),
          m_to_ft(-2.45),
          m_to_ft(2.45),
          m_to_ft(2.45),

          m_to_ft(2.40),
          m_to_ft(2.40),
          m_to_ft(-2.40),
          m_to_ft(-2.40),

          m_to_ft(-2.45)
        )
      )
    },

    'm' = {
      data.frame(
        x = c(
          -14,
          -8.2,
          -8.2,
          -14,

          -14,
          -8.25,
          -8.25,
          -14,

          -14
        ),

        y = c(
          -2.45,
          -2.45,
          2.45,
          2.45,

          2.40,
          2.40,
          -2.40,
          -2.40,

          -2.45
        )
      )
    }
  )

  painted_area = switch(
    unit,
    'ft' = {
      create_rectangle(
        x_min = m_to_ft(-14),
        x_max = m_to_ft(-8.25),
        y_min = m_to_ft(-2.40),
        y_max = m_to_ft(2.40)
      )
    },

    'm' = {
      create_rectangle(
        x_min = -14,
        x_max = -8.25,
        y_min = -2.40,
        y_max = 2.40
      )
    }
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    free_throw_lane = rbind(
      free_throw_lane,
      reflect(
        free_throw_lane,
        over_y = TRUE
      )
    )

    painted_area = rbind(
      painted_area,
      reflect(
        painted_area,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    free_throw_lane = rotate_coords(
      free_throw_lane,
      rotation_dir
    )

    painted_area = rotate_coords(
      painted_area,
      rotation_dir
    )
  }

  # Return the feature's data frames as a list
  lane_painted_area = list(
    free_throw_lane = free_throw_lane,
    painted_area = painted_area
  )

  return(lane_painted_area)
}

#' Generate the data frames for the points that comprise the free-throw lane
#' lines (the blocks)
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param unit A string indicating the unit with which to make the plot.
#'   Default: \code{'ft'}
#'
#' @return A list of data frames containing the points that comprise the
#'   free-throw lane lines (the blocks)
fiba_feature_free_throw_lane_lines = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw', unit = 'ft'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The first set of lane lines are 1.75m from the interior of the baseline,
  # measure 5cm in width, and extend 10cm towards the nearest sideline
  lane_line_1 = switch(
    unit,
    'ft' = {
      create_rectangle(
        x_min = m_to_ft(-12.25),
        x_max = m_to_ft(-12.20),
        y_min = m_to_ft(- 2.55),
        y_max = m_to_ft(- 2.45)
      )
    },

    'm' = {
      create_rectangle(
        x_min = -12.25,
        x_max = -12.20,
        y_min = - 2.55,
        y_max = - 2.45
      )
    }
  )

  # The second set of lane lines are 85cm from the first lane line, and measure
  # 40cm in width, and extend 10cm towards the nearest sideline
  lane_line_2 = switch(
    unit,
    'ft' = {
      create_rectangle(
        x_min = m_to_ft(-11.35),
        x_max = m_to_ft(-10.95),
        y_min = m_to_ft(- 2.55),
        y_max = m_to_ft(- 2.45)
      )
    },

    'm' = {
      create_rectangle(
        x_min = -11.35,
        x_max = -10.95,
        y_min = - 2.55,
        y_max = - 2.45
      )
    }
  )

  # The third set of lane lines are 85cm from the second lane line, and measure
  # 5cm in width, and extend 10cm towards the nearest sideline
  lane_line_3 = switch(
    unit,
    'ft' = {
      create_rectangle(
        x_min = m_to_ft(-10.10),
        x_max = m_to_ft(-10.05),
        y_min = m_to_ft(-2.55),
        y_max = m_to_ft(-2.45)
      )
    },

    'm' = {
      create_rectangle(
        x_min = -10.10,
        x_max = -10.05,
        y_min = - 2.55,
        y_max = - 2.45
      )
    }
  )

  # The fourth set of lane lines are 85cm from the third lane line, and measure
  # 5cm in width, and extend 10cm towards the nearest sideline
  lane_line_4 = switch(
    unit,
    'ft' = {
      create_rectangle(
        x_min = m_to_ft(-9.20),
        x_max = m_to_ft(-9.15),
        y_min = m_to_ft(-2.55),
        y_max = m_to_ft(-2.45)
      )
    },

    'm' = {
      create_rectangle(
        x_min = -9.20,
        x_max = -9.15,
        y_min = -2.55,
        y_max = -2.45
      )
    }
  )

  # Lane line 4 but on the other side of the free-throw lane
  lane_line_5 = reflect(
    lane_line_4,
    over_x = TRUE,
    over_y = FALSE
  )

  # Lane line 3 but on the other side of the free-throw lane
  lane_line_6 = reflect(
    lane_line_3,
    over_x = TRUE,
    over_y = FALSE
  )

  # Lane line 2 but on the other side of the free-throw lane
  lane_line_7 = reflect(
    lane_line_2,
    over_x = TRUE,
    over_y = FALSE
  )

  # Lane line 1 but on the other side of the free-throw lane
  lane_line_8 = reflect(
    lane_line_1,
    over_x = TRUE,
    over_y = FALSE
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    lane_line_1 = rbind(
      lane_line_1,
      reflect(
        lane_line_1,
        over_y = TRUE
      )
    )

    lane_line_2 = rbind(
      lane_line_2,
      reflect(
        lane_line_2,
        over_y = TRUE
      )
    )

    lane_line_3 = rbind(
      lane_line_3,
      reflect(
        lane_line_3,
        over_y = TRUE
      )
    )

    lane_line_4 = rbind(
      lane_line_4,
      reflect(
        lane_line_4,
        over_y = TRUE
      )
    )

    lane_line_5 = rbind(
      lane_line_5,
      reflect(
        lane_line_5,
        over_y = TRUE
      )
    )

    lane_line_6 = rbind(
      lane_line_6,
      reflect(
        lane_line_6,
        over_y = TRUE
      )
    )

    lane_line_7 = rbind(
      lane_line_7,
      reflect(
        lane_line_7,
        over_y = TRUE
      )
    )

    lane_line_8 = rbind(
      lane_line_8,
      reflect(
        lane_line_8,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    lane_line_1 = rotate_coords(
      lane_line_1,
      rotation_dir
    )

    lane_line_2 = rotate_coords(
      lane_line_2,
      rotation_dir
    )

    lane_line_3 = rotate_coords(
      lane_line_3,
      rotation_dir
    )

    lane_line_4 = rotate_coords(
      lane_line_4,
      rotation_dir
    )

    lane_line_5 = rotate_coords(
      lane_line_5,
      rotation_dir
    )

    lane_line_6 = rotate_coords(
      lane_line_6,
      rotation_dir
    )

    lane_line_7 = rotate_coords(
      lane_line_7,
      rotation_dir
    )

    lane_line_8 = rotate_coords(
      lane_line_8,
      rotation_dir
    )
  }

  # Return the feature's data frames as a list
  lane_lines = list(
    lane_line_1 = lane_line_1,
    lane_line_2 = lane_line_2,
    lane_line_3 = lane_line_3,
    lane_line_4 = lane_line_4,
    lane_line_5 = lane_line_5,
    lane_line_6 = lane_line_6,
    lane_line_7 = lane_line_7,
    lane_line_8 = lane_line_8
  )

  return(lane_lines)
}

#' Generate the data frames for the points that comprise the free-throw
#' semi-circles (where a free-throw shooter would shoot from)
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param unit A string indicating the unit with which to make the plot.
#'   Default: \code{'ft'}
#'
#' @return A list of data frame containing the points that comprise the
#'   free-throw semi-circles (where a free-throw shooter would shoot from)
fiba_feature_free_throw_semi_circle = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw', unit = 'ft'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The free-throw semi-circle has radius 1.8m and is centered at the center of
  # the free-throw line (5.8m from the interior edge of the baseline)
  free_throw_semi_circle_line = switch(
    unit,
    'ft' = {
      rbind(
        create_circle(
          center = c(m_to_ft(-8.2), m_to_ft(0)),
          start = .5,
          end = -.5,
          d = m_to_ft(3.6)
        ),

        data.frame(
          x = m_to_ft(-8.2),
          y = m_to_ft(-1.8)
        ),

        create_circle(
          center = c(m_to_ft(-8.2), m_to_ft(0)),
          start = -.5,
          end = .5,
          d = m_to_ft(3.5)
        ),

        data.frame(
          x = m_to_ft(-8.2),
          y = m_to_ft(1.8)
        )
      )
    },

    'm' = {
      rbind(
        create_circle(
          center = c(-8.2, 0),
          start = .5,
          end = -.5,
          d = 3.6
        ),

        data.frame(
          x = -8.2,
          y = -1.8
        ),

        create_circle(
          center = c(-8.2, 0),
          start = -.5,
          end = .5,
          d = 3.5
        ),

        data.frame(
          x = -8.2,
          y =  1.8
        )
      )
    }
  )

  free_throw_semi_circle_fill = switch(
    unit,
    'ft' = {
      rbind(
        create_circle(
          center = c(m_to_ft(-8.2), m_to_ft(0)),
          start = -.5,
          end = .5,
          d = m_to_ft(3.5)
        ),

        create_circle(
          center = c(m_to_ft(-8.2), m_to_ft(0)),
          start = -.5,
          end = .5,
          d = m_to_ft(3.5)
        )[1, ]
      )
    },

    'm' = {
      rbind(
        create_circle(
          center = c(-8.2, 0),
          start = -.5,
          end = .5,
          d = 3.5
        ),

        create_circle(
          center = c(-8.2, 0),
          start = -.5,
          end = .5,
          d = 3.5
        )[1, ]
      )
    }
  )

  free_throw_semi_circle_fill = switch(
    unit,
    'ft' = {
      # Keep the centering aligned, but only keep the points that are inside the
      # semi-circle's fill
      free_throw_semi_circle_fill[free_throw_semi_circle_fill$x >= m_to_ft(-8.2), ]
    },

    'm' = {
      # Keep the centering aligned, but only keep the points that are inside the
      # semi-circle's fill
      free_throw_semi_circle_fill[free_throw_semi_circle_fill$x >= -8.2, ]
    }
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    free_throw_semi_circle_line = rbind(
      free_throw_semi_circle_line,
      reflect(
        free_throw_semi_circle_line,
        over_y = TRUE
      )
    )

    free_throw_semi_circle_fill = rbind(
      free_throw_semi_circle_fill,
      reflect(
        free_throw_semi_circle_fill,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    free_throw_semi_circle_line = rotate_coords(
      free_throw_semi_circle_line,
      rotation_dir
    )

    free_throw_semi_circle_fill = rotate_coords(
      free_throw_semi_circle_fill,
      rotation_dir
    )
  }

  # Return the feature's data frames as a list
  free_throw_semi_circle = list(
    free_throw_semi_circle_line = free_throw_semi_circle_line,
    free_throw_semi_circle_fill = free_throw_semi_circle_fill
  )

  return(free_throw_semi_circle)
}

#' Generate the data frame for the points that comprise the restricted area arc
#' underneath the basket
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param unit A string indicating the unit with which to make the plot.
#'   Default: \code{'ft'}
#'
#' @return A data frame containing the points that comprise the restricted area
#'   arc underneath the basket
fiba_feature_restricted_area_arc = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw', unit = 'ft'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # Following the same process as for the three-point line, the restricted area
  # arc's starting and ending angle can be computed
  start_y = switch(
    unit,
    'ft' = {
      m_to_ft(-1.3)
    },

    'm' = {
      -1.3
    }
  )

  # The rule book describes the arc as having an inner radius of 1.25m and
  # thickness of 5cm
  radius_outer = switch(
    unit,
    'ft' = {
      m_to_ft(1.3)
    },

    'm' = {
      1.3
    }
  )

  # From here, the calculation is relatively straightforward. To determine the
  # angle, the inverse sine is needed. It will be multiplied by pi so that it
  # can be passed to the create_circle() function
  start_angle_outer = asin(start_y / radius_outer) / pi
  end_angle_outer = -start_angle_outer

  # The same method can be used for the inner angles, however, since the inner
  # radius will be traced from bottom to top, the angle must be negative to
  # start
  radius_inner = switch(
    unit,
    'ft' = {
      m_to_ft(1.25)
    },

    'm' = {
      1.25
    }
  )

  start_angle_inner = switch(
    unit,
    'ft' = {
      -asin(m_to_ft(-1.25) / radius_inner) / pi
    },

    'm' = {
      -asin(-1.25 / radius_inner) / pi
    }
  )

  end_angle_inner = -start_angle_inner

  # The restricted area arc is an arc of radius 1.25m from the center of the
  # basket, and extending in a straight line to the front face of the backboard,
  # and having thickness of 5cm
  restricted_area_arc = switch(
    unit,
    'ft' = {
      rbind(
        data.frame(
          x = m_to_ft(-12.8),
          y = m_to_ft(-1.3)
        ),

        create_circle(
          center = c(m_to_ft(-12.425), m_to_ft(0)),
          start = start_angle_outer,
          end = end_angle_outer,
          d = m_to_ft(2.6)
        ),

        data.frame(
          x = c(m_to_ft(-12.8), m_to_ft(-12.8)),
          y = c(m_to_ft(1.3), m_to_ft(1.25))
        ),

        create_circle(
          center = c(m_to_ft(-12.425), m_to_ft(0)),
          start = start_angle_inner,
          end = end_angle_inner,
          d = m_to_ft(2.5)
        ),

        data.frame(
          x = c(m_to_ft(-12.8), m_to_ft(-12.8)),
          y = c(m_to_ft(-1.25), m_to_ft(-1.3))
        )
      )
    },

    'm' = {
      rbind(
        data.frame(
          x = -12.8,
          y = -1.3
        ),

        create_circle(
          center = c(-12.425, 0),
          start = start_angle_outer,
          end = end_angle_outer,
          d = 2.6
        ),

        data.frame(
          x = c(-12.8, -12.8),
          y = c(1.3, 1.25)
        ),

        create_circle(
          center = c(-12.425, 0),
          start = start_angle_inner,
          end = end_angle_inner,
          d = 2.5
        ),

        data.frame(
          x = c(-12.8, -12.8),
          y = c(-1.25, -1.3)
        )
      )
    }
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    restricted_area_arc = rbind(
      restricted_area_arc,
      reflect(
        restricted_area_arc,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    restricted_area_arc = rotate_coords(
      restricted_area_arc,
      rotation_dir
    )
  }

  return(restricted_area_arc)
}

#' Generate the data frame for the points that comprise the backboard
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param unit A string indicating the unit with which to make the plot.
#'   Default: \code{'ft'}
#'
#' @return A data frame containing the points that comprise the backboard
fiba_feature_backboard = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw', unit = 'ft'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # In FIBA, the backboard is 1.8m in width, with its front edge 1.2m from the
  # interior of the baseline
  backboard = switch(
    unit,
    'ft' = {
      create_rectangle(
        x_min = m_to_ft(-12.8),
        x_max = m_to_ft(-12.9),
        y_min = m_to_ft(- 0.9),
        y_max = m_to_ft(  0.9)
      )
    },

    'm' = {
      create_rectangle(
        x_min = -12.8,
        x_max = -12.9,
        y_min = - 0.9,
        y_max =   0.9
      )
    }
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    backboard = rbind(
      backboard,
      reflect(
        backboard,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    backboard = rotate_coords(
      backboard,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(backboard)
}

#' Generate the data frame for the points that comprise the basket ring
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param unit A string indicating the unit with which to make the plot.
#'   Default: \code{'ft'}
#'
#' @return A data frame containing the points that comprise the basket ring
fiba_feature_basket_ring = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw', unit = 'ft'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The connector has a width of .126m, so .063m are on either side of the x
  # axis. The ring has a radius of .225m, so the arcsine of these measurements
  # should give the angle at which point they connect
  start_angle = switch(
    unit,
    'ft' = {
      pi - asin(m_to_ft(.063)/m_to_ft(.225))
    },

    'm' = {
      pi - asin(0.063/0.225)
    }
  )

  # The ending angle of the ring would be the negative of the starting angle
  end_angle = -start_angle

  # Get the ring
  basket_ring = switch(
    unit,
    'ft' = {
      rbind(
        data.frame(
          x = c(
            m_to_ft(-12.8),
            m_to_ft(-12.8) - (m_to_ft(.45) * cos(start_angle))
          ),

          y = c(
            m_to_ft(.063),
            m_to_ft(.063)
          )
        ),

        create_circle(
          center = c(m_to_ft(-12.425), m_to_ft(0)),
          start = start_angle,
          end = end_angle,
          d = m_to_ft(.55)
        ),

        data.frame(
          x = c(
            m_to_ft(-12.8) - (m_to_ft(.45) * cos(start_angle)),
            m_to_ft(-12.8),
            m_to_ft(-12.8)
          ),

          y = c(
            m_to_ft(-.063),
            m_to_ft(-.063),
            m_to_ft(.063)
          )
        )
      )
    },

    'm' = {
      rbind(
        data.frame(
          x = c(
            -12.8,
            -12.8 - (.45) * cos(start_angle)
          ),

          y = c(
            .063,
            .063
          )
        ),

        create_circle(
          center = c(-12.425, 0),
          start = start_angle,
          end = end_angle,
          d = .55
        ),

        data.frame(
          x = c(
            -12.8 - (.45) * cos(start_angle),
            -12.8,
            -12.8
          ),

          y = c(
            -.063,
            -.063,
            .063
          )
        )
      )
    }
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    basket_ring = rbind(
      basket_ring,
      reflect(
        basket_ring,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    basket_ring = rotate_coords(
      basket_ring,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(basket_ring)
}

#' Generate the data frame for the points that comprise the net
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#' @param unit A string indicating the unit with which to make the plot.
#'   Default: \code{'ft'}
#'
#' @return A data frame containing the points that comprise the net
fiba_feature_net = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw', unit = 'ft'){
  # The ring's center is .375m from the backboard, and 1.575m from the baseline,
  # which means it is centered at (+/-12.425m, 0). The ring has an interior
  # diameter of 450mm, which is where the net is visible from above
  net = switch(
    unit,
    'ft' = {
      create_circle(
        center = c(m_to_ft(-12.425), m_to_ft(0)),
        start = 0,
        end = 2,
        d = m_to_ft(.450)
      )
    },

    'm' = {
      create_circle(
        center = c(-12.425, 0),
        start = 0,
        end = 2,
        d = .450
      )
    }
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    net = rbind(
      net,
      reflect(
        net,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    net = rotate_coords(
      net,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(net)
}

#' Generate the list of colors for a FIBA court plot. The defaults can be
#' overwritten by supplying the names of the list elements to the
#' \code{geom_fiba()} function (or its wrapper \code{geom_basketball()})
#'
#' @param court_background_color A hexadecimal string representing the color to
#'   use for this feature
#' @param center_circle_color A hexadecimal string representing the color to use
#'   for this feature
#' @param division_line_color A hexadecimal string representing the color to use
#'   for this feature
#' @param endline_color A hexadecimal string representing the color to use for
#'   this feature
#' @param sideline_color A hexadecimal string representing the color to use for
#'   this feature
#' @param team_bench_color A hexadecimal string representing the color to use
#'   for this feature
#' @param court_apron_color A hexadecimal string representing the color to use
#'   for this feature
#' @param three_point_line_color A hexadecimal string representing the color to
#'   use for this feature
#' @param two_point_range_color A hexadecimal string representing the color to
#'   use for this feature
#' @param free_throw_lane_color A hexadecimal string representing the color to
#'   use for this feature
#' @param painted_area_color A hexadecimal string representing the color to use
#'   for this feature
#' @param free_throw_lane_lines_color A hexadecimal string representing the
#'   color to use for this feature
#' @param free_throw_semi_circle_line_color A hexadecimal string representing
#'   the color to use for this feature
#' @param free_throw_semi_circle_fill_color A hexadecimal string representing
#'   the color to use for this feature
#' @param restricted_area_arc_color A hexadecimal string representing the color
#'   to use for this feature
#' @param backboard_color A hexadecimal string representing the color to use for
#'   this feature
#' @param basket_ring_color A hexadecimal string representing the color to use
#'   for this feature
#' @param net_color A hexadecimal string representing the color to use for this
#'   feature
#'
#' @return A list of hexadecimal colors to use to color the features on the
#'   resulting plot
fiba_features_set_colors = function(court_background_color = '#d2ab6f',
                                    center_circle_color = '#000000',
                                    division_line_color = '#000000',
                                    endline_color = '#000000',
                                    sideline_color = '#000000',
                                    team_bench_color = '#000000',
                                    court_apron_color = '#d2ab6f',
                                    three_point_line_color = '#000000',
                                    two_point_range_color = '#d2ab6f',
                                    free_throw_lane_color = '#000000',
                                    painted_area_color = '#d2ab6f',
                                    free_throw_lane_lines_color = '#000000',
                                    free_throw_semi_circle_line_color = '#000000',
                                    free_throw_semi_circle_fill_color = '#d2ab6f',
                                    restricted_area_arc_color = '#000000',
                                    backboard_color = '#000000',
                                    basket_ring_color = '#000000',
                                    net_color = '#ffffff'
){

  # Create the colors to use for the plot
  feature_colors = list(
    court_background_color = court_background_color,
    center_circle_color = center_circle_color,
    division_line_color = division_line_color,
    endline_color = endline_color,
    sideline_color = sideline_color,
    team_bench_color = team_bench_color,
    court_apron_color = court_apron_color,
    three_point_line_color = three_point_line_color,
    two_point_range_color = two_point_range_color,
    free_throw_lane_color = free_throw_lane_color,
    painted_area_color = painted_area_color,
    free_throw_lane_lines_color = free_throw_lane_lines_color,
    free_throw_semi_circle_line_color = free_throw_semi_circle_line_color,
    free_throw_semi_circle_fill_color = free_throw_semi_circle_fill_color,
    restricted_area_arc_color = restricted_area_arc_color,
    backboard_color = backboard_color,
    basket_ring_color = basket_ring_color,
    net_color = net_color
  )

  # Return the list of colors
  return(feature_colors)
}

#' Create a ggplot2 instance that represents a regulation FIBA court, with the
#' center of the court corresponding to (0, 0)
#'
#' @param full_surf A boolean indicating whether or not to draw a full-surface
#'   representation of the playing surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not the surface representation
#'   needs to be rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the surface
#'   representation. Default: \code{'ccw'}
#' @param unit A string indicating the unit with which to make the plot.
#'   Default: \code{'ft'}
#' @param caption_color A hexadecimal string representing the color to use for
#'   the plot's caption. Default: '#707372' (grey)
#' @param background_color A hexadecimal string representing the color to use for
#'   the plot's background. Default: \code{NULL}
#' @param ... Additional arguments to pass to the function. These should be the
#'   colors to pass to the \code{fiba_features_set_colors()} function
#'
#' @return A ggplot2 instance that represents a regulation NBA court
geom_fiba = function(full_surf = TRUE,
                    rotate = FALSE,
                    rotation_dir = 'ccw',
                    unit = 'ft',
                    caption_color = '#707372',
                    background_color = NULL,
                    ...
){
  # Create the colors to use for the plot
  color_list = fiba_features_set_colors(...)

  # Generate the data frames for the features of an NBA court
  court_background = fiba_feature_court_bground(full_surf, rotate, rotation_dir, unit)
  center_circle = fiba_feature_center_circle(full_surf, rotate, rotation_dir, unit)
  division_line = fiba_feature_division_line(full_surf, rotate, rotation_dir, unit)
  endline = fiba_feature_endline(full_surf, rotate, rotation_dir, unit)
  sideline = fiba_feature_sideline(full_surf, rotate, rotation_dir, unit)
  court_apron = fiba_feature_court_apron(full_surf, rotate, rotation_dir, unit)
  team_bench = fiba_feature_team_bench(full_surf, rotate, rotation_dir, unit)
  three_point_line = fiba_feature_three_point_line(full_surf, rotate, rotation_dir, unit)
  free_throw_lane = fiba_feature_free_throw_lane(full_surf, rotate, rotation_dir, unit)
  free_throw_lane_lines = fiba_feature_free_throw_lane_lines(full_surf, rotate, rotation_dir, unit)
  free_throw_semi_circle = fiba_feature_free_throw_semi_circle(full_surf, rotate, rotation_dir, unit)
  restricted_area_arc = fiba_feature_restricted_area_arc(full_surf, rotate, rotation_dir, unit)
  backboard = fiba_feature_backboard(full_surf, rotate, rotation_dir, unit)
  basket_ring = fiba_feature_basket_ring(full_surf, rotate, rotation_dir, unit)
  net = fiba_feature_net(full_surf, rotate, rotation_dir, unit)

  # Create the initial ggplot2 instance onto which the features will be added
  g = create_plot_base(rotate, caption_color, background_color)

  # Add the features to the ggplot2 instance
  g = add_feature(g, court_background, color_list$court_background_color)
  g = add_feature(g, center_circle, color_list$center_circle_color)
  g = add_feature(g, division_line, color_list$division_line_color)
  g = add_feature(g, endline, color_list$endline_color)
  g = add_feature(g, sideline$sideline_1, color_list$sideline_color)
  g = add_feature(g, sideline$sideline_2, color_list$sideline_color)
  g = add_feature(g, court_apron, color_list$court_apron_color)
  g = add_feature(g, team_bench$team_bench_1, color_list$team_bench_color)
  g = add_feature(g, team_bench$team_bench_2, color_list$team_bench_color)
  g = add_feature(g, three_point_line$three_point_line, color_list$three_point_line_color)
  g = add_feature(g, three_point_line$two_point_range, color_list$two_point_range_color)
  g = add_feature(g, free_throw_lane$free_throw_lane, color_list$free_throw_lane_color)
  g = add_feature(g, free_throw_lane$painted_area, color_list$painted_area_color)
  g = add_feature(g, free_throw_lane_lines$lane_line_1, color_list$free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$lane_line_2, color_list$free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$lane_line_3, color_list$free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$lane_line_4, color_list$free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$lane_line_5, color_list$free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$lane_line_6, color_list$free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$lane_line_7, color_list$free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$lane_line_8, color_list$free_throw_lane_lines_color)
  g = add_feature(g, free_throw_semi_circle$free_throw_semi_circle_line, color_list$free_throw_semi_circle_line_color)
  g = add_feature(g, free_throw_semi_circle$free_throw_semi_circle_fill, color_list$free_throw_semi_circle_fill_color)
  g = add_feature(g, restricted_area_arc, color_list$restricted_area_arc_color)
  g = add_feature(g, backboard, color_list$backboard_color)
  g = add_feature(g, basket_ring, color_list$basket_ring_color)
  g = add_feature(g, net, color_list$net_color)

  # Return the ggplot2 instance that contains the court plot
  return(g)
}
