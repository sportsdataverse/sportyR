#' Generate the data frame for the points that comprise a regulation NCAA
#' basketball court
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the court
ncaa_bb_feature_court_bground = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # A regulation college court is 94' long and 50' wide, but this will only draw
  # the half-court, so it will measure 47' long and 25' wide
  court = create_rectangle(
    x_min = -47.0,
    x_max =   0.0,
    y_min = -25.0,
    y_max =  25.0
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

#' Generate the data frame for the points that comprise the center circle
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A list of data frames containing the points that comprise the center
#'   circle
ncaa_bb_feature_center_circle = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The NCAA court's center circle is 12' in diameter (exterior) with thickness
  # 2"
  center_circle = rbind(
    create_circle(
      center = c(0, 0),
      start = .5,
      end = 1.5,
      d = 12
    ),
    data.frame(
      x = 0,
      y = -6 + (2/12)
    ),
    create_circle(
      center = c(0, 0),
      start = 1.5,
      end = .5,
      d = 12 - (4/12)
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

#' Generate the data frame for the points that comprise the division line
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the division line
ncaa_bb_feature_division_line = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The line is 2" thick, goes right through the middle of the court (1" of its
  # width on each side of it), and spans the width of the court
  division_line = create_rectangle(
    x_min = -1/12,
    x_max = 0,
    y_min = -25,
    y_max = 25
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
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the end lines
ncaa_bb_feature_endline = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The endline is 47' (interior) from the center of the court, spans the width
  # of the court, and is 2" in thickness
  endline = create_rectangle(
    x_min = -47 - (2/12),
    x_max = -47,
    y_min = -25,
    y_max = 25
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

#' Generate the data frames for the points that comprise the sidelines
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A list of data frames containing the points that comprise the
#'   sidelines
ncaa_bb_feature_sideline = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The sideline is 25' (interior) from the center of the court, spans the
  # length of the court, and is 2" in thickness

  # First sideline
  sideline_1 = create_rectangle(
    x_min = -47 - (2/12),
    x_max = 0,
    y_min = -25 - (2/12),
    y_max = -25
  )

  # Second sideline
  sideline_2 = create_rectangle(
    x_min = -47 - (2/12),
    x_max = 0,
    y_min = 25,
    y_max = 25 + (2/12)
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
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A list of data frames containing the points that comprise the team
#'   bench
ncaa_bb_feature_team_bench = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The team bench is 28' (interior) from the nearest endline sideline,
  # protrudes 3' into the court, and is 2" in thickness
  team_bench = create_rectangle(
    x_min = -19,
    x_max = -19 + (2/12),
    y_min = 22,
    y_max = 28
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
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
    team_bench = rotate_coords(
      team_bench,
      rotation_dir
    )
  }

  return(team_bench)
}

#' Generate the data frame for the points that comprise the substitution area
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the substitution area
ncaa_bb_feature_substitution_area = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The substitution areas are 8' 2" (interior) apart, or 4' 1" from the center
  # of the division line. They extend 4' from the boundary line, and are 2"
  # thick

  # Substitution area
  substitution_area = create_rectangle(
    x_min = -9,
    x_max = -9 + (2/12),
    y_min = 25 + (2/12),
    y_max = 27 + (2/12)
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    substitution_area = rbind(
      substitution_area,
      reflect(
        substitution_area,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    substitution_area = rotate_coords(
      substitution_area,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(substitution_area)
}

#' Generate the data frame for the points that comprise the court apron
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the court apron
ncaa_bb_feature_court_apron = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The court apron is the boundary on the court, which may be made of a
  # contrasting color. It must be at least 6' behind the baseline and 3' from
  # the sideline, but 4' will be used for the sideline apron
  court_apron = data.frame(
    x = c(
      0,
      -47 - (2/12),
      -47 - (2/12),
      -19,
      -19,
      -19 + (2/12),
      -19 + (2/12),
      -9,
      -9,
      -9 + (2/12),
      -9 + (2/12),
      0,
      0,
      -53,
      -53,
      0,
      0
    ),

    y = c(
      -25 - (2/12),
      -25 - (2/12),
      25 + (2/12),
      25 + (2/12),
      28,
      28,
      25 + (2/12),
      25 + (2/12),
      27,
      27,
      25 + (2/12),
      25 + (2/12),
      29 + (2/12),
      29 + (2/12),
      -29 - (2/12),
      -29 - (2/12),
      -25 - (2/12)
    )
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
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A list of data frames containing the points that comprise the
#'   three-point line and two-point range
ncaa_bb_feature_three_point_line = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # First, a bit of math is needed to determine the starting and ending angles
  # of the three-point arc, relative to 0 radians. Since in the end, the angle
  # is what matters, the units of measure do not. Inches are easier to use for
  # this calculation. The angle begins 9' 10 3/8" from the interior edge of the
  # endline
  start_x = (9 * 12) + 10 + (3/8)

  # However, the rule book describes the arc as having a radius of 22' 1.75"
  # from the center of the basket. The basket's center is 63" away from the
  # interior of the endline, so this must be subtracted from our starting x
  # position to get the starting x position *relative to the center of the
  # basket*
  start_x = start_x - 63
  radius_outer = (22 * 12) + 1.75

  # From here, the calculation is relatively straightforward. To determine the
  # angle, the inverse cosine is needed. It will be multiplied by pi so that it
  # can be passed to the create_circle() function
  start_angle_outer = acos(start_x / radius_outer) / pi
  end_angle_outer = -start_angle_outer

  # The same method can be used for the inner angles, however, since the inner
  # radius will be traced from bottom to top, the angle must be negative to
  # start
  radius_inner = (22 * 12) + 1.75 - 2
  start_angle_inner = -acos(start_x / radius_inner) / pi
  end_angle_inner = -start_angle_inner

  # According to the rulebook, the men's three-point line is 21' 7 7/8" in the
  # corners
  m_three_point_line = rbind(
    data.frame(
      x = -47,
      y = 21 + ((7 + (7/8)) / 12)
    ),

    create_circle(
      center = c(-41.75, 0),
      start = start_angle_outer,
      end = end_angle_outer,
      d = 2 * (((22 * 12) + 1.75) / 12)
    ),

    data.frame(
      x = c(
        -47,
        -47,
        -47 + (((9 * 12) + 10 + (3/8))/12)
      ),

      y = c(
        -21 - ((7 + (7/8))/12),
        -21 - ((7 + (7/8))/12) + (2/12),
        -21 - ((7 + (7/8))/12) + (2/12)
      )
    ),

    create_circle(
      center = c(-41.75, 0),
      start = start_angle_inner,
      end = end_angle_inner,
      d = 2 * ((((22 * 12) + 1.75)/12) - (2/12))
    ),

    data.frame(
      x = c(
        -47 + (((9 * 12) + 10 + (3/8))/12),
        -47,
        -47
      ),

      y = c(
        21 + ((7 + (7/8))/12) - (2/12),
        21 + ((7 + (7/8))/12) - (2/12),
        21 + ((7 + (7/8))/12)
      )
    )
  )

  m_two_point_range = rbind(
    data.frame(
      x = c(
        -47,
        -47 + (((9 * 12) + 10 + (3/8))/12)
      ),

      y = c(
        -21 - ((7 + (7/8))/12) + (2/12),
        -21 - ((7 + (7/8))/12) + (2/12)
      )
    ),

    create_circle(
      center = c(-41.75, 0),
      start = start_angle_inner,
      end = end_angle_inner,
      d = 2 * ((((22 * 12) + 1.75)/12) - (2/12))
    ),

    data.frame(
      x = c(
        -47 + (((9 * 12) + 10 + (3/8))/12),
        -47,
        -47
      ),

      y = c(
        21 + ((7 + (7/8))/12) - (2/12),
        21 + ((7 + (7/8))/12) - (2/12),
        -21 - ((7 + (7/8))/12) + (2/12)
      )
    )
  )

  # The women's three-point has a distance of 20' 9" in the corners, and an arc
  # of radius 20' 9"
  w_three_point_line = rbind(
    data.frame(
      x = -47,
      y = -20.75
    ),

    create_circle(
      center = c(-41.75, 0),
      start = -.5,
      end = .5,
      d = 41.5
    ),

    data.frame(
      x = c(-47, -47),
      y = c(20.75, 20.75 - (2/12))
    ),

    create_circle(
      center = c(-41.75, 0),
      start = .5,
      end = -.5,
      d = 41.5 - (4/12)
    ),

    data.frame(
      x = c(-47, -47),
      y = c(-20.75 + (2/12), -20.75)
    )
  )

  w_two_point_range = rbind(
    data.frame(
      x = -47,
      y = 20.75 - (2/12)
    ),

    create_circle(
      center = c(-41.75, 0),
      start = .5,
      end = -.5,
      d = 41.5 - (4/12)
    ),

    data.frame(
      x = c(-47, -47),
      y = c(-20.75 + (2/12), 20.75 - (2/12))
    )
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    m_three_point_line = rbind(
      m_three_point_line,
      reflect(
        m_three_point_line,
        over_y = TRUE
      )
    )

    m_two_point_range = rbind(
      m_two_point_range,
      reflect(
        m_two_point_range,
        over_y = TRUE
      )
    )

    w_three_point_line = rbind(
      w_three_point_line,
      reflect(
        w_three_point_line,
        over_y = TRUE
      )
    )

    w_two_point_range = rbind(
      w_two_point_range,
      reflect(
        w_two_point_range,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    m_three_point_line = rotate_coords(
      m_three_point_line,
      rotation_dir
    )

    m_two_point_range = rotate_coords(
      m_two_point_range,
      rotation_dir
    )

    w_three_point_line = rotate_coords(
      w_three_point_line,
      rotation_dir
    )

    w_two_point_range = rotate_coords(
      w_two_point_range,
      rotation_dir
    )
  }

  # Return the feature's data frames as a list
  three_point_arc_two_point_range = list(
    m_three_point_line = m_three_point_line,
    w_three_point_line = w_three_point_line,
    m_two_point_range = m_two_point_range,
    w_two_point_range = w_two_point_range
  )

  return(three_point_arc_two_point_range)
}

#' Generate the data frames for the points that comprise both the professional
#' and amateur free-throw lanes
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A list of data frames containing the points that comprise both the
#'   professional and amateur free-throw lanes
ncaa_bb_feature_free_throw_lane = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The free-throw lane measures 19' from the inside edge of the endline to the
  # outside edge of the free-throw line, measures 8' wide (exterior) and has
  # lines of width of 2"
  professional_free_throw_lane = data.frame(
    x = c(
      -47,
      -28,
      -28,
      -47,
      -47,
      -28 - (2/12),
      -28 - (2/12),
      -47,
      -47
    ),

    y = c(
      -8,
      -8,
      8,
      8,
      8 - (2/12),
      8 - (2/12),
      -8 + (2/12),
      -8 + (2/12),
      -8
    )
  )

  professional_painted_area = create_rectangle(
    x_min = -47,
    x_max = -28 - (2/12),
    y_min = -8 + (2/12),
    y_max = 8 - (2/12)
  )

  # Some courts have the amateur (NCAA) free-throw lane included as well
  amateur_free_throw_lane = data.frame(
    x = c(
      -47,
      -28,
      -28,
      -47,
      -47,
      -28 - (2/12),
      -28 - (2/12),
      -47,
      -47
    ),

    y = c(
      -6,
      -6,
      6,
      6,
      6 - (2/12),
      6 - (2/12),
      -6 + (2/12),
      -6 + (2/12),
      -6
    )
  )

  amateur_painted_area = create_rectangle(
    x_min = -47,
    x_max = -28 - (2/12),
    y_min = -6 + (2/12),
    y_max = 6 - (2/12)
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    professional_free_throw_lane = rbind(
      professional_free_throw_lane,
      reflect(
        professional_free_throw_lane,
        over_y = TRUE
      )
    )

    professional_painted_area = rbind(
      professional_painted_area,
      reflect(
        professional_painted_area,
        over_y = TRUE
      )
    )

    amateur_free_throw_lane = rbind(
      amateur_free_throw_lane,
      reflect(
        amateur_free_throw_lane,
        over_y = TRUE
      )
    )

    amateur_painted_area = rbind(
      amateur_painted_area,
      reflect(
        amateur_painted_area,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    professional_free_throw_lane = rotate_coords(
      professional_free_throw_lane,
      rotation_dir
    )

    professional_painted_area = rotate_coords(
      professional_painted_area,
      rotation_dir
    )

    amateur_free_throw_lane = rotate_coords(
      amateur_free_throw_lane,
      rotation_dir
    )

    amateur_painted_area = rotate_coords(
      amateur_painted_area,
      rotation_dir
    )
  }

  # Return the feature's data frames as a list
  lane_painted_area = list(
    professional_free_throw_lane = professional_free_throw_lane,
    professional_painted_area = professional_painted_area,
    amateur_free_throw_lane = amateur_free_throw_lane,
    amateur_painted_area = amateur_painted_area
  )

  return(lane_painted_area)
}

#' Generate the data frames for the points that comprise the free-throw lane
#' lines (the blocks)
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A list of data frames containing the points that comprise the
#'   free-throw lane lines (the blocks)
ncaa_bb_feature_free_throw_lane_lines = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The first block is 7' (interior) from the endline, measures 2" in width, and
  # extends 6" towards the nearest sideline
  professional_lane_line_1 = create_rectangle(
    x_min = -40,
    x_max = -40 + (2 / 12),
    y_min = -8.5,
    y_max = -8
  )

  # The second lane line is 8" (interior) from the first lane line, measures 2"
  # in width, and extend 6" towards the nearest sideline
  professional_lane_line_2 = create_rectangle(
    x_min = -40 + (10 / 12),
    x_max = -39,
    y_min = -8.5,
    y_max = -8
  )

  # The third lane line is 3' (interior) from the second lane line, measures 2"
  # in width, and extend 6" towards the nearest sideline
  professional_lane_line_3 = create_rectangle(
    x_min = -36,
    x_max = -36 + (2 / 12),
    y_min = -8.5,
    y_max = -8
  )

  # The fourth lane line is 3' (interior) from the third lane line, measures 2"
  # in width, and extend 6" towards the nearest sideline
  professional_lane_line_4 = create_rectangle(
    x_min = -33 + (2 / 12),
    x_max = -33 + (4 / 12),
    y_min = -8.5,
    y_max = -8
  )

  # Lane line 4 but on the other side of the free-throw lane
  professional_lane_line_5 = reflect(
    professional_lane_line_4,
    over_x = TRUE,
    over_y = FALSE
  )

  # Lane line 3 but on the other side of the free-throw lane
  professional_lane_line_6 = reflect(
    professional_lane_line_3,
    over_x = TRUE,
    over_y = FALSE
  )

  # Lane line 2 but on the other side of the free-throw lane
  professional_lane_line_7 = reflect(
    professional_lane_line_2,
    over_x = TRUE,
    over_y = FALSE
  )

  # Lane line 1 but on the other side of the free-throw lane
  professional_lane_line_8 = reflect(
    professional_lane_line_1,
    over_x = TRUE,
    over_y = FALSE
  )

  # The first set of lane lines are 7' from the interior of the baseline,
  # measure 1' in width, and extend 8" towards the nearest sideline
  amateur_lane_line_1 = create_rectangle(
    x_min = -40,
    x_max = -39,
    y_min = -6 - (8 / 12),
    y_max = -6
  )

  # The second set of lane lines are 3' from the first lane line, measure 2" in
  # width, and extend 8" towards the nearest sideline
  amateur_lane_line_2 = create_rectangle(
    x_min = -36,
    x_max = -36 + (2 / 12),
    y_min = -6 - (8 / 12),
    y_max = -6
  )

  # The third set of lane lines are 3' from the second lane line, measure 2" in
  # width, and extend 8" towards the nearest sideline
  amateur_lane_line_3 = create_rectangle(
    x_min = -33 + (2 / 12),
    x_max = -33 + (4 / 12),
    y_min = -6 - (8 / 12),
    y_max = -6
  )

  # The fourth set of lane lines are 3' from the third lane line, measure 2" in
  # width, and extend 8" towards the nearest sideline
  amateur_lane_line_4 = create_rectangle(
    x_min = -30 + (4 / 12),
    x_max = -30 + (6 / 12),
    y_min = -6 - (8 / 12),
    y_max = -6
  )

  # Lane line 4 but on the other side of the free-throw lane
  amateur_lane_line_5 = reflect(
    amateur_lane_line_4,
    over_x = TRUE,
    over_y = FALSE
  )

  # Lane line 3 but on the other side of the free-throw lane
  amateur_lane_line_6 = reflect(
    amateur_lane_line_3,
    over_x = TRUE,
    over_y = FALSE
  )

  # Lane line 2 but on the other side of the free-throw lane
  amateur_lane_line_7 = reflect(
    amateur_lane_line_2,
    over_x = TRUE,
    over_y = FALSE
  )

  # Lane line 1 but on the other side of the free-throw lane
  amateur_lane_line_8 = reflect(
    amateur_lane_line_1,
    over_x = TRUE,
    over_y = FALSE
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    professional_lane_line_1 = rbind(
      professional_lane_line_1,
      reflect(
        professional_lane_line_1,
        over_y = TRUE
      )
    )

    professional_lane_line_2 = rbind(
      professional_lane_line_2,
      reflect(
        professional_lane_line_2,
        over_y = TRUE
      )
    )

    professional_lane_line_3 = rbind(
      professional_lane_line_3,
      reflect(
        professional_lane_line_3,
        over_y = TRUE
      )
    )

    professional_lane_line_4 = rbind(
      professional_lane_line_4,
      reflect(
        professional_lane_line_4,
        over_y = TRUE
      )
    )

    professional_lane_line_5 = rbind(
      professional_lane_line_5,
      reflect(
        professional_lane_line_5,
        over_y = TRUE
      )
    )

    professional_lane_line_6 = rbind(
      professional_lane_line_6,
      reflect(
        professional_lane_line_6,
        over_y = TRUE
      )
    )

    professional_lane_line_7 = rbind(
      professional_lane_line_7,
      reflect(
        professional_lane_line_7,
        over_y = TRUE
      )
    )

    professional_lane_line_8 = rbind(
      professional_lane_line_8,
      reflect(
        professional_lane_line_8,
        over_y = TRUE
      )
    )

    amateur_lane_line_1 = rbind(
      amateur_lane_line_1,
      reflect(
        amateur_lane_line_1,
        over_y = TRUE
      )
    )

    amateur_lane_line_2 = rbind(
      amateur_lane_line_2,
      reflect(
        amateur_lane_line_2,
        over_y = TRUE
      )
    )

    amateur_lane_line_3 = rbind(
      amateur_lane_line_3,
      reflect(
        amateur_lane_line_3,
        over_y = TRUE
      )
    )

    amateur_lane_line_4 = rbind(
      amateur_lane_line_4,
      reflect(
        amateur_lane_line_4,
        over_y = TRUE
      )
    )

    amateur_lane_line_5 = rbind(
      amateur_lane_line_5,
      reflect(
        amateur_lane_line_5,
        over_y = TRUE
      )
    )

    amateur_lane_line_6 = rbind(
      amateur_lane_line_6,
      reflect(
        amateur_lane_line_6,
        over_y = TRUE
      )
    )

    amateur_lane_line_7 = rbind(
      amateur_lane_line_7,
      reflect(
        amateur_lane_line_7,
        over_y = TRUE
      )
    )

    amateur_lane_line_8 = rbind(
      amateur_lane_line_8,
      reflect(
        amateur_lane_line_8,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    professional_lane_line_1 = rotate_coords(
      professional_lane_line_1,
      rotation_dir
    )

    professional_lane_line_2 = rotate_coords(
      professional_lane_line_2,
      rotation_dir
    )

    professional_lane_line_3 = rotate_coords(
      professional_lane_line_3,
      rotation_dir
    )

    professional_lane_line_4 = rotate_coords(
      professional_lane_line_4,
      rotation_dir
    )

    professional_lane_line_5 = rotate_coords(
      professional_lane_line_5,
      rotation_dir
    )

    professional_lane_line_6 = rotate_coords(
      professional_lane_line_6,
      rotation_dir
    )

    professional_lane_line_7 = rotate_coords(
      professional_lane_line_7,
      rotation_dir
    )

    professional_lane_line_8 = rotate_coords(
      professional_lane_line_8,
      rotation_dir
    )

    amateur_lane_line_1 = rotate_coords(
      amateur_lane_line_1,
      rotation_dir
    )

    amateur_lane_line_2 = rotate_coords(
      amateur_lane_line_2,
      rotation_dir
    )

    amateur_lane_line_3 = rotate_coords(
      amateur_lane_line_3,
      rotation_dir
    )

    amateur_lane_line_4 = rotate_coords(
      amateur_lane_line_4,
      rotation_dir
    )

    amateur_lane_line_5 = rotate_coords(
      amateur_lane_line_5,
      rotation_dir
    )

    amateur_lane_line_6 = rotate_coords(
      amateur_lane_line_6,
      rotation_dir
    )

    amateur_lane_line_7 = rotate_coords(
      amateur_lane_line_7,
      rotation_dir
    )

    amateur_lane_line_8 = rotate_coords(
      amateur_lane_line_8,
      rotation_dir
    )
  }

  # Return the feature's data frames as a list
  lane_lines = list(
    professional_lane_line_1 = professional_lane_line_1,
    professional_lane_line_2 = professional_lane_line_2,
    professional_lane_line_3 = professional_lane_line_3,
    professional_lane_line_4 = professional_lane_line_4,
    professional_lane_line_5 = professional_lane_line_5,
    professional_lane_line_6 = professional_lane_line_6,
    professional_lane_line_7 = professional_lane_line_7,
    professional_lane_line_8 = professional_lane_line_8,
    amateur_lane_line_1 = amateur_lane_line_1,
    amateur_lane_line_2 = amateur_lane_line_2,
    amateur_lane_line_3 = amateur_lane_line_3,
    amateur_lane_line_4 = amateur_lane_line_4,
    amateur_lane_line_5 = amateur_lane_line_5,
    amateur_lane_line_6 = amateur_lane_line_6,
    amateur_lane_line_7 = amateur_lane_line_7,
    amateur_lane_line_8 = amateur_lane_line_8
  )

  return(lane_lines)
}

#' Generate the data frames for the points that comprise the free-throw
#' semi-circles (where a free-throw shooter would shoot from)
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A list of data frame containing the points that comprise the
#'   free-throw semi-circles (where a free-throw shooter would shoot from)
ncaa_bb_feature_free_throw_semi_circle = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The free-throw semi-circle has radius 6' and is centered at the midpoint of
  # the outer edge of the free-throw line
  free_throw_semi_circle_line = rbind(
    create_circle(
      center = c(-28, 0),
      start = .5,
      end = -.5,
      d = 12
    ),

    data.frame(
      x = -28,
      y = -6
    ),

    create_circle(
      center = c(-28, 0),
      start = -.5,
      end = .5,
      d = 12 - (4/12)
    ),

    data.frame(
      x = -28,
      y = 6
    )
  )

  # Sometimes, the actual semi-circle is a different color than the court or
  # two-point area
  free_throw_semi_circle_fill = rbind(
    create_circle(
      center = c(-28, 0),
      start = -.5,
      end = .5,
      d = 12 - (4/12)
    ),

    create_circle(
      center = c(-28, 0),
      start = -.5,
      end = .5,
      d = 12 - (4/12)
    )[1, ]
  )

  # Keep the centering aligned, but only keep the points that are inside the
  # semi-circle's fill
  free_throw_semi_circle_fill = free_throw_semi_circle_fill[free_throw_semi_circle_fill$x >= -28, ]

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

#' Generate the data frames for the points that comprise the lower defensive box
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the lower defensive
#'   box
ncaa_bb_feature_lower_defensive_box = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The lower defensive box is comprised of two dashes along the baseline that
  # are 3' (interior) from the edges of the free-throw lane, protrude 12" into
  # the court, and have a 2" width

  # Dash 1 is the line along the baseline
  dash_1 = create_rectangle(
    x_min = -47,
    x_max = -46,
    y_min = 9,
    y_max = 9 + (2/12)
  )

  # Dash 2 is the reflection of dash 1 over the x axis
  dash_2 = reflect(
    dash_1,
    over_x = TRUE,
    over_y = FALSE
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    dash_1 = rbind(
      dash_1,
      reflect(
        dash_1,
        over_y = TRUE
      )
    )

    dash_2 = rbind(
      dash_2,
      reflect(
        dash_2,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    dash_1 = rotate_coords(
      dash_1,
      rotation_dir
    )

    dash_2 = rotate_coords(
      dash_2,
      rotation_dir
    )
  }

  # Return the feature's data frames as a list
  lower_defensive_box = list(
    dash_1 = dash_1,
    dash_2 = dash_2
  )

  return(lower_defensive_box)
}

#' Generate the data frame for the points that comprise the restricted area arc
#' underneath the basket
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the restricted area
#'   arc underneath the basket
ncaa_bb_feature_restricted_area_arc = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # Following the same process as for the three-point line, the restricted area
  # arc's starting and ending angle can be computed
  start_y = -4 - (2/12)

  # The rule book describes the arc as having a radius of 4'
  radius_outer = 4 + (2/12)

  # From here, the calculation is relatively straightforward. To determine the
  # angle, the inverse sine is needed. It will be multiplied by pi so that it
  # can be passed to the create_circle() function
  start_angle_outer = asin(start_y / radius_outer) / pi
  end_angle_outer = -start_angle_outer

  # The same method can be used for the inner angles, however, since the inner
  # radius will be traced from bottom to top, the angle must be negative to
  # start
  radius_inner = 4
  start_angle_inner = -asin((start_y + (2/12)) / radius_inner) / pi
  end_angle_inner = -start_angle_inner

  # The restricted area arc is an arc of radius 4' from the center of the
  # basket, and extending in a straight line to the front face of the backboard,
  # and having thickness of 2"
  restricted_area_arc = rbind(
    data.frame(
      x = -43,
      y = -4 - (2/12)
    ),

    create_circle(
      center = c(-41.75, 0),
      start = start_angle_outer,
      end = end_angle_outer,
      d = 8 + (4/12)
    ),

    data.frame(
      x = c(-43, -43),
      y = c(4 + (2/12), 4)
    ),

    create_circle(
      center = c(-41.75, 0),
      start = start_angle_inner,
      end = end_angle_inner,
      d = 8
    ),

    data.frame(
      x = c(-43, -43),
      y = c(-4, -4 - (2/12))
    )
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

#' Generate the data frame for the points that comprise the restricted area arc
#' underneath the basket
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the restricted area
#'   arc underneath the basket
ncaa_bb_feature_backboard = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # In college, the backboard is 6' in width, with its front edge 4' from the
  # interior of the baseline
  backboard = create_rectangle(
    x_min = -43 - (4/12),
    x_max = -43,
    y_min = -3,
    y_max = 3
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
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the basket ring
ncaa_bb_feature_basket_ring = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The connector has a width of 7", so 3.5" are on either side of the x
  # axis. The ring has a radius of 9", so the arcsine of these measurements
  # should give the angle at which point they connect
  start_angle = pi - asin(3.5/9)

  # The ending angle of the ring would be the negative of the starting angle
  end_angle = -start_angle

  # Get the ring
  basket_ring = rbind(
    data.frame(
      x = c(-43, -41.75 - ((9/12) * cos(start_angle))),
      y = c(3.5/12, 3.5/12)
    ),

    create_circle(
      center = c(-41.75, 0),
      start = start_angle,
      end = end_angle,
      d = 1.5 + (4/12)
    ),

    data.frame(
      x = c(
        -41.75 - ((9/12) * cos(start_angle)),
        -43,
        -43
      ),

      y = c(
        -3.5/12,
        -3.5/12,
        3.5/12
      )
    )
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
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the net
ncaa_bb_feature_net = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The ring's center is 15" from the backboard, and 63" from the baseline,
  # which means it is centered at (+/-41.75, 0). The ring has an interior
  # diameter of 18", which is where the net is visible from above
  net = create_circle(
    center = c(-41.75, 0),
    start = 0,
    end = 2,
    d = 1.5
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

#' Generate the list of colors for an ncaa_bb court plot. The defaults can be
#' overwritten by supplying the names of the list elements to the
#' \code{geom_ncaa_bb()} function (or its wrapper \code{geom_basketball()})
#'
#' @param court_background_color A hexadecimal string representing the color to
#'   use for this feature
#' @param center_circle_color A hexadecimal string representing the color to use for this feature
#' @param division_line_color A hexadecimal string representing the color to use
#'   for this feature
#' @param endline_color A hexadecimal string representing the color to use for
#'   this feature
#' @param sideline_color A hexadecimal string representing the color to use for
#'   this feature
#' @param team_bench_color A hexadecimal string representing the color to use
#'   for this feature
#' @param substitution_area_color A hexadecimal string representing the color to
#'   use for this feature
#' @param court_apron_color A hexadecimal string representing the color to use
#'   for this feature
#' @param m_three_point_line_color A hexadecimal string representing the color to
#'   use for this feature
#' @param m_two_point_range_color A hexadecimal string representing the color to
#'   use for this feature
#' @param w_three_point_line_color A hexadecimal string representing the color to
#'   use for this feature
#' @param w_two_point_range_color A hexadecimal string representing the color to
#'   use for this feature
#' @param professional_free_throw_lane_color A hexadecimal string representing
#'   the color to use for this feature
#' @param professional_painted_area_color A hexadecimal string representing the
#'   color to use for this feature
#' @param amateur_free_throw_lane_color A hexadecimal string representing the
#'   color to use for this feature
#' @param amateur_painted_area_color A hexadecimal string representing the color
#'   to use for this feature
#' @param professional_free_throw_lane_lines_color A hexadecimal string
#'   representing the color to use for this feature
#' @param amateur_free_throw_lane_lines_color A hexadecimal string representing
#'   the color to use for this feature
#' @param free_throw_semi_circle_line_color A hexadecimal string representing
#'   the color to use for this feature
#' @param free_throw_semi_circle_fill_color A hexadecimal string representing
#'   the color to use for this feature
#' @param lower_defensive_box_color A hexadecimal string representing the color
#'   to use for this feature
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
ncaa_bb_features_set_colors = function(court_background_color = '#d2ab6f',
                                       center_circle_color = '#000000',
                                       division_line_color = '#000000',
                                       endline_color = '#000000',
                                       sideline_color = '#000000',
                                       team_bench_color = '#000000',
                                       substitution_area_color = '#000000',
                                       court_apron_color = '#d2ab6f',
                                       m_three_point_line_color = '#000000',
                                       w_three_point_line_color = '#000000',
                                       m_two_point_range_color = '#d2ab6f',
                                       w_two_point_range_color = '#d2ab6f',
                                       professional_free_throw_lane_color = '#000000',
                                       professional_painted_area_color = '#d2ab6f',
                                       amateur_free_throw_lane_color = '#000000',
                                       amateur_painted_area_color = '#d2ab6f',
                                       professional_free_throw_lane_lines_color = '#000000',
                                       amateur_free_throw_lane_lines_color = '#000000',
                                       free_throw_semi_circle_line_color = '#000000',
                                       free_throw_semi_circle_fill_color = '#d2ab6f',
                                       lower_defensive_box_color = '#000000',
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
    substitution_area_color = substitution_area_color,
    court_apron_color = court_apron_color,
    m_three_point_line_color = m_three_point_line_color,
    w_three_point_line_color = w_three_point_line_color,
    m_two_point_range_color = m_two_point_range_color,
    w_two_point_range_color = w_two_point_range_color,
    professional_free_throw_lane_color = professional_free_throw_lane_color,
    professional_painted_area_color = professional_painted_area_color,
    amateur_free_throw_lane_color = amateur_free_throw_lane_color,
    amateur_painted_area_color = amateur_painted_area_color,
    professional_free_throw_lane_lines_color = professional_free_throw_lane_lines_color,
    amateur_free_throw_lane_lines_color = amateur_free_throw_lane_lines_color,
    free_throw_semi_circle_line_color = free_throw_semi_circle_line_color,
    free_throw_semi_circle_fill_color = free_throw_semi_circle_fill_color,
    lower_defensive_box_color = lower_defensive_box_color,
    restricted_area_arc_color = restricted_area_arc_color,
    backboard_color = backboard_color,
    basket_ring_color = basket_ring_color,
    net_color = net_color
  )

  # Return the list of colors
  return(feature_colors)
}

#' Create a ggplot2 instance that represents a regulation NCAA basketball court,
#' with the center of the court corresponding to (0, 0)
#'
#' @param full_surf A boolean indicating whether or not to draw a full-surface
#'   representation of the playing surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not the surface representation
#'   needs to be rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the surface
#'   representation. Default: \code{'ccw'}
#' @param include_m_three_point_line A boolean indicating whether or not to
#'   include the men's three-point line in the final plot. Default: \code{TRUE}
#' @param include_w_three_point_line A boolean indicating whether or not to
#'   include the women's three-point line in the final plot. Default: \code{TRUE}
#' @param include_professional_free_throw_lane A boolean indicating whether or
#'   not to include the professional((W)NBA) free-throw lane in the final plot.
#'   Default: \code{FALSE}
#' @param include_professional_free_throw_lane_lines A boolean indicating
#'   whether or not to include the professional((W)NBA) free-throw lane lines in
#'   the final plot. Default: \code{FALSE}
#' @param caption_color A hexadecimal string representing the color to use for
#'   the plot's caption. Default: '#707372' (grey)
#' @param background_color A hexadecimal string representing the color to use
#'   for the plot's background. Default: \code{NULL}
#' @param ... Additional arguments to pass to the function. These should be the
#'   colors to pass to the \code{ncaa_bb_features_set_colors()} function
#'
#' @return A ggplot2 instance that represents a regulation NCAA basketball court
geom_ncaa_bb = function(full_surf = TRUE,
                        rotate = FALSE,
                        rotation_dir = 'ccw',
                        include_m_three_point_line = TRUE,
                        include_w_three_point_line = TRUE,
                        include_professional_free_throw_lane = FALSE,
                        include_professional_free_throw_lane_lines = FALSE,
                        caption_color = '#707372',
                        background_color = NULL,
                        ...
){
  # Create the colors to use for the plot
  color_list = ncaa_bb_features_set_colors(...)

  # Generate the data frames for the features of an NCAA basketball court
  court_background = ncaa_bb_feature_court_bground(full_surf, rotate, rotation_dir)
  center_circle = ncaa_bb_feature_center_circle(full_surf, rotate, rotation_dir)
  division_line = ncaa_bb_feature_division_line(full_surf, rotate, rotation_dir)
  endline = ncaa_bb_feature_endline(full_surf, rotate, rotation_dir)
  sideline = ncaa_bb_feature_sideline(full_surf, rotate, rotation_dir)
  court_apron = ncaa_bb_feature_court_apron(full_surf, rotate, rotation_dir)
  team_bench = ncaa_bb_feature_team_bench(full_surf, rotate, rotation_dir)
  substitution_area = ncaa_bb_feature_substitution_area(full_surf, rotate, rotation_dir)
  three_point_line = ncaa_bb_feature_three_point_line(full_surf, rotate, rotation_dir)
  free_throw_lane = ncaa_bb_feature_free_throw_lane(full_surf, rotate, rotation_dir)
  free_throw_lane_lines = ncaa_bb_feature_free_throw_lane_lines(full_surf, rotate)
  free_throw_semi_circle = ncaa_bb_feature_free_throw_semi_circle(full_surf, rotate, rotation_dir)
  restricted_area_arc = ncaa_bb_feature_restricted_area_arc(full_surf, rotate, rotation_dir)
  lower_defensive_box = ncaa_bb_feature_lower_defensive_box(full_surf, rotate, rotation_dir)
  backboard = ncaa_bb_feature_backboard(full_surf, rotate, rotation_dir)
  basket_ring = ncaa_bb_feature_basket_ring(full_surf, rotate, rotation_dir)
  net = ncaa_bb_feature_net(full_surf, rotate, rotation_dir)

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
  g = add_feature(g, team_bench, color_list$team_bench_color)
  g = add_feature(g, substitution_area, color_list$substitution_area_color)
  if(include_m_three_point_line){
    g = add_feature(g, three_point_line$m_three_point_line, color_list$m_three_point_line_color)
    g = add_feature(g, three_point_line$m_two_point_range, color_list$m_two_point_range_color)
  }
  if(include_w_three_point_line){
    g = add_feature(g, three_point_line$w_three_point_line, color_list$w_three_point_line_color)
    g = add_feature(g, three_point_line$w_two_point_range, color_list$w_two_point_range_color)
  }
  if(include_professional_free_throw_lane){
    g = add_feature(g, free_throw_lane$professional_free_throw_lane, color_list$professional_free_throw_lane_color)
    g = add_feature(g, free_throw_lane$professional_painted_area, color_list$professional_painted_area_color)
  }
  if(include_professional_free_throw_lane_lines){
    g = add_feature(g, free_throw_lane_lines$professional_lane_line_1, color_list$professional_free_throw_lane_lines_color)
    g = add_feature(g, free_throw_lane_lines$professional_lane_line_2, color_list$professional_free_throw_lane_lines_color)
    g = add_feature(g, free_throw_lane_lines$professional_lane_line_3, color_list$professional_free_throw_lane_lines_color)
    g = add_feature(g, free_throw_lane_lines$professional_lane_line_4, color_list$professional_free_throw_lane_lines_color)
    g = add_feature(g, free_throw_lane_lines$professional_lane_line_5, color_list$professional_free_throw_lane_lines_color)
    g = add_feature(g, free_throw_lane_lines$professional_lane_line_6, color_list$professional_free_throw_lane_lines_color)
    g = add_feature(g, free_throw_lane_lines$professional_lane_line_7, color_list$professional_free_throw_lane_lines_color)
    g = add_feature(g, free_throw_lane_lines$professional_lane_line_8, color_list$professional_free_throw_lane_lines_color)
  }
  g = add_feature(g, free_throw_lane$amateur_free_throw_lane, color_list$amateur_free_throw_lane_color)
  g = add_feature(g, free_throw_lane$amateur_painted_area, color_list$amateur_painted_area_color)
  g = add_feature(g, free_throw_lane_lines$amateur_lane_line_1, color_list$amateur_free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$amateur_lane_line_2, color_list$amateur_free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$amateur_lane_line_3, color_list$amateur_free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$amateur_lane_line_4, color_list$amateur_free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$amateur_lane_line_5, color_list$amateur_free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$amateur_lane_line_6, color_list$amateur_free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$amateur_lane_line_7, color_list$amateur_free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$amateur_lane_line_8, color_list$amateur_free_throw_lane_lines_color)
  g = add_feature(g, free_throw_semi_circle$free_throw_semi_circle_line, color_list$free_throw_semi_circle_line_color)
  g = add_feature(g, free_throw_semi_circle$free_throw_semi_circle_fill, color_list$free_throw_semi_circle_fill_color)
  g = add_feature(g, restricted_area_arc, color_list$restricted_area_arc_color)
  g = add_feature(g, lower_defensive_box$dash_1, color_list$lower_defensive_box_color)
  g = add_feature(g, lower_defensive_box$dash_2, color_list$lower_defensive_box_color)
  g = add_feature(g, backboard, color_list$backboard_color)
  g = add_feature(g, basket_ring, color_list$basket_ring_color)
  g = add_feature(g, net, color_list$net_color)

  # Return the ggplot2 instance that contains the court plot
  return(g)
}
