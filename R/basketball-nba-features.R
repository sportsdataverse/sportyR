#' Generate the data frame for the points that comprise a regulation NBA court
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the court
nba_feature_court_bground = function(full_surf = TRUE,
                                     rotate = FALSE,
                                     rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # A regulation NBA court is 94' long and 50' wide, but this will only draw the
  # half-court, so it will measure 47' long and 25' wide
  court = create_rectangle(
    x_min = -47,
    x_max = 0,
    y_min = -25,
    y_max = 25
  )

  if (full_surf) {
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

  if (rotate) {
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
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A list of data frames containing the points that comprise the center
#'   circles
nba_feature_center_circles = function(full_surf = TRUE,
                                      rotate = FALSE,
                                      rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # An NBA court has two circles in the center: one of radius 2' (interior), and
  # one of radius 6' (exterior). The lines are 2" thick

  # Inner circle is 2' in radius (interior)
  inner_circle = rbind(
    create_circle(
      center = c(0, 0),
      start = .5,
      end = 1.5,
      d = 4
    ),

    data.frame(
      x = 0,
      y = -4
    ),

    create_circle(
      center = c(0, 0),
      start = 1.5,
      end = .5,
      d = 4 + (4 / 12)
    )
  )

  # Outer circle is 6' in radius (exterior)
  outer_circle = rbind(
    create_circle(
      center = c(0, 0),
      start = .5,
      end = 1.5,
      d = 12
    ),

    data.frame(
      x = 0,
      y = -12
    ),

    create_circle(
      center = c(0, 0),
      start = 1.5,
      end = .5,
      d = 12 - (4 / 12)
    )
  )

  if (full_surf) {
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    inner_circle = rbind(
      inner_circle,
      reflect(
        inner_circle,
        over_y = TRUE
      )
    )

    outer_circle = rbind(
      outer_circle,
      reflect(
        outer_circle,
        over_y = TRUE
      )
    )
  }

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    inner_circle = rotate_coords(
      inner_circle,
      rotation_dir
    )

    outer_circle = rotate_coords(
      outer_circle,
      rotation_dir
    )
  }

  # Return the feature's data frames as a list
  center_circles = list(
    inner_circle = inner_circle,
    outer_circle = outer_circle
  )

  return(center_circles)
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
nba_feature_division_line = function(full_surf = TRUE,
                                     rotate = FALSE,
                                     rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The line is 2" thick, goes right through the middle of the court (1" of its
  # width on each side of it), and spans the width of the court
  division_line = create_rectangle(
    x_min = -1 / 12,
    x_max = 0,
    y_min = -25,
    y_max = 25
  )

  if (full_surf) {
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

  if (rotate) {
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
nba_feature_endline = function(full_surf = TRUE,
                               rotate = FALSE,
                               rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The endline is 47' (interior) from the center of the court, spans the width
  # of the court, and is 2" in thickness
  endline = create_rectangle(
    x_min = -47 - (2 / 12),
    x_max = -47,
    y_min = -25,
    y_max = 25
  )

  if (full_surf) {
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

  if (rotate) {
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
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A list of data frames containing the points that comprise the
#'   sidelines
nba_feature_sideline = function(full_surf = TRUE,
                                rotate = FALSE,
                                rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The sideline is 25' (interior) from the center of the court, spans the
  # length of the court, and is 2" in thickness

  # First sideline
  sideline_1 = create_rectangle(
    x_min = -47 - (2 / 12),
    x_max = 0,
    y_min = -25 - (2 / 12),
    y_max = -25
  )

  # Second sideline
  sideline_2 = create_rectangle(
    x_min = -47 - (2 / 12),
    x_max = 0,
    y_min = 25,
    y_max = 25 + (2 / 12)
  )

  if (full_surf) {
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

  if (rotate) {
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
nba_feature_team_bench = function(full_surf = TRUE,
                                  rotate = FALSE,
                                  rotation_dir = "ccw") {
  # The team bench is 28' (interior) from the nearest endline sideline,
  # protrudes 3' into the court, and is 2" in thickness

  # The first bench markings
  team_bench_1 = create_rectangle(
    x_min = -19,
    x_max = -19 + (2 / 12),
    y_min = -25,
    y_max = -22
  )

  # Then, the opposite side
  team_bench_2 = create_rectangle(
    x_min = -19,
    x_max = -19 + (2 / 12),
    y_min = 22,
    y_max = 25
  )

  if (full_surf) {
    # If the surface being drawn is a full-surface representation, reflect over
    # the y axis
    team_bench_1 = rbind(
      team_bench_1,
      reflect(
        team_bench_1,
        over_y = TRUE
      )
    )

    team_bench_2 = rbind(
      team_bench_2,

      reflect(
        team_bench_2,
        over_y = TRUE
      )
    )
  }

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    team_bench_1 = rotate_coords(
      team_bench_1,
      rotation_dir
    )

    team_bench_2 = rotate_coords(
      team_bench_2,
      rotation_dir
    )
  }

  # Return the feature's data frames as a list
  team_benches = list(
    team_bench_1 = team_bench_1,
    team_bench_2 = team_bench_2
  )

  return(team_benches)
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
nba_feature_substitution_area = function(full_surf = TRUE,
                                         rotate = FALSE,
                                         rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The substitution areas are 8' 2" (interior) apart, or 4' 1" from the center
  # of the division line. They extend 4' from the boundary line, and are 2"
  # thick

  # Substitution area
  substitution_area = create_rectangle(
    x_min = -4 - (3 / 12),
    x_max = -4 - (1 / 12),
    y_min = 25 + (2 / 12),
    y_max = 29 + (2 / 12)
  )

  if (full_surf) {
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

  if (rotate) {
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
nba_feature_court_apron = function(full_surf = TRUE,
                                   rotate = FALSE,
                                   rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The court apron is the boundary on the court, which may be made of a
  # contrasting color. It must be 8' behind the baseline and 5' from the
  # sideline
  court_apron = data.frame(
    x = c(
      0,
      -47 - (2 / 12),
      -47 - (2 / 12),
      -4 - (3 / 12),
      -4 - (3 / 12),
      -4 - (1 / 12),
      -4 - (1 / 12),
      0,
      0,
      -56,
      -56,
      0,
      0
    ),

    y = c(
      -25 - (2 / 12),
      -25 - (2 / 12),
      25 + (2 / 12),
      25 + (2 / 12),
      29 + (2 / 12),
      29 + (2 / 12),
      25 + (2 / 12),
      25 + (2 / 12),
      30,
      30,
      -30,
      -30,
      -25 - (2 / 12)
    )
  )

  if (full_surf) {
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

  if (rotate) {
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
nba_feature_three_point_line = function(full_surf = TRUE,
                                        rotate = FALSE,
                                        rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # First, a bit of math is needed to determine the starting and ending angles
  # of the three-point arc, relative to 0 radians. Since in the end, the angle
  # is what matters, the units of measure do not. Inches are easier to use for
  # this calculation. The angle begins 3' from the interior edge of the sideline
  start_y = (22 * 12)

  # The rule book describes the arc as having a radius of 23' 9"
  radius_outer = (23 * 12) + 9

  # From here, the calculation is relatively straightforward. To determine the
  # angle, the inverse sine is needed. It will be multiplied by pi so that it
  # can be passed to the create_circle() function
  start_angle_outer = asin(start_y / radius_outer) / pi
  end_angle_outer = -start_angle_outer

  # The same method can be used for the inner angles, however, since the inner
  # radius will be traced from bottom to top, the angle must be negative to
  # start
  radius_inner = (23 * 12) + 9 - 2
  start_angle_inner = -asin((start_y - 2) / radius_inner) / pi
  end_angle_inner = -start_angle_inner

  # NBA three-point line
  three_point_line = rbind(
    data.frame(
      x = -47,
      y = 22
    ),

    create_circle(
      center = c(-41.75, 0),
      start = start_angle_outer,
      end = end_angle_outer,
      d = 2 * (radius_outer / 12)
    ),

    data.frame(
      x = c(-47, -47),
      y = c(-22, -22 + (2 / 12))
    ),

    create_circle(
      center = c(-41.75, 0),
      start = start_angle_inner,
      end = end_angle_inner,
      d = 2 * (radius_inner / 12)
    ),

    data.frame(
      x = c(-47, -47),
      y = c(22 - (2 / 12), 22)
    )
  )

  # Sometimes, the inside of the three-point arc (aka 2-point range) will be a
  # different color than the floor. This section allows for this to happen in a
  # future iteration
  two_point_range = rbind(
    data.frame(
      x = -47,
      y = -22 + (2 / 12)
    ),

    create_circle(
      center = c(-41.75, 0),
      start = start_angle_inner,
      end = end_angle_inner,
      d = 2 * (radius_inner / 12)
    ),

    data.frame(
      x = c(-47, -47),
      y = c(22 - (2 / 12), -22 + (2 / 12))
    )
  )

  if (full_surf) {
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

  if (rotate) {
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
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A list of data frames containing the points that comprise both the
#'   professional and amateur free-throw lanes
nba_feature_free_throw_lane = function(full_surf = TRUE,
                                       rotate = FALSE,
                                       rotation_dir = "ccw") {
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
      -28 - (2 / 12),
      -28 - (2 / 12),
      -47,
      -47
    ),

    y = c(
      -8,
      -8,
      8,
      8,
      8 - (2 / 12),
      8 - (2 / 12),
      -8 + (2 / 12),
      -8 + (2 / 12),
      -8
    )
  )

  professional_painted_area = create_rectangle(
    x_min = -47,
    x_max = -28 - (2 / 12),
    y_min = -8 + (2 / 12),
    y_max = 8 - (2 / 12)
  )

  # Some courts have the amateur (NCAA) free-throw lane included as well
  amateur_free_throw_lane = data.frame(
    x = c(
      -47,
      -28,
      -28,
      -47,
      -47,
      -28 - (2 / 12),
      -28 - (2 / 12),
      -47,
      -47
    ),

    y = c(
      -6,
      -6,
      6,
      6,
      6 - (2 / 12),
      6 - (2 / 12),
      -6 + (2 / 12),
      -6 + (2 / 12),
      -6
    )
  )

  amateur_painted_area = create_rectangle(
    x_min = -47,
    x_max = -28 - (2 / 12),
    y_min = -6 + (2 / 12),
    y_max = 6 - (2 / 12)
  )

  if (full_surf) {
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

  if (rotate) {
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
nba_feature_free_throw_lane_lines = function(full_surf = TRUE,
                                             rotate = FALSE,
                                             rotation_dir = "ccw") {
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

  if (full_surf) {
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

  if (rotate) {
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
nba_feature_free_throw_semi_circle = function(full_surf = TRUE,
                                              rotate = FALSE,
                                              rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The free-throw semi-circle has radius 6' and is centered at the midpoint of
  # the free-throw line (18' 11" from the interior edge of the baseline)
  free_throw_semi_circle_line = rbind(
    create_circle(
      center = c(-28 - (1 / 12), 0),
      start = .5,
      end = -.5,
      d = 12
    ),

    data.frame(
      x = -28 - (1 / 12),
      y = -6
    ),

    create_circle(
      center = c(-28 - (1 / 12), 0),
      start = -.5,
      end = .5,
      d = 12 - (4 / 12)
    ),

    data.frame(
      x = -28 - (1 / 12),
      y = 6
    )
  )

  # Sometimes, the actual semi-circle is a different color than the court or
  # two-point area
  free_throw_semi_circle_fill = rbind(
    create_circle(
      center = c(-28 - (1 / 12), 0),
      start = -.5,
      end = .5,
      d = 12 - (4 / 12)
    )
  )

  # Keep the centering aligned, but only keep the points that are inside the
  # semi-circle's fill
  free_throw_semi_circle_fill = free_throw_semi_circle_fill[
    free_throw_semi_circle_fill$x >= -28,
  ]

  # Force the polygon to be closed
  free_throw_semi_circle_fill = rbind(
    free_throw_semi_circle_fill,
    free_throw_semi_circle_fill[1, ]
  )

  if (full_surf) {
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

  if (rotate) {
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

#' Generate the data frames for the points that comprise the dashed part of the
#' free-throw circle
#'
#' @param full_surf A boolean indicating whether or not this feature is needed
#'   for a full-surface representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A list of data frames containing the points that comprise the dashed
#'   part of the free-throw circle
nba_feature_free_throw_dashed_semi_circle = function(full_surf = TRUE,
                                                     rotate = FALSE,
                                                     rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # Per the NBA rule book, the solid portion of the circle extends along an arc
  # of length 12.29" behind the free-throw line. The angle theta must be
  # calculated to determine where to start. It can be determined via the
  # relationship s = r*theta, where s is the arc length, r is the radius, and
  # theta is the angle (in radians)

  # First, define s to be the arc length in feet
  s = 12.29 / 12

  # The outer radius is 6'
  r = 6

  # Theta is therefore s/r, but since the create_circle() function takes an
  # angle in radians/pi, this must be divided out as well
  theta = (s / r) / pi

  # Since the circle must extend theta radians past +/-pi/2, theta must be
  # added/subtracted from 1/2 accordingly
  start_angle = .5
  end_angle = .5 + theta

  # Create the first dash
  dash_1 = rbind(
    create_circle(
      center = c(-28 - (1 / 12), 0),
      start = start_angle,
      end = end_angle,
      d = 12
    ),

    create_circle(
      center = c(-28 - (1 / 12), 0),
      start = end_angle,
      end = start_angle,
      d = 12 - (4 / 12)
    ),

    create_circle(
      center = c(-28 - (1 / 12), 0),
      start = start_angle,
      end = end_angle,
      d = 12
    )[1, ]
  )

  # The dashed sections of the free-throw circle are all of length 15.5", and
  # are spaced 15.5" from each other. Following a similar process as above, the
  # starting and ending angles can be computed

  # First, compute the arc length in feet
  s = 15.5 / 12

  # The outer radius is 6'
  r = 6

  # Finally, compute the angle traced by the dashed lines
  theta_dashes = (s / r) / pi

  # This theta must be added to start_angle above to get the starting angle for
  # each dash, and added twice to get the ending angle for each dash. This
  # pattern can be repeated, taking the end angle of the previous dash as the
  # start angle for the following dash
  dash_2_start_angle = end_angle + theta_dashes
  dash_2_end_angle = end_angle + (2 * theta_dashes)

  dash_3_start_angle = dash_2_end_angle + theta_dashes
  dash_3_end_angle = dash_2_end_angle + (2 * theta_dashes)

  dash_4_start_angle = dash_3_end_angle + theta_dashes
  dash_4_end_angle = dash_3_end_angle + (2 * theta_dashes)

  # Create the remaining dashes to complete the quarter-circle
  dash_2 = rbind(
    create_circle(
      center = c(-28 - (1 / 12), 0),
      start = dash_2_start_angle,
      end = dash_2_end_angle,
      d = 12
    ),

    create_circle(
      center = c(-28 - (1 / 12), 0),
      start = dash_2_end_angle,
      end = dash_2_start_angle,
      d = 12 - (4 / 12)
    ),

    create_circle(
      center = c(-28 - (1 / 12), 0),
      start = dash_2_start_angle,
      end = dash_2_end_angle,
      d = 12
    )[1, ]
  )

  dash_3 = rbind(
    create_circle(
      center = c(-28 - (1 / 12), 0),
      start = dash_3_start_angle,
      end = dash_3_end_angle,
      d = 12
    ),

    create_circle(
      center = c(-28 - (1 / 12), 0),
      start = dash_3_end_angle,
      end = dash_3_start_angle,
      d = 12 - (4 / 12)
    ),

    create_circle(
      center = c(-28 - (1 / 12), 0),
      start = dash_3_start_angle,
      end = dash_3_end_angle,
      d = 12
    )[1, ]
  )

  dash_4 = rbind(
    create_circle(
      center = c(-28 - (1 / 12), 0),
      start = dash_4_start_angle,
      end = dash_4_end_angle,
      d = 12
    ),

    create_circle(
      center = c(-28 - (1 / 12), 0),
      start = dash_4_end_angle,
      end = dash_4_start_angle,
      d = 12 - (4 / 12)
    ),

    create_circle(
      center = c(-28 - (1 / 12), 0),
      start = dash_4_start_angle,
      end = dash_4_end_angle,
      d = 12
    )[1, ]
  )

  # Reflect these over the x axis to get the remaining quarter-circle
  dash_5 = reflect(
    dash_4,
    over_x = TRUE,
    over_y = FALSE
  )

  dash_6 = reflect(
    dash_3,
    over_x = TRUE,
    over_y = FALSE
  )

  dash_7 = reflect(
    dash_2,
    over_x = TRUE,
    over_y = FALSE
  )

  dash_8 = reflect(
    dash_1,
    over_x = TRUE,
    over_y = FALSE
  )

  if (full_surf) {
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

    dash_3 = rbind(
      dash_3,
      reflect(
        dash_3,
        over_y = TRUE
      )
    )

    dash_4 = rbind(
      dash_4,
      reflect(
        dash_4,
        over_y = TRUE
      )
    )

    dash_5 = rbind(
      dash_5,
      reflect(
        dash_5,
        over_y = TRUE
      )
    )

    dash_6 = rbind(
      dash_6,
      reflect(
        dash_6,
        over_y = TRUE
      )
    )

    dash_7 = rbind(
      dash_7,
      reflect(
        dash_7,
        over_y = TRUE
      )
    )

    dash_8 = rbind(
      dash_8,
      reflect(
        dash_8,
        over_y = TRUE
      )
    )
  }

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    dash_1 = rotate_coords(
      dash_1,
      rotation_dir
    )

    dash_2 = rotate_coords(
      dash_2,
      rotation_dir
    )

    dash_3 = rotate_coords(
      dash_3,
      rotation_dir
    )

    dash_4 = rotate_coords(
      dash_4,
      rotation_dir
    )

    dash_5 = rotate_coords(
      dash_5,
      rotation_dir
    )

    dash_6 = rotate_coords(
      dash_6,
      rotation_dir
    )

    dash_7 = rotate_coords(
      dash_7,
      rotation_dir
    )

    dash_8 = rotate_coords(
      dash_8,
      rotation_dir
    )
  }

  # Return the feature's data frames as a list
  free_throw_semi_circle_dashes = list(
    dash_1 = dash_1,
    dash_2 = dash_2,
    dash_3 = dash_3,
    dash_4 = dash_4,
    dash_5 = dash_5,
    dash_6 = dash_6,
    dash_7 = dash_7,
    dash_8 = dash_8
  )

  return(free_throw_semi_circle_dashes)
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
nba_feature_lower_defensive_box = function(full_surf = TRUE,
                                           rotate = FALSE,
                                           rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The lower defensive box is comprised of four dashes: two along the baseline
  # that are 3' (interior) from the edges of the free-throw lane, protrude 6"
  # into the court, and have a 2" width, and the other is located 13' (interior)
  # from the baseline and 3' (exterior) from the edge of the free-throw lane,
  # has a length of 6", and a width of 2"

  # Dash 1 is the line along the baseline
  dash_1 = create_rectangle(
    x_min = -47,
    x_max = -46.5,
    y_min = 11,
    y_max = 11 + (2 / 12)
  )

  # Dash 2 is the line in the painted area
  dash_2 = create_rectangle(
    x_min = -34,
    x_max = -34 + (2 / 12),
    y_min = 5,
    y_max = 5.5
  )

  # Dash 3 is the reflection of dash 2 over the x axis
  dash_3 = reflect(
    dash_2,
    over_x = TRUE,
    over_y = FALSE
  )

  # Dash 4 is the reflection of dash 1 over the x axis
  dash_4 = reflect(
    dash_1,
    over_x = TRUE,
    over_y = FALSE
  )

  if (full_surf) {
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

    dash_3 = rbind(
      dash_3,
      reflect(
        dash_3,
        over_y = TRUE
      )
    )

    dash_4 = rbind(
      dash_4,
      reflect(
        dash_4,
        over_y = TRUE
      )
    )
  }

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    dash_1 = rotate_coords(
      dash_1,
      rotation_dir
    )

    dash_2 = rotate_coords(
      dash_2,
      rotation_dir
    )

    dash_3 = rotate_coords(
      dash_3,
      rotation_dir
    )

    dash_4 = rotate_coords(
      dash_4,
      rotation_dir
    )
  }

  # Return the feature's data frames as a list
  lower_defensive_box = list(
    dash_1 = dash_1,
    dash_2 = dash_2,
    dash_3 = dash_3,
    dash_4 = dash_4
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
nba_feature_restricted_area_arc = function(full_surf = TRUE,
                                           rotate = FALSE,
                                           rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # Following the same process as for the three-point line, the restricted rea
  # arc's starting and ending angle can be computed
  start_y = -4 - (2 / 12)

  # The rule book describes the arc as having a radius of 4'
  radius_outer = 4 + (2 / 12)

  # From here, the calculation is relatively straightforward. To determine the
  # angle, the inverse sine is needed. It will be multiplied by pi so that it
  # can be passed to the create_circle() function
  start_angle_outer = asin(start_y / radius_outer) / pi
  end_angle_outer = -start_angle_outer

  # The same method can be used for the inner angles, however, since the inner
  # radius will be traced from bottom to top, the angle must be negative to
  # start
  radius_inner = 4
  start_angle_inner = -asin((start_y + (2 / 12)) / radius_inner) / pi
  end_angle_inner = -start_angle_inner

  # The restricted area arc is an arc of radius 4' from the center of the
  # basket, and extending in a straight line to the front face of the backboard,
  # and having thickness of 2"
  restricted_area_arc = rbind(
    data.frame(
      x = -43,
      y = -4 - (2 / 12)
    ),

    create_circle(
      center = c(-41.75, 0),
      start = start_angle_outer,
      end = end_angle_outer,
      d = 8 + (4 / 12)
    ),

    data.frame(
      x = c(-43, -43),
      y = c(4 + (2 / 12), 4)
    ),

    create_circle(
      center = c(-41.75, 0),
      start = start_angle_inner,
      end = end_angle_inner,
      d = 8
    ),

    data.frame(
      x = c(-43, -43),
      y = c(-4, -4 - (2 / 12))
    )
  )

  if (full_surf) {
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

  if (rotate) {
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
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the backboard
nba_feature_backboard = function(full_surf = TRUE,
                                 rotate = FALSE,
                                 rotation_dir = "ccw") {
  # In the NBA, the backboard is 6' in width, with its front edge 4' from the
  # interior of the baseline
  backboard = create_rectangle(
    x_min = -43 - (4 / 12),
    x_max = -43,
    y_min = -3,
    y_max = 3
  )

  if (full_surf) {
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

  if (rotate) {
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
nba_feature_basket_ring = function(full_surf = TRUE,
                                   rotate = FALSE,
                                   rotation_dir = "ccw") {
  # Initialize x and y (to pass checks)
  x = y = NULL

  # The connector has a width of 7", so 3.5" are on either side of the x
  # axis. The ring has a radius of 9", so the arcsine of these measurements
  # should give the angle at which point they connect
  start_angle = pi - asin(3.5 / 9)

  # The ending angle of the ring would be the negative of the starting angle
  end_angle = -start_angle

  # Get the ring
  basket_ring = rbind(
    data.frame(
      x = c(-43, -41.75 - ((9 / 12) * cos(start_angle))),
      y = c(3.5 / 12, 3.5 / 12)
    ),

    create_circle(
      center = c(-41.75, 0),
      start = start_angle,
      end = end_angle,
      d = 1.5 + (4 / 12)
    ),

    data.frame(
      x = c(
        -41.75 - ((9 / 12) * cos(start_angle)),
        -43,
        -43
      ),

      y = c(
        -3.5 / 12,
        -3.5 / 12,
        3.5 / 12
      )
    )
  )

  if (full_surf) {
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

  if (rotate) {
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
nba_feature_net = function(full_surf = TRUE,
                           rotate = FALSE,
                           rotation_dir = "ccw") {
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

  if (full_surf) {
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

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    net = rotate_coords(
      net,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(net)
}

#' Generate the list of colors for an NBA court plot. The defaults can be
#' overwritten by supplying the names of the list elements to the
#' \code{geom_nba()} function (or its wrapper \code{geom_basketball()})
#'
#' @param court_background_color A hexadecimal string representing the color to
#'   use for this feature
#' @param inner_center_circle_color A hexadecimal string representing the color
#'   to use for this feature
#' @param outer_center_circle_color A hexadecimal string representing the color
#'   to use for this feature
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
#' @param three_point_line_color A hexadecimal string representing the color to
#'   use for this feature
#' @param two_point_range_color A hexadecimal string representing the color to
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
#' @param free_throw_dashed_semi_circle_color A hexadecimal string representing
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
nba_features_set_colors = function(court_background_color = "#d2ab6f",
                                   inner_center_circle_color = "#000000",
                                   outer_center_circle_color = "#000000",
                                   division_line_color = "#000000",
                                   endline_color = "#000000",
                                   sideline_color = "#000000",
                                   team_bench_color = "#000000",
                                   substitution_area_color = "#000000",
                                   court_apron_color = "#d2ab6f",
                                   three_point_line_color = "#000000",
                                   two_point_range_color = "#d2ab6f",
                                   professional_free_throw_lane_color = "#000000",
                                   professional_painted_area_color = "#d2ab6f",
                                   amateur_free_throw_lane_color = "#000000",
                                   amateur_painted_area_color = "#d2ab6f",
                                   professional_free_throw_lane_lines_color = "#000000",
                                   amateur_free_throw_lane_lines_color = "#000000",
                                   free_throw_semi_circle_line_color = "#000000",
                                   free_throw_semi_circle_fill_color = "#d2ab6f",
                                   free_throw_dashed_semi_circle_color = "#000000",
                                   lower_defensive_box_color = "#000000",
                                   restricted_area_arc_color = "#000000",
                                   backboard_color = "#000000",
                                   basket_ring_color = "#000000",
                                   net_color = "#ffffff") {

  # Create the colors to use for the plot
  feature_colors = list(
    court_background_color = court_background_color,
    inner_center_circle_color = inner_center_circle_color,
    outer_center_circle_color = outer_center_circle_color,
    division_line_color = division_line_color,
    endline_color = endline_color,
    sideline_color = sideline_color,
    team_bench_color = team_bench_color,
    substitution_area_color = substitution_area_color,
    court_apron_color = court_apron_color,
    three_point_line_color = three_point_line_color,
    two_point_range_color = two_point_range_color,
    professional_free_throw_lane_color = professional_free_throw_lane_color,
    professional_painted_area_color = professional_painted_area_color,
    amateur_free_throw_lane_color = amateur_free_throw_lane_color,
    amateur_painted_area_color = amateur_painted_area_color,
    professional_free_throw_lane_lines_color = professional_free_throw_lane_lines_color,
    amateur_free_throw_lane_lines_color = amateur_free_throw_lane_lines_color,
    free_throw_semi_circle_line_color = free_throw_semi_circle_line_color,
    free_throw_semi_circle_fill_color = free_throw_semi_circle_fill_color,
    free_throw_dashed_semi_circle_color = free_throw_dashed_semi_circle_color,
    lower_defensive_box_color = lower_defensive_box_color,
    restricted_area_arc_color = restricted_area_arc_color,
    backboard_color = backboard_color,
    basket_ring_color = basket_ring_color,
    net_color = net_color
  )

  # Return the list of colors
  return(feature_colors)
}

#' Create a ggplot2 instance that represents a regulation NBA court, with the
#' center of the court corresponding to (0, 0)
#'
#' @param full_surf A boolean indicating whether or not to draw a full-surface
#'   representation of the playing surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not the surface representation
#'   needs to be rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the surface
#'   representation. Default: \code{'ccw'}
#' @param unit A string indicating the units with which to make the plot.
#'   Default: \code{'ft'}
#' @param include_amateur_free_throw_lane A boolean indicating whether or not to
#'   include the amateur (NCAA) free-throw lane in the final plot. Default: \code{TRUE}
#' @param include_amateur_free_throw_lane_lines A boolean indicating whether or
#'   not to include the amateur (NCAA) free-throw lane lines in the final plot.
#'   Default: \code{TRUE}
#' @param background_color A hexadecimal string representing the color to use
#'   for the plot's background. Default: \code{NULL}
#' @param ... Additional arguments to pass to the function. These should be the
#'   colors to pass to the \code{nba_features_set_colors()} function
#'
#' @return A ggplot2 instance that represents a regulation NBA court
geom_nba = function(full_surf = TRUE,
                    rotate = FALSE,
                    rotation_dir = "ccw",
                    unit = "ft",
                    include_amateur_free_throw_lane = TRUE,
                    include_amateur_free_throw_lane_lines = TRUE,
                    background_color = NULL,
                    ...) {
  # Force the plot unit to be lower case
  unit = tolower(unit)

  # Create the colors to use for the plot
  color_list = nba_features_set_colors(...)

  # Generate the data frames for the features of an NBA court
  court_background = nba_feature_court_bground(full_surf, rotate, rotation_dir)
  center_circles = nba_feature_center_circles(full_surf, rotate, rotation_dir)
  division_line = nba_feature_division_line(full_surf, rotate, rotation_dir)
  endline = nba_feature_endline(full_surf, rotate, rotation_dir)
  sideline = nba_feature_sideline(full_surf, rotate, rotation_dir)
  court_apron = nba_feature_court_apron(full_surf, rotate, rotation_dir)
  team_bench = nba_feature_team_bench(full_surf, rotate, rotation_dir)
  substitution_area = nba_feature_substitution_area(full_surf, rotate, rotation_dir)
  three_point_line = nba_feature_three_point_line(full_surf, rotate, rotation_dir)
  free_throw_lane = nba_feature_free_throw_lane(full_surf, rotate, rotation_dir)
  free_throw_lane_lines = nba_feature_free_throw_lane_lines(full_surf, rotate)
  free_throw_semi_circle = nba_feature_free_throw_semi_circle(full_surf, rotate, rotation_dir)
  free_throw_dashed_semi_circle = nba_feature_free_throw_dashed_semi_circle(full_surf, rotate, rotation_dir)
  restricted_area_arc = nba_feature_restricted_area_arc(full_surf, rotate, rotation_dir)
  lower_defensive_box = nba_feature_lower_defensive_box(full_surf, rotate, rotation_dir)
  backboard = nba_feature_backboard(full_surf, rotate, rotation_dir)
  basket_ring = nba_feature_basket_ring(full_surf, rotate, rotation_dir)
  net = nba_feature_net(full_surf, rotate, rotation_dir)

  # Convert between units as necessary
  if (!(unit %in% c("ft", "feet"))) {
    court_background = convert_units(court_background, "ft", unit, conversion_columns = c("x", "y"))
    center_circles$inner_circle = convert_units(center_circles$inner_circle, "ft", unit, conversion_columns = c("x", "y"))
    center_circles$outer_circle = convert_units(center_circles$outer_circle, "ft", unit, conversion_columns = c("x", "y"))
    division_line = convert_units(division_line, "ft", unit, conversion_columns = c("x", "y"))
    endline = convert_units(endline, "ft", unit, conversion_columns = c("x", "y"))
    sideline$sideline_1 = convert_units(sideline$sideline_1, "ft", unit, conversion_columns = c("x", "y"))
    sideline$sideline_2 = convert_units(sideline$sideline_2, "ft", unit, conversion_columns = c("x", "y"))
    court_apron = convert_units(court_apron, "ft", unit, conversion_columns = c("x", "y"))
    team_bench$team_bench_1 = convert_units(team_bench$team_bench_1, "ft", unit, conversion_columns = c("x", "y"))
    team_bench$team_bench_2 = convert_units(team_bench$team_bench_2, "ft", unit, conversion_columns = c("x", "y"))
    substitution_area = convert_units(substitution_area, "ft", unit, conversion_columns = c("x", "y"))
    three_point_line$three_point_line = convert_units(three_point_line$three_point_line, "ft", unit, conversion_columns = c("x", "y"))
    three_point_line$two_point_range = convert_units(three_point_line$two_point_range, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_lane$professional_free_throw_lane = convert_units(free_throw_lane$professional_free_throw_lane, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_lane$professional_painted_area = convert_units(free_throw_lane$professional_painted_area, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_lane_lines$professional_lane_line_1 = convert_units(free_throw_lane_lines$professional_lane_line_1, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_lane_lines$professional_lane_line_2 = convert_units(free_throw_lane_lines$professional_lane_line_2, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_lane_lines$professional_lane_line_3 = convert_units(free_throw_lane_lines$professional_lane_line_3, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_lane_lines$professional_lane_line_4 = convert_units(free_throw_lane_lines$professional_lane_line_4, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_lane_lines$professional_lane_line_5 = convert_units(free_throw_lane_lines$professional_lane_line_5, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_lane_lines$professional_lane_line_6 = convert_units(free_throw_lane_lines$professional_lane_line_6, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_lane_lines$professional_lane_line_7 = convert_units(free_throw_lane_lines$professional_lane_line_7, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_lane_lines$professional_lane_line_8 = convert_units(free_throw_lane_lines$professional_lane_line_8, "ft", unit, conversion_columns = c("x", "y"))

    if (include_amateur_free_throw_lane) {
      free_throw_lane$amateur_free_throw_lane = convert_units(free_throw_lane$amateur_free_throw_lane, "ft", unit, conversion_columns = c("x", "y"))
      free_throw_lane$amateur_painted_area = convert_units(free_throw_lane$amateur_painted_area, "ft", unit, conversion_columns = c("x", "y"))
    }

    if (include_amateur_free_throw_lane_lines) {
      free_throw_lane_lines$amateur_lane_line_1 = convert_units(free_throw_lane_lines$amateur_lane_line_1, "ft", unit, conversion_columns = c("x", "y"))
      free_throw_lane_lines$amateur_lane_line_2 = convert_units(free_throw_lane_lines$amateur_lane_line_2, "ft", unit, conversion_columns = c("x", "y"))
      free_throw_lane_lines$amateur_lane_line_3 = convert_units(free_throw_lane_lines$amateur_lane_line_3, "ft", unit, conversion_columns = c("x", "y"))
      free_throw_lane_lines$amateur_lane_line_4 = convert_units(free_throw_lane_lines$amateur_lane_line_4, "ft", unit, conversion_columns = c("x", "y"))
      free_throw_lane_lines$amateur_lane_line_5 = convert_units(free_throw_lane_lines$amateur_lane_line_5, "ft", unit, conversion_columns = c("x", "y"))
      free_throw_lane_lines$amateur_lane_line_6 = convert_units(free_throw_lane_lines$amateur_lane_line_6, "ft", unit, conversion_columns = c("x", "y"))
      free_throw_lane_lines$amateur_lane_line_7 = convert_units(free_throw_lane_lines$amateur_lane_line_7, "ft", unit, conversion_columns = c("x", "y"))
      free_throw_lane_lines$amateur_lane_line_8 = convert_units(free_throw_lane_lines$amateur_lane_line_8, "ft", unit, conversion_columns = c("x", "y"))
    }

    free_throw_semi_circle$free_throw_semi_circle_line = convert_units(free_throw_semi_circle$free_throw_semi_circle_line, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_semi_circle$free_throw_semi_circle_fill = convert_units(free_throw_semi_circle$free_throw_semi_circle_fill, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_dashed_semi_circle$dash_1 = convert_units(free_throw_dashed_semi_circle$dash_1, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_dashed_semi_circle$dash_2 = convert_units(free_throw_dashed_semi_circle$dash_2, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_dashed_semi_circle$dash_3 = convert_units(free_throw_dashed_semi_circle$dash_3, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_dashed_semi_circle$dash_4 = convert_units(free_throw_dashed_semi_circle$dash_4, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_dashed_semi_circle$dash_5 = convert_units(free_throw_dashed_semi_circle$dash_5, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_dashed_semi_circle$dash_6 = convert_units(free_throw_dashed_semi_circle$dash_6, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_dashed_semi_circle$dash_7 = convert_units(free_throw_dashed_semi_circle$dash_7, "ft", unit, conversion_columns = c("x", "y"))
    free_throw_dashed_semi_circle$dash_8 = convert_units(free_throw_dashed_semi_circle$dash_8, "ft", unit, conversion_columns = c("x", "y"))
    restricted_area_arc = convert_units(restricted_area_arc, "ft", unit, conversion_columns = c("x", "y"))
    lower_defensive_box$dash_1 = convert_units(lower_defensive_box$dash_1, "ft", unit, conversion_columns = c("x", "y"))
    lower_defensive_box$dash_2 = convert_units(lower_defensive_box$dash_2, "ft", unit, conversion_columns = c("x", "y"))
    lower_defensive_box$dash_3 = convert_units(lower_defensive_box$dash_3, "ft", unit, conversion_columns = c("x", "y"))
    lower_defensive_box$dash_4 = convert_units(lower_defensive_box$dash_4, "ft", unit, conversion_columns = c("x", "y"))
    backboard = convert_units(backboard, "ft", unit, conversion_columns = c("x", "y"))
    basket_ring = convert_units(basket_ring, "ft", unit, conversion_columns = c("x", "y"))
    net = convert_units(net, "ft", unit, conversion_columns = c("x", "y"))
  }

  # Create the initial ggplot2 instance onto which the features will be added
  g = create_plot_base(rotate, background_color)

  # Add the features to the ggplot2 instance
  g = add_feature(g, court_background, color_list$court_background_color)
  g = add_feature(g, center_circles$inner_circle, color_list$inner_center_circle_color)
  g = add_feature(g, center_circles$outer_circle, color_list$outer_center_circle_color)
  g = add_feature(g, division_line, color_list$division_line_color)
  g = add_feature(g, endline, color_list$endline_color)
  g = add_feature(g, sideline$sideline_1, color_list$sideline_color)
  g = add_feature(g, sideline$sideline_2, color_list$sideline_color)
  g = add_feature(g, court_apron, color_list$court_apron_color)
  g = add_feature(g, team_bench$team_bench_1, color_list$team_bench_color)
  g = add_feature(g, team_bench$team_bench_2, color_list$team_bench_color)
  g = add_feature(g, substitution_area, color_list$substitution_area_color)
  g = add_feature(g, three_point_line$three_point_line, color_list$three_point_line_color)
  g = add_feature(g, three_point_line$two_point_range, color_list$two_point_range_color)
  g = add_feature(g, free_throw_lane$professional_free_throw_lane, color_list$professional_free_throw_lane_color)
  g = add_feature(g, free_throw_lane$professional_painted_area, color_list$professional_painted_area_color)
  g = add_feature(g, free_throw_lane_lines$professional_lane_line_1, color_list$professional_free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$professional_lane_line_2, color_list$professional_free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$professional_lane_line_3, color_list$professional_free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$professional_lane_line_4, color_list$professional_free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$professional_lane_line_5, color_list$professional_free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$professional_lane_line_6, color_list$professional_free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$professional_lane_line_7, color_list$professional_free_throw_lane_lines_color)
  g = add_feature(g, free_throw_lane_lines$professional_lane_line_8, color_list$professional_free_throw_lane_lines_color)

  if (include_amateur_free_throw_lane) {
    g = add_feature(g, free_throw_lane$amateur_free_throw_lane, color_list$amateur_free_throw_lane_color)
    g = add_feature(g, free_throw_lane$amateur_painted_area, color_list$amateur_painted_area_color)
  }

  if (include_amateur_free_throw_lane_lines) {
    g = add_feature(g, free_throw_lane_lines$amateur_lane_line_1, color_list$amateur_free_throw_lane_lines_color)
    g = add_feature(g, free_throw_lane_lines$amateur_lane_line_2, color_list$amateur_free_throw_lane_lines_color)
    g = add_feature(g, free_throw_lane_lines$amateur_lane_line_3, color_list$amateur_free_throw_lane_lines_color)
    g = add_feature(g, free_throw_lane_lines$amateur_lane_line_4, color_list$amateur_free_throw_lane_lines_color)
    g = add_feature(g, free_throw_lane_lines$amateur_lane_line_5, color_list$amateur_free_throw_lane_lines_color)
    g = add_feature(g, free_throw_lane_lines$amateur_lane_line_6, color_list$amateur_free_throw_lane_lines_color)
    g = add_feature(g, free_throw_lane_lines$amateur_lane_line_7, color_list$amateur_free_throw_lane_lines_color)
    g = add_feature(g, free_throw_lane_lines$amateur_lane_line_8, color_list$amateur_free_throw_lane_lines_color)
  }

  g = add_feature(g, free_throw_semi_circle$free_throw_semi_circle_line, color_list$free_throw_semi_circle_line_color)
  g = add_feature(g, free_throw_semi_circle$free_throw_semi_circle_fill, color_list$free_throw_semi_circle_fill_color)
  g = add_feature(g, free_throw_dashed_semi_circle$dash_1, color_list$free_throw_dashed_semi_circle_color)
  g = add_feature(g, free_throw_dashed_semi_circle$dash_2, color_list$free_throw_dashed_semi_circle_color)
  g = add_feature(g, free_throw_dashed_semi_circle$dash_3, color_list$free_throw_dashed_semi_circle_color)
  g = add_feature(g, free_throw_dashed_semi_circle$dash_4, color_list$free_throw_dashed_semi_circle_color)
  g = add_feature(g, free_throw_dashed_semi_circle$dash_5, color_list$free_throw_dashed_semi_circle_color)
  g = add_feature(g, free_throw_dashed_semi_circle$dash_6, color_list$free_throw_dashed_semi_circle_color)
  g = add_feature(g, free_throw_dashed_semi_circle$dash_7, color_list$free_throw_dashed_semi_circle_color)
  g = add_feature(g, free_throw_dashed_semi_circle$dash_8, color_list$free_throw_dashed_semi_circle_color)
  g = add_feature(g, restricted_area_arc, color_list$restricted_area_arc_color)
  g = add_feature(g, lower_defensive_box$dash_1, color_list$lower_defensive_box_color)
  g = add_feature(g, lower_defensive_box$dash_2, color_list$lower_defensive_box_color)
  g = add_feature(g, lower_defensive_box$dash_3, color_list$lower_defensive_box_color)
  g = add_feature(g, lower_defensive_box$dash_4, color_list$lower_defensive_box_color)
  g = add_feature(g, backboard, color_list$backboard_color)
  g = add_feature(g, basket_ring, color_list$basket_ring_color)
  g = add_feature(g, net, color_list$net_color)

  # Return the ggplot2 instance that contains the court plot
  return(g)
}
