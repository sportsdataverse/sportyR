# Surface Base Features --------------------------------------------------------

#' The dirt that comprises the infield. This includes the base paths, infield
#' arc, and home plate circle.
#'
#' The home plate circle may be drawn over in other shapes as needed (example:
#' Detroit's Comerica Park has a home plate shaped dirt area as the home plate
#' "circle")
#'
#' @param home_plate_circle_radius The radius of the circle around home plate
#' @param foul_line_to_foul_grass The distance from the outer edge of the foul
#'   line to the inner edge of the grass in foul territory
#' @param pitchers_plate_distance The distance from the back tip of home plate
#'   to the front edge of the pitcher's plate
#' @param infield_arc_radius The distance from the front edge of the pitcher's
#'   plate to the back of the infield dirt
#'
#' @return A data frame that comprises the entirety of the infield dirt and dirt
#'   circles around home plate
#'
#' @keywords internal
baseball_infield_dirt <- function(home_plate_circle_radius = 0,
                                  foul_line_to_foul_grass = 0,
                                  pitchers_plate_distance = 0,
                                  infield_arc_radius = 0) {
  # Start by finding the point where the home plate circle intersects the grass
  # on the third base side
  home_plate_x2 <- 2
  home_plate_x1 <- 2 * foul_line_to_foul_grass
  home_plate_x0 <-
    (foul_line_to_foul_grass^2) - (home_plate_circle_radius^2)

  # Find roots of where the home plate dirt circle meets
  # the line y = -x - {foul_line_to_foul_grass}
  home_plate_roots <- quadratic_formula(
    home_plate_x2,
    home_plate_x1,
    home_plate_x0
  )

  # Third base side so need -x
  home_plate_x <- home_plate_roots[which(home_plate_roots < 0)]

  # Get the starting and ending theta. If acos(home_plate_x /
  # home_plate_circle_radius) is undefined use theta = pi
  if ((home_plate_circle_radius == 0) ||
      is.na(acos(home_plate_x / home_plate_circle_radius))) {
    home_plate_start_theta <- 1
  } else {
    home_plate_start_theta <-
      acos(home_plate_x / home_plate_circle_radius) / pi
  }

  # Define the stopping point of home plate circle's theta
  home_plate_end_theta <- 3 - home_plate_start_theta

  # Find infield thetas
  infield_x2 <- 2
  infield_x1 <- (-2 * foul_line_to_foul_grass) -
    (2 * pitchers_plate_distance)
  infield_x0 <- (foul_line_to_foul_grass^2) +
    (2 * foul_line_to_foul_grass * pitchers_plate_distance) +
    (pitchers_plate_distance^2) -
    (infield_arc_radius^2)

  # Find roots of where the infield dirt arc meets
  # the line y = x - {foul_line_to_foul_grass}
  infield_roots <- quadratic_formula(
    infield_x2,
    infield_x1,
    infield_x0
  )

  # Infield drawn from first base to third base side, so need positive
  # intersection with y = x - {foul_line_to_foul_grass}
  infield_x <- infield_roots[which(infield_roots > 0)]

  # Get the starting and ending theta. If acos(infield_x / infield_arc_radius)
  # is undefined, use theta = pi / 4
  if ((infield_arc_radius == 0) ||
      (is.na(acos(infield_x / infield_arc_radius)))) {
    infield_start_theta <- 0.25
  } else {
    infield_start_theta <- acos(infield_x / infield_arc_radius) / pi
  }

  infield_end_theta <- 1 - infield_start_theta

  infield_dirt <- rbind(
    create_circle(
      center = c(0, pitchers_plate_distance),
      start = infield_start_theta,
      end = infield_end_theta,
      r = infield_arc_radius
    ),
    create_circle(
      center = c(0, 0),
      start = home_plate_start_theta,
      end = home_plate_end_theta,
      r = home_plate_circle_radius
    )
  )

  return(infield_dirt)
}

#' The dirt that comprises the infield grass. This is the area inside the lines
#' drawn by the basepaths
#'
#' @param home_plate_circle_radius The radius of the circle around home plate
#' @param foul_line_to_infield_grass The distance from the outer edge of the
#'   foul line to the inner edge of the infield grass
#' @param baseline_distance The distance from the back tip of home plate to the
#'   back corner of either first or third base along the foul line
#'
#' @return A data frame that comprises the entirety of the infield grass
#'
#' @keywords internal
baseball_infield_grass <- function(home_plate_circle_radius = 0,
                                   foul_line_to_infield_grass = 0,
                                   baseline_distance = 0,
                                   base_anchor_to_infield_grass = 0) {
  # Find the intersection point on the first base line of where the home plate
  # circle will intersect the infield grass
  home_plate_1b_x2 <- 2
  home_plate_1b_x1 <- 2 * foul_line_to_infield_grass
  home_plate_1b_x0 <- (foul_line_to_infield_grass^2) -
    (home_plate_circle_radius^2)

  # Find the roots
  home_plate_1b_roots <- quadratic_formula(
    home_plate_1b_x2,
    home_plate_1b_x1,
    home_plate_1b_x0
  )

  # First base side, so need x > 0. If none exist, use the radius (pi = 0)
  if (length(which(home_plate_1b_roots > 0)) == 0) {
    home_plate_1b_x <- home_plate_circle_radius
  } else {
    home_plate_1b_x <-
      home_plate_1b_roots[which(home_plate_1b_roots > 0)]
  }

  # Find the angle to use at first base
  if (home_plate_circle_radius == 0) {
    home_plate_1b_theta <- 0
  } else {
    home_plate_1b_theta <-
      acos(home_plate_1b_x / home_plate_circle_radius) / pi
  }

  # Repeat for third base
  home_plate_3b_x2 <- 2
  home_plate_3b_x1 <- -2 * foul_line_to_infield_grass
  home_plate_3b_x0 <- (foul_line_to_infield_grass^2) -
    (home_plate_circle_radius^2)

  home_plate_3b_roots <- quadratic_formula(
    home_plate_3b_x2,
    home_plate_3b_x1,
    home_plate_3b_x0
  )

  if (length(which(home_plate_3b_roots > 0)) == 0) {
    home_plate_3b_x <- home_plate_circle_radius
  } else {
    home_plate_3b_x <-
      home_plate_3b_roots[which(home_plate_3b_roots < 0)]
  }

  if (home_plate_circle_radius == 0) {
    home_plate_3b_theta <- 1
  } else {
    home_plate_3b_theta <-
      acos(home_plate_3b_x / home_plate_circle_radius) / pi
  }

  # Find the start and end angle at first base in the same manner
  first_base_x2 <- 2
  first_base_x1 <- (2 * foul_line_to_infield_grass) -
    (2 * sqrt(2) * baseline_distance)
  first_base_x0 <- (foul_line_to_infield_grass^2) -
    (sqrt(2) * foul_line_to_infield_grass * baseline_distance) +
    (baseline_distance^2) -
    (base_anchor_to_infield_grass^2)
  first_base_roots <- quadratic_formula(
    first_base_x2,
    first_base_x1,
    first_base_x0
  )
  first_base_x <- first_base_roots[
    which(first_base_roots < baseline_distance * cos(pi / 4))
  ]
  first_base_delta <- abs(first_base_x - (baseline_distance * cos(pi / 4)))
  if (base_anchor_to_infield_grass == 0) {
    first_base_theta <- 0
  } else {
    first_base_theta <- acos(
      first_base_delta / base_anchor_to_infield_grass
    ) / pi
  }

  # Rotate the theta around to get the arc starting and ending radians (per unit
  # pi) of each base
  first_base_start_theta <- 1 + first_base_theta
  first_base_end_theta <- 1 - first_base_theta

  second_base_start_theta <- 1.5 + first_base_theta
  second_base_end_theta <- 1.5 - first_base_theta

  third_base_start_theta <- 1 - first_base_end_theta
  third_base_end_theta <- 1 - first_base_start_theta

  infield_grass_df <- rbind(
    create_circle(
      center = c(0, 0),
      start = home_plate_3b_theta,
      end = home_plate_1b_theta,
      r = home_plate_circle_radius
    ),
    create_circle(
      center = c(
        baseline_distance * cos(pi / 4),
        baseline_distance * sin(pi / 4)
      ),
      start = first_base_start_theta,
      end = first_base_end_theta,
      r = base_anchor_to_infield_grass
    ),
    create_circle(
      center = c(0, baseline_distance * sqrt(2)),
      start = second_base_start_theta,
      end = second_base_end_theta,
      r = base_anchor_to_infield_grass
    ),
    create_circle(
      center = c(
        baseline_distance * cos(3 * pi / 4),
        baseline_distance * sin(3 * pi / 4)
      ),
      start = third_base_start_theta,
      end = third_base_end_theta,
      r = base_anchor_to_infield_grass
    ),
    create_circle(
      center = c(0, 0),
      start = home_plate_3b_theta,
      end = home_plate_1b_theta,
      r = home_plate_circle_radius
    )[1, ]
  )

  return(infield_grass_df)
}





# Surface Boundaries -----------------------------------------------------------
# TODO: add wall functionality, warning track, dugouts





# Surface Lines ----------------------------------------------------------------

#' The batter's boxes on the field. This is where a batter must stand to legally
#' hit the ball
#'
#' @param batters_box_length The length of the batter's box (in the y direction)
#'   measured from the outside of the chalk lines
#' @param batters_box_width The width of the batter's box (in the x direction)
#'   measured from the outside of the chalk lines
#' @param batters_box_y_adj The shift off of center in the y direction that the
#'   batter's box is to be moved to properly align
#' @param batters_box_thickness The thickness of the chalk lines that comprise
#'   the batter's box
#'
#' @return A data frame of the batter's box
#'
#' @keywords internal
baseball_batters_box <- function(batters_box_length = 0,
                                 batters_box_width = 0,
                                 batters_box_y_adj = 0,
                                 batters_box_thickness = 0) {
  # This is a rectangular feature, but because the feature has a thickness, it
  # will be drawn and reflected over the y axis
  batters_box_df <- data.frame(
    x = c(
      0,
      batters_box_width / 2,
      batters_box_width / 2,
      0,
      0,
      (batters_box_width / 2) - batters_box_thickness,
      (batters_box_width / 2) - batters_box_thickness,
      0,
      0
    ),
    y = c(
      batters_box_length / 2,
      batters_box_length / 2,
      -batters_box_length / 2,
      -batters_box_length / 2,
      ((-batters_box_length / 2) + batters_box_thickness),
      ((-batters_box_length / 2) + batters_box_thickness),
      (batters_box_length / 2) - batters_box_thickness,
      (batters_box_length / 2) - batters_box_thickness,
      batters_box_length
    )
  )

  # Reflect the half-box over the y axis to get the full box
  batters_box_df <- rbind(
    batters_box_df,
    reflect(batters_box_df, over_x = FALSE, over_y = TRUE)
  )

  # Add in the y-adjustment for proper alignment
  batters_box_df["y"] <- batters_box_df["y"] + batters_box_y_adj

  return(batters_box_df)
}

#' The catcher's box. This is where the catcher is located on defense, usually
#' marked by two white lines and a back line as well. The box may take various
#' shapes, which are controlled by the \code{catchers_box_shape} parameter
#'
#' @param catchers_box_depth The distance from the back tip of home plate to the
#'   back edge of the catcher's box
#' @param catchers_box_width The distance between the outer edges of the
#'   catcher's box at the widest point
#' @param batters_box_length The length of the batter's box (in the y direction)
#'   measured from the outside of the chalk lines
#' @param batters_box_y_adj The shift off of center in the y direction that the
#'   batter's box is to be moved to properly align
#' @param catchers_box_shape A string representing the shape of the catcher's
#'   box to draw
#' @param catchers_box_thickness The thickness of the chalk lines that comprise
#'   the catcher's box
#' @param home_plate_circle_radius The radius of the circle around home plate
#'
#' @return A data frame containing the bounding box of the catcher's box
#'
#' @keywords internal
baseball_catchers_box <- function(catchers_box_depth = 0,
                                  catchers_box_width = 0,
                                  batters_box_length = 0,
                                  batters_box_y_adj = 0,
                                  catchers_box_shape = "rectangle",
                                  catchers_box_thickness = 0,
                                  home_plate_circle_radius = 0) {
  # The catcher's box is shape-dependent in the following way. This is a
  # switch() statement for ease of future implementation of new styles

  # Full rectangle -------------------------------------------------------------
  catchers_box_full_rectangle_df <- data.frame(
    x = c(
      catchers_box_width / 2,
      catchers_box_width / 2,
      -catchers_box_width / 2,
      -catchers_box_width / 2,
      ((-catchers_box_width / 2) + catchers_box_thickness),
      ((-catchers_box_width / 2) + catchers_box_thickness),
      (catchers_box_width / 2) - catchers_box_thickness,
      (catchers_box_width / 2) - catchers_box_thickness,
      catchers_box_width / 2
    ),
    y = c(
      -batters_box_length / 2,
      -catchers_box_depth,
      -catchers_box_depth,
      -batters_box_length / 2,
      -batters_box_length / 2,
      -catchers_box_depth + catchers_box_thickness,
      -catchers_box_depth + catchers_box_thickness,
      -batters_box_length / 2,
      -batters_box_length / 2
    )
  )

  catchers_box_full_rectangle_df["y"] <- catchers_box_full_rectangle_df["y"] +
    batters_box_y_adj

  # Trapezoid ------------------------------------------------------------------
  # Calculate the base, where base 1 (b1) corresponds to the smaller of the
  # two bases and base 2 (b2) is the longer
  catchers_box_b1_y <- (-batters_box_length / 2) + batters_box_y_adj
  catchers_box_b1_r <- catchers_box_b1_y / cos(pi / 4)
  catchers_box_b1 <- 2 * (abs(catchers_box_b1_r) * sin(pi / 4))
  catchers_box_b2_y_outer <-
    -catchers_box_depth - catchers_box_thickness
  catchers_box_b2_y_inner <- -catchers_box_depth
  catchers_box_b2_r_inner <- home_plate_circle_radius -
    (catchers_box_thickness / cos(pi / 4))
  catchers_box_b2_r_outer <- home_plate_circle_radius
  catchers_box_b2_inner <-
    2 * (abs(catchers_box_b2_r_inner) * sin(pi / 4))
  catchers_box_b2_outer <-
    2 * (abs(catchers_box_b2_r_outer) * sin(pi / 4))
  catchers_box_trapezoid_df <- data.frame(
    x = c(
      catchers_box_b1 / 2,
      (catchers_box_b2_outer / 2) - catchers_box_thickness,
      (-catchers_box_b2_outer / 2) + catchers_box_thickness,
      -catchers_box_b1 / 2,
      (-catchers_box_b1 / 2) - catchers_box_thickness,
      (-catchers_box_b2_inner / 2) - (2 * catchers_box_thickness),
      (catchers_box_b2_inner / 2) + (2 * catchers_box_thickness),
      (catchers_box_b1 / 2) + catchers_box_thickness,
      catchers_box_b1 / 2
    ),
    y = c(
      catchers_box_b1_y,
      catchers_box_b2_y_inner + catchers_box_thickness,
      catchers_box_b2_y_inner + catchers_box_thickness,
      catchers_box_b1_y,
      catchers_box_b1_y,
      catchers_box_b2_y_outer + catchers_box_thickness,
      catchers_box_b2_y_outer + catchers_box_thickness,
      catchers_box_b1_y,
      catchers_box_b1_y
    )
  )

  # Cases ----------------------------------------------------------------------
  catchers_box_df <- switch(
    tolower(catchers_box_shape),

    # Rectangle cases
    "rect" = catchers_box_full_rectangle_df,
    "rectangle" = catchers_box_full_rectangle_df,
    "rectangular" = catchers_box_full_rectangle_df,

    # Trapezoid cases
    "trap" = catchers_box_trapezoid_df,
    "trapezoid" = catchers_box_trapezoid_df,
    "trapezoidal" = catchers_box_trapezoid_df,

    # Error if not a valid shape
    stop(message(
      glue::glue(
        "{catchers_box_shape} is not a valid shape for the catcher's box. ",
        "Defaulting to rectangle"
      )
    ))
  )

  return(catchers_box_df)
}

#' The foul line. These are the white lines that extend from the back tip of
#' home plate (but not visibly through the batter's boxes) out to the fair/foul
#' pole in the outfield. Since a ball on the line is considered in fair
#' territory, the outer edge of the baseline must lie in fair territory (aka the
#' line y = +/- x)
#'
#' @param is_line_1b Whether or not the line is the first base line
#' @param line_distance The straight-line distance from the back tip of home
#'   plate to the terminus of the line at the foul pole
#' @param batters_box_length The length of the batter's box (in the y direction)
#'   measured from the outside of the chalk lines
#' @param batters_box_width The width of the batter's box (in the x direction)
#'   measured from the outside of the chalk lines
#' @param batters_box_y_adj The shift off of center in the y direction that the
#'   batter's box is to be moved to properly align
#' @param home_plate_side_to_batters_box The distance from the outer edge of the
#'   batter's box to the inner edge of home plate
#' @param foul_line_thickness The thickness of the chalk line that comprise the
#'   foul line
#'
#' @return A data frame containing the foul line's bounding coordinates
#'
#' @keywords internal
baseball_foul_line <- function(is_line_1b = FALSE,
                               line_distance = 0,
                               batters_box_length = 0,
                               batters_box_width = 0,
                               batters_box_y_adj = 0,
                               home_plate_side_to_batters_box = 0,
                               foul_line_thickness = 0) {
  # Check where the foul line intersects the batter's box
  batters_box_corner_coord_x <- batters_box_width +
    home_plate_side_to_batters_box
  batters_box_corner_coord_y <- (batters_box_length / 2) + batters_box_y_adj

  # If the line intersects the batter's box on the pitcher's mound side of the
  # box, use the y coordinate as the bounding limit
  if (batters_box_corner_coord_x > batters_box_corner_coord_y) {
    starting_coord <- batters_box_corner_coord_y
  } else {
    starting_coord <- batters_box_corner_coord_x +
      (batters_box_y_adj / 2) +
      foul_line_thickness
  }

  # Third base line
  if (!is_line_1b) {
    foul_line_df <- data.frame(
      x = c(
        -starting_coord,
        line_distance * cos(3 * pi / 4),
        (line_distance * cos(3 * pi / 4)) + foul_line_thickness,
        -starting_coord + foul_line_thickness,
        -starting_coord
      ),
      y = c(
        starting_coord,
        line_distance * sin(3 * pi / 4),
        line_distance * sin(3 * pi / 4),
        starting_coord,
        starting_coord
      )
    )
  } else {
    # First base line
    foul_line_df <- data.frame(
      x = c(
        starting_coord,
        line_distance * cos(pi / 4),
        (line_distance * cos(pi / 4)) - foul_line_thickness,
        starting_coord - foul_line_thickness,
        starting_coord
      ),
      y = c(
        starting_coord,
        line_distance * sin(pi / 4),
        line_distance * sin(pi / 4),
        starting_coord,
        starting_coord
      )
    )
  }

  return(foul_line_df)
}

#' The running lane is entirely in foul territory. The depth should be measured
#' from the foul-side edge of the baseline to the outer edge of the running lane
#' mark
#'
#' All measurements should be given "looking down the line" (e.g. as they would
#' be measured by an observer standing behind home plate)
#'
#' @param running_lane_depth The distance from the outer edge of the foul line
#'   to the outer edge of the running lane
#' @param running_lane_length The total distance of the running lane, from where
#'   it first starts to its terminus near first base
#' @param running_lane_start_distance The distance from the back tip of home
#'   plate that the running lane starts
#' @param running_lane_thickness The thickness of the chalk line that comprises
#'   the running lane
#'
#' @return A data frame containing the running lane's bounding coordinates
#'
#' @keywords internal
baseball_running_lane <- function(running_lane_depth = 0,
                                  running_lane_length = 0,
                                  running_lane_start_distance = 0,
                                  running_lane_thickness = 0) {
  running_lane_df <- data.frame(
    x = c(
      running_lane_start_distance / sqrt(2),
      (running_lane_start_distance + running_lane_depth) / sqrt(2),
      (
        running_lane_start_distance +
          running_lane_depth +
          running_lane_length
      ) / sqrt(2),
      (
        running_lane_start_distance +
          running_lane_depth +
          running_lane_length -
          running_lane_thickness
      ) / sqrt(2),
      (running_lane_start_distance + running_lane_depth) / sqrt(2),
      (running_lane_start_distance + running_lane_thickness) / sqrt(2),
      running_lane_start_distance / sqrt(2)
    ),
    y = c(
      running_lane_start_distance / sqrt(2),
      (
        running_lane_start_distance -
          running_lane_depth
      ) / sqrt(2),
      (
        running_lane_start_distance -
          running_lane_depth +
          running_lane_length
      ) / sqrt(2),
      (
        running_lane_start_distance -
          running_lane_depth +
          running_lane_length +
          running_lane_thickness
      ) / sqrt(2),
      (
        running_lane_start_distance -
          running_lane_depth +
          (2 * running_lane_thickness)
      ) / sqrt(2),
      (
        running_lane_start_distance +
          running_lane_thickness
      ) / sqrt(2),
      running_lane_start_distance / sqrt(2)
    )
  )

  return(running_lane_df)
}





# Surface Features -------------------------------------------------------------

#' Home plate. This is a pentagonal shape with its back tip located at the
#' origin of the coordinate system. The angled sides of home plate intersect the
#' baselines
#'
#' @param home_plate_edge_length The length of a single edge of home plate
#'
#' @return A data frame that contains the boundary of home plate
#'
#' @keywords internal
baseball_home_plate <- function(home_plate_edge_length = 0) {
  home_plate_df <- data.frame(
    x = c(
      0,
      home_plate_edge_length / 2,
      home_plate_edge_length / 2,
      -home_plate_edge_length / 2,
      -home_plate_edge_length / 2,
      0
    ),
    y = c(
      0,
      home_plate_edge_length / 2,
      home_plate_edge_length,
      home_plate_edge_length,
      home_plate_edge_length / 2,
      0
    )
  )

  return(home_plate_df)
}

#' One of the bases on the diamond, or really any base on the field. These are
#' squares that are rotated 45 degrees
#'
#' @param base_side_length The length of each side of the base
#' @param adjust_x_left Whether or not the base should be adjusted in the -x
#'   direction (e.g. third base)
#' @param adjust_x_right Whether or not the base should be adjusted in the +x
#'   direction (e.g. first base)
#'
#' @return A data frame that comprises the boundary of the base
#'
#' @keywords internal
baseball_base <- function(base_side_length = 0,
                          adjust_x_left = FALSE,
                          adjust_x_right = FALSE) {
  # Start with a center adjustment of x to be 0
  center_x_adj <- 0

  # If the base's center needs to be adjusted, calculate the adjustment
  if (adjust_x_left) {
    adjustment_amount <- base_side_length * sqrt(2) / 2
    center_x_adj <- center_x_adj - adjustment_amount
  }
  if (adjust_x_right) {
    adjustment_amount <- base_side_length * sqrt(2) / 2
    center_x_adj <- center_x_adj + adjustment_amount
  }

  # Create the base
  base_df <- create_square(
    side_length = base_side_length,
    center = c(0, 0)
  )

  base_df <- rotate_coords(
    df = base_df,
    angle = 45
  )

  # Adjust the base's x-positioning by the calculated adjustment
  base_df["x"] <- base_df["x"] + center_x_adj

  return(base_df)
}

#' The pitcher's mound. This is where the pitcher's plate is located, but the
#' pitcher's plate is not necessarily centered on the pitcher's mound
#'
#' @param pitchers_mound_radius The radius of the pitcher's mound
#'
#' @return A data frame of the pitcher's mound's bounding coordinates
#'
#' @keywords internal
baseball_pitchers_mound <- function(pitchers_mound_radius = 0) {
  # The pitcher's mound is a circle
  pitchers_mound_df <- create_circle(
    center = c(0, 0),
    start = 0,
    end = 2,
    r = pitchers_mound_radius
  )

  return(pitchers_mound_df)
}

#' The pitcher's plate. This is where the pitcher must throw the ball from. It's
#' usually a long rectangle with its front edge as its anchor point
#'
#' @param pitchers_plate_length the length (x-direction) of the pitcher's plate
#' @param pitchers_plate_width the width (y-direction) of the pitcher's plate
#'
#' @return A data frame of the pitcher's plate's bounding coordinates
#'
#' @keywords internal
baseball_pitchers_plate <- function(pitchers_plate_length = 0,
                                    pitchers_plate_width = 0) {
  # This feature is a rectangle
  pitchers_plate_df <- create_rectangle(
    x_min = -pitchers_plate_length / 2,
    x_max = pitchers_plate_length / 2,
    y_min = 0,
    y_max = pitchers_plate_width
  )

  return(pitchers_plate_df)
}
