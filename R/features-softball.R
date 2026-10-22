# Surface Base Features --------------------------------------------------------

#' Softball Infield Dirt
#'
#' The dirt that comprises the infield. Unlike a baseball field, a softball
#' infield is typically fully skinned (there is no infield grass). This includes
#' the base paths, the infield arc (the "grass line"), and the dirt circle
#' around home plate.
#'
#' The NCAA softball rules recommend that the skinned area be determined by
#' measuring a 60-foot arc from the front center of the pitcher's plate
#'
#' @param home_plate_circle_radius The radius of the dirt circle around home
#'   plate
#' @param foul_line_to_foul_grass The distance from the outer edge of the foul
#'   line to the inner edge of the grass in foul territory
#' @param pitchers_plate_distance The distance from the back tip of home plate
#'   to the front edge of the pitcher's plate
#' @param infield_arc_radius The distance from the front edge of the pitcher's
#'   plate to the back of the infield dirt (the grass line)
#'
#' @return A data frame that comprises the entirety of the infield dirt and dirt
#'   circle around home plate
#'
#' @keywords internal
softball_infield_dirt <- function(home_plate_circle_radius = 0,
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
  home_plate_theta_undefined <- (home_plate_circle_radius == 0) ||
    (length(home_plate_x) == 0) ||
    is.na(acos(home_plate_x / home_plate_circle_radius))

  if (home_plate_theta_undefined) {
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
  infield_theta_undefined <- (infield_arc_radius == 0) ||
    (length(infield_x) == 0) ||
    is.na(acos(infield_x / infield_arc_radius))

  if (infield_theta_undefined) {
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





# Surface Boundaries -----------------------------------------------------------
# TODO: add home run fence, warning track, backstop, dugouts





# Surface Lines ----------------------------------------------------------------

#' Softball Batter's Box
#'
#' The batter's boxes on the field. This is where a batter must stand to legally
#' hit the ball. Per the NCAA softball rules, each box is 3 feet by 7 feet
#' (including the lines), with the front line of each box 4 feet in front of a
#' line drawn through the center of home plate
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
softball_batters_box <- function(batters_box_length = 0,
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
      batters_box_length / 2
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

#' Softball Catcher's Box
#'
#' The catcher's box. This is where the catcher is located on defense. Per the
#' NCAA softball rules, the catcher's box is a rectangle that extends 7 feet
#' behind the rear outside corners of the batter's boxes, and is 8 feet, 5
#' inches wide (including the lines). Its side lines are extensions of the
#' outer lines of the batter's boxes
#'
#' @param catchers_box_depth The distance from the rear edge of the batter's
#'   boxes to the back edge of the catcher's box
#' @param catchers_box_width The distance between the outer edges of the
#'   catcher's box
#' @param batters_box_length The length of the batter's box (in the y direction)
#'   measured from the outside of the chalk lines
#' @param batters_box_y_adj The shift off of center in the y direction that the
#'   batter's box is to be moved to properly align
#' @param catchers_box_thickness The thickness of the chalk lines that comprise
#'   the catcher's box
#'
#' @return A data frame containing the bounding box of the catcher's box
#'
#' @keywords internal
softball_catchers_box <- function(catchers_box_depth = 0,
                                  catchers_box_width = 0,
                                  batters_box_length = 0,
                                  batters_box_y_adj = 0,
                                  catchers_box_thickness = 0) {
  # The front of the catcher's box is the rear edge of the batter's boxes
  catchers_box_front_y <- (-batters_box_length / 2) + batters_box_y_adj

  # The back of the catcher's box is catchers_box_depth behind that
  catchers_box_back_y <- catchers_box_front_y - catchers_box_depth

  catchers_box_df <- data.frame(
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
      catchers_box_front_y,
      catchers_box_back_y,
      catchers_box_back_y,
      catchers_box_front_y,
      catchers_box_front_y,
      catchers_box_back_y + catchers_box_thickness,
      catchers_box_back_y + catchers_box_thickness,
      catchers_box_front_y,
      catchers_box_front_y
    )
  )

  return(catchers_box_df)
}

#' Softball Foul Line
#'
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
#' @param home_plate_edge_length The length of a single edge of home plate
#' @param foul_line_thickness The thickness of the chalk line that comprise the
#'   foul line
#'
#' @return A data frame containing the foul line's bounding coordinates
#'
#' @keywords internal
softball_foul_line <- function(is_line_1b = FALSE,
                               line_distance = 0,
                               batters_box_length = 0,
                               batters_box_width = 0,
                               batters_box_y_adj = 0,
                               home_plate_side_to_batters_box = 0,
                               home_plate_edge_length = 0,
                               foul_line_thickness = 0) {
  # Find the outer (foul-side) and front (pitcher-side) edges of the batter's
  # box. The foul line y = x will exit the batter's box through whichever of
  # these edges it reaches first
  batters_box_outer_x <- (home_plate_edge_length / 2) +
    home_plate_side_to_batters_box +
    batters_box_width
  batters_box_front_y <- (batters_box_length / 2) + batters_box_y_adj

  starting_coord <- min(batters_box_outer_x, batters_box_front_y)

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

#' Softball Running Lane
#'
#' The running lane (called the "runner's lane" in the NCAA softball rules) is
#' entirely in foul territory. The depth should be measured from the foul-side
#' edge of the baseline to the outer edge of the running lane mark
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
softball_running_lane <- function(running_lane_depth = 0,
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

#' Softball Pitcher's Circle
#'
#' The pitcher's circle. This is a circular line with its outer edge a fixed
#' radius from the center of the front edge of the pitcher's plate. The circle
#' is drawn with its center at the origin, and is anchored to the front edge of
#' the pitcher's plate when plotted
#'
#' @param pitchers_circle_radius The radius of the pitcher's circle, measured
#'   to the outer edge of the line
#' @param pitchers_circle_thickness The thickness of the chalk line that
#'   comprises the pitcher's circle
#'
#' @return A data frame of the pitcher's circle's bounding coordinates
#'
#' @keywords internal
softball_pitchers_circle <- function(pitchers_circle_radius = 0,
                                     pitchers_circle_thickness = 0) {
  pitchers_circle_df <- rbind(
    create_circle(
      center = c(0, 0),
      start = 0,
      end = 2,
      r = pitchers_circle_radius
    ),
    create_circle(
      center = c(0, 0),
      start = 2,
      end = 0,
      r = pitchers_circle_radius - pitchers_circle_thickness
    )
  )

  return(pitchers_circle_df)
}

#' Softball Pitcher's Lane
#'
#' The pitcher's lane. This is the area to which the pitcher is restricted when
#' delivering a pitch. Per the NCAA softball rules, it is marked by two lines,
#' each of a fixed length, that extend from the outer edges of the pitcher's
#' plate toward the inside front corners of the batter's boxes. The outside
#' edge of each line corresponds with the outside edge of the pitcher's plate.
#'
#' This function draws the first-base side line. The line starts at the front
#' edge of the pitcher's plate (the feature's anchor point) and is reflected
#' over the \code{y} axis to create the third-base side line
#'
#' @param pitchers_lane_length The length of each pitcher's lane line
#' @param pitchers_plate_length The length (x-direction) of the pitcher's plate
#' @param pitchers_plate_front_to_home_plate The distance from the back tip of
#'   home plate to the front edge of the pitcher's plate
#' @param home_plate_edge_length The length of a single edge of home plate
#' @param home_plate_side_to_batters_box The distance from the outer edge of the
#'   batter's box to the inner edge of home plate
#' @param batters_box_length The length of the batter's box (in the y direction)
#'   measured from the outside of the chalk lines
#' @param batters_box_y_adj The shift off of center in the y direction that the
#'   batter's box is to be moved to properly align
#' @param pitchers_lane_thickness The thickness of the chalk line that
#'   comprises the pitcher's lane
#'
#' @return A data frame of the pitcher's lane line's bounding coordinates
#'
#' @keywords internal
softball_pitchers_lane <- function(pitchers_lane_length = 0,
                                   pitchers_plate_length = 0,
                                   pitchers_plate_front_to_home_plate = 0,
                                   home_plate_edge_length = 0,
                                   home_plate_side_to_batters_box = 0,
                                   batters_box_length = 0,
                                   batters_box_y_adj = 0,
                                   pitchers_lane_thickness = 0) {
  # Find the inside front corner of the first-base side batter's box
  batters_box_inner_x <- (home_plate_edge_length / 2) +
    home_plate_side_to_batters_box
  batters_box_front_y <- (batters_box_length / 2) + batters_box_y_adj

  # Start at the outer edge of the front of the pitcher's plate
  start_x <- pitchers_plate_length / 2

  # Find the direction from the start of the line toward the inside front
  # corner of the batter's box. This is relative to the front edge of the
  # pitcher's plate, which serves as the anchor
  delta_x <- batters_box_inner_x - start_x
  delta_y <- batters_box_front_y - pitchers_plate_front_to_home_plate

  line_theta <- atan2(delta_y, delta_x)

  # If the distances are all 0, the line should point straight toward home
  # plate
  if (delta_x == 0 && delta_y == 0) {
    line_theta <- -pi / 2
  }

  end_x <- start_x + (pitchers_lane_length * cos(line_theta))
  end_y <- pitchers_lane_length * sin(line_theta)

  # The line's thickness extends toward the inside of the lane (toward the y
  # axis) so that the outer edge of the line matches the outer edge of the
  # pitcher's plate
  pitchers_lane_df <- data.frame(
    x = c(
      start_x,
      end_x,
      end_x - pitchers_lane_thickness,
      start_x - pitchers_lane_thickness,
      start_x
    ),
    y = c(
      0,
      end_y,
      end_y,
      0,
      0
    )
  )

  return(pitchers_lane_df)
}

#' Softball Coach's Box
#'
#' The coach's box. Per the NCAA softball rules, each coach's box is marked by
#' two lines. The first is a line drawn parallel to and a fixed distance from
#' the first- and third-base lines, extending from the back edge of the base
#' toward home plate. The second is a shorter line drawn perpendicular to the
#' end of the first line closest to home plate.
#'
#' This function draws the first-base side coach's box. It should be reflected
#' over the \code{y} axis to draw the third-base side coach's box
#'
#' @param baseline_distance The distance from the back tip of home plate to the
#'   back corner of either first or third base along the foul line
#' @param coaches_box_depth The distance from the foul-side edge of the foul
#'   line to the coach's box's line that is parallel to the foul line
#' @param coaches_box_length The length of the coach's box's line that is
#'   parallel to the foul line
#' @param coaches_box_width The length of the coach's box's line that is
#'   perpendicular to the foul line
#' @param coaches_box_thickness The thickness of the chalk lines that comprise
#'   the coach's box
#'
#' @return A data frame of the coach's box's bounding coordinates
#'
#' @keywords internal
softball_coaches_box <- function(baseline_distance = 0,
                                 coaches_box_depth = 0,
                                 coaches_box_length = 0,
                                 coaches_box_width = 0,
                                 coaches_box_thickness = 0) {
  # Work in a coordinate system aligned with the first base line, where "a" is
  # the distance along the line from the back tip of home plate and "d" is the
  # distance into foul territory from the outer edge of the foul line. These
  # are then converted into the x-y coordinate system
  a_end <- baseline_distance
  a_start <- baseline_distance - coaches_box_length
  d_in <- coaches_box_depth
  d_out <- coaches_box_depth + coaches_box_width
  t <- coaches_box_thickness

  # The box is an "L" shape: the long line is parallel to the foul line, and
  # the short line is at the end closest to home plate
  coaches_box_ad <- data.frame(
    a = c(
      a_end,
      a_start,
      a_start,
      a_start + t,
      a_start + t,
      a_end,
      a_end
    ),
    d = c(
      d_in,
      d_in,
      d_out,
      d_out,
      d_in + t,
      d_in + t,
      d_in
    )
  )

  coaches_box_df <- data.frame(
    x = (coaches_box_ad$a + coaches_box_ad$d) / sqrt(2),
    y = (coaches_box_ad$a - coaches_box_ad$d) / sqrt(2)
  )

  return(coaches_box_df)
}





# Surface Features -------------------------------------------------------------

#' Softball Home Plate
#'
#' Home plate. This is a pentagonal shape with its back tip located at the
#' origin of the coordinate system. The angled sides of home plate intersect the
#' baselines
#'
#' @param home_plate_edge_length The length of a single edge of home plate
#'
#' @return A data frame that contains the boundary of home plate
#'
#' @keywords internal
softball_home_plate <- function(home_plate_edge_length = 0) {
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

#' Softball Base
#'
#' One of the bases on the diamond, or really any base on the field. These are
#' squares that are rotated 45 degrees
#'
#' @param base_side_length The length of each side of the base
#' @param adjust_x_left Whether or not the base should be adjusted in the -x
#'   direction (e.g. first base)
#' @param adjust_x_right Whether or not the base should be adjusted in the +x
#'   direction (e.g. third base)
#'
#' @return A data frame that comprises the boundary of the base
#'
#' @keywords internal
softball_base <- function(base_side_length = 0,
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

#' Softball Pitcher's Plate
#'
#' The pitcher's plate. This is where the pitcher must throw the ball from. It's
#' usually a long rectangle with its front edge as its anchor point
#'
#' @param pitchers_plate_length the length (x-direction) of the pitcher's plate
#' @param pitchers_plate_width the width (y-direction) of the pitcher's plate
#'
#' @return A data frame of the pitcher's plate's bounding coordinates
#'
#' @keywords internal
softball_pitchers_plate <- function(pitchers_plate_length = 0,
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
