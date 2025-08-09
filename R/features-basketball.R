# Surface Base Features --------------------------------------------------------

#' Basketball Half Court
#' 
#' Each half court spans from the inner edge of the baseline to the center of
#' the division line, and serves as the base layer of the court plot
#'
#' @param court_length The length of the court
#' @param court_width The width of the court
#'
#' @return A data frame of the bounding box of half a basketball court
#'
#' @keywords internal
basketball_half_court <- function(court_length = 0, court_width = 0) {
  half_court_df <- create_rectangle(
    # Using quarter-court lengths to account for feature positioning adjustment
    x_min = -court_length / 4,
    x_max = court_length / 4,

    # No adjustment needed in y direction since the half court will take up the
    # entire width of the court
    y_min = -court_width / 2,
    y_max = court_width / 2
  )

  return(half_court_df)
}

#' Basketball Two-Point Range
#' 
#' If a court has a three-point line (see [basketball_three_point_line()]), then
#' any made basket (not including free throws) made from inside of the arc are
#' worth two points. The area inside of this arc is therefore referred to as two
#' point range, which this feature draws. This feature is enclosed by the
#' three-point line's outer edge and the baseline's inner edge
#'
#' It should also be noted that as this corresponds strictly to the area
#' contained by the three-point line, the interior angle is what's needed. While
#' utilizing the corner-three distance as the outer edge should work generally,
#' an issue may arise if the z-order of the feature's plotting characteristic is
#' changed to be greater than that of the three-point line itself. This should
#' not happen, but the interior edge is therefore what is used here
#'
#' Start by getting the distance from the center of the basket to a corner
#' three-point shot. This is referred to as \code{start_y}
#'
#' Next, get the starting angle with which to trace out the two-point range.
#' Taking the distance start_y to be a y coordinate, and the (outer) radius of
#' the arc of the three-point line to be a radius, we the sine of the starting
#' angle is given as \code{start_y / {three_point_arc_radius -
#' three_point_line_thickness}}
#'
#' As the TV-right angle of the start of the arc is what's drawn here, the
#' starting and ending angles need to be adjusted relative to 1 radian (the arc
#' opens to the right, like a \code{(} character)
#'
#' The starting angle is therefore given as \code{1 - angle}, and the ending
#' angle is \code{1 + angle}
#'
#' @param basket_center_to_baseline The distance from the center of the basket
#'   ring to the inner edge of the baseline
#' @param basket_center_to_corner_three The distance from the center of the
#'   basket ring to the outer edge of the three-point line in the corner in the
#'   court's specified units
#' @param line_thickness The thickness of the three-point line
#' @param two_point_range_radius The radius of the arc portion of the
#'   three-point line
#'
#' @return A data frame of the bounding coordinates of two-point range
#'
#' @keywords internal
basketball_two_point_range <- function(basket_center_to_baseline = 0,
                                       basket_center_to_corner_three = 0,
                                       line_thickness = 0,
                                       two_point_range_radius = 0) {
  # Find start_y
  start_y <- basket_center_to_corner_three - line_thickness

  # Compute the angle
  if (is.na(asin(start_y / two_point_range_radius))) {
    angle <- 0
  } else {
    angle <- asin(start_y / two_point_range_radius) / pi
  }

  start_angle <- 1 - angle
  end_angle <- 1 + angle

  two_point_range_df <- rbind(
    data.frame(
      x = basket_center_to_baseline,
      y = start_y
    ),
    create_circle(
      center = c(0, 0),
      start = start_angle,
      end = end_angle,
      r = two_point_range_radius
    ),
    data.frame(
      x = c(basket_center_to_baseline, basket_center_to_baseline),
      y = c(-start_y, start_y)
    )
  )

  return(two_point_range_df)
}

#' Basketball Center Circle (Interior)
#' 
#' The center circle is broken into two parts: the
#' [basketball_center_circle_outline()], and the fill (this feature), which is
#' the court coloring inside of the inner edge of this circle
#'
#' @param center_circle_radius The outer radius of the center circle
#' @param line_thickness The thickness of the line that comprises the center
#'   circle
#'
#' @return A data frame of the boundary of the center circle. The interior of
#'   these coordinates correspond to the filled section
#'
#' @keywords internal
basketball_center_circle_fill <- function(center_circle_radius = 0,
                                          line_thickness = 0) {
  center_circle_fill_df <- create_circle(
    center = c(0, 0),
    start = 0,
    end = 2,
    r = center_circle_radius - line_thickness
  )

  return(center_circle_fill_df)
}

#' Basketball Painted Area
#' 
#' The painted area is the area contained by the free throw lane (see
#' [basketball_free_throw_lane_boundary()] for more information on the free
#' throw lane)
#'
#' The painted area may be a different color than the rest of the two point
#' range area (see [basketball_two_point_range()] for more information on
#' two-point range), but may also be the same color
#'
#' @param lane_length The length of the free throw lane
#' @param lane_width The width of the free throw
#' @param paint_margin The distance from the painted area of the lane to the
#'   free throw lane boundary lines
#' @param line_thickness The thickness of the line of the free throw lane
#'   boundary line
#'
#' @return A data frame of the bounding coordinates of the free throw lane's
#'   painted area
#'
#' @keywords internal
basketball_painted_area <- function(lane_length = 0,
                                    lane_width = 0,
                                    paint_margin = 0,
                                    line_thickness = 0) {
  painted_area_df <- create_rectangle(
    x_min = paint_margin,
    x_max = lane_length - line_thickness - paint_margin,
    y_min = (-lane_width / 2) + line_thickness + paint_margin,
    y_max = (lane_width / 2) - line_thickness - paint_margin
  )

  return(painted_area_df)
}

#' Basketball Free Throw Circle (Interior)
#' 
#' The filled-in section of the free throw circle. The circle is the area where
#' a free throw shooter stands when attempting the free throw. The outline of
#' this area will be created separately via
#' [basketball_free_throw_circle()]
#'
#' @param free_throw_circle_radius The outer radius of the free throw circle,
#'   measured from the center of the free throw line
#' @param line_thickness The thickness of the outline of the free throw circle
#'
#' @return A data frame containing the bounding coordinates of the free throw
#'   circle's semi-circular filling
#'
#' @keywords internal
basketball_free_throw_circle_fill <- function(free_throw_circle_radius = 0,
                                              line_thickness = 0) {
  free_throw_circle_fill_df <- create_circle(
    center = c(0, 0),
    start = 0.5,
    end = 1.5,
    r = free_throw_circle_radius - line_thickness
  )

  return(free_throw_circle_fill_df)
}





# Surface Boundaries -----------------------------------------------------------

#' Basketball Court Apron
#' 
#' The apron of the court is the colored boundary around the exterior of some
#' courts. If no such colored boundary exists, this should take the same color
#' as the court floor
#'
#' @param court_length The length of the court
#' @param court_width The width of the court
#' @param court_apron_endline The thickness of the court's apron beyond the
#'   endline
#' @param court_apron_sideline The thickness of the court's apron beyond the
#'   sideline
#' @param court_apron_to_boundary The distance from the inner edge of the court
#'   apron to the outer edge of the court's boundary line (sideline and endline
#'   will be spaced the same)
#' @param line_thickness The thickness of the endline and sideline
#'
#' @return A data frame of the bounding coordinates of the court apron
#'
#' @keywords internal
basketball_court_apron <- function(court_length = 0,
                                   court_width = 0,
                                   court_apron_endline = 0,
                                   court_apron_sideline = 0,
                                   court_apron_to_boundary = 0,
                                   line_thickness = 0) {
  court_apron_df <- data.frame(
    x = c(
      0,
      (court_length / 2) + court_apron_endline,
      (court_length / 2) + court_apron_endline,
      0,
      0,
      (court_length / 2) + line_thickness + court_apron_to_boundary,
      (court_length / 2) + line_thickness + court_apron_to_boundary,
      0,
      0
    ),
    y = c(
      (court_width / 2) + court_apron_sideline,
      (court_width / 2) + court_apron_sideline,
      (-court_width / 2) - court_apron_sideline,
      (-court_width / 2) - court_apron_sideline,
      -((court_width / 2) + line_thickness + court_apron_to_boundary),
      -((court_width / 2) + line_thickness + court_apron_to_boundary),
      (court_width / 2) + line_thickness + court_apron_to_boundary,
      (court_width / 2) + line_thickness + court_apron_to_boundary,
      (court_width / 2) + court_apron_sideline
    )
  )

  return(court_apron_df)
}

#' Basketball Endline
#' 
#' The endline on a basketball court, also called the baseline, is located
#' beyond each basket. In cases where the endline is the court apron, the
#' endline should still be generated and its color should be set equal to the
#' court apron's color (see [basketball_court_apron()] for more information on
#' the court apron)
#'
#' @param court_width The width of the court
#' @param line_thickness The thickness of the endline and sideline
#'
#' @return A data frame of the bounding coordinates of the endline
#'
#' @keywords internal
basketball_endline <- function(court_width = 0, line_thickness = 0) {
  endline_df <- create_rectangle(
    x_min = 0,
    x_max = line_thickness,
    y_min = (-court_width / 2) - line_thickness,
    y_max = (court_width / 2) + line_thickness
  )

  return(endline_df)
}

#' Basketball Sideline
#' 
#' The sideline on a basketball court run the full length of the court,
#' typically with the team bench areas and substitution areas on their exterior.
#' In cases where the sideline is the court apron, the sideline should still be
#' generated and its color should be set equal to the court apron's color (see
#' [basketball_court_apron()] for more information on the court apron)
#'
#' @param court_length The length of the court
#' @param line_thickness The thickness of the endline and sideline
#'
#' @return A data frame of the bounding coordinates of the sideline
#'
#' @keywords internal
basketball_sideline <- function(court_length = 0, line_thickness = 0) {
  sideline_df <- create_rectangle(
    x_min = (-court_length / 2) - line_thickness,
    x_max = (court_length / 2) + line_thickness,
    y_min = 0,
    y_max = line_thickness
  )

  return(sideline_df)
}





# Surface Lines ----------------------------------------------------------------

#' Basketball Center Circle (Outline)
#' 
#' The center circle is broken into two parts: the outline (this feature) and
#' the fill, which is the court coloring inside of the inner edge of this circle
#'
#' @param center_circle_radius The outer radius of the center circle
#' @param line_thickness The thickness of the line that comprises the center
#'   circle
#'
#' @return A data frame of the boundary of the center circle
#'
#' @keywords internal
basketball_center_circle_outline <- function(center_circle_radius = 0,
                                             line_thickness = 0) {
  center_circle_outline_df <- rbind(
    # Outer edge of circle
    create_circle(
      center = c(0, 0),
      start = 0.5,
      end = 1.5,
      r = center_circle_radius
    ),

    # Inner edge of circle
    create_circle(
      center = c(0, 0),
      start = 1.5,
      end = 0.5,
      r = center_circle_radius - line_thickness
    )
  )

  return(center_circle_outline_df)
}

#' Basketball Division Line (Half Court Line)
#' 
#' The division line divides the court into two halves, and is sometimes
#' referred to as the time line or half-court line. The center of this line goes
#' through the y axis, with half of the line lying in a team's offensive half
#' court and the other half in their defensive half court
#'
#' @param court_width The width of the court
#' @param line_thickness The thickness of the division line
#' @param division_line_extension The distance that the division line extends
#'   beyond the sideline. This may be omitted if the value is 0
#'
#' @return A data frame of the bounding box for the division line of the court
#'
#' @keywords internal
basketball_division_line <- function(court_width = 0,
                                     line_thickness = 0,
                                     division_line_extension = 0) {
  if (division_line_extension > 0) {
    division_line_df <- create_rectangle(
      x_min = -line_thickness / 2,
      x_max = line_thickness / 2,
      y_min = (-court_width / 2) - division_line_extension - line_thickness,
      y_max = (court_width / 2) - division_line_extension - line_thickness
    )
  } else {
    division_line_df <- create_rectangle(
      x_min = -line_thickness / 2,
      x_max = line_thickness / 2,
      y_min = -court_width / 2,
      y_max = court_width / 2
    )
  }

  return(division_line_df)
}

#' Basketball Three-Point Line
#' 
#' An arc on the court, behind which any made basket counts as three points and
#' in front of which, any made basket will count as two points (see
#' [basketball_two_point_range()] for more information).
#'
#' Start by getting the distance from the center of the basket to a corner
#' three-point shot. This is referred to as \code{start_y}
#'
#' Next, get the starting angle with which to trace out the two-point range.
#' Taking the distance start_y to be a y coordinate, and the radius of the arc
#' of the three-point line to be a radius, we the sine of the starting angle is
#' given as \code{start_y / three_point_arc_radius}
#'
#' As the TV-right angle of the start of the arc is what's drawn here, the
#' starting and ending angles need to be adjusted relative to 1 radian (the arc
#' opens to the right, like a \code{(} character)
#'
#' The starting angle is therefore given as \code{1 - angle}, and the ending
#' angle is \code{1 + angle}
#'
#' @param basket_center_to_baseline The distance from the center of the basket
#'   ring to the inner edge of the baseline
#' @param basket_center_to_corner_three The distance from the center of the
#'   basket ring to the outer edge of the three-point line in the corner in the
#'   court's specified units
#' @param line_thickness The thickness of the three-point line
#' @param three_point_line_radius The outer radius of the arc portion of the
#'   three-point line
#'
#' @return A data frame of the bounding coordinates of the three-point line
#'
#' @keywords internal
basketball_three_point_line <- function(basket_center_to_baseline = 0,
                                        basket_center_to_corner_three = 0,
                                        line_thickness = 0,
                                        three_point_line_radius = 0) {
  # Get the y coordinate of the corner three-point line. These are where both
  # the inner and outer portions of the line start
  start_y_outer <- basket_center_to_corner_three
  start_y_inner <- start_y_outer - line_thickness

  # Define the inner and outer radius
  radius_outer <- three_point_line_radius
  radius_inner <- radius_outer - line_thickness

  # Compute the angles for the inner and outer curves of the three-point arc
  if (is.na(asin(start_y_outer / radius_outer))) {
    angle_outer <- 0
  } else {
    angle_outer <- asin(start_y_outer / radius_outer) / pi
  }

  if (is.na(asin(start_y_inner / radius_inner))) {
    angle_inner <- 0
  } else {
    angle_inner <- asin(start_y_inner / radius_inner) / pi
  }

  # Set the starting and ending angles for the outer and inner tracings, as
  # these should be relative to 1 radian
  start_angle_outer <- 1 - angle_outer
  end_angle_outer <- 1 + angle_outer
  start_angle_inner <- 1 + angle_inner
  end_angle_inner <- 1 - angle_inner

  # Draw the line
  three_point_line_df <- rbind(
    data.frame(
      x = basket_center_to_baseline,
      y = start_y_outer
    ),
    create_circle(
      center = c(0, 0),
      start = start_angle_outer,
      end = end_angle_outer,
      r = radius_outer
    ),
    data.frame(
      x = c(basket_center_to_baseline, basket_center_to_baseline),
      y = c(-start_y_outer, -start_y_inner)
    ),
    create_circle(
      center = c(0, 0),
      start = start_angle_inner,
      end = end_angle_inner,
      r = radius_inner
    ),
    data.frame(
      x = c(basket_center_to_baseline, basket_center_to_baseline),
      y = c(start_y_inner, start_y_outer)
    )
  )

  return(three_point_line_df)
}

#' Basketball Free Throw Lane (Boundary)
#' 
#' The lines providing the boundary to the free throw lane. When a player is
#' shooting a free throw, all non-shooting players must be outside of this
#' boundary
#'
#' NOTE: This does not include lane space markings (blocks), which will be
#' created via [basketball_lane_space_mark()].
#'
#' @param lane_length The length of the free throw lane
#' @param lane_width The width of the free throw
#' @param line_thickness The thickness of the free throw lane boundary
#'
#' @return A data frame of the bounding coordinates of the free throw lane
#'   boundary
#'
#' @keywords internal
basketball_free_throw_lane_boundary <- function(lane_length = 0,
                                                lane_width = 0,
                                                line_thickness = 0) {
  free_throw_lane_boundary_df <- data.frame(
    x = c(
      0,
      -lane_length,
      -lane_length,
      0,
      0,
      -lane_length + line_thickness,
      -lane_length + line_thickness,
      0,
      0
    ),
    y = c(
      lane_width / 2,
      lane_width / 2,
      -lane_width / 2,
      -lane_width / 2,
      (-lane_width / 2) + line_thickness,
      (-lane_width / 2) + line_thickness,
      (lane_width / 2) - line_thickness,
      (lane_width / 2) - line_thickness,
      lane_width / 2
    )
  )

  return(free_throw_lane_boundary_df)
}

#' Basketball Free Throw Circle (Outline)
#' 
#' The outline of the free throw circle. The interior filling area is created
#' via [basketball_free_throw_circle_fill()]
#'
#' @param overhang The arc length of the free throw circle that hangs past the
#'   free throw line
#' @param free_throw_circle_radius The radius of the free throw circle
#' @param line_thickness The thickness of the free throw line
#'
#' @return A data frame of the bounding coordinates of the free throw circle
#'
#' @keywords internal
basketball_free_throw_circle <- function(overhang = 0,
                                         free_throw_circle_radius = 0,
                                         line_thickness = 0) {
  if (!is.na((overhang / free_throw_circle_radius) / pi)) {
    theta <- (overhang / free_throw_circle_radius) / pi
  } else {
    theta <- 0
  }

  start_angle <- 0.5 - theta
  end_angle <- 1.5 + theta

  free_throw_circle_outline_df <- rbind(
    create_circle(
      center = c(0, 0),
      start = start_angle,
      end = end_angle,
      r = free_throw_circle_radius
    ),
    create_circle(
      center = c(0, 0),
      start = end_angle,
      end = start_angle,
      r = free_throw_circle_radius - line_thickness
    )
  )

  return(free_throw_circle_outline_df)
}

#' Basketball Free Throw Circle (Dashes)
#' 
#' On some courts, there are a series of dashes that comprise the bottom half of
#' the free throw circle (e.g. the half closer to the basket). This function
#' generates a single dash
#'
#' @param feature_radius The radius of the free throw circle
#' @param line_thickness The thickness of the dash
#' @param start_angle The angle, in \code{radians / pi}, at which the dash
#'   should start
#' @param end_angle The angle, in \code{radians / pi}, at which the dash should
#'   end
#'
#' @returns A data frame containing the bounding coordinates of a dash on the
#'   free throw circle
#'
#' @keywords internal
basketball_free_throw_circle_dash <- function(feature_radius = 0,
                                              line_thickness = 0,
                                              start_angle = 0,
                                              end_angle = 0) {
  free_throw_circle_dash_df <- rbind(
    create_circle(
      center = c(0, 0),
      start = start_angle,
      end = end_angle,
      r = feature_radius
    ),

    create_circle(
      center = c(0, 0),
      start = end_angle,
      end = start_angle,
      r = feature_radius - line_thickness
    )
  )

  return(free_throw_circle_dash_df)
}

#' Basketball Lane Space Mark (Blocks)
#' 
#' The lane space marks, also known as the blocks, denote where non-shooting
#' players stand during free throws. Players may not cross these lines before
#' the ball touches the rim on the shot attempt
#'
#' @param feature_thickness The thickness of the lane space mark (this is the
#'   distance in the \code{x} direction in "TV View")
#' @param mark_depth The distance from the exterior edge of the free throw lane
#'   boundary that the lane space mark extends towards the sideline
#'
#' @returns A data frame containing the bounding box of the lane space mark
#'
#' @keywords internal
basketball_lane_space_mark <- function(feature_thickness = 0, mark_depth = 0) {
  lane_space_mark_df <- create_rectangle(
    x_min = -feature_thickness,
    x_max = 0,
    y_min = 0,
    y_max = mark_depth
  )

  return(lane_space_mark_df)
}

#' Basketball Inbounding Line
#' 
#' The inbounding line is where the ball is inbounded on the sideline when
#' necessary. Lines drawn on the top of the court should be drawn in a top-down
#' direction, and lines on the bottom of the court should be drawn in the
#' bottom-up direction
#'
#' @param line_thickness The thickness of the inbounding line
#' @param in_play_ext The extension of the inbounding line into the court
#' @param out_of_bounds_ext The extension of the inbounding line away from the
#'   court
#' @param drawn_direction A string indicating which way, in an un-rotated plot,
#'   the line should be drawn when looking at the plot in TV View
#'
#' @returns A data frame containing the bounding box of the inbounding line
#'
#' @keywords internal
basketball_inbounding_line <- function(line_thickness = 0,
                                       in_play_ext = 0,
                                       out_of_bounds_ext = 0,
                                       drawn_direction = "") {
  if (tolower(drawn_direction) == "top_down") {
    inbounding_line_df <- create_rectangle(
      x_min = -line_thickness,
      x_max = 0,
      y_min = -in_play_ext,
      y_max = out_of_bounds_ext + line_thickness
    )
  } else {
    inbounding_line_df <- create_rectangle(
      x_min = -line_thickness,
      x_max = 0,
      y_min = -(out_of_bounds_ext + line_thickness),
      y_max = in_play_ext
    )
  }

  return(inbounding_line_df)
}

#' Basketball Substitution Line
#' 
#' The substitution line is where players checking into the game wait for a
#' stoppage. Lines drawn on the top of the court should be drawn in a top-down
#' direction, and lines on the bottom of the court should be drawn in the
#' bottom-up direction
#'
#' @param line_thickness The thickness of the substitution line
#' @param substitution_line_width The width of the substitution line, from top
#'   to bottom when viewing the plot in TV view
#' @param drawn_direction A string indicating which way, in an un-rotated plot,
#'   the line should be drawn when looking at the plot in TV View
#'
#' @returns A data frame containing the bounding coordinates of the substitution
#'   line
#'
#' @keywords internal
basketball_substitution_line <- function(line_thickness = 0,
                                         substitution_line_width = 0,
                                         drawn_direction = "") {
  if (tolower(drawn_direction) == "bottom_up") {
    substitution_line_df <- create_rectangle(
      x_min = 0,
      x_max = -line_thickness,
      y_min = 0,
      y_max = substitution_line_width + line_thickness
    )
  } else {
    substitution_line_df <- create_rectangle(
      x_min = 0,
      x_max = -line_thickness,
      y_min = -(substitution_line_width + line_thickness),
      y_max = 0
    )
  }

  return(substitution_line_df)
}

#' Basketball Team Bench Line
#' 
#' Players not in the game must stay within the team bench lines unless moving
#' to the substitution area (see [basketball_substitution_line()] class)
#'
#' @param line_thickness The thickness of the team bench line
#' @param extension The extension of the team bench line out of the court
#' @param drawn_direction A string indicating which way, in an un-rotated plot,
#'   the line should be drawn when looking at the plot in TV View
#'
#' @returns A data frame containing the bounding coordinates of the team bench
#'   line
#'
#' @keywords internal
basketball_team_bench_line <- function(line_thickness = 0,
                                       extension = 0,
                                       drawn_direction = "") {
  if (tolower(drawn_direction) == "bottom_up") {
    team_bench_line_df <- create_rectangle(
      x_min = 0,
      x_max = line_thickness,
      y_min = 0,
      y_max = extension
    )
  } else {
    team_bench_line_df <- create_rectangle(
      x_min = 0,
      x_max = -line_thickness,
      y_min = -extension,
      y_max = 0
    )
  }

  return(team_bench_line_df)
}

#' Basketball Restricted Arc
#' 
#' The arc located in the free throw lane is called the restricted arc. The
#' interior radius should be specified for this feature.
#'
#' @param feature_radius The interior radius of the restricted arc
#' @param line_thickness The thickness of the restricted arc line
#' @param backboard_to_center_of_basket The distance from the backboard to the
#'   center of the basket
#'
#' @returns A data frame containing the bounding coordinates of the restricted
#'   arc
#'
#' @keywords internal
basketball_restricted_arc <- function(feature_radius = 0,
                                      line_thickness = 0,
                                      backboard_to_center_of_basket = 0) {
  restricted_arc_df <- rbind(
    data.frame(
      x = 0,
      y = feature_radius
    ),

    create_circle(
      center = c(-backboard_to_center_of_basket, 0),
      start = 0.5,
      end = 1.5,
      r = feature_radius
    ),

    data.frame(
      x = c(0, 0),
      y = c(-feature_radius, -(feature_radius + line_thickness))
    ),

    create_circle(
      center = c(-backboard_to_center_of_basket, 0),
      start = 1.5,
      end = 0.5,
      r = feature_radius + line_thickness
    ),

    data.frame(
      x = c(0, 0),
      y = c(feature_radius + line_thickness, feature_radius)
    )
  )

  return(restricted_arc_df)
}





# Surface Features -------------------------------------------------------------

#' Basketball Lower Defensive Box Mark
#' 
#' The lower defensive box is an imaginary box on the court extending from the
#' lines on the baseline to the lines inside the painted area. This box helps
#' determine when a block/charge call should take place, as an offensive player
#' is entitled to move outside of (and subsequently enter) this box without
#' contact
#'
#' @param drawn_direction A string indicating which way to draw the lower
#'   defensive box mark
#' @param extension The amount that the lower defensive box mark extends in the
#'   drawn direction
#' @param line_thickness The thickness of the line representing the lower
#'   defensive box
#'
#' @return A data frame of the bounding box of a lower defensive box marking
#'
#' @keywords internal
basketball_lower_defensive_box_mark <- function(drawn_direction = "",
                                                extension = 0,
                                                line_thickness = 0) {
  if (tolower(drawn_direction) == "left_to_right") {
    lower_defensive_box_mark_df <- create_rectangle(
      x_min = -extension,
      x_max = 0,
      y_min = 0,
      y_max = line_thickness
    )
  }

  if (tolower(drawn_direction) == "top_down") {
    lower_defensive_box_mark_df <- create_rectangle(
      x_min = -line_thickness,
      x_max = 0,
      y_min = extension,
      y_max = 0
    )
  }

  return(lower_defensive_box_mark_df)
}

#' Basketball Backboard
#' 
#' The backboard is the backing onto which the basket ring (created by
#' [basketball_basket_ring()]) is affixed. This will be drawn as a
#' rectangle on the court as the court is drawn from an aerial view
#'
#' @param backboard_width The width of the backboard when viewed from above.
#'   This is the x-direction dimension of the backboard when taking the point of
#'   view of a free throw shooter
#' @param backboard_thickness The thickness of the backboard when viewed from
#'   above
#'
#' @return A data frame of the bounding box of the backboard
#'
#' @keywords internal
basketball_backboard <- function(backboard_width = 0, backboard_thickness = 0) {
  backboard_df <- create_rectangle(
    x_min = 0,
    x_max = backboard_thickness,
    y_min = -backboard_width / 2,
    y_max = backboard_width / 2
  )

  return(backboard_df)
}

#' Basketball Basket Ring (Rim)
#' 
#' The hoop through which the ball must pass to score points for a team is
#' called the basket ring
#'
#' An explanation of the math used to generate the basket ring (and its
#' connecting portion that attaches the ring to the backboard) is walked through
#' below using NBA dimensions, but is generalized in the code
#'
#' The connector has a width of 7", so 3.5" are on each side of the x axis. The
#' ring has a radius of 9", so the arcsine of these measurements should give the
#' angle at which point the ring and connector connect
#'
#' @param basket_ring_connector_width The width of the basket ring connector
#' @param backboard_face_to_ring_cent How far off the face of the backboard
#'   the center of the basket ring's circle is located
#' @param basket_ring_inner_radius The inner radius of the circular part of the
#'   basket ring
#' @param basket_ring_thickness The thickness of the basket ring's circular part
#'
#' @return A data frame of the boundary of the basket ring and connector
#'
#' @keywords internal
basketball_basket_ring <- function(basket_ring_connector_width = 0,
                                   backboard_face_to_ring_cent = 0,
                                   basket_ring_inner_radius = 0,
                                   basket_ring_thickness = 0) {
  basket_ring_outer_radius <- basket_ring_inner_radius + basket_ring_thickness
  if (
    is.na(asin((basket_ring_connector_width / 2) / basket_ring_outer_radius))
  ) {
    start_angle <- 0
  } else {
    start_angle <- asin(
      (basket_ring_connector_width / 2) / basket_ring_outer_radius
    ) / pi
  }

  end_angle <- 2 - start_angle

  basket_ring_df <- rbind(
    data.frame(
      x = c(
        0,
        -backboard_face_to_ring_cent +
          (basket_ring_outer_radius * cos(start_angle * pi))
      ),
      y = c(
        basket_ring_connector_width / 2,
        basket_ring_connector_width / 2
      )
    ),
    create_circle(
      center = c(-backboard_face_to_ring_cent, 0),
      start = start_angle,
      end = end_angle,
      r = basket_ring_outer_radius
    ),
    data.frame(
      x = c(
        -backboard_face_to_ring_cent +
          (basket_ring_outer_radius * cos(start_angle * pi)),
        0,
        0
      ),
      y = c(
        -basket_ring_connector_width / 2,
        -basket_ring_connector_width / 2,
        basket_ring_connector_width / 2
      )
    )
  )

  return(basket_ring_df)
}

#' Basketball Net
#' 
#' To make the basket ring easier to identify, the nets will also be drawn onto
#' the plot. They will typically be white in color
#'
#' @param basket_ring_inner_radius The radius of the interior of the basket ring
#'
#' @return A data frame of the net's circular outline
#'
#' @keywords internal
basketball_net <- function(basket_ring_inner_radius = 0) {
  net_df <- create_circle(
    center = c(0, 0),
    start = 0,
    end = 2,
    r = basket_ring_inner_radius
  )

  return(net_df)
}
