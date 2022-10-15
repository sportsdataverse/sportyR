# Surface Base Features --------------------------------------------------------

#' Each half of the football field spans from the back edge of the endzone to
#' the center of the major yard line at midfield
#'
#' @param field_length The length of the field
#' @param field_width The width of the field
#' @param endzone_length The length of the endzone
#'
#' @return A data frame of the bounding box of half a football field
#'
#' @keywords internal
football_half_field <- function(field_length = 0,
                                field_width = 0,
                                endzone_length = 0) {
  half_field_df <- create_rectangle(
    # Using quarter-field lengths to account for feature positioning adjustment
    x_min = -((field_length / 4) + (endzone_length / 2)),
    x_max = (field_length / 4) + (endzone_length / 2),
    y_min = -field_width / 2,
    y_max = field_width / 2
  )

  return(half_field_df)
}

#' The endzones are the area beyond the goal line. Any offensive player who is
#' in legal possession of the ball while in the endzone, or who catches the ball
#' in the endzone, scores a touchdown for their team. This area stretches from
#' the back field boundary to the edge of the goal line closest to the center of
#' the field
#'
#' @param endzone_length The length of the endzone
#' @param field_width The width of the field
#'
#' @return A data frame of the bounding box of the endzone
#'
#' @keywords internal
football_endzone <- function(field_width = 0, endzone_length = 0) {
  endzone_df <- create_rectangle(
    x_min = -endzone_length / 2,
    x_max = endzone_length / 2,
    y_min = -field_width / 2,
    y_max = field_width / 2
  )

  return(endzone_df)
}

#' The field should have an apron to appropriately see all out-of-bounds
#' features. This is typically the same color as the field itself, but will be
#' created separately so as to allow for more customized plotting
#'
#' @param field_length The length of the field
#' @param field_width The width of the field
#' @param endzone_length The length of the endzone
#' @param boundary_thickness The thickness of the field boundary
#' @param field_border_thickness The thickness of the field border
#' @param restricted_area_length The length of the restricted area
#' @param restricted_area_width The width of the restricted area
#' @param coaching_box_length The length of the coaching box
#' @param coaching_box_width The width of the coaching box
#' @param team_bench_length_field_side The length of the team bench area nearest
#'   the field
#' @param team_bench_length_back_side The length of the team bench area furthest
#'   from the field
#' @param team_bench_width The width of the team bench area
#' @param team_bench_area_border_thickness The thickness of the border around
#'   the team bench area
#' @param extra_apron_padding Any additional distance to add to the apron of the
#'   field
#'
#' @return A data frame of the bounding coordinates of the field apron
#'
#' @keywords internal
football_field_apron <- function(field_length = 0,
                                 field_width = 0,
                                 endzone_length = 0,
                                 boundary_thickness = 0,
                                 field_border_thickness = 0,
                                 restricted_area_length = 0,
                                 restricted_area_width = 0,
                                 coaching_box_length = 0,
                                 coaching_box_width = 0,
                                 team_bench_length_field_side = 0,
                                 team_bench_length_back_side = 0,
                                 team_bench_width = 0,
                                 team_bench_area_border_thickness = 0,
                                 extra_apron_padding = 0) {
  # Define the extreme values of x and y
  ext_x <- (field_length / 2) +
    endzone_length +
    boundary_thickness +
    field_border_thickness +
    extra_apron_padding

  ext_y <- (field_width / 2) +
    boundary_thickness +
    field_border_thickness +
    restricted_area_width +
    coaching_box_width +
    team_bench_width +
    extra_apron_padding

  field_apron_df <- create_rectangle(
    x_min = -ext_x,
    x_max = ext_x,
    y_min = -ext_y,
    y_max = ext_y
  )

  return(field_apron_df)
}




# Surface Boundaries -----------------------------------------------------------

#' The end line is the line beyond the back of the endzone. Its interior edge is
#' considered out of bounds
#'
#' @param feature_thickness The thickness of the boundary lines
#' @param field_width The width of the field
#'
#' @return A data frame of the bounding box of the end line
#'
#' @keywords internal
football_end_line <- function(feature_thickness = 0, field_width = 0) {
  end_line <- create_rectangle(
    x_min = 0,
    x_max = feature_thickness,
    y_min = -((field_width / 2) + feature_thickness),
    y_max = (field_width / 2) + feature_thickness
  )

  return(end_line)
}

#' The sidelines are the lines that run the length of the field, stretching from
#' the back of one endzone to the back of the other endzone. Its interior edge
#' is considered out of bounds
#'
#' @param feature_thickness The thickness of the boundary lines
#' @param field_length The length of the field
#' @param endzone_length The length of the endzone
#'
#' @return A data frame of the bounding box of the sidelines
#'
#' @keywords internal
football_sideline <- function(feature_thickness = 0,
                              field_length = 0,
                              endzone_length = 0) {
  sideline <- create_rectangle(
    x_min = -((field_length / 2) + endzone_length),
    x_max = (field_length / 2) + endzone_length,
    y_min = 0,
    y_max = feature_thickness
  )

  return(sideline)
}

#' The field border is the border line around the outer edge of the sideline and
#' end line. They may not be present on every field, but this is not the same as
#' the sideline or end line (although they may be the same color)
#'
#' @param field_length The length of the field
#' @param field_width The width of the field
#' @param feature_thickness The thickness of the field border
#' @param endzone_length The length of the endzone
#' @param boundary_thickness The thickness of the boundary lines
#' @param restricted_area_length The length of the restricted area
#' @param restricted_area_width The width of the restricted area
#' @param coaching_box_length The length of the coaching box
#' @param coaching_box_width The width of the coaching box
#' @param team_bench_length_field_side The length of the side of the team bench
#'   closest to the field
#' @param team_bench_length_back_side The length of the side of the team bench
#'   furthest from the field
#' @param team_bench_width The width of the team bench
#' @param team_bench_area_border_thickness The thickness of the border around
#'   the team bench
#' @param surrounds_team_bench_area A boolean of whether or not the field border
#'   should surround the team bench
#' @param bench_shape A string of the shape of the bench. Currently, this checks
#'   for \code{"rectangle"}
#'
#' @return A data frame of the bounding box of the field border
#'
#' @keywords internal
football_field_border <- function(field_length = 0,
                                  field_width = 0,
                                  feature_thickness = 0,
                                  endzone_length = 0,
                                  boundary_line_thickness = 0,
                                  restricted_area_length = 0,
                                  restricted_area_width = 0,
                                  coaching_box_length = 0,
                                  coaching_box_width = 0,
                                  team_bench_length_field_side = 0,
                                  team_bench_length_back_side = 0,
                                  team_bench_width = 0,
                                  team_bench_area_border_thickness = 0,
                                  surrounds_team_bench_area = FALSE,
                                  bench_shape = "") {
  if (!surrounds_team_bench_area) {
    field_border_df <- data.frame(
      x = c(
        (restricted_area_length / 2) + team_bench_area_border_thickness,
        (field_length / 2) + endzone_length + boundary_line_thickness,
        (field_length / 2) + endzone_length + boundary_line_thickness,
        (restricted_area_length / 2) + team_bench_area_border_thickness,
        (restricted_area_length / 2) + team_bench_area_border_thickness,
        (
          (field_length / 2) +
            endzone_length +
            boundary_line_thickness +
            feature_thickness
        ),
        (
          (field_length / 2) +
            endzone_length +
            boundary_line_thickness +
            feature_thickness
        ),
        (restricted_area_length / 2) + team_bench_area_border_thickness,
        (restricted_area_length / 2) + team_bench_area_border_thickness
      ),
      y = c(
        (field_width / 2) + boundary_line_thickness,
        (field_width / 2) + boundary_line_thickness,
        -((field_width / 2) + boundary_line_thickness),
        -((field_width / 2) + boundary_line_thickness),
        -((field_width / 2) + boundary_line_thickness + feature_thickness),
        -((field_width / 2) + boundary_line_thickness + feature_thickness),
        (field_width / 2) + boundary_line_thickness + feature_thickness,
        (field_width / 2) + boundary_line_thickness + feature_thickness,
        (field_width / 2) + boundary_line_thickness
      )
    )
  } else {
    starting_depth <- (field_width / 2) +
      boundary_line_thickness +
      restricted_area_width +
      coaching_box_width +
      team_bench_width +
      team_bench_area_border_thickness

    if (tolower(bench_shape) %in% c("rectangle", "rectangular")) {
      m <- team_bench_width / (
        (team_bench_length_back_side / 2) - (team_bench_length_field_side / 2)
      )

      y2 <- starting_depth + feature_thickness
      y1 <- starting_depth - team_bench_width - team_bench_area_border_thickness

      x1 <- (team_bench_length_field_side / 2) +
        team_bench_area_border_thickness +
        feature_thickness

      outer_corner_x_dist <- (((y2 - y1) / m) + x1)
    } else {
      outer_corner_x_dist <- (team_bench_length_back_side / 2) +
        team_bench_area_border_thickness +
        (feature_thickness / 2)
    }

    field_border_df <- data.frame(
      x = c(
        0,
        (team_bench_length_back_side / 2) + team_bench_area_border_thickness,
        (team_bench_length_field_side / 2) + team_bench_area_border_thickness,
        (coaching_box_length / 2) + team_bench_area_border_thickness,
        (restricted_area_length / 2) + team_bench_area_border_thickness,
        (field_length / 2) + endzone_length + boundary_line_thickness,
        (field_length / 2) + endzone_length + boundary_line_thickness,
        (restricted_area_length / 2) + team_bench_area_border_thickness,
        (coaching_box_length / 2) + team_bench_area_border_thickness,
        (team_bench_length_field_side / 2) + team_bench_area_border_thickness,
        (team_bench_length_back_side / 2) + team_bench_area_border_thickness,
        0,
        0,
        outer_corner_x_dist,
        (team_bench_length_field_side / 2) +
          team_bench_area_border_thickness +
          feature_thickness,
        (coaching_box_length / 2) +
          team_bench_area_border_thickness +
          feature_thickness,
        (restricted_area_length / 2) +
          team_bench_area_border_thickness +
          feature_thickness,
        (field_length / 2) +
          endzone_length +
          boundary_line_thickness +
          feature_thickness,
        (field_length / 2) +
          endzone_length +
          boundary_line_thickness +
          feature_thickness,
        (restricted_area_length / 2) +
          team_bench_area_border_thickness +
          feature_thickness,
        (coaching_box_length / 2) +
          team_bench_area_border_thickness +
          feature_thickness,
        (team_bench_length_field_side / 2) +
          team_bench_area_border_thickness +
          feature_thickness,
        outer_corner_x_dist,
        0,
        0
      ),
      y = c(
        starting_depth,
        starting_depth,
        starting_depth - team_bench_width - team_bench_area_border_thickness,
        starting_depth - team_bench_width - team_bench_area_border_thickness,
        (field_width / 2) + boundary_line_thickness,
        (field_width / 2) + boundary_line_thickness,
        -(field_width / 2) - boundary_line_thickness,
        -(field_width / 2) - boundary_line_thickness,
        -starting_depth + team_bench_width + team_bench_area_border_thickness,
        -starting_depth + team_bench_width + team_bench_area_border_thickness,
        -starting_depth,
        -starting_depth,
        -starting_depth - feature_thickness,
        -starting_depth - feature_thickness,
        -starting_depth + team_bench_width + team_bench_area_border_thickness,
        -starting_depth +
          team_bench_width +
          team_bench_area_border_thickness +
          coaching_box_width,
        -(field_width / 2) -
          boundary_line_thickness -
          feature_thickness,
        -(field_width / 2) -
          boundary_line_thickness -
          feature_thickness,
        (field_width / 2) +
          boundary_line_thickness +
          feature_thickness,
        (field_width / 2) +
          boundary_line_thickness +
          feature_thickness,
        starting_depth -
          team_bench_width -
          team_bench_area_border_thickness -
          coaching_box_width,
        starting_depth -
          team_bench_width -
          team_bench_area_border_thickness,
        starting_depth + feature_thickness,
        starting_depth + feature_thickness,
        starting_depth
      )
    )

    return(field_border_df)
  }
}

#' The field border's outline is the outline around the outer edge of the field
#' border. They may not be present on every field, but this is not the same as
#' the sideline or end line (although they may be the same color)
#'
#' @param field_length The length of the field
#' @param field_width The width of the field
#' @param feature_thickness The thickness of the field border's outline
#' @param endzone_length The length of the endzone
#' @param boundary_thickness The thickness of the boundary lines
#' @param restricted_area_length The length of the restricted area
#' @param restricted_area_width The width of the restricted area
#' @param coaching_box_length The length of the coaching box
#' @param coaching_box_width The width of the coaching box
#' @param team_bench_length_field_side The length of the side of the team bench
#'   closest to the field
#' @param team_bench_length_back_side The length of the side of the team bench
#'   furthest from the field
#' @param team_bench_width The width of the team bench
#' @param team_bench_area_border_thickness The thickness of the border around
#'   the team bench
#' @param field_border_thickness The thickness of the field border
#' @param surrounds_team_bench_area A boolean of whether or not the field border
#'   should surround the team bench
#' @param bench_shape A string of the shape of the bench. Currently, this checks
#'   for \code{"rectangle"}
#'
#' @return A data frame of the bounding box of the field border's outline
#'
#' @keywords internal
football_field_border_outline <- function(field_length = 0,
                                          field_width = 0,
                                          feature_thickness = 0,
                                          endzone_length = 0,
                                          boundary_line_thickness = 0,
                                          restricted_area_length = 0,
                                          restricted_area_width = 0,
                                          coaching_box_length = 0,
                                          coaching_box_width = 0,
                                          team_bench_length_field_side = 0,
                                          team_bench_length_back_side = 0,
                                          team_bench_width = 0,
                                          team_bench_area_border_thickness = 0,
                                          field_border_thickness = 0,
                                          surrounds_team_bench_area = TRUE,
                                          bench_shape = "") {
  if (!surrounds_team_bench_area) {
    field_border_outline_df <- data.frame(
      x = c(
        (restricted_area_length / 2) + team_bench_area_border_thickness,
        (field_length / 2) +
          endzone_length +
          boundary_line_thickness +
          field_border_thickness,
        (field_length / 2) +
          endzone_length +
          boundary_line_thickness +
          field_border_thickness,
        (restricted_area_length / 2) + team_bench_area_border_thickness,
        (restricted_area_length / 2) + team_bench_area_border_thickness,
        (field_length / 2) +
          endzone_length +
          boundary_line_thickness +
          field_border_thickness +
          feature_thickness,
        (field_length / 2) +
          endzone_length +
          boundary_line_thickness +
          field_border_thickness +
          feature_thickness,
        (restricted_area_length / 2) + team_bench_area_border_thickness,
        (restricted_area_length / 2) + team_bench_area_border_thickness
      ),
      y = c(
        (field_width / 2) + boundary_line_thickness + field_border_thickness,
        (field_width / 2) + boundary_line_thickness + field_border_thickness,
        -((field_width / 2) + boundary_line_thickness + field_border_thickness),
        -((field_width / 2) + boundary_line_thickness + field_border_thickness),
        -(
          (field_width / 2) +
            boundary_line_thickness +
            field_border_thickness +
            feature_thickness
        ),
        -(
          (field_width / 2) +
            boundary_line_thickness +
            field_border_thickness +
            feature_thickness
        ),
        (field_width / 2) +
          boundary_line_thickness +
          field_border_thickness +
          feature_thickness,
        (field_width / 2) +
          boundary_line_thickness +
          field_border_thickness +
          feature_thickness,
        (field_width / 2) + boundary_line_thickness + field_border_thickness
      )
    )

    return(field_border_outline_df)
  } else {
    starting_depth <- (field_width / 2) +
      boundary_line_thickness +
      restricted_area_width +
      coaching_box_width +
      team_bench_width +
      team_bench_area_border_thickness +
      field_border_thickness

    if (tolower(bench_shape) %in% c("rectangle", "rectangular")) {
      m <- team_bench_width / (
        (team_bench_length_back_side / 2) - (team_bench_length_field_side / 2)
      )

      y2 <- starting_depth + field_border_thickness
      y1 <- starting_depth - team_bench_width - team_bench_area_border_thickness
      x1 <- (team_bench_length_field_side / 2) +
        team_bench_area_border_thickness +
        field_border_thickness

      outer_corner_x_dist <- (((y2 - y1) / m) + x1)
    } else {
      outer_corner_x_dist <- (team_bench_length_back_side / 2) +
        team_bench_area_border_thickness +
        (field_border_thickness / 2)
    }

    field_border_outline_df <- data.frame(
      x = c(
        # Start
        0,

        # Short edge of bench (top)
        outer_corner_x_dist,

        # Long edge of bench (top)
        (team_bench_length_field_side / 2) +
          team_bench_area_border_thickness +
          field_border_thickness,

        # Coaching box (top)
        (coaching_box_length / 2) +
          team_bench_area_border_thickness +
          field_border_thickness,

        # Restricted area (top)
        (restricted_area_length / 2) +
          team_bench_area_border_thickness +
          field_border_thickness,

        # Edge of field (top)
        (field_length / 2) +
          endzone_length +
          boundary_line_thickness +
          field_border_thickness,

        # Edge of field (bottom)
        (field_length / 2) +
          endzone_length +
          boundary_line_thickness +
          field_border_thickness,

        # Restricted area (bottom)
        (restricted_area_length / 2) +
          team_bench_area_border_thickness +
          field_border_thickness,

        # Coaching box (bottom)
        (coaching_box_length / 2) +
          team_bench_area_border_thickness +
          field_border_thickness,

        # Long edge of bench (bottom)
        (team_bench_length_field_side / 2) +
          team_bench_area_border_thickness +
          field_border_thickness,

        # Short edge of bench (bottom)
        outer_corner_x_dist,

        # Zero
        0,

        # Outward
        0,

        # Short edge of bench (bottom)
        outer_corner_x_dist + feature_thickness,

        # Long edge of bench (bottom)
        (team_bench_length_field_side / 2) +
          team_bench_area_border_thickness +
          field_border_thickness +
          feature_thickness,

        # Coaching box (bottom)
        (coaching_box_length / 2) +
          team_bench_area_border_thickness +
          field_border_thickness +
          feature_thickness,

        # Restricted area (bottom)
        (restricted_area_length / 2) +
          team_bench_area_border_thickness +
          field_border_thickness +
          feature_thickness,

        # Edge of field (bottom)
        (field_length / 2) +
          boundary_line_thickness +
          endzone_length +
          field_border_thickness +
          feature_thickness,

        # Edge of field (top)
        (field_length / 2) +
          boundary_line_thickness +
          endzone_length +
          field_border_thickness +
          feature_thickness,

        # Restricted area (top)
        (restricted_area_length / 2) +
          team_bench_area_border_thickness +
          field_border_thickness +
          feature_thickness,

        # Coaching box (top)
        (coaching_box_length / 2) +
          team_bench_area_border_thickness +
          field_border_thickness +
          feature_thickness,

        # Long edge of bench (top)
        (team_bench_length_field_side / 2) +
          team_bench_area_border_thickness +
          field_border_thickness +
          feature_thickness,

        # Short edge of bench (top)
        outer_corner_x_dist + feature_thickness,

        # Zero
        0,

        # End
        0
      ),
      y = c(
        # Start
        (
          (field_width / 2) +
            boundary_line_thickness +
            restricted_area_width +
            coaching_box_width +
            team_bench_width +
            team_bench_area_border_thickness +
            field_border_thickness
        ),
        # Short edge of bench (top)
        (
          (field_width / 2) +
            boundary_line_thickness +
            restricted_area_width +
            coaching_box_width +
            team_bench_width +
            team_bench_area_border_thickness +
            field_border_thickness
        ),
        # Long edge of bench (top)
        (
          (field_width / 2) +
            boundary_line_thickness +
            restricted_area_width +
            coaching_box_width
        ),
        # Coaching box (top)
        (
          (field_width / 2) +
            boundary_line_thickness +
            restricted_area_width
        ),
        # Restricted area (top)
        (
          (field_width / 2) +
            boundary_line_thickness +
            field_border_thickness
        ),
        # Edge of field (top)
        (
          (field_width / 2) +
            boundary_line_thickness +
            field_border_thickness
        ),
        # Edge of field (bottom)
        -(
          (field_width / 2) +
            boundary_line_thickness +
            field_border_thickness
        ),
        # Restricted area (bottom)
        -(
          (field_width / 2) +
            boundary_line_thickness +
            field_border_thickness
        ),
        # Coaching box (bottom)
        -(
          (field_width / 2) +
            boundary_line_thickness +
            restricted_area_width
        ),
        # Long edge of bench (bottom)
        -(
          (field_width / 2) +
            boundary_line_thickness +
            restricted_area_width +
            coaching_box_width
        ),
        # Short edge of bench (bottom)
        -(
          (field_width / 2) +
            boundary_line_thickness +
            restricted_area_width +
            coaching_box_width +
            team_bench_width +
            team_bench_area_border_thickness +
            field_border_thickness
        ),
        # Zero
        -(
          (field_width / 2) +
            boundary_line_thickness +
            restricted_area_width +
            coaching_box_width +
            team_bench_width +
            team_bench_area_border_thickness +
            field_border_thickness
        ),
        # Outward
        -(
          (field_width / 2) +
            boundary_line_thickness +
            restricted_area_width +
            coaching_box_width +
            team_bench_width +
            team_bench_area_border_thickness +
            field_border_thickness +
            feature_thickness
        ),
        # Short edge of bench (bottom)
        -(
          (field_width / 2) +
            boundary_line_thickness +
            restricted_area_width +
            coaching_box_width +
            team_bench_width +
            team_bench_area_border_thickness +
            field_border_thickness +
            feature_thickness
        ),
        # Long edge of bench (bottom)
        -(
          (field_width / 2) +
            boundary_line_thickness +
            restricted_area_width +
            coaching_box_width
        ),
        # Coaching box (bottom)
        -(
          (field_width / 2) +
            boundary_line_thickness +
            restricted_area_width
        ),
        # Restricted area (bottom)
        -(
          (field_width / 2) +
            boundary_line_thickness +
            field_border_thickness +
            feature_thickness
        ),
        # Edge of field (bottom)
        -(
          (field_width / 2) +
            boundary_line_thickness +
            field_border_thickness +
            feature_thickness
        ),
        # Edge of field (top)
        (
          (field_width / 2) +
            boundary_line_thickness +
            field_border_thickness +
            feature_thickness
        ),
        # Restricted area (top)
        (
          (field_width / 2) +
            boundary_line_thickness +
            field_border_thickness +
            feature_thickness
        ),
        # Coaching box (top)
        (
          (field_width / 2) +
            boundary_line_thickness +
            restricted_area_width
        ),
        # Long edge of bench (top)
        (
          (field_width / 2) +
            boundary_line_thickness +
            restricted_area_width +
            coaching_box_width
        ),
        # Short edge of bench (top)
        (
          (field_width / 2) +
            boundary_line_thickness +
            restricted_area_width +
            coaching_box_width +
            team_bench_width +
            team_bench_area_border_thickness +
            field_border_thickness +
            feature_thickness
        ),
        # Zero
        (
          (field_width / 2) +
            boundary_line_thickness +
            restricted_area_width +
            coaching_box_width +
            team_bench_width +
            team_bench_area_border_thickness +
            field_border_thickness +
            feature_thickness
        ),
        # End
        (
          (field_width / 2) +
            boundary_line_thickness +
            restricted_area_width +
            coaching_box_width +
            team_bench_width +
            team_bench_area_border_thickness +
            field_border_thickness
        )
      )
    )
  }

  return(field_border_outline_df)
}

#' The field border (see [football_field_border()]) may have a different color
#' along the red zone than it does along the rest of the field. This is not
#' always the case, but the feature is provided for convenience
#'
#' @param feature_thickness The thickness of the field border
#'
#' @return A data frame of the bounding box of the red zone border
#'
#' @keywords internal
football_red_zone_border <- function(feature_thickness = 0) {
  red_zone_border_df <- create_rectangle(
    x_min = 0,
    x_max = 20,
    y_min = 0,
    y_max = feature_thickness
  )

  return(red_zone_border_df)
}

#' The outline of the [football_red_zone_border()] may be a different color than
#' the rest of the field border. This is not always the case, but the feature is
#' provided for convenience
#'
#' @param feature_thickness The thickness of the field border's outline
#'
#' @return A data frame of the bounding box of the red zone border's outline
#'
#' @keywords internal
football_red_zone_border_outline <- function(feature_thickness = 0) {
  red_zone_border_outline_df <- create_rectangle(
    x_min = 0,
    x_max = 20,
    y_min = 0,
    y_max = feature_thickness
  )

  return(red_zone_border_outline_df)
}





# Surface Lines ----------------------------------------------------------------

#' The goal lines are the lines the ball must cross while being either passed or
#' ran in order to score a touchdown. The interior edge of the goal line
#' (relative to the center of the field of play) should lie at the 0 yard line,
#' and the center of the 1 yard line should be exactly 1 yard from this edge of
#' the goal line
#'
#' @param field_width The width of the field
#' @param feature_thickness The thickness of the goal line
#'
#' @return A data frame of the bounding box of the goal line
#'
#' @keywords internal
football_goal_line <- function(field_width = 0, feature_thickness = 0) {
  goal_line_df <- create_rectangle(
    x_min = 0,
    x_max = feature_thickness,
    y_min = -field_width / 2,
    y_max = field_width / 2
  )

  return(goal_line_df)
}

#' The major yard lines are the yard lines that span the entire width of the
#' field. Typically, these lines are placed every 5 yards, but the customization
#' is left to the user. These lines may feature a cross-hash, which runs in the
#' x-direction
#'
#' @param field_width The width of the field
#' @param feature_thickness The thickness of each of the major yard lines
#' @param dist_to_sideline The distance from the end of the yard line to the
#'   interior edge of the sideline
#' @param cross_hash_length The length of each cross-hash mark
#' @param cross_hash_separation The interior separation between the cross-hashes
#'
#' @return A data frame containing the bounding box of the major yard lines
#'
#' @keywords internal
football_major_yard_line <- function(field_width = 0,
                                     feature_thickness = 0,
                                     dist_to_sideline = 0,
                                     cross_hash_length = 0,
                                     cross_hash_separation = 0) {
  major_yard_line_df <- data.frame(
    x = c(
      -feature_thickness / 2,
      -feature_thickness / 2,
      -((feature_thickness / 2) + cross_hash_length),
      -((feature_thickness / 2) + cross_hash_length),
      -feature_thickness / 2,
      -feature_thickness / 2,
      -((feature_thickness / 2) + cross_hash_length),
      -((feature_thickness / 2) + cross_hash_length),
      -feature_thickness / 2,
      -feature_thickness / 2,
      feature_thickness / 2,
      feature_thickness / 2,
      (feature_thickness / 2) + cross_hash_length,
      (feature_thickness / 2) + cross_hash_length,
      feature_thickness / 2,
      feature_thickness / 2,
      (feature_thickness / 2) + cross_hash_length,
      (feature_thickness / 2) + cross_hash_length,
      feature_thickness / 2,
      feature_thickness / 2,
      -feature_thickness / 2
    ),
    y = c(
      -((field_width / 2) - dist_to_sideline),
      -((cross_hash_separation / 2) + feature_thickness),
      -((cross_hash_separation / 2) + feature_thickness),
      -cross_hash_separation / 2,
      -cross_hash_separation / 2,
      cross_hash_separation / 2,
      cross_hash_separation / 2,
      (cross_hash_separation / 2) + feature_thickness,
      (cross_hash_separation / 2) + feature_thickness,
      (field_width / 2) - dist_to_sideline,
      (field_width / 2) - dist_to_sideline,
      (cross_hash_separation / 2) + feature_thickness,
      (cross_hash_separation / 2) + feature_thickness,
      cross_hash_separation / 2,
      cross_hash_separation / 2,
      -cross_hash_separation / 2,
      -cross_hash_separation / 2,
      -((cross_hash_separation / 2) + feature_thickness),
      -((cross_hash_separation / 2) + feature_thickness),
      -((field_width / 2) - dist_to_sideline),
      -((field_width / 2) - dist_to_sideline)
    )
  )

  return(major_yard_line_df)
}

#' The minor yard lines are the yard lines in between all of the major yard
#' lines. Typically, there are four sets of minor yard lines: one near each
#' sideline, and two near the middle of the field
#'
#' @param yard_line_height The height (in the y-direction) of each yard line
#' @param feature_thickness The thickness of each minor yard line
#'
#' @return A data frame of the bounding box of a minor yard line
#'
#' @keywords internal
football_minor_yard_line <- function(yard_line_height = 0,
                                     feature_thickness = 0) {
  minor_yard_line_df <- create_rectangle(
    x_min = -feature_thickness / 2,
    x_max = feature_thickness / 2,
    y_min = 0,
    y_max = yard_line_height
  )

  return(minor_yard_line_df)
}

#' The try mark is the mark from which all tries start. This line is located
#' directly on the line \code{y = 0}. This line is not typically considered an
#' official yard line, which is why it is created independently
#'
#' @param try_mark_width The width (in the y-direction) of the try mark
#' @param feature_thickness The thickness of the try mark
#'
#' @return A data frame of the bounding box of the try mark
#'
#' @keywords internal
football_try_mark <- function(try_mark_width = 0, feature_thickness = 0) {
  try_mark_df <- create_rectangle(
    x_min = -feature_thickness / 2,
    x_max = feature_thickness / 2,
    y_min = -try_mark_width / 2,
    y_max = try_mark_width / 2
  )

  return(try_mark_df)
}

#' The coaching box line is the line that separates the team bench area from the
#' coaching box. This line should be a different color than the bench area and
#' coaching box, which may be the same color
#'
#' @param coaching_box_line_length The length of the line forming the coaching
#'   box line
#' @param feature_thickness The thickness with which to draw the line
#'
#' @return A data frame of the bounding box of the coaching box line
#'
#' @keywords internal
football_coaching_box_line <- function(coaching_box_line_length = 0,
                                       feature_thickness = 0) {
  coaching_box_line_df <- create_rectangle(
    x_min = -coaching_box_line_length / 2,
    x_max = coaching_box_line_length / 2,
    y_min = 0,
    y_max = feature_thickness
  )

  return(coaching_box_line_df)
}





# Surface Features -------------------------------------------------------------

#' The directional arrows point towards the nearest goal line from the yardage
#' marker they are closest to. These arrows are described by their base (which
#' runs parallel to the goal line) and their length, which extends from the tip
#' to the base
#'
#' @param arrow_base The length of the base of the arrow
#' @param arrow_length The length of the arrow from tip to base
#'
#' @return A data frame of the bounding coordinates of the directional arrow
#'
#' @keywords internal
football_directional_arrow <- function(arrow_base = 0, arrow_length = 0) {
  arrow_df <- data.frame(
    x = c(
      0,
      arrow_length,
      0,
      0
    ),
    y = c(
      arrow_base / 2,
      0,
      -arrow_base / 2,
      arrow_base / 2
    )
  )

  return(arrow_df)
}

#' The restricted area is the area nearest the sideline's exterior edge. This
#' area is distinct from the coaching box (immediately behind the restricted
#' area) and team bench area
#'
#' @param restricted_area_length The length of the restricted area
#' @param feature_thickness The depth beyond the exterior edge of the sideline
#'   that the restricted area protrudes
#'
#' @return A data frame of the bounding coordinates of the restricted area
#'
#' @keywords internal
football_restricted_area <- function(restricted_area_length = 0,
                                     feature_thickness = 0) {
  restricted_area_df <- create_rectangle(
    x_min = -restricted_area_length / 2,
    x_max = restricted_area_length / 2,
    y_min = 0,
    y_max = feature_thickness
  )

  return(restricted_area_df)
}

#' The coaching box is the area between the restricted area and team bench area.
#' It may or may not be distinct from either of these areas, but is typically
#' separated by the coaching box line (see [football_coaching_box_line()] for
#' more information on the coaching box line)
#'
#' @param coaching_box_length The length of the coaching box
#' @param feature_thickness The depth beyond the exterior edge of the restricted
#'   area that the coaching box protrudes
#'
#' @return A data frame of the bounding box of coaching box
#'
#' @keywords internal
football_coaching_box <- function(coaching_box_length = 0,
                                  feature_thickness = 0) {
  coaching_box_df <- create_rectangle(
    x_min = -coaching_box_length / 2,
    x_max = coaching_box_length / 2,
    y_min = 0,
    y_max = feature_thickness
  )

  return(coaching_box_df)
}

#' The team bench area is the area beyond the restricted area and coaching box.
#' It is where the team benches, non-playing players, and team staff are to
#' remain during the game
#'
#' @param team_bench_length_field_side The length of the side of the team bench
#'   area closest to the field
#' @param team_bench_length_back_side The length of the side of the team bench
#'   area furthest from the field
#' @param team_bench_width The depth beyond the outer edge of the coaching box
#'   line that the team bench area protrudes
#'
#' @return A data frame of the bounding coordinates of the team bench area
#'
#' @keywords internal
football_team_bench_area <- function(team_bench_length_field_side = 0,
                                     team_bench_length_back_side = 0,
                                     team_bench_width = 0) {
  team_bench_area_df <- data.frame(
    x = c(
      -team_bench_length_field_side / 2,
      team_bench_length_field_side / 2,
      team_bench_length_back_side / 2,
      -team_bench_length_back_side / 2,
      -team_bench_length_field_side / 2
    ),
    y = c(
      0,
      0,
      team_bench_width,
      team_bench_width,
      0
    )
  )

  return(team_bench_area_df)
}

#' The outline of the team bench area runs beyond the team bench, but is inside
#' of any field border that may run behind the team bench area (see
#' [football_field_border()] for more information on this feature)
#'
#' @param restricted_area_length The length of the restricted area
#' @param restricted_area_width The width of the restricted area
#' @param coaching_box_length The length of the coaching box
#' @param coaching_box_width The width of the coaching box
#' @param team_bench_length_field_side The length of the side of the team bench
#'   closest to the field
#' @param team_bench_length_back_side The length of the side of the team bench
#'   furthest from the field
#' @param team_bench_width The width of the team bench
#' @param feature_thickness The thickness of the outline of the team bench area
#'
#' @return A data frame containing the bounding coordinates of the team bench
#'   area's outline
#'
#' @keywords internal
football_team_bench_area_outline <- function(restricted_area_length = 0,
                                             restricted_area_width = 0,
                                             coaching_box_length = 0,
                                             coaching_box_width = 0,
                                             team_bench_length_field_side = 0,
                                             team_bench_length_back_side = 0,
                                             team_bench_width = 0,
                                             feature_thickness = 0) {
  team_bench_area_outline_df <- data.frame(
    x = c(
      -restricted_area_length / 2,
      -coaching_box_length / 2,
      -team_bench_length_field_side / 2,
      -team_bench_length_back_side / 2,
      team_bench_length_back_side / 2,
      team_bench_length_field_side / 2,
      coaching_box_length / 2,
      restricted_area_length / 2,
      (restricted_area_length / 2) + feature_thickness,
      (coaching_box_length / 2) + feature_thickness,
      (team_bench_length_field_side / 2) + feature_thickness,
      (team_bench_length_back_side / 2) + feature_thickness,
      -((team_bench_length_back_side / 2) + feature_thickness),
      -((team_bench_length_field_side / 2) + feature_thickness),
      -((coaching_box_length / 2) + feature_thickness),
      -((restricted_area_length / 2) + feature_thickness),
      -restricted_area_length / 2
    ),
    y = c(
      0,
      restricted_area_width,
      restricted_area_width + coaching_box_width,
      restricted_area_width + coaching_box_width + team_bench_width,
      restricted_area_width + coaching_box_width + team_bench_width,
      restricted_area_width + coaching_box_width,
      restricted_area_width,
      0,
      0,
      restricted_area_width,
      restricted_area_width + coaching_box_width,
      (
        restricted_area_width +
          coaching_box_width +
          team_bench_width +
          feature_thickness
      ),
      (
        restricted_area_width +
          coaching_box_width +
          team_bench_width +
          feature_thickness
      ),
      restricted_area_width + coaching_box_width,
      restricted_area_width,
      0,
      0
    )
  )

  return(team_bench_area_outline_df)
}
