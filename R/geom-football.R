#' Set the colors to be used for the plot. The values provided in the arguments
#' are the defaults, and, where specified, are the rule-book specified values.
#'
#' Hexadecimal values are the passed vales to this function by default, but it
#' is also possible to use string-named values (e.g. \code{"dodgerblue"}) when
#' specifying.
#'
#' @param plot_background A hexadecimal string representing the color to use for
#'   this feature
#' @param field_apron A hexadecimal string representing the color to use for
#'   this feature
#' @param offensive_half A hexadecimal string representing the color to use for
#'   this feature
#' @param defensive_half A hexadecimal string representing the color to use for
#'   this feature
#' @param offensive_endzone A hexadecimal string representing the color to use
#'   for this feature
#' @param defensive_endzone A hexadecimal string representing the color to use
#'   for this feature
#' @param end_line A hexadecimal string representing the color to use for this
#'   feature
#' @param sideline A hexadecimal string representing the color to use for this
#'   feature
#' @param field_border A hexadecimal string representing the color to use for
#'   this feature
#' @param field_border_outline A hexadecimal string representing the color to
#'   use for this feature
#' @param red_zone_border A hexadecimal string representing the color to use for
#'   this feature
#' @param red_zone_border_outline A hexadecimal string representing the color to
#'   use for this feature
#' @param major_yard_line A hexadecimal string representing the color to use for
#'   this feature
#' @param goal_line A hexadecimal string representing the color to use for this
#'   feature
#' @param minor_yard_line A hexadecimal string representing the color to use for
#'   this feature
#' @param directional_arrow A hexadecimal string representing the color to use
#'   for this feature
#' @param try_mark A hexadecimal string representing the color to use for this
#'   feature
#' @param yardage_marker A hexadecimal string representing the color to use for
#'   this feature
#' @param restricted_area A hexadecimal string representing the color to use for
#'   this feature
#' @param coaching_box A hexadecimal string representing the color to use for
#'   this feature
#' @param team_bench_area A hexadecimal string representing the color to use for
#'   this feature
#' @param team_bench_area_outline A hexadecimal string representing the color to
#'   use for this feature
#' @param coaching_box_line A hexadecimal string representing the color to use
#'   for this feature
#'
#' @return A list of hexadecimal colors to use to color the features on the
#'   resulting plot
#'
#' @keywords internal
football_features_set_colors <- function(plot_background = NULL,
                                         field_apron = "#196f0c",
                                         offensive_half = "#196f0c",
                                         defensive_half = "#196f0c",
                                         offensive_endzone = "#196f0c",
                                         defensive_endzone = "#196f0c",
                                         end_line = "#ffffff",
                                         sideline = "#ffffff",
                                         field_border = "#196f0c",
                                         field_border_outline = "#ffffff00",
                                         red_zone_border = "#196f0c00",
                                         red_zone_border_outline = "#ffffff00",
                                         major_yard_line = "#ffffff",
                                         goal_line = "#ffffff",
                                         minor_yard_line = "#ffffff",
                                         directional_arrow = "#ffffff",
                                         try_mark = "#ffffff",
                                         yardage_marker = "#ffffff",
                                         restricted_area = "#ffffff",
                                         coaching_box = "#ffffff",
                                         team_bench_area = "#196f0c",
                                         team_bench_area_outline = "#ffffff",
                                         coaching_box_line = "#ffcb05") {
  feature_colors <- list(
    plot_background = plot_background,
    field_apron = field_apron,
    offensive_half = offensive_half,
    defensive_half = defensive_half,
    offensive_endzone = offensive_endzone,
    defensive_endzone = defensive_endzone,
    end_line = end_line,
    sideline = sideline,
    field_border = field_border,
    field_border_outline = field_border_outline,
    red_zone_border = red_zone_border,
    red_zone_border_outline = red_zone_border_outline,
    major_yard_line = major_yard_line,
    goal_line = goal_line,
    minor_yard_line = minor_yard_line,
    directional_arrow = directional_arrow,
    try_mark = try_mark,
    yardage_marker = yardage_marker,
    restricted_area = restricted_area,
    coaching_box = coaching_box,
    team_bench_area = team_bench_area,
    team_bench_area_outline = team_bench_area_outline,
    coaching_box_line = coaching_box_line
  )

  return(feature_colors)
}

#' Generate a \code{ggplot2} instance containing a football field for a
#' specified league
#'
#' @param league The league for which to draw the surface. This is
#'   case-insensitive
#' @param display_range A case-insensitive string indicating the display range
#'   to use for the plot. The default is \code{"full"}, which will be returned
#'   when either an invalid or no value is passed to the function.
#'
#'   The possible display ranges are:
#'
#'   \describe{
#'     \item{\code{"full"}}{The full field. This is the default}
#'     \item{\code{"offense"}}{The TV-right half of the field}
#'     \item{\code{"offence"}}{The TV-right half of the field}
#'     \item{\code{"offensivehalffield"}}{The TV-right half of the field}
#'     \item{\code{"offensive_half_field"}}{The TV-right half of the field}
#'     \item{\code{"offensive half field"}}{The TV-right half of the field}
#'     \item{\code{"defense"}}{The TV-left half of the field}
#'     \item{\code{"defence"}}{The TV-left half of the field}
#'     \item{\code{"defensivehalffield"}}{The TV-left half of the field}
#'     \item{\code{"defensive_half_field"}}{The TV-left half of the field}
#'     \item{\code{"defensive half field"}}{The TV-left half of the field}
#'     \item{\code{"redzone"}}{
#'        The offensive red zone of the field. This is by definition 20 yards
#'        from the goal line
#'     }
#'     \item{\code{"red_zone"}}{
#'        The offensive red zone of the field. This is by definition 20 yards
#'        from the goal line
#'     }
#'     \item{\code{"red zone"}}{
#'        The offensive red zone of the field. This is by definition 20 yards
#'        from the goal line
#'     }
#'     \item{\code{"oredzone"}}{
#'        The offensive red zone of the field. This is by definition 20 yards
#'        from the goal line
#'     }
#'     \item{\code{"offensive_red_zone"}}{
#'        The offensive red zone of the field. This is by definition 20 yards
#'        from the goal line
#'     }
#'     \item{\code{"offensive red zone"}}{
#'        The offensive red zone of the field. This is by definition 20 yards
#'        from the goal line
#'     }
#'     \item{\code{"dredzone"}}{
#'        The defensive red zone of the field. This is by definition 20 yards
#'        from the goal line
#'     }
#'     \item{\code{"defensive_red_zone"}}{
#'        The defensive red zone of the field. This is by definition 20 yards
#'        from the goal line
#'     }
#'     \item{\code{"defensive red zone"}}{
#'        The defensive red zone of the field. This is by definition 20 yards
#'        from the goal line
#'     }
#'   }
#' @param field_updates A list of updates to the field's parameters. These will
#'   overwrite the parameters of the league
#' @param color_updates A list of updates to the field's default colors, which
#'   are set by [football_features_set_colors()]
#' @param rotation An angle, given in degrees, through which the plot should be
#'   rotated
#' @param x_trans The amount that the \code{x} coordinates are to be shifted. By
#'   convention, the +\code{x} axis extends from the center of the field towards
#'   the right-hand endzone when viewing the field in TV View
#' @param y_trans The amount that the \code{y} coordinates are to be shifted. By
#'   convention, the +\code{y} axis extends from the center of the field towards
#'   the sideline when viewing the field in TV view
#' @param field_units The units with which to draw the field. The default is
#'   \code{NULL}, which will apply the rule-book specified units
#' @param xlims The limits on the final display in the \code{x} direction. The
#'   default is \code{NULL}, which will utilize the \code{xlims} specified by
#'   the \code{display_range} parameter
#' @param ylims The limits on the final display in the \code{y} direction. The
#'   default is \code{NULL}, which will utilize the \code{ylims} specified by
#'   the \code{display_range} parameter
#'
#' @return A \code{ggplot2} instance with a full-surface representation of a
#'   football field
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   geom_football(league = "NFL", rotation = 270, display_range = "red_zone")
#'   geom_football(league = "cfl", field_units = "ft")
#' }
geom_football <- function(league,
                          display_range = "full",
                          field_updates = list(),
                          color_updates = list(),
                          rotation = 0,
                          x_trans = 0,
                          y_trans = 0,
                          field_units = NULL,
                          xlims = NULL,
                          ylims = NULL) {
  # Input cleansing and data gathering -----------------------------------------

  # If no league is supplied, error and alert user
  if (missing(league)) {
    stop(
      glue::glue(
        "league parameter must be supplied. \"custom\" is a valid league if ",
        "you wish to specify your own field parameterization, but you must ",
        "use the field_updates parameter to do so"
      )
    )
  }

  # Force the league to be all lower case
  league <- tolower(league)

  # Get the dimensions for the specified league
  field_params <- surface_dimensions[["football"]][[league]]

  # Update the field parameters as necessary
  field_params <- utils::modifyList(field_params, field_updates)

  # Get the major yard lines as a sequence of evenly-spaced numbers
  major_yard_lines <- seq(
    0,
    ((field_params$field_length %or% 0) / 2) - 1,
    field_params$major_yard_line_distance
  )

  # Repeat for the minor yard lines
  minor_yard_lines <- seq(
    0,
    ((field_params$field_length %or% 0) / 2) - 1
  )

  # Add any additional minor yard lines
  if (!is.null(field_params$additional_minor_yard_lines)) {
    minor_yard_lines <- append(
      minor_yard_lines,
      field_params$additional_minor_yard_lines
    )
  }

  # Remove the major yard lines
  minor_yard_lines <- unlist(
    minor_yard_lines[
      minor_yard_lines %% (field_params$major_yard_line_distance %or% 1) != 0 |
        minor_yard_lines < 0
    ]
  )

  # Define all lines that have a directional arrow
  arrow_lines <- 1:((field_params$field_length %or% 0) / 2)
  arrow_lines <- arrow_lines[
    (arrow_lines %% (field_params$arrow_line_dist %or% 0) == 0) &
      (arrow_lines != ((field_params$field_length %or% 0) / 2))
  ]

  # Define all lines to be marked with a number
  marked_lines <- 1:((field_params$field_length %or% 0) - 1)
  marked_lines <- marked_lines[
    marked_lines %% (field_params$arrow_line_dist %or% 0) == 0
  ] - ((field_params$field_length %or% 0) / 2)

  # Duplicate each marked line
  marked_lines <- rep(marked_lines, each = 2)

  # If 0 (the midfield line) is not in the set of marked lines, include it
  if (!(0 %in% marked_lines)) {
    marked_lines <- sort(c(0, marked_lines))
  }

  # Handle cases where there is an odd number of marked lines
  if ((length(field_params$numbers_bottom) %or% 0) %% 2 != 0) {
    dist_to_line_bottom <- rep(
      field_params$number_to_yard_line %or% 0,
      (length(field_params$numbers_bottom) %or% 0 / 2) - 0.5
    )

    dist_to_line_bottom <- c(dist_to_line_bottom, 0)

    dist_to_line_bottom <- c(
      dist_to_line_bottom,
      rep(
        field_params$number_to_yard_line %or% 0,
        (length(field_params$numbers_bottom) %or% 0 / 2) - 0.5
      )
    )
  } else {
    dist_to_line_bottom <- rep(
      field_params$number_to_yard_line %or% 0,
      length(field_params$numbers_bottom) %or% 0
    )
  }

  if ((length(field_params$numbers_top) %or% 0) %% 2 != 0) {
    dist_to_line_top <- rep(
      field_params$number_to_yard_line %or% 0,
      (length(field_params$numbers_top) %or% 0 / 2) - 0.5
    )

    dist_to_line_top <- c(dist_to_line_top, 0)

    dist_to_line_top <- c(
      dist_to_line_top,
      rep(
        field_params$number_to_yard_line %or% 0,
        (length(field_params$numbers_top) %or% 0 / 2) - 0.5
      )
    )
  } else {
    dist_to_line_top <- rep(
      field_params$number_to_yard_line %or% 0,
      length(field_params$numbers_top) %or% 0
    )
  }

  # Make the marked lines data frame
  marked_yardages <- rbind(
    data.frame(
      marking = field_params$numbers_bottom,
      marker_rotation = rep(0, length(field_params$numbers_bottom) %or% 0),
      side = rep("bottom", length(field_params$numbers_bottom) %or% 0),
      marked_line = marked_lines,
      dist_to_line = dist_to_line_bottom
    ),
    data.frame(
      marking = field_params$numbers_top,
      marker_rotation = rep(180, length(field_params$numbers_top) %or% 0),
      side = rep("top", length(field_params$numbers_top) %or% 0),
      marked_line = marked_lines,
      dist_to_line = dist_to_line_top
    )
  )

  marked_yardages["x_min"] <- 0
  marked_yardages["x_max"] <- 0
  marked_yardages["y_min"] <- 0
  marked_yardages["y_max"] <- 0
  line_side <- "left"
  for (row in seq_len(nrow(marked_yardages))) {
    if (marked_yardages[row, "dist_to_line"] == 0) {
      line_side <- "center"
    }
    if (line_side == "left") {
      marked_yardages[row, "x_min"] <- marked_yardages$marked_line[row] -
        (field_params$number_to_yard_line %or% 0) -
        ((field_params$minor_line_thickness %or% 0) / 2) -
        (field_params$number_width %or% 0)
      marked_yardages[row, "x_max"] <- marked_yardages$marked_line[row] -
        (field_params$number_to_yard_line %or% 0) -
        ((field_params$minor_line_thickness %or% 0) / 2)

      line_side <- "right"
    } else if (line_side == "center") {
      marked_yardages[row, "x_min"] <- marked_yardages$marked_line[row] -
        ((field_params$number_width %or% 0) / 2)
      marked_yardages[row, "x_max"] <- marked_yardages$marked_line[row] +
        ((field_params$number_width %or% 0) / 2)

      line_side <- "left"
    } else {
      marked_yardages[row, "x_min"] <- marked_yardages$marked_line[row] +
        (field_params$number_to_yard_line %or% 0) +
        ((field_params$minor_line_thickness %or% 0) / 2)
      marked_yardages[row, "x_max"] <- marked_yardages$marked_line[row] +
        (field_params$number_to_yard_line %or% 0) +
        ((field_params$minor_line_thickness %or% 0) / 2) +
        (field_params$number_width %or% 0)

      line_side <- "left"
    }

    if (marked_yardages[row, "side"] == "bottom") {
      marked_yardages[row, "y_min"] <-
        (-(field_params$field_width %or% 0) / 2) +
        (field_params$sideline_to_bottom_of_numbers %or% 0)
      marked_yardages[row, "y_max"] <-
        (-(field_params$field_width %or% 0) / 2) +
        (field_params$sideline_to_bottom_of_numbers %or% 0) +
        (1.5 * (field_params$number_height %or% 0))
    } else {
      marked_yardages[row, "y_min"] <- ((field_params$field_width %or% 0) / 2) -
        (field_params$sideline_to_bottom_of_numbers %or% 0)
      marked_yardages[row, "y_max"] <- ((field_params$field_width %or% 0) / 2) -
        (field_params$sideline_to_bottom_of_numbers %or% 0) -
        (1.5 * (field_params$number_height %or% 0))
    }
  }

  marked_yardages["x"] <-
    (marked_yardages["x_min"] + marked_yardages["x_max"]) / 2
  marked_yardages["y"] <-
    (marked_yardages["y_min"] + marked_yardages["y_max"]) / 2

  # Feature initialization -----------------------------------------------------
  field_features <- list(

    ## Surface Base Features ---------------------------------------------------

    #### Defensive Half Field ####
    defensive_half_field = football_half_field(
      field_length = field_params$field_length %or% 0,
      field_width = field_params$field_width %or% 0,
      endzone_length = field_params$endzone_length %or% 0
    ),

    #### Offensive Half Field ####
    offensive_half_field = football_half_field(
      field_length = field_params$field_length %or% 0,
      field_width = field_params$field_width %or% 0,
      endzone_length = field_params$endzone_length %or% 0
    ),

    #### Endzone ####
    endzone = football_endzone(
      field_width = field_params$field_width %or% 0,
      endzone_length = field_params$endzone_length %or% 0
    ),

    #### Restricted Area ####
    restricted_area = football_restricted_area(
      restricted_area_length = field_params$team_bench_length_field_side %or% 0,
      feature_thickness = field_params$restricted_area_width %or% 0
    ),

    #### Coaching Box ####
    coaching_box = football_coaching_box(
      coaching_box_length = field_params$team_bench_length_field_side %or% 0,
      feature_thickness = field_params$coaching_box_width %or% 0
    ),

    #### Team Bench Area ####
    team_bench_area = football_team_bench_area(
      team_bench_length_field_side =
        field_params$team_bench_length_field_side %or% 0,
      team_bench_length_back_side =
        field_params$team_bench_length_back_side %or% 0,
      team_bench_width = field_params$team_bench_width %or% 0
    ),

    #### Field Apron ####
    field_apron = football_field_apron(
      field_length = field_params$field_length %or% 0,
      field_width = field_params$field_width %or% 0,
      endzone_length = field_params$endzone_length %or% 0,
      boundary_thickness = field_params$boundary_line_thickness %or% 0,
      field_border_thickness = field_params$field_border_thickness %or% 0,
      restricted_area_length =
        field_params$team_bench_length_field_side %or% 0,
      restricted_area_width = field_params$restricted_area_width %or% 0,
      coaching_box_length =
        field_params$team_bench_length_field_side %or% 0,
      coaching_box_width = field_params$coaching_box_width %or% 0,
      team_bench_length_field_side =
        field_params$team_bench_length_field_side %or% 0,
      team_bench_length_back_side =
        field_params$team_bench_length_back_side %or% 0,
      team_bench_width = field_params$team_bench_width %or% 0,
      team_bench_area_border_thickness =
        field_params$team_bench_area_border_thickness %or% 0,
      extra_apron_padding = field_params$extra_apron_padding %or% 0
    ),

    ## Surface Boundaries ------------------------------------------------------

    #### End Line ####
    end_line = football_end_line(
      feature_thickness = field_params$boundary_line_thickness %or% 0,
      field_width = field_params$field_width %or% 0
    ),

    #### Sideline ####
    sideline = football_sideline(
      feature_thickness = field_params$boundary_line_thickness %or% 0,
      field_length = field_params$field_length %or% 0,
      endzone_length = field_params$endzone_length %or% 0
    ),

    #### Field Border ####
    field_border = football_field_border(
      field_length = field_params$field_length %or% 0,
      field_width = field_params$field_width %or% 0,
      feature_thickness = field_params$field_border_thickness %or% 0,
      endzone_length = field_params$endzone_length %or% 0,
      boundary_line_thickness = field_params$boundary_line_thickness %or% 0,
      restricted_area_length = field_params$team_bench_length_field_side %or% 0,
      restricted_area_width = field_params$restricted_area_width %or% 0,
      coaching_box_length = field_params$team_bench_length_field_side %or% 0,
      coaching_box_width = field_params$coaching_box_width %or% 0,
      team_bench_length_field_side =
        field_params$team_bench_length_field_side %or% 0,
      team_bench_length_back_side =
        field_params$team_bench_length_back_side %or% 0,
      team_bench_width = field_params$team_bench_width %or% 0,
      team_bench_area_border_thickness =
        field_params$team_bench_area_border_thickness %or% 0,
      surrounds_team_bench_area =
        field_params$field_border_behind_bench %or% TRUE,
      bench_shape = field_params$bench_shape %or% "rectangle"
    ),

    #### Field Border Outline ####
    field_border_outline = football_field_border_outline(
      field_length = field_params$field_length %or% 0,
      field_width = field_params$field_width %or% 0,
      feature_thickness = field_params$minor_line_thickness %or% 0,
      endzone_length = field_params$endzone_length %or% 0,
      boundary_line_thickness = field_params$boundary_line_thickness %or% 0,
      restricted_area_length = field_params$team_bench_length_field_side %or% 0,
      restricted_area_width = field_params$restricted_area_width %or% 0,
      coaching_box_length = field_params$team_bench_length_field_side %or% 0,
      coaching_box_width = field_params$coaching_box_width %or% 0,
      team_bench_length_field_side =
        field_params$team_bench_length_field_side %or% 0,
      team_bench_length_back_side =
        field_params$team_bench_length_back_side %or% 0,
      team_bench_width = field_params$team_bench_width %or% 0,
      team_bench_area_border_thickness =
        field_params$team_bench_area_border_thickness %or% 0,
      field_border_thickness = field_params$field_border_thickness %or% 0,
      surrounds_team_bench_area =
        field_params$field_border_behind_bench %or% TRUE,
      bench_shape = field_params$bench_shape %or% "rectangle"
    ),

    #### Red Zone Border ####
    red_zone_border = football_red_zone_border(
      feature_thickness = field_params$field_border_thickness %or% 0
    ),

    #### Red Zone Border Outline ####
    red_zone_border_outline = football_red_zone_border_outline(
      feature_thickness = field_params$minor_line_thickness %or% 0
    ),

    ## Surface Lines -----------------------------------------------------------

    #### Goal Line ####
    goal_line = football_goal_line(
      field_width = field_params$field_width %or% 0,
      feature_thickness = field_params$goal_line_thickness %or% 0
    ),

    #### Try Mark ####
    try_mark = football_try_mark(
      try_mark_width = field_params$try_mark_width %or% 0,
      feature_thickness = field_params$minor_line_thickness %or% 0
    ),

    #### Coaching Box Line ####
    coaching_box_line = football_coaching_box_line(
      coaching_box_line_length =
        field_params$team_bench_length_field_side %or% 0,
      feature_thickness = field_params$minor_line_thickness %or% 0
    ),

    #### Team Bench Area Outline ####
    team_bench_area_outline = football_team_bench_area_outline(
      restricted_area_length = field_params$team_bench_length_field_side %or% 0,
      restricted_area_width = field_params$restricted_area_width %or% 0,
      coaching_box_length = field_params$team_bench_length_field_side %or% 0,
      coaching_box_width = field_params$coaching_box_width %or% 0,
      team_bench_length_field_side =
        field_params$team_bench_length_field_side %or% 0,
      team_bench_length_back_side =
        field_params$team_bench_length_back_side %or% 0,
      team_bench_width = field_params$team_bench_width %or% 0,
      feature_thickness = field_params$team_bench_area_border_thickness %or% 0
    ),

    ## Surface Features --------------------------------------------------------

    #### Directional Arrow ####
    directional_arrow = football_directional_arrow(
      arrow_base = field_params$arrow_base %or% 0,
      arrow_length = field_params$arrow_length %or% 0
    )
  )

  #### Major Yard Lines ####
  for (yard_line in major_yard_lines) {
    field_features[[glue::glue("{yard_line}_yard_line")]] <-
      football_major_yard_line(
        field_width = field_params$field_width %or% 0,
        feature_thickness = field_params$minor_line_thickness %or% 0,
        dist_to_sideline = field_params$sideline_to_major_yard_line %or% 0,
        cross_hash_length = field_params$inbound_cross_hashmark_length %or% 0,
        cross_hash_separation =
          field_params$inbound_cross_hashmark_separation %or% 0
      )
  }

  #### Minor Yard Lines ####
  for (yard_line in minor_yard_lines) {
    field_features[[glue::glue("{yard_line}_yard_line")]] <-
      football_minor_yard_line(
        yard_line_height = field_params$minor_yard_line_height %or% 0,
        feature_thickness = field_params$minor_line_thickness %or% 0
      )
  }

  #### Yardage Markers ####
  for(i in seq_len(nrow(marked_yardages))) {
    field_features[[glue::glue("marked_yardages_{i}")]] <-
      create_rectangle(
        x_min = marked_yardages[i, "x_min"],
        x_max = marked_yardages[i, "x_max"],
        y_min = marked_yardages[i, "y_min"],
        y_max = marked_yardages[i, "y_max"]
      )
  }


  # Coordinate Transformations -------------------------------------------------

  # Convert the units as needed
  if (
    !is.null(field_units) &&
      tolower(field_params$field_units %or% "ft") != tolower(field_units)
  ) {
    field_features <- lapply(
      field_features,
      convert_units,
      from_unit = field_params$field_units %or% "ft",
      to_unit = field_units,
      conversion_columns = c("x", "y")
    )
  }

  # Generate the Plot ----------------------------------------------------------

  # Start by getting the colors to use to make the plot
  feature_colors <- football_features_set_colors()

  # Update the features' colors as specified by the user
  feature_colors <- utils::modifyList(feature_colors, color_updates)

  # Create the base of the plot
  field_plot <- create_plot_base(
    plot_background = feature_colors$plot_background
  )

  #### Field Apron ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$field_apron,
    feature_color = feature_colors$field_apron,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Field Border ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$field_border,
    feature_color = feature_colors$field_border,
    feature_outline_color = feature_colors$field_border,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Field Border Outline ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$field_border_outline,
    feature_color = feature_colors$field_border_outline,
    feature_outline_color = feature_colors$field_border_outline,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Defensive Half Field ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = -0.25 * (field_params$field_length %or% 0),
    y_anchor = 0,
    feature_df = field_features$defensive_half,
    feature_color = feature_colors$defensive_half,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Offensive Half Field ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0.25 * (field_params$field_length %or% 0),
    y_anchor = 0,
    feature_df = field_features$offensive_half,
    feature_color = feature_colors$offensive_half,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Restricted Area ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = ((field_params$field_width %or% 0) / 2) +
      (field_params$boundary_line_thickness %or% 0),
    feature_df = field_features$restricted_area,
    feature_color = feature_colors$restricted_area,
    feature_outline_color = feature_colors$restricted_area,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Coaching Box ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = ((field_params$field_width %or% 0) / 2) +
      (field_params$boundary_line_thickness %or% 0) +
      (field_params$restricted_area_width %or% 0),
    feature_df = field_features$coaching_box,
    feature_color = feature_colors$coaching_box,
    feature_outline_color = feature_colors$coaching_box,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Team Bench Area ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = ((field_params$field_width %or% 0) / 2) +
      (field_params$boundary_line_thickness %or% 0) +
      (field_params$restricted_area_width %or% 0) +
      (field_params$coaching_box_width %or% 0),
    feature_df = field_features$team_bench_area,
    feature_color = feature_colors$team_bench_area,
    feature_outline_color = feature_colors$team_bench_area,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Coaching Box Line ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor =
      ((field_params$field_width %or% 0) / 2.0) +
      (field_params$boundary_line_thickness %or% 0) +
      (field_params$restricted_area_width %or% 0) +
      (field_params$coaching_box_width %or% 0),
    feature_df = field_features$coaching_box_line,
    feature_color = feature_colors$coaching_box_line,
    feature_outline_color = feature_colors$coaching_box_line,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Team Bench Area Outline ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = ((field_params$field_width %or% 0) / 2) +
      (field_params$boundary_line_thickness %or% 0),
    feature_df = field_features$team_bench_area_outline,
    feature_color = feature_colors$team_bench_area_outline,
    feature_outline_color = feature_colors$team_bench_area_outline,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Red Zone Border ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = ((field_params$field_length %or% 0) / 2) - 20,
    y_anchor = ((field_params$field_width %or% 0) / 2) +
      (field_params$boundary_line_thickness %or% 0),
    feature_df = field_features$red_zone_border,
    feature_color = feature_colors$red_zone_border,
    feature_outline_color = feature_colors$red_zone_border,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Red Zone Border Outline ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = ((field_params$field_length %or% 0) / 2) - 20,
    y_anchor = ((field_params$field_width %or% 0) / 2) +
      (field_params$boundary_line_thickness %or% 0) +
      (field_params$field_border_thickness %or% 0),
    feature_df = field_features$red_zone_border_outline,
    feature_color = feature_colors$red_zone_border_outline,
    feature_outline_color = feature_colors$red_zone_border_outline,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Sideline ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = (field_params$field_width %or% 0) / 2,
    feature_df = field_features$sideline,
    feature_color = feature_colors$sideline,
    feature_outline_color = feature_colors$sideline,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### End Line ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = ((field_params$field_length %or% 0) / 2) +
      (field_params$endzone_length %or% 0),
    y_anchor = 0,
    feature_df = field_features$end_line,
    feature_color = feature_colors$end_line,
    feature_outline_color = feature_colors$end_line,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Defensive Endzone ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = -((field_params$field_length %or% 0) / 2) -
      ((field_params$endzone_length %or% 0) / 2),
    y_anchor = 0,
    feature_df = field_features$endzone,
    feature_color = feature_colors$defensive_endzone,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Offensive Endzone ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = ((field_params$field_length %or% 0) / 2) +
      ((field_params$endzone_length %or% 0) / 2),
    y_anchor = 0,
    feature_df = field_features$endzone,
    feature_color = feature_colors$offensive_endzone,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Major Yard Lines ####
  for (yard_line in major_yard_lines) {
    field_plot <- add_feature(
      field_plot,
      x_anchor = yard_line,
      y_anchor = 0,
      feature_df = field_features[[glue::glue("{yard_line}_yard_line")]],
      feature_color = feature_colors$major_yard_line,
      reflect_x = TRUE,
      reflect_y = FALSE,
      x_trans = x_trans,
      y_trans = y_trans,
      rotation = rotation
    )
  }

  #### Minor Yard Lines (nearest boundary) ####
  for (yard_line in minor_yard_lines) {
    field_plot <- add_feature(
      field_plot,
      x_anchor = ((field_params$field_length %or% 0) / 2) - yard_line,
      y_anchor = ((field_params$field_width %or% 0) / 2.0) -
        (field_params$sideline_to_outer_yard_line %or% 0) -
        (field_params$minor_yard_line_height %or% 0),
      feature_df = field_features[[glue::glue("{yard_line}_yard_line")]],
      feature_color = feature_colors$minor_yard_line,
      reflect_x = TRUE,
      reflect_y = TRUE,
      x_trans = x_trans,
      y_trans = y_trans,
      rotation = rotation
    )
  }

  #### Minor Yard Lines (nearest center of field) ####
  for (yard_line in minor_yard_lines) {
    field_plot <- add_feature(
      field_plot,
      x_anchor = ((field_params$field_length %or% 0) / 2) - yard_line,
      y_anchor = ((field_params$inbound_hashmark_separation %or% 0) / 2),
      feature_df = field_features[[glue::glue("{yard_line}_yard_line")]],
      feature_color = feature_colors$minor_yard_line,
      reflect_x = TRUE,
      reflect_y = TRUE,
      x_trans = x_trans,
      y_trans = y_trans,
      rotation = rotation
    )
  }

  #### Goal Line ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = (field_params$field_length %or% 0) / 2,
    y_anchor = 0,
    feature_df = field_features$goal_line,
    feature_color = feature_colors$goal_line,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Try Mark ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = ((field_params$field_length %or% 0) / 2) -
      (field_params$try_mark_distance %or% 0),
    y_anchor = 0,
    feature_df = field_features$try_mark,
    feature_color = feature_colors$try_mark,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Marked Yardages ####
  for(i in seq_len(nrow(marked_yardages))) {
    yardage_marker <- field_features[[glue::glue("marked_yardages_{i}")]]
    yardage_marker["x"] <- yardage_marker["x"] + x_trans
    yardage_marker["y"] <- yardage_marker["y"] + y_trans
    yardage_marker_rot <- rotate_coords(
      yardage_marker,
      angle = rotation
    )
    yardage_marker_df <- data.frame(
      x_min = min(yardage_marker_rot["x"]),
      x_max = max(yardage_marker_rot["x"]),
      y_min = min(yardage_marker_rot["y"]),
      y_max = max(yardage_marker_rot["y"]),
      marking = marked_yardages[i, "marking"],
      marker_rotation = marked_yardages[i, "marker_rotation"]
    )
    bb_polygon <- create_rectangle(
      x_min = min(yardage_marker_rot["x"]),
      x_max = max(yardage_marker_rot["x"]),
      y_min = min(yardage_marker_rot["y"]),
      y_max = max(yardage_marker_rot["y"])
    )
    bb_polygon["x"] <- bb_polygon["x"] + x_trans
    bb_polygon["y"] <- bb_polygon["y"] + y_trans
    field_plot <- field_plot +
      ggplot2::geom_polygon(
        data = bb_polygon,
        ggplot2::aes_string(
          x = "x",
          y = "y"
        ),
        fill = "#ffffff00"
      ) +
      ggfittext::geom_fit_text(
        data = yardage_marker_df,
        ggplot2::aes_string(
          xmin = "x_min",
          xmax = "x_max",
          ymin = "y_min",
          ymax = "y_max"
        ),
        label = yardage_marker_df$marking,
        angle = rotation + yardage_marker_df$marker_rotation,
        color = feature_colors$yardage_marker,
        grow = TRUE,
        fullheight = TRUE,
        padding.x = grid::unit(0, "mm"),
        padding.y = grid::unit(0, "mm")
      )
  }

  #### Directional Arrows ####
  for (arrow_line in arrow_lines) {
    field_plot <- add_feature(
      field_plot,
      x_anchor = ((field_params$field_length %or% 0) / 2) -
        arrow_line +
        (field_params$yard_line_to_arrow %or% 0),
      y_anchor = ((field_params$field_width %or% 0) / 2) -
        (field_params$sideline_to_bottom_of_numbers %or% 0) -
        (field_params$number_height %or% 0) +
        (field_params$top_number_to_arrow %or% 0),
      feature_df = field_features$directional_arrow,
      feature_color = feature_colors$directional_arrow,
      reflect_x = TRUE,
      reflect_y = TRUE,
      x_trans = x_trans,
      y_trans = y_trans,
      rotation = rotation
    )
  }

  # Set Display Range-----------------------------------------------------------
  half_field_length <- ((field_params$field_length %or% 0) / 2) +
    (field_params$endzone_length %or% 0) +
    (field_params$boundary_line_thickness %or% 0) +
    (field_params$field_border_thickness %or% 0) +
    (field_params$minor_line_thickness %or% 0) +
    5
  half_field_width <- ((field_params$field_width %or% 0) / 2) +
    (field_params$boundary_line_thickness %or% 0) +
    (field_params$restricted_area_width %or% 0) +
    (field_params$coaching_box_width %or% 0) +
    (field_params$team_bench_width %or% 0) +
    (field_params$field_border_thickness %or% 0) +
    (field_params$minor_line_thickness %or% 0) +
    5

  if (is.null(xlims)) {
    xlims <- switch(tolower(display_range),
      # Full surface
      "full" = c(-half_field_length, half_field_length),

      # Half-field plots
      "offense" = c(0, half_field_length),
      "offence" = c(0, half_field_length),
      "offensivehalffield" = c(0, half_field_length),
      "offensive_half_field" = c(0, half_field_length),
      "offensive half field" = c(0, half_field_length),
      "defense" = c(-half_field_length, 0),
      "defence" = c(-half_field_length, 0),
      "defensivehalffield" = c(-half_field_length, 0),
      "defensive_half_field" = c(-half_field_length, 0),
      "defensive half field" = c(-half_field_length, 0),

      # Offensive Red Zone
      "redzone" = c(
        ((field_params$field_length %or% 0) / 2) -
          20,
        half_field_length
      ),
      "red_zone" = c(
        ((field_params$field_length %or% 0) / 2) -
          20,
        half_field_length
      ),
      "red zone" = c(
        ((field_params$field_length %or% 0) / 2) -
          20,
        half_field_length
      ),
      "oredzone" = c(
        ((field_params$field_length %or% 0) / 2) -
          20,
        half_field_length
      ),
      "offensive_red_zone" = c(
        ((field_params$field_length %or% 0) / 2) -
          20,
        half_field_length
      ),
      "offensive red zone" = c(
        ((field_params$field_length %or% 0) / 2) -
          20,
        half_field_length
      ),

      # Defensive Red Zone
      "dredzone" = c(
        -half_field_length,
        -((field_params$field_length %or% 0) / 2) +
          20
      ),
      "defensive_red_zone" = c(
        -half_field_length,
        -((field_params$field_length %or% 0) / 2) +
          20
      ),
      "defensive red zone" = c(
        -half_field_length,
        -((field_params$field_length %or% 0) / 2) +
          20
      ),

      # Default case
      c(-half_field_length, half_field_length)
    )

    # Adjust the x limits of the plot per the specified x translation
    xlims <- xlims + x_trans
  }

  if (is.null(ylims)) {
    ylims <- switch(tolower(display_range),
      # Full surface
      "full" = c(-half_field_width, half_field_width),

      # Half-field plots
      "offense" = c(-half_field_width, half_field_width),
      "offence" = c(-half_field_width, half_field_width),
      "offensivehalffield" = c(
        -half_field_width,
        half_field_width
      ),
      "offensive_half_field" = c(
        -half_field_width,
        half_field_width
      ),
      "offensive half field" = c(
        -half_field_width,
        half_field_width
      ),
      "defense" = c(-half_field_width, half_field_width),
      "defence" = c(-half_field_width, half_field_width),
      "defensivehalffield" = c(
        -half_field_width,
        half_field_width
      ),
      "defensive_half_field" = c(
        -half_field_width,
        half_field_width
      ),
      "defensive half field" = c(
        -half_field_width,
        half_field_width
      ),

      # Offensive Red Zone
      "redzone" = c(-half_field_width, half_field_width),
      "red_zone" = c(-half_field_width, half_field_width),
      "red zone" = c(-half_field_width, half_field_width),
      "oredzone" = c(-half_field_width, half_field_width),
      "offensive_red_zone" = c(
        -half_field_width,
        half_field_width
      ),
      "offensive red zone" = c(
        -half_field_width,
        half_field_width
      ),

      # Defensive Red Zone
      "dredzone" = c(-half_field_width, half_field_width),
      "defensive_red_zone" = c(
        -half_field_width,
        half_field_width
      ),
      "defensive red zone" = c(
        -half_field_width,
        half_field_width
      ),

      # Default case
      c(-half_field_width, half_field_width)
    )

    # Adjust the y limits of the plot per the specified y translation
    ylims <- ylims + y_trans
  }

  # Rotate the limits of the plot. First, create the bounding box, then rotate
  # the vertices of the bounding box accordingly
  if (rotation != 0) {
    plot_lims <- rotate_coords(
      create_rectangle(
        x_min = min(xlims),
        x_max = max(xlims),
        y_min = min(ylims),
        y_max = max(ylims)
      ),
      angle = rotation
    )

    xlims <- c(min(plot_lims$x), max(plot_lims$x))
    ylims <- c(min(plot_lims$y), max(plot_lims$y))
  }

  ## Force the aspect ratio to be 1/1, and the x and y limits to be set
  field_plot <- field_plot +
    ggplot2::coord_fixed(
      xlim = xlims,
      ylim = ylims
    )

  # Return the ggplot2 instance
  return(field_plot)
}
