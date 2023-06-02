#' Set the colors to be used for the plot. The values provided in the arguments
#' are the defaults, and, where specified, are the rule-book specified values.
#'
#' Hexadecimal values are the passed vales to this function by default, but it
#' is also possible to use string-named values (e.g. \code{"dodgerblue"}) when
#' specifying.
#'
#' @param plot_background A hexadecimal string representing the color to use for
#'   this feature
#' @return A list of hexadecimal colors to use to color the features on the
#'   resulting plot
#'
#' @keywords internal
lacrosse_features_set_colors <- function(plot_background = NULL,
                                         field_apron = "#195f0c",
                                         defensive_zone = "#195f0c",
                                         neutral_zone = "#195f0c",
                                         offensive_zone = "#195f0c",
                                         team_a_bench = "#a5acaf4d",
                                         team_b_bench = "#a5acaf4d",
                                         team_a_penalty_box = "#a5acaf4d",
                                         team_b_penalty_box = "#a5acaf4d",
                                         off_field_officials_box = "#a5acaf",
                                         boards = "#ffa500",
                                         end_line = "#ffffff",
                                         sideline = "#ffffff",
                                         center_line = "#ffffff",
                                         wing_line = "#ffffff",
                                         restraining_line = "#ffffff",
                                         defensive_area_line = "#ffffff",
                                         goal_line = "#ffffff",
                                         referee_crease = "#ffffff",
                                         referee_crease_fill = "#195f0c",
                                         goal_circle = "#ffffff",
                                         goal_circle_fill = "#195f0c",
                                         goal_arc = "#ffffff",
                                         goal_fan = "#ffffff",
                                         goal_fan_hash_mark = "#ffffff",
                                         goal_mouth_hash_mark = "#ffffff",
                                         goal_mouth = "#ffffff",
                                         below_goal_marking = "#ffffff",
                                         goal_frame = "#ffa500",
                                         goal_net = "#a5acaf4d",
                                         center_circle = "#ffffff",
                                         center_face_off_marker = "#ffffff",
                                         corner_face_off_marker = "#ffffff",
                                         change_area_outline = "#ffffff",
                                         change_area_fill = "#195f0c") {
  feature_colors <- list(
    plot_background = plot_background,
    field_apron = field_apron,
    defensive_zone = defensive_zone,
    neutral_zone = neutral_zone,
    offensive_zone = offensive_zone,
    team_a_bench = team_a_bench,
    team_b_bench = team_b_bench,
    team_a_penalty_box = team_a_penalty_box,
    team_b_penalty_box = team_b_penalty_box,
    off_field_officials_box = off_field_officials_box,
    boards = boards,
    end_line = end_line,
    sideline = sideline,
    center_line = center_line,
    wing_line = wing_line,
    restraining_line = restraining_line,
    defensive_area_line = defensive_area_line,
    goal_line = goal_line,
    referee_crease = referee_crease,
    referee_crease_fill = referee_crease_fill,
    goal_circle = goal_circle,
    goal_circle_fill = goal_circle_fill,
    goal_arc = goal_arc,
    goal_fan = goal_fan,
    goal_fan_hash_mark = goal_fan_hash_mark,
    goal_mouth_hash_mark = goal_mouth_hash_mark,
    goal_mouth = goal_mouth,
    below_goal_marking = below_goal_marking,
    goal_frame = goal_frame,
    goal_net = goal_net,
    center_circle = center_circle,
    center_face_off_marker = center_face_off_marker,
    corner_face_off_marker = corner_face_off_marker,
    change_area_outline = change_area_outline,
    change_area_fill = change_area_fill
  )

  return(feature_colors)
}

#' Generate a \code{ggplot2} instance containing a lacrosse field for a
#' specified league
#'
#' @param league The league for which to draw the surface. This is
#'   case-insensitive
#' @param display_range A case-insensitive string indicating the display range
#'   to use for the plot. The default is \code{"full"}, which will be returned
#'   when either an invalid or no value is passed to the function.
#'
#'   The possible display ranges are:
#'   \describe{
#'     \item{\code{"full"}}{
#'       The full field. This is the default
#'     }
#'     \item{\code{"offense"}}{
#'       The offensive half of the field. This is the right half of the field
#'       in TV view
#'     }
#'     \item{\code{"offence"}}{
#'       The offensive half of the field. This is the right half of the field
#'       in TV view
#'     }
#'     \item{\code{"offensivehalffield"}}{
#'       The offensive half of the field. This is the right half of the field
#'       in TV view
#'     }
#'     \item{\code{"offensive_half_field"}}{
#'       The offensive half of the field. This is the right half of the field
#'       in TV view
#'     }
#'     \item{\code{"offensive half field"}}{
#'       The offensive half of the field. This is the right half of the field
#'       in TV view
#'     }
#'     \item{\code{"defense"}}{
#'       The defensive half of the field. This is the left half of the field in
#'       TV view
#'     }
#'     \item{\code{"defence"}}{
#'       The defensive half of the field. This is the left half of the field in
#'       TV view
#'     }
#'     \item{\code{"defensivehalffield"}}{
#'       The defensive half of the field. This is the left half of the field in
#'       TV view
#'     }
#'     \item{\code{"defensive_half_field"}}{
#'       The defensive half of the field. This is the left half of the field in
#'       TV view
#'     }
#'     \item{\code{"defensive half field"}}{
#'       The defensive half of the field. This is the left half of the field in
#'       TV view
#'     }
#'   }
#'
#' @param field_updates A list of updates to the fields' parameters. These will
#'   overwrite the parameters of the league
#' @param color_updates A list of updates to the fields' default colors, which
#'   are set by [lacrosse_features_set_colors()]
#' @param rotation An angle, given in degrees, through which the plot should be
#'   rotated
#' @param x_trans The amount that the \code{x} coordinates are to be shifted. By
#'   convention, the +\code{x} axis extends from the center of the field towards
#'   the right-hand basket when viewing the field in TV View
#' @param y_trans The amount that the \code{y} coordinates are to be shifted. By
#'   convention, the +\code{y} axis extends from the center of the field towards
#'   the top of the field when viewing the field in TV view
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
#'   lacrosse field
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   geom_lacrosse(league = "NCAA", rotation = 270, display_range = "offense")
#'   geom_lacrosse(league = "FIVB", field_units = "ft")
#' }
geom_lacrosse <- function(league,
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
  field_params <- surface_dimensions[["lacrosse"]][[league]]

  # Update the field parameters as necessary
  field_params <- utils::modifyList(field_params, field_updates)

  # Start by getting the colors to use to make the plot
  feature_colors <- lacrosse_features_set_colors()

  # If the center color needs to be a different color, it will by default be
  # given #ffcb05
  center_face_off_marker_color_contrasting = (
    field_params$center_face_off_marker_color_contrasting %or% FALSE
  )

  if (center_face_off_marker_color_contrasting == TRUE) {
    if (feature_colors$center_face_off_marker == feature_colors$center_line) {
      color_updates[["center_face_off_marker"]] = "#ffcb05"
    }
  }

  # Update the features' colors as specified by the user
  feature_colors <- utils::modifyList(feature_colors, color_updates)

  # Feature initialization -----------------------------------------------------
  field_features <- list(

    ## Surface Base Features ---------------------------------------------------

    #### Defensive Zone ####
    defensive_zone = lacrosse_defensive_zone(
      field_length = field_params$field_length %or% 0,
      field_width = field_params$field_width %or% 0,
      corner_radius = field_params$corner_radius %or% 0,
      nzone_length = field_params$nzone_length %or% 0,
      field_shape = field_params$field_shape %or% "rectangle"
    ),

    #### Neutral Zone ####
    neutral_zone = lacrosse_neutral_zone(
      field_width = field_params$field_width %or% 0,
      nzone_length = field_params$nzone_length %or% 0
    ),

    #### Offensive Zone ####
    offensive_zone = lacrosse_offensive_zone(
      field_length = field_params$field_length %or% 0,
      field_width = field_params$field_width %or% 0,
      corner_radius = field_params$corner_radius %or% 0,
      nzone_length = field_params$nzone_length %or% 0,
      field_shape = field_params$field_shape %or% "rectangle"
    ),

    #### Field Apron ####
    field_apron = lacrosse_field_apron(
      field_length = field_params$field_length %or% 0,
      field_width = field_params$field_width %or% 0,
      field_apron_thickness = field_params$field_apron_thickness %or% 0,
      field_shape = field_params$field_shape %or% "rectangle"
    ),

    ## Surface Boundaries ------------------------------------------------------

    #### Sidelines ####
    sideline = lacrosse_sideline(
      field_length = field_params$field_length %or% 0,
      line_thickness = field_params$boundary_thickness %or% 0
    ),

    #### End Lines ####
    end_line = lacrosse_end_line(
      field_width = field_params$field_width %or% 0,
      line_thickness = field_params$boundary_thickness %or% 0
    ),

    #### Boards ####
    boards = lacrosse_boards(
      field_length = field_params$field_length %or% 0,
      field_width = field_params$field_width %or% 0,
      corner_radius = field_params$corner_radius %or% 0,
      boundary_thickness = field_params$boards_thickness %or% 0
    ),

    #### Player Benches ####
    player_bench_outline = lacrosse_player_bench_outline(
      bench_area_outline_thickness = field_params$boards_thickness %or% 0,
      bench_length = field_params$bench_length %or% 0,
      bench_depth = field_params$bench_depth %or% 0
    ),

    player_bench_fill = lacrosse_player_bench_area_fill(
      bench_area_outline_thickness = field_params$boards_thickness %or% 0,
      bench_length = field_params$bench_length %or% 0,
      bench_depth = field_params$bench_depth %or% 0
    ),

    #### Penalty Boxes ####
    penalty_box_outline = lacrosse_penalty_box_outline(
      penalty_box_outline_thickness = field_params$boards_thickness %or% 0,
      penalty_box_length = (field_params$penalty_box_length %or% 0),
      penalty_box_depth = field_params$penalty_box_depth %or% 0,
      penalty_box_separation = field_params$penalty_box_separation %or% 0
    ),

    penalty_box_fill = lacrosse_penalty_box_fill(
      penalty_box_outline_thickness = field_params$boards_thickness %or% 0,
      penalty_box_length = field_params$penalty_box_length %or% 0,
      penalty_box_depth = field_params$penalty_box_depth %or% 0
    ),

    #### Off-Field Officials' Box ####
    off_field_officials_box = lacrosse_off_field_officials_box(
      officials_box_thickness = field_params$boards_thickness %or% 0,
      officials_box_length = field_params$penalty_box_separation %or% 0,
      officials_box_depth = field_params$penalty_box_depth %or% 0
    ),


    ## Surface Lines -----------------------------------------------------------

    #### Center Line ####
    center_line = lacrosse_center_line(
      center_line_width = field_params$center_line_width %or% 0,
      line_thickness = field_params$center_line_thickness %or% 0
    ),

    #### Wing Lines ####
    wing_line = lacrosse_wing_line(
      wing_line_length = field_params$wing_line_length %or% 0,
      line_thickness = field_params$wing_line_thickness %or% 0
    ),

    #### Restraining Lines ####
    restraining_line = lacrosse_restraining_line(
      field_width = field_params$field_width %or% 0,
      line_thickness = field_params$restraining_line_thickness %or% 0
    ),

    #### Defensive Area Lines ####
    defensive_area_line = lacrosse_defensive_area_line(
      defensive_area_line_length =
        field_params$defensive_area_line_length %or% 0,
      line_thickness = field_params$defensive_area_line_thickness %or% 0
    ),

    #### Goal Lines ####
    goal_line = lacrosse_goal_line(
      goal_frame_width = field_params$goal_frame_width %or% 0,
      line_thickness = field_params$goal_line_thickness %or% 0,
      goal_line_full_diameter = field_params$goal_line_full_diameter %or% 0,
      goal_circle_radius = field_params$goal_circle_radius %or% 0
    ),

    #### Referee's Crease ####
    referee_crease = lacrosse_referee_crease(
      referee_crease_radius = field_params$referee_crease_radius %or% 0,
      line_thickness = field_params$referee_crease_thickness %or% 0
    ),

    #### Referee's Crease Fill ####
    referee_crease_fill = lacrosse_referee_crease_fill(
      referee_crease_radius = field_params$referee_crease_radius %or% 0,
      line_thickness = field_params$referee_crease_thickness %or% 0
    ),

    ## Surface Features --------------------------------------------------------

    #### Goal Circle ####
    goal_circle = lacrosse_goal_circle(
      goal_circle_radius = field_params$goal_circle_radius %or% 0,
      line_thickness = field_params$goal_circle_thickness %or% 0,
      goal_circle_full_360 = field_params$goal_circle_full_360 %or% FALSE,
      goal_depth = field_params$goal_depth %or% 0,
      goal_depth_to_circle = field_params$goal_depth_to_circle %or% 0
    ),

    #### Goal Circle Fill ####
    goal_circle_fill = lacrosse_goal_circle_fill(
      goal_circle_radius = field_params$goal_circle_radius %or% 0,
      line_thickness = field_params$goal_circle_thickness %or% 0,
      goal_circle_full_360 = field_params$goal_circle_full_360 %or% FALSE,
      goal_depth = field_params$goal_depth %or% 0,
      goal_depth_to_circle = field_params$goal_depth_to_circle %or% 0
    ),

    #### Goal Arc ####
    goal_arc = lacrosse_goal_arc(
      goal_arc_extension = field_params$goal_arc_extension %or% 0,
      goal_arc_radius = field_params$goal_arc_radius %or% 0,
      line_thickness = field_params$goal_arc_line_thickness
    ),

    #### Goal Fan ####
    goal_fan = lacrosse_goal_fan(
      goal_fan_radius = field_params$goal_fan_radius %or% 0,
      goal_circle_radius = field_params$goal_circle_radius %or% 0,
      line_thickness = field_params$goal_fan_line_thickness %or% 0
    ),

    #### Goal Mouth ####
    goal_mouth = lacrosse_goal_mouth(
      goal_mouth_radius = field_params$goal_mouth_radius %or% 0,
      line_thickness = field_params$goal_mouth_line_thickness %or% 0,
      goal_mouth_semi_circle_separation =
        field_params$goal_mouth_semi_circle_separation %or% 0
    ),

    #### Goal Mouth Hash Mark ####
    goal_mouth_hash_mark = lacrosse_goal_mouth_hash_mark(
      goal_mouth_hash_mark_length =
        field_params$goal_mouth_hash_mark_length %or% 0,
      line_thickness = field_params$goal_mouth_hash_mark_line_thickness %or% 0
    ),

    #### Below Goal Marking ####
    below_goal_marking = lacrosse_below_goal_marking(
      below_goal_marking_radius = field_params$below_goal_marking_radius %or% 0
    ),

    #### Goal Frame ####
    goal_frame = lacrosse_goal_frame(
      goal_frame_opening_interior =
        field_params$goal_frame_opening_interior %or% 0,
      goal_post_thickness = field_params$goal_post_thickness %or% 0,
      goal_depth = field_params$goal_depth %or% 0
    ),

    #### Goal Net ####
    goal_net = lacrosse_goal_net(
      goal_frame_opening_interior =
        field_params$goal_frame_opening_interior %or% 0,
      goal_post_thickness = field_params$goal_post_thickness %or% 0,
      goal_depth = field_params$goal_depth %or% 0
    ),

    #### Center Circle ####
    center_circle = lacrosse_center_circle(
      center_circle_radius = field_params$center_circle_radius %or% 0,
      center_circle_thickness = field_params$center_circle_thickness %or% 0
    ),

    #### Center Face-Off Marker ####
    center_face_off_marker = lacrosse_face_off_marker(
      shape = field_params$center_face_off_marker_shape %or% "O",
      feature_thickness = field_params$center_face_off_marker_side_width %or% 0,
      side_length = field_params$center_face_off_marker_side_length %or% 0,
      feature_radius = field_params$center_face_off_marker_radius %or% 0
    ),

    #### Corner Face-Off Marker ####
    corner_face_off_marker = lacrosse_face_off_marker(
      shape = field_params$corner_face_off_marker_shape %or% "O",
      feature_thickness = field_params$corner_face_off_marker_bar_width %or% 0,
      side_length = field_params$corner_face_off_marker_bar_length %or% 0,
      feature_radius = field_params$corner_face_off_marker_radius %or% 0
    ),

    #### Change Area Outline ####
    change_area_outline = lacrosse_change_area_outline(
      change_area_length = (
        ((field_params$bench_separation %or% 0) / 2) +
          (field_params$bench_length %or% 0) +
          (field_params$change_area_extension %or% 0)
      ),
      change_area_width = field_params$change_area_width %or% 0,
      feature_thickness = field_params$change_area_outline_thickness %or% 0
    ),

    #### Change Area Fill ####
    change_area_fill = lacrosse_change_area_fill(
      change_area_length = (
        ((field_params$bench_separation %or% 0) / 2) +
          (field_params$bench_length %or% 0) +
          (field_params$change_area_extension %or% 0)
      ),
      change_area_width = field_params$change_area_width %or% 0
    )
  )

  #### Goal Fan Hash Mark ####
  if (
    (length(field_params$goal_fan_hash_mark_separation_from_center) > 0) %or%
    FALSE
  ) {
    for (i in 1:length(field_params$goal_fan_hash_mark_separation_from_center)){
      separation <- field_params$goal_fan_hash_mark_separation_from_center[i]
      theta <- separation / (
        (field_params$goal_fan_radius %or% 1) +
          (field_params$goal_circle_radius %or% 0)
      )
      field_features[[glue::glue("goal_fan_hash_mark_{separation}")]] <-
        lacrosse_goal_fan_hash_mark(
          goal_fan_hash_mark_length =
            field_params$goal_fan_hash_mark_length %or% 0,
          line_thickness =
            field_params$goal_fan_hash_mark_line_thickness %or% 0,
          rotational_angle = -theta * 180 / pi
        )
    }
  }

  # Coordinate Transformations -------------------------------------------------

  # Convert the units as needed
  if (
    !is.null(field_units) &&
    tolower(field_params$field_units %or% "ft") != tolower(field_units)
  )  {
    field_features <- lapply(
      field_features,
      convert_units,
      from_unit = field_params$field_units %or% "ft",
      to_unit = field_units,
      conversion_columns = c("x", "y")
    )
  }

  # Generate the Plot ----------------------------------------------------------

  # Create the base of the plot
  field_plot <- create_plot_base(
    plot_background = feature_colors$plot_background
  )

  # Add the features to the plot

  #### Field Apron ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$field_apron,
    feature_color = feature_colors$field_apron,
    feature_outline_color = feature_colors$field_apron,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Defensive Zone ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$defensive_zone,
    feature_color = feature_colors$defensive_zone,
    feature_outline_color = feature_colors$defensive_zone,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Neutral Zone ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$neutral_zone,
    feature_color = feature_colors$neutral_zone,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Offensive Zone ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$offensive_zone,
    feature_color = feature_colors$offensive_zone,
    feature_outline_color = feature_colors$offensive_zone,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Referee's Crease Fill ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = -(field_params$field_width %or% 0) / 2,
    feature_df = field_features$referee_crease_fill,
    feature_color = feature_colors$referee_crease_fill,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Sidelines ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = (field_params$field_width %or% 0) / 2,
    feature_df = field_features$sideline,
    feature_color = ifelse(
      field_params$has_sidelines %or% TRUE,
      feature_colors$sideline,
      "#ffffff00"
    ),
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### End Lines ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = (field_params$field_length %or% 0) / 2,
    y_anchor = 0,
    feature_df = field_features$end_line,
    feature_color = ifelse(
      field_params$has_endlines %or% TRUE,
      feature_colors$end_line,
      "#ffffff00"
    ),
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Change Area ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = (field_params$field_width %or% 0) / 2,
    feature_df = field_features$change_area_outline,
    feature_color = feature_colors$change_area_outline,
    feature_outline_color = feature_colors$change_area_outline,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = (field_params$field_width %or% 0) / 2,
    feature_df = field_features$change_area_fill,
    feature_color = feature_colors$change_area_fill,
    feature_outline_color = feature_colors$change_area_fill,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Center Line ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$center_line,
    feature_color = feature_colors$center_line,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Team Bench Areas ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = -(
      ((field_params$bench_separation %or% 0) / 2) +
        ((field_params$bench_length %or% 0) / 2)
    ),
    y_anchor = (field_params$field_width %or% 0) / 2,
    feature_df = field_features$player_bench_fill,
    feature_color = feature_colors$team_a_bench,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  field_plot <- add_feature(
    field_plot,
    x_anchor = (
      ((field_params$bench_separation %or% 0) / 2) +
        ((field_params$bench_length %or% 0) / 2)
    ),
    y_anchor = (field_params$field_width %or% 0) / 2,
    feature_df = field_features$player_bench_fill,
    feature_color = feature_colors$team_b_bench,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  field_plot <- add_feature(
    field_plot,
    x_anchor = (field_params$bench_separation %or% 0) / 2,
    y_anchor = (field_params$field_width %or% 0) / 2,
    feature_df = field_features$player_bench_outline,
    feature_color = feature_colors$boards,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Penalty Boxes ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = -(
      ((field_params$penalty_box_separation %or% 0) / 2) +
        ((field_params$board_thickness %or% 0)) +
        ((field_params$penalty_box_length %or% 0) / 2)
    ),
    y_anchor = -(field_params$field_width %or% 0) / 2,
    feature_df = field_features$penalty_box_fill,
    feature_color = feature_colors$team_a_penalty_box,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  field_plot <- add_feature(
    field_plot,
    x_anchor = (
      ((field_params$penalty_box_separation %or% 0) / 2) +
        ((field_params$board_thickness %or% 0)) +
        ((field_params$penalty_box_length %or% 0) / 2)
    ),
    y_anchor = -(field_params$field_width %or% 0) / 2,
    feature_df = field_features$penalty_box_fill,
    feature_color = feature_colors$team_b_penalty_box,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = -((field_params$field_width %or% 0) / 2),
    feature_df = field_features$penalty_box_outline,
    feature_color = feature_colors$boards,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Off-Field Officials Box ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = -((field_params$field_width %or% 0) / 2),
    feature_df = field_features$off_field_officials_box,
    feature_color = feature_colors$off_field_officials_box,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Boards ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$boards,
    feature_color = feature_colors$boards,
    feature_outline_color = ifelse(
      field_params$has_boards %or% FALSE,
      feature_colors$boards,
      "#ffffff00"
    ),
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Wing Lines ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = (
      ((field_params$field_width %or% 0) / 2) -
        (field_params$wing_line_center_to_sideline %or% 0)
    ),
    feature_df = field_features$wing_line,
    feature_color = feature_colors$wing_line,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Restraining Lines ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = field_params$restraining_line_outer_edge_to_center %or% 0,
    y_anchor = 0,
    feature_df = field_features$restraining_line,
    feature_color = feature_colors$restraining_line,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Defensive Area Lines ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = (
      ((field_params$field_length %or% 0) / 2) -
        (field_params$defensive_area_line_length %or% 0)
    ),
    y_anchor = (
      ((field_params$field_width %or% 0) / 2) -
        (field_params$defensive_area_line_center_to_sideline %or% 0)
    ),
    feature_df = field_features$defensive_area_line,
    feature_color = feature_colors$defensive_area_line,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Referee's Crease ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = -(field_params$field_width %or% 0) / 2,
    feature_df = field_features$referee_crease,
    feature_color = feature_colors$referee_crease,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Center Circle ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$center_circle,
    feature_color = feature_colors$center_circle,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Goal Circle Fill ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = ((field_params$field_length %or% 0) / 2) -
      (field_params$goal_line_center_to_end_line %or% 0),
    y_anchor = 0,
    feature_df = field_features$goal_circle_fill,
    feature_color = feature_colors$goal_circle_fill,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Goal Circle ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = ((field_params$field_length %or% 0) / 2) -
      (field_params$goal_line_center_to_end_line %or% 0),
    y_anchor = 0,
    feature_df = field_features$goal_circle,
    feature_color = feature_colors$goal_circle,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Goal Arc ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = ((field_params$field_length %or% 0) / 2) -
      (field_params$goal_line_center_to_end_line %or% 0),
    y_anchor = 0,
    feature_df = field_features$goal_arc,
    feature_color = feature_colors$goal_arc,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Goal Fan ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = ((field_params$field_length %or% 0) / 2) -
      (field_params$goal_line_center_to_end_line %or% 0),
    y_anchor = 0,
    feature_df = field_features$goal_fan,
    feature_color = feature_colors$goal_fan,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  # #### Goal Fan Hash Mark ####
  if (
    (length(field_params$goal_fan_hash_mark_separation_from_center) > 0) %or%
    FALSE
  ) {
    for (i in 1:length(field_params$goal_fan_hash_mark_separation_from_center)){
      separation <- field_params$goal_fan_hash_mark_separation_from_center[i]
      theta <- (separation / (field_params$goal_fan_radius %or% 1)) / pi
      field_plot <- add_feature(
        field_plot,
        x_anchor = (
          ((field_params$field_length %or% 0) / 2) -
            (field_params$goal_line_center_to_end_line %or% 0) +
            (field_params$goal_circle_radius %or% 0) +
            (
              (
                (field_params$goal_fan_radius %or% 0) +
                  (field_params$goal_circle_radius %or% 0)
              ) * cos((1 - theta) * pi)
            )
        ),
        y_anchor = (
          (
            (field_params$goal_fan_radius %or% 0) +
              (field_params$goal_circle_radius %or% 0)
          ) * sin((1 - theta) * pi)
        ),
        feature_df =
          field_features[[glue::glue("goal_fan_hash_mark_{separation}")]],
        feature_color = feature_colors$goal_fan_hash_mark,
        reflect_x = TRUE,
        reflect_y = TRUE,
        x_trans = x_trans,
        y_trans = y_trans,
        rotation = rotation
      ) # + coord_fixed()
    }
  }

  #### Goal Mouth ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = ((field_params$field_length %or% 0) / 2) -
      (field_params$goal_line_center_to_end_line %or% 0),
    y_anchor = 0,
    feature_df = field_features$goal_mouth,
    feature_color = feature_colors$goal_mouth,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Goal Mouth Hash Mark ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = ((field_params$field_length %or% 0) / 2) -
      (field_params$goal_line_center_to_end_line %or% 0),
    y_anchor = field_params$goal_fan_radius %or% 0,
    feature_df = field_features$goal_mouth_hash_mark,
    feature_color = feature_colors$goal_mouth_hash_mark,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Below Goal Marking ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = (
      ((field_params$field_length %or% 0) / 2) -
        (field_params$goal_line_center_to_end_line %or% 0) +
        (field_params$below_goal_marking_to_goal_line %or% 0)
    ),
    y_anchor = (field_params$goal_fan_radius %or% 0) / 2,
    feature_df = field_features$below_goal_marking,
    feature_color = feature_colors$below_goal_marking,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Goal Net ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = ((field_params$field_length %or% 0) / 2) -
      (field_params$goal_line_center_to_end_line %or% 0),
    y_anchor = 0,
    feature_df = field_features$goal_net,
    feature_color = feature_colors$goal_net,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Goal Frame ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = ((field_params$field_length %or% 0) / 2) -
      (field_params$goal_line_center_to_end_line %or% 0),
    y_anchor = 0,
    feature_df = field_features$goal_frame,
    feature_color = feature_colors$goal_frame,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Goal Lines ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = ((field_params$field_length %or% 0) / 2) -
      (field_params$goal_line_center_to_end_line %or% 0),
    y_anchor = 0,
    feature_df = field_features$goal_line,
    feature_color = feature_colors$goal_line,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Center Face-Off Marker ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$center_face_off_marker,
    feature_color = feature_colors$center_face_off_marker,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Corner Face-Off Markers ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = (
      (field_params$restraining_line_outer_edge_to_center %or% 0) +
        ((field_params$corner_face_off_marker_to_attack_line_outer %or% 0) / 2)
    ),
    y_anchor = (
      ((field_params$field_width %or% 0) / 2) -
        (field_params$corner_face_off_marker_to_boards %or% 0)
    ),
    feature_df = field_features$corner_face_off_marker,
    feature_color = feature_colors$corner_face_off_marker,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )


  # Set Display Range-----------------------------------------------------------
  half_field_length <- ((field_params$field_length %or% 0) / 2) +
    (field_params$field_apron_thickness %or% 0)

  half_field_width <- ((field_params$field_width %or% 0) / 2) +
    (field_params$field_apron_thickness %or% 0)

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
