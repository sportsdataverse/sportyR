#' Set the colors to be used for the plot. The values provided in the arguments
#' are the defaults, and, where specified, are the rule-book specified values.
#'
#' Hexadecimal values are the passed vales to this function by default, but it
#' is also possible to use string-named values (e.g. \code{"dodgerblue"}) when
#' specifying.
#'
#' @param plot_background A hexadecimal string representing the color to use for
#'   this feature
#' @param boards_color A hexadecimal string representing the color to use for
#'   this feature
#' @param center_line_color A hexadecimal string representing the color to use
#'   for this feature
#' @param blue_line_color A hexadecimal string representing the color to use for
#'   this feature
#' @param goal_line_color A hexadecimal string representing the color to use for
#'   this feature
#' @param goalkeepers_restricted_area_color A hexadecimal string representing
#'   the color to use for this feature
#' @param goal_crease_outline_color A hexadecimal string representing the color
#'   to use for this feature
#' @param goal_crease_fill_color A hexadecimal string representing the color to
#'   use for this feature
#' @param referee_crease_color A hexadecimal string representing the color to
#'   use for this feature
#' @param center_faceoff_spot_color A hexadecimal string representing the color
#'   to use for this feature
#' @param faceoff_spot_outer_ring_color A hexadecimal string representing the
#'   color to use for this feature
#' @param faceoff_spot_fill_color A hexadecimal string representing the color to
#'   use for this feature
#' @param center_faceoff_circle_color A hexadecimal string representing the
#'   color to use for this feature
#' @param non_center_faceoff_circle_color A hexadecimal string representing the
#'   color to use for this feature
#' @param faceoff_line_color A hexadecimal string representing the color to use
#'   for this feature
#' @param goal_color A hexadecimal string representing the color to use for this
#'   feature
#' @param goal_fill_color A hexadecimal string representing the color to use for
#'   this feature
#'
#' @return A list of hexadecimal colors to use to color the features on the
#'   resulting plot
#'
#' @keywords internal
hockey_features_set_colors <- function(plot_background = NULL,
                                       boards = "#000000",
                                       ozone_ice = "#ffffff",
                                       nzone_ice = "#ffffff",
                                       dzone_ice = "#ffffff",
                                       center_line = "#c8102e",
                                       zone_line = "#0033a0",
                                       goal_line = "#c8102e",
                                       restricted_trapezoid = "#c8102e",
                                       goal_crease_outline = "#c8102e",
                                       goal_crease_fill = "#41b6e6",
                                       referee_crease = "#c8102e",
                                       center_faceoff_spot = "#0033a0",
                                       faceoff_spot_ring = "#c8102e",
                                       faceoff_spot_stripe = "#c8102e",
                                       center_faceoff_circle = "#0033a0",
                                       odzone_faceoff_circle = "#c8102e",
                                       faceoff_line = "#c8102e",
                                       goal_frame = "#c8102e",
                                       goal_fill = "#a5acaf4d",
                                       team_a_bench = "#ffffff",
                                       team_b_bench = "#ffffff",
                                       team_a_penalty_box = "#ffffff",
                                       team_b_penalty_box = "#ffffff",
                                       off_ice_officials_box = "#a5acaf") {
  feature_colors <- list(
    plot_background = plot_background,
    boards = boards,
    ozone_ice = ozone_ice,
    nzone_ice = nzone_ice,
    dzone_ice = dzone_ice,
    center_line = center_line,
    zone_line = zone_line,
    goal_line = goal_line,
    restricted_trapezoid = restricted_trapezoid,
    goal_crease_outline = goal_crease_outline,
    goal_crease_fill = goal_crease_fill,
    referee_crease = referee_crease,
    center_faceoff_spot = center_faceoff_spot,
    faceoff_spot_ring = faceoff_spot_ring,
    faceoff_spot_stripe = faceoff_spot_stripe,
    center_faceoff_circle = center_faceoff_circle,
    odzone_faceoff_circle = odzone_faceoff_circle,
    faceoff_line = faceoff_line,
    goal_frame = goal_frame,
    goal_fill = goal_fill,
    team_a_bench = team_a_bench,
    team_b_bench = team_b_bench,
    team_a_penalty_box = team_a_penalty_box,
    team_b_penalty_box = team_b_penalty_box,
    off_ice_officials_box = off_ice_officials_box
  )

  return(feature_colors)
}

#' Generate a \code{ggplot2} instance containing an ice rink for a specified
#' league
#'
#' @param league The league for which to draw the surface. This is
#'   case-insensitive
#' @param display_range A case-insensitive string indicating the display range
#'   to use for the plot. The default is \code{"full"}, which will be returned
#'   when either an invalid or no value is passed to the function.
#'
#'   The possible display ranges are:
#'   \describe{
#'     \item{\code{"full"}}{The full ice surface. This is the default}
#'     \item{\code{"offense"}}{The TV-right half of the ice surface}
#'     \item{\code{"offence"}}{The TV-right half of the ice surface}
#'     \item{\code{"defense"}}{The TV-left half of the ice surface}
#'     \item{\code{"defence"}}{The TV-left half of the ice surface}
#'     \item{\code{"ozone"}}{The TV-right zone of the ice surface}
#'     \item{\code{"offensive_zone"}}{The TV-right zone of the ice surface}
#'     \item{\code{"offensive zone"}}{The TV-right zone of the ice surface}
#'     \item{\code{"attacking_zone"}}{The TV-right zone of the ice surface}
#'     \item{\code{"attacking zone"}}{The TV-right zone of the ice surface}
#'     \item{\code{"dzone"}}{The TV-left zone of the ice surface}
#'     \item{\code{"defensive_zone"}}{The TV-left zone of the ice surface}
#'     \item{\code{"defensive zone"}}{The TV-left zone of the ice surface}
#'     \item{\code{"defending_zone"}}{The TV-left zone of the ice surface}
#'     \item{\code{"defending zone"}}{The TV-left zone of the ice surface}
#'     \item{\code{"nzone"}}{The middle zone of the ice surface}
#'     \item{\code{"neutral"}}{The middle zone of the ice surface}
#'     \item{\code{"neutral_zone"}}{The middle zone of the ice surface}
#'     \item{\code{"neutral zone"}}{The middle zone of the ice surface}
#'   }
#' @param rink_updates A list of updates to the rink's parameters. These will
#'   overwrite the parameters of the league
#' @param color_updates A list of updates to the courts' default colors, which
#'   are set by [hockey_features_set_colors()]
#' @param rotation An angle, given in degrees, through which the plot should be
#'   rotated
#' @param x_trans The amount that the \code{x} coordinates are to be shifted. By
#'   convention, the +\code{x} axis extends from the center of the ice surface
#'   towards the right-hand goal when viewing the rink in TV View
#' @param y_trans The amount that the \code{y} coordinates are to be shifted. By
#'   convention, the +\code{y} axis extends from the center of the ice surface
#'   towards the top of the rink when viewing the rink in TV view
#' @param rink_units The units with which to draw the rink. The default is
#'   \code{NULL}, which will apply the rule-book specified units
#' @param xlims The limits on the final display in the \code{x} direction. The
#'   default is \code{NULL}, which will utilize the \code{xlims} specified by
#'   the \code{display_range} parameter
#' @param ylims The limits on the final display in the \code{y} direction. The
#'   default is \code{NULL}, which will utilize the \code{ylims} specified by
#'   the \code{display_range} parameter
#'
#' @return A \code{ggplot2} instance with a full-surface representation of an
#'   ice hockey rink
#'
#' @export
#'
#' @examples
#' geom_hockey(league = "NHL", rotation = 270, display_range = "ozone")
#' geom_hockey(league = "iihf", rink_units = "ft")
geom_hockey <- function(league,
                        display_range = "full",
                        rink_updates = list(),
                        color_updates = list(),
                        rotation = 0,
                        x_trans = 0,
                        y_trans = 0,
                        rink_units = NULL,
                        xlims = NULL,
                        ylims = NULL) {
  # Input cleansing and data gathering -----------------------------------------

  # If no league is supplied, error and alert user
  if (missing(league)) {
    stop(
      glue::glue(
        "league parameter must be supplied. \"custom\" is a valid league if ",
        "you wish to specify your own rink parameterization, but you must use ",
        "the rink_updates parameter to do so"
      )
    )
  }

  # Force the league to be all lower case
  league <- tolower(league)

  # Get the dimensions for the specified league
  rink_params <- surface_dimensions[["hockey"]][[league]]

  # Update the rink parameters as necessary
  rink_params <- utils::modifyList(rink_params, rink_updates)

  # Feature initialization -----------------------------------------------------
  rink_features <- list(

    ## Surface Base Features ---------------------------------------------------

    #### Defensive Zone ####
    dzone = hockey_defensive_zone(
      rink_length = rink_params$rink_length %or% 0,
      rink_width = rink_params$rink_width %or% 0,
      feature_radius = rink_params$corner_radius %or% 0,
      nzone_length = rink_params$nzone_length %or% 0
    ),

    #### Neutral Zone ####
    nzone = hockey_neutral_zone(
      rink_width = rink_params$rink_width %or% 0,
      feature_thickness = rink_params$nzone_length %or% 0
    ),

    #### Offensive Zone ####
    ozone = hockey_offensive_zone(
      rink_length = rink_params$rink_length %or% 0,
      rink_width = rink_params$rink_width %or% 0,
      feature_radius = rink_params$corner_radius %or% 0,
      nzone_length = rink_params$nzone_length %or% 0
    ),

    ## Surface Boundaries ------------------------------------------------------

    #### Boards ####
    boards = hockey_boards(
      rink_length = rink_params$rink_length %or% 0,
      rink_width = rink_params$rink_width %or% 0,
      feature_radius = rink_params$corner_radius %or% 0,
      feature_thickness = rink_params$board_thickness %or% 0
    ),

    #### Player Benches ####
    player_bench_outline = hockey_player_bench_outline(
      feature_thickness = rink_params$board_thickness %or% 0,
      bench_length = rink_params$bench_length %or% 0,
      bench_depth = rink_params$bench_depth %or% 0
    ),

    player_bench_fill = hockey_player_bench_area_fill(
      feature_thickness = rink_params$board_thickness %or% 0,
      bench_length = rink_params$bench_length %or% 0,
      bench_depth = rink_params$bench_depth %or% 0
    ),

    #### Penalty Boxes ####
    penalty_box_outline = hockey_penalty_box_outline(
      feature_thickness = rink_params$board_thickness %or% 0,
      penalty_box_length = (rink_params$penalty_box_length %or% 0),
      penalty_box_depth = rink_params$penalty_box_depth %or% 0,
      penalty_box_separation = rink_params$penalty_box_separation %or% 0
    ),

    penalty_box_fill = hockey_penalty_box_fill(
      feature_thickness = rink_params$board_thickness %or% 0,
      penalty_box_length = rink_params$penalty_box_length %or% 0,
      penalty_box_depth = rink_params$penalty_box_depth %or% 0
    ),

    #### Off-Ice Officials' Box ####
    off_ice_officials_box = hockey_off_ice_officials_box(
      feature_thickness = rink_params$board_thickness %or% 0,
      officials_box_length = rink_params$penalty_box_separation %or% 0,
      officials_box_depth = rink_params$penalty_box_depth %or% 0
    ),

    ## Surface Lines -----------------------------------------------------------

    #### Center Line ####
    center_line = hockey_center_line(
      feature_thickness = rink_params$major_line_thickness %or% 0,
      rink_width = rink_params$rink_width %or% 0,
      center_faceoff_spot_gap = rink_params$center_faceoff_spot_gap %or% 0
    ),

    #### Zone Line ####
    zone_line = hockey_zone_line(
      rink_width = rink_params$rink_width %or% 0,
      feature_thickness = rink_params$major_line_thickness %or% 0
    ),

    #### Referee's Crease ####
    referee_crease = hockey_referee_crease(
      feature_radius = rink_params$referee_crease_radius %or% 0,
      feature_thickness = rink_params$minor_line_thickness %or% 0
    ),

    #### Goal Line ####
    goal_line = hockey_goal_line(
      rink_length = rink_params$rink_length %or% 0,
      rink_width = rink_params$rink_width %or% 0,
      feature_radius = rink_params$corner_radius %or% 0,
      feature_thickness = rink_params$minor_line_thickness %or% 0,
      x_anchor = ((rink_params$rink_length %or% 0) / 2) -
        (rink_params$goal_line_to_boards) %or% 0
    ),

    #### Offensive/Defensive Zone Faceoff Lines ####
    odzone_faceoff_lines_1 = hockey_odzone_faceoff_lines(
      feature_thickness = rink_params$minor_line_thickness %or% 0,
      faceoff_line_dist_x = rink_params$faceoff_line_dist_x %or% 0,
      faceoff_line_dist_y = rink_params$faceoff_line_dist_y %or% 0,
      faceoff_line_length = rink_params$faceoff_line_length %or% 0,
      faceoff_line_width = rink_params$faceoff_line_width %or% 0
    ),

    odzone_faceoff_lines_2 = hockey_odzone_faceoff_lines(
      feature_thickness = rink_params$minor_line_thickness %or% 0,
      faceoff_line_dist_x = -1 * rink_params$faceoff_line_dist_x %or% 0,
      faceoff_line_dist_y = rink_params$faceoff_line_dist_y %or% 0,
      faceoff_line_length = -1 * rink_params$faceoff_line_length %or% 0,
      faceoff_line_width = rink_params$faceoff_line_width %or% 0
    ),

    ## Surface Features --------------------------------------------------------

    #### Center Faceoff Spot ####
    center_faceoff_spot = hockey_center_faceoff_spot(
      feature_radius = rink_params$center_faceoff_spot_radius %or% 0
    ),

    #### Offensive/Defensive Zone Faceoff Spot ####
    nodzone_faceoff_spot_ring = hockey_nodzone_faceoff_spot_ring(
      feature_radius = rink_params$noncenter_faceoff_spot_radius %or% 0,
      feature_thickness = rink_params$minor_line_thickness %or% 0
    ),

    nodzone_faceoff_spot_stripe = hockey_nodzone_faceoff_spot_stripe(
      feature_radius = rink_params$noncenter_faceoff_spot_radius %or% 0,
      feature_thickness = rink_params$minor_line_thickness %or% 0,
      gap_width = rink_params$noncenter_faceoff_spot_gap_width %or% 0
    ),

    #### Goal Crease ####
    goal_crease_outline = hockey_goal_crease_outline(
      feature_radius = rink_params$goal_crease_radius %or% 0,
      feature_thickness = rink_params$minor_line_thickness %or% 0,
      crease_style = rink_params$goal_crease_style %or% "",
      crease_length = rink_params$goal_crease_length %or% 0,
      crease_width = rink_params$goal_crease_width %or% 0,
      notch_dist_x = rink_params$goal_crease_notch_dist_x %or% 0,
      notch_width = rink_params$goal_crease_notch_width %or% 0
    ),

    goal_crease_fill = hockey_goal_crease_fill(
      feature_radius = rink_params$goal_crease_radius %or% 0,
      feature_thickness = rink_params$minor_line_thickness %or% 0,
      crease_style = rink_params$goal_crease_style %or% "",
      crease_length = rink_params$goal_crease_length %or% 0,
      crease_width = rink_params$goal_crease_width %or% 0,
      notch_dist_x = rink_params$goal_crease_notch_dist_x %or% 0,
      notch_width = rink_params$goal_crease_notch_width %or% 0
    ),

    #### Center Faceoff Circle ####
    center_faceoff_circle = hockey_center_faceoff_circle(
      feature_radius = rink_params$faceoff_circle_radius %or% 0,
      feature_thickness = rink_params$minor_line_thickness %or% 0
    ),

    #### Offensive/Defensive Zone Faceoff Circle ####
    odzone_faceoff_circle = hockey_odzone_faceoff_circle(
      feature_radius = rink_params$faceoff_circle_radius %or% 0,
      feature_thickness = rink_params$minor_line_thickness %or% 0,
      hashmark_width = rink_params$hashmark_width %or% 0,
      hashmark_ext_spacing = rink_params$hashmark_ext_spacing %or% 0
    ),

    #### Goal Frame ####
    goal_frame = hockey_goal_frame(
      feature_radius = rink_params$goal_radius %or% 0,
      goal_mouth_width = rink_params$goal_mouth_width %or% 0,
      goal_back_width = rink_params$goal_back_width %or% 0,
      goal_depth = rink_params$goal_depth %or% 0,
      goal_post_diameter = rink_params$goal_post_diameter %or% 0
    ),

    goal_fill = hockey_goal_frame_fill(
      feature_radius = rink_params$goal_radius %or% 0,
      goal_mouth_width = rink_params$goal_mouth_width %or% 0,
      goal_back_width = rink_params$goal_back_width %or% 0,
      goal_depth = rink_params$goal_depth %or% 0,
      goal_post_diameter = rink_params$goal_post_diameter %or% 0
    )
  )

  #### Goaltender's Restricted Area (when necessary) ####
  if (rink_params$has_trapezoid %or% FALSE) {
    rink_features[["goaltenders_restricted_area"]] <-
      hockey_goaltenders_restricted_area(
        rink_length = rink_params$rink_length %or% 0,
        feature_thickness = rink_params$minor_line_thickness,
        short_base_width = rink_params$short_base_width,
        long_base_width = rink_params$long_base_width,
        x_anchor = ((rink_params$rink_length %or% 0) / 2) -
          rink_params$goal_line_to_boards
      )
  }

  # Coordinate Transformations -------------------------------------------------

  # Convert the units as needed
  if (
    !is.null(rink_units) &&
    tolower(rink_params$rink_units %or% "ft") != tolower(rink_units)
  )  {
    rink_features <- lapply(
      rink_features,
      convert_units,
      from_unit = rink_params$rink_units %or% "ft",
      to_unit = rink_units,
      conversion_columns = c("x", "y")
    )
  }

  # Generate the Plot ----------------------------------------------------------

  # Start by getting the colors to use to make the plot
  feature_colors <- hockey_features_set_colors()

  # Update the features' colors as specified by the user
  feature_colors <- utils::modifyList(feature_colors, color_updates)

  # Create the base of the plot
  rink_plot <- create_plot_base(
    plot_background = feature_colors$plot_background
  )

  # Add the features to the plot

  #### Defensive Zone Ice ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = rink_features$dzone,
    feature_color = feature_colors$dzone_ice,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Neutral Zone Ice ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = rink_features$nzone,
    feature_color = feature_colors$nzone_ice,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Offensive Zone Ice ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = rink_features$ozone,
    feature_color = feature_colors$ozone_ice,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Center Line ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = rink_features$center_line,
    feature_color = feature_colors$center_line,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Zone Lines ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = (rink_params$nzone_length %or% 0) / 2,
    y_anchor = 0,
    feature_df = rink_features$zone_line,
    feature_color = feature_colors$zone_line,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  ### Goaltender's Restricted Area (when necessary) ####
  if (rink_params$has_trapezoid %or% FALSE) {
    rink_plot <- add_feature(
      rink_plot,
      x_anchor = ((rink_params$rink_length %or% 0) / 2) -
        (rink_params$goal_line_to_boards %or% 0),
      y_anchor = 0,
      feature_df = rink_features$goaltenders_restricted_area,
      feature_color = feature_colors$restricted_trapezoid,
      reflect_x = TRUE,
      reflect_y = FALSE,
      x_trans = x_trans,
      y_trans = y_trans,
      rotation = rotation
    )
  }

  #### Goal Crease ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = ((rink_params$rink_length %or% 0) / 2) -
      (rink_params$goal_line_to_boards %or% 0),
    y_anchor = 0,
    feature_df = rink_features$goal_crease_fill,
    feature_color = feature_colors$goal_crease_fill,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  rink_plot <- add_feature(
    rink_plot,
    x_anchor = ((rink_params$rink_length %or% 0) / 2) -
      (rink_params$goal_line_to_boards %or% 0),
    y_anchor = 0,
    feature_df = rink_features$goal_crease_outline,
    feature_color = feature_colors$goal_crease_outline,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Goal Lines ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = ((rink_params$rink_length %or% 0) / 2) -
      (rink_params$goal_line_to_boards %or% 0),
    y_anchor = 0,
    feature_df = rink_features$goal_line,
    feature_color = feature_colors$goal_line,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Goal ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = ((rink_params$rink_length %or% 0) / 2) -
      (rink_params$goal_line_to_boards %or% 0),
    y_anchor = 0,
    feature_df = rink_features$goal_fill,
    feature_color = feature_colors$goal_fill,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  rink_plot <- add_feature(
    rink_plot,
    x_anchor = ((rink_params$rink_length %or% 0) / 2) -
      (rink_params$goal_line_to_boards %or% 0),
    y_anchor = 0,
    feature_df = rink_features$goal_frame,
    feature_color = feature_colors$goal_frame,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Offensive/Defensive Zone Faceoff Circles ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = ((rink_params$rink_length %or% 0) / 2) -
      (rink_params$odzone_faceoff_spot_to_boards %or% 0),
    y_anchor = rink_params$noncenter_faceoff_spot_y,
    feature_df = rink_features$odzone_faceoff_circle,
    feature_color = feature_colors$odzone_faceoff_circle,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Offensive/Defensive Faceoff Spots ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = ((rink_params$rink_length %or% 0) / 2) -
      (rink_params$odzone_faceoff_spot_to_boards %or% 0),
    y_anchor = rink_params$noncenter_faceoff_spot_y,
    feature_df = rink_features$nodzone_faceoff_spot_stripe,
    feature_color = feature_colors$faceoff_spot_stripe,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  rink_plot <- add_feature(
    rink_plot,
    x_anchor = ((rink_params$rink_length %or% 0) / 2) -
      (rink_params$odzone_faceoff_spot_to_boards %or% 0),
    y_anchor = rink_params$noncenter_faceoff_spot_y,
    feature_df = rink_features$nodzone_faceoff_spot_ring,
    feature_color = feature_colors$faceoff_spot_ring,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Offensive/Defensive Faceoff Lines ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = ((rink_params$rink_length %or% 0) / 2) -
      (rink_params$odzone_faceoff_spot_to_boards %or% 0),
    y_anchor = rink_params$noncenter_faceoff_spot_y,
    feature_df = rink_features$odzone_faceoff_lines_1,
    feature_color = feature_colors$faceoff_line,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  rink_plot <- add_feature(
    rink_plot,
    x_anchor = ((rink_params$rink_length %or% 0) / 2) -
      (rink_params$odzone_faceoff_spot_to_boards %or% 0),
    y_anchor = rink_params$noncenter_faceoff_spot_y,
    feature_df = rink_features$odzone_faceoff_lines_2,
    feature_color = feature_colors$faceoff_line,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Neutral Zone Faceoff Spots ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = ((rink_params$nzone_length %or% 0) / 2) -
      (rink_params$nzone_faceoff_spot_to_zone_line %or% 0),
    y_anchor = rink_params$noncenter_faceoff_spot_y,
    feature_df = rink_features$nodzone_faceoff_spot_stripe,
    feature_color = feature_colors$faceoff_spot_stripe,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  rink_plot <- add_feature(
    rink_plot,
    x_anchor = ((rink_params$nzone_length %or% 0) / 2) -
      (rink_params$nzone_faceoff_spot_to_zone_line %or% 0),
    y_anchor = rink_params$noncenter_faceoff_spot_y,
    feature_df = rink_features$nodzone_faceoff_spot_ring,
    feature_color = feature_colors$faceoff_spot_ring,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Center Faceoff Circle ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = rink_features$center_faceoff_circle,
    feature_color = feature_colors$center_faceoff_circle,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Referee Crease ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = 0,
    y_anchor = -(rink_params$rink_width %or% 0) / 2,
    feature_df = rink_features$referee_crease,
    feature_color = feature_colors$referee_crease,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Center Faceoff Spot ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = rink_features$center_faceoff_spot,
    feature_color = feature_colors$center_faceoff_spot,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Team Bench Areas ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = -(
      ((rink_params$bench_separation %or% 0) / 2) +
        ((rink_params$bench_length %or% 0) / 2)
    ),
    y_anchor = (rink_params$rink_width %or% 0) / 2,
    feature_df = rink_features$player_bench_fill,
    feature_color = feature_colors$team_a_bench,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  rink_plot <- add_feature(
    rink_plot,
    x_anchor = (
      ((rink_params$bench_separation %or% 0) / 2) +
        ((rink_params$bench_length %or% 0) / 2)
    ),
    y_anchor = (rink_params$rink_width %or% 0) / 2,
    feature_df = rink_features$player_bench_fill,
    feature_color = feature_colors$team_b_bench,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  rink_plot <- add_feature(
    rink_plot,
    x_anchor = (rink_params$bench_separation %or% 0) / 2,
    y_anchor = (rink_params$rink_width %or% 0) / 2,
    feature_df = rink_features$player_bench_outline,
    feature_color = feature_colors$boards,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Penalty Boxes ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = -(
      ((rink_params$penalty_box_separation %or% 0) / 2) +
        ((rink_params$board_thickness %or% 0)) +
        ((rink_params$penalty_box_length %or% 0) / 2)
    ),
    y_anchor = -(rink_params$rink_width %or% 0) / 2,
    feature_df = rink_features$penalty_box_fill,
    feature_color = feature_colors$team_a_penalty_box,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  rink_plot <- add_feature(
    rink_plot,
    x_anchor = (
      ((rink_params$penalty_box_separation %or% 0) / 2) +
        ((rink_params$board_thickness %or% 0)) +
      ((rink_params$penalty_box_length %or% 0) / 2)
    ),
    y_anchor = -(rink_params$rink_width %or% 0) / 2,
    feature_df = rink_features$penalty_box_fill,
    feature_color = feature_colors$team_b_penalty_box,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  rink_plot <- add_feature(
    rink_plot,
    x_anchor = 0,
    y_anchor = -((rink_params$rink_width %or% 0) / 2),
    feature_df = rink_features$penalty_box_outline,
    feature_color = feature_colors$boards,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Off-Ice Officials Box ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = 0,
    y_anchor = -((rink_params$rink_width %or% 0) / 2),
    feature_df = rink_features$off_ice_officials_box,
    feature_color = feature_colors$off_ice_officials_box,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Boards ####
  rink_plot <- add_feature(
    rink_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = rink_features$boards,
    feature_color = feature_colors$boards,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  # Set Display Range-----------------------------------------------------------
  half_rink_length <- ((rink_params$rink_length %or% 0) / 2) +
    (3 * (rink_params$board_thickness %or% 0)) +
    5

  half_rink_width <- ((rink_params$rink_width %or% 0) / 2) +
    max(
      rink_params$bench_depth %or% 0,
      rink_params$penalty_box_depth %or% 0
    ) +
    (3 * (rink_params$board_thickness %or% 0)) +
    5

  half_nzone_length <- ((rink_params$nzone_length %or% 0) / 2) +
    (rink_params$major_line_thickness %or% 0) +
    5

  if (is.null(xlims)) {
    xlims <- switch(tolower(display_range),
      # Full surface
      "full" = c(-half_rink_length, half_rink_length),

      # Half-rink plots
      "offense" = c(0, half_rink_length),
      "offence" = c(0, half_rink_length),
      "defense" = c(-half_rink_length, 0),
      "defence" = c(-half_rink_length, 0),

      # Neutral zone
      "nzone" = c(-half_nzone_length, half_nzone_length),
      "neutral" = c(-half_nzone_length, half_nzone_length),
      "neutral_zone" = c(-half_nzone_length, half_nzone_length),
      "neutral zone" = c(-half_nzone_length, half_nzone_length),

      # Offensive zone
      "ozone" = c(half_nzone_length, half_rink_length),
      "offensive_zone" = c(half_nzone_length, half_rink_length),
      "offensive zone" = c(half_nzone_length, half_rink_length),
      "attacking_zone" = c(half_nzone_length, half_rink_length),
      "attacking zone" = c(half_nzone_length, half_rink_length),

      # Defensive zone
      "dzone" = c(-half_rink_length, -half_nzone_length),
      "defensive_zone" = c(-half_rink_length, -half_nzone_length),
      "defensive zone" = c(-half_rink_length, -half_nzone_length),
      "defending_zone" = c(-half_rink_length, -half_nzone_length),
      "defending zone" = c(-half_rink_length, -half_nzone_length),

      # Default case
      c(-half_rink_length, half_rink_length)
    )

    # Adjust the x limits of the plot per the specified x translation
    xlims <- xlims + x_trans
  }

  if (is.null(ylims)) {
    ylims <- switch(tolower(display_range),
      # Full surface
      "full" = c(-(half_rink_width), half_rink_width),

      # Half-rink plots
      "offense" = c(-half_rink_width, half_rink_width),
      "offence" = c(-half_rink_width, half_rink_width),
      "defense" = c(-half_rink_width, half_rink_width),
      "defence" = c(-half_rink_width, half_rink_width),

      # Neutral zone
      "nzone" = c(-half_rink_width, half_rink_width),
      "neutral" = c(-half_rink_width, half_rink_width),
      "neutral_zone" = c(-half_rink_width, half_rink_width),
      "neutral zone" = c(-half_rink_width, half_rink_width),

      # Offensive zone
      "ozone" = c(-half_rink_width, half_rink_width),
      "offensive_zone" = c(-half_rink_width, half_rink_width),
      "offensive zone" = c(-half_rink_width, half_rink_width),
      "attacking_zone" = c(-half_rink_width, half_rink_width),
      "attacking zone" = c(-half_rink_width, half_rink_width),

      # Defensive zone
      "dzone" = c(-half_rink_width, half_rink_width),
      "defensive_zone" = c(-half_rink_width, half_rink_width),
      "defensive zone" = c(-half_rink_width, half_rink_width),
      "defending_zone" = c(-half_rink_width, half_rink_width),
      "defending zone" = c(-half_rink_width, half_rink_width),

      # Default case
      c(-half_rink_width, half_rink_width)
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
  rink_plot <- rink_plot +
    ggplot2::coord_fixed(
      xlim = xlims,
      ylim = ylims
    )

  # Return the ggplot2 instance
  return(rink_plot)
}
