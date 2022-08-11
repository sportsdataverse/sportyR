#' Set the colors to be used for the plot. The values provided in the arguments
#' are the defaults, and, where specified, are the rule-book specified values.
#'
#' Hexadecimal values are the passed vales to this function by default, but it
#' is also possible to use string-named values (e.g. \code{"dodgerblue"}) when
#' specifying.
#'
#' @param plot_background A hexadecimal string representing the color to use for
#'   this feature
#' @param touchline A hexadecimal string representing the color to use for this
#'   feature
#' @param goal_line A hexadecimal string representing the color to use for this
#'   feature
#' @param halfway_line A hexadecimal string representing the color to use for
#'   this feature
#' @param center_circle A hexadecimal string representing the color to use for
#'   this feature
#' @param center_mark A hexadecimal string representing the color to use for
#'   this feature
#' @param penalty_box A hexadecimal string representing the color to use for
#'   this feature
#' @param goal_box A hexadecimal string representing the color to use for this
#'   feature
#' @param penalty_mark A hexadecimal string representing the color to use for
#'   this feature
#' @param corner_defensive_mark A hexadecimal string representing the color to
#'   use for this feature
#' @param goal A hexadecimal string representing the color to use for this
#'   feature
#'
#' @return A list of hexadecimal colors to use to color the features on the
#'   resulting plot
#'
#' @keywords internal
soccer_features_set_colors <- function(plot_background = NULL,
                                       offensive_half_pitch = "#195f0c",
                                       defensive_half_pitch = "#195f0c",
                                       pitch_apron = "#195f0c",
                                       touchline = "#ffffff",
                                       goal_line = "#ffffff",
                                       corner_arc = "#ffffff",
                                       halfway_line = "#ffffff",
                                       center_circle = "#ffffff",
                                       center_mark = "#ffffff",
                                       penalty_box = "#ffffff",
                                       goal_box = "#ffffff",
                                       penalty_mark = "#ffffff",
                                       corner_defensive_mark = "#ffffff",
                                       goal = "#ffffff") {
  feature_colors <- list(
    plot_background = plot_background,
    offensive_half_pitch = offensive_half_pitch,
    defensive_half_pitch = defensive_half_pitch,
    pitch_apron = pitch_apron,
    touchline = touchline,
    goal_line = goal_line,
    corner_arc = corner_arc,
    halfway_line = halfway_line,
    center_circle = center_circle,
    center_mark = center_mark,
    penalty_box = penalty_box,
    goal_box = goal_box,
    penalty_mark = penalty_mark,
    corner_defensive_mark = corner_defensive_mark,
    goal = goal
  )

  return(feature_colors)
}

#' Generate a \code{ggplot2} instance containing a soccer pitch for a specified
#' league
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
#'     \item{\code{"full"}}{The full pitch. This is the default}
#'     \item{\code{"offense"}}{The TV-right half of the pitch}
#'     \item{\code{"offence"}}{The TV-right half of the pitch}
#'     \item{\code{"offensivehalfpitch"}}{The TV-right half of the pitch}
#'     \item{\code{"offensive_half_pitch"}}{The TV-right half of the pitch}
#'     \item{\code{"offensive half pitch"}}{The TV-right half of the pitch}
#'     \item{\code{"defense"}}{The TV-left half of the pitch}
#'     \item{\code{"defence"}}{The TV-left half of the pitch}
#'     \item{\code{"defensivehalfpitch"}}{The TV-left half of the pitch}
#'     \item{\code{"defensive_half_pitch"}}{The TV-left half of the pitch}
#'     \item{\code{"defensive half pitch"}}{The TV-left half of the pitch}
#'   }
#' @param pitch_updates A list of updates to the pitch's parameters. These will
#'   overwrite the parameters of the league
#' @param color_updates A list of updates to the pitch's default colors, which
#'   are set by [soccer_features_set_colors()]
#' @param rotation An angle, given in degrees, through which the plot should be
#'   rotated
#' @param x_trans The amount that the \code{x} coordinates are to be shifted. By
#'   convention, the +\code{x} axis extends from the center of the pitch towards
#'   the right-hand goal when viewing the pitch in TV View
#' @param y_trans The amount that the \code{y} coordinates are to be shifted. By
#'   convention, the +\code{y} axis extends from the center of the pitch towards
#'   the top of the pitch when viewing the pitch in TV view
#' @param pitch_units The units with which to draw the pitch. The default is
#'   \code{NULL}, which will apply the rule-book specified units
#' @param xlims The limits on the final display in the \code{x} direction. The
#'   default is \code{NULL}, which will utilize the \code{xlims} specified by
#'   the \code{display_range} parameter
#' @param ylims The limits on the final display in the \code{y} direction. The
#'   default is \code{NULL}, which will utilize the \code{ylims} specified by
#'   the \code{display_range} parameter
#'
#' @return A \code{ggplot2} instance with a full-surface representation of a
#'   soccer pitch
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   geom_soccer(league = "EPL", rotation = 270, display_range = "offense")
#'   geom_soccer(league = "fifa", pitch_units = "ft")
#' }
geom_soccer <- function(league,
                        display_range = "full",
                        pitch_updates = list(),
                        color_updates = list(),
                        rotation = 0,
                        x_trans = 0,
                        y_trans = 0,
                        pitch_units = NULL,
                        xlims = NULL,
                        ylims = NULL) {
  # Input cleansing and data gathering -----------------------------------------

  # If no league is supplied, error and alert user
  if (missing(league)) {
    stop(
      glue::glue(
        "league parameter must be supplied. \"custom\" is a valid league if ",
        "you wish to specify your own pitch parameterization, but you must ",
        "use the pitch_updates parameter to do so"
      )
    )
  }

  # Force the league to be all lower case
  league <- tolower(league)

  # Get the dimensions for the specified league
  pitch_params <- surface_dimensions[["soccer"]][[league]]

  # Update the pitch parameters as necessary
  pitch_params <- utils::modifyList(pitch_params, pitch_updates)

  # Feature initialization -----------------------------------------------------
  pitch_features <- list(

    ## Surface Base Features ---------------------------------------------------

    #### Defensive Half Pitch ####
    defensive_half_pitch = soccer_half_pitch(
      pitch_length = pitch_params$pitch_length %or% 0,
      pitch_width = pitch_params$pitch_width %or% 0
    ),

    #### Offensive Half Pitch ####
    offensive_half_pitch = soccer_half_pitch(
      pitch_length = pitch_params$pitch_length %or% 0,
      pitch_width = pitch_params$pitch_width %or% 0
    ),

    ## Surface Boundaries ------------------------------------------------------

    #### Pitch Apron ####
    pitch_apron = soccer_pitch_apron(
      pitch_length = pitch_params$pitch_length %or% 0,
      pitch_width = pitch_params$pitch_width %or% 0,
      pitch_apron_touchline = pitch_params$pitch_apron_touchline %or% 0,
      pitch_apron_goal_line = pitch_params$pitch_apron_goal_line %or% 0,
      goal_depth = pitch_params$goal_depth
    ),

    #### Touchline ####
    touchline = soccer_touchline(
      pitch_length = pitch_params$pitch_length %or% 0,
      feature_thickness = pitch_params$line_thickness %or% 0
    ),

    #### Goal Line ####
    goal_line = soccer_goal_line(
      pitch_width = pitch_params$pitch_width %or% 0,
      feature_thickness = pitch_params$line_thickness %or% 0
    ),

    ## Surface Lines -----------------------------------------------------------

    #### Halfway Line ####
    halfway_line = soccer_halfway_line(
      pitch_width = pitch_params$pitch_width %or% 0,
      feature_thickness = pitch_params$line_thickness %or% 0
    ),

    #### Penalty Box ####
    penalty_box = soccer_penalty_box(
      feature_radius = pitch_params$penalty_circle_radius %or% 0,
      feature_thickness = pitch_params$line_thickness %or% 0,
      box_length = pitch_params$penalty_box_length %or% 0,
      penalty_mark_dist = pitch_params$penalty_mark_dist %or% 0,
      goal_width = pitch_params$goal_width %or% 0,
      goal_post_to_box_edge =
        pitch_params$interior_of_goal_post_to_penalty_box %or% 0
    ),

    #### Goal Box ####
    goal_box = soccer_goal_box(
      feature_thickness = pitch_params$line_thickness %or% 0,
      box_length = pitch_params$goal_box_length %or% 0,
      goal_width = pitch_params$goal_width %or% 0,
      goal_post_to_box_edge =
        pitch_params$interior_of_goal_post_to_goal_box %or% 0
    ),

    ## Surface Features --------------------------------------------------------

    #### Corner Arc ####
    corner_arc = soccer_corner_arc(
      feature_radius = pitch_params$corner_arc_radius %or% 0,
      feature_thickness = pitch_params$line_thickness %or% 0
    ),

    #### Center Circle ####
    center_circle = soccer_center_circle(
      feature_radius = pitch_params$center_circle_radius %or% 0,
      feature_thickness = pitch_params$line_thickness %or% 0
    ),

    #### Center Mark ####
    center_mark = soccer_center_mark(
      feature_radius = pitch_params$center_mark_radius %or% 0
    ),

    #### Penalty Mark ####
    penalty_mark = soccer_penalty_mark(
      feature_radius = pitch_params$penalty_mark_radius %or% 0
    ),

    #### Touchline Corner Defensive Marks ####
    touchline_corner_defensive_mark = soccer_corner_defensive_marks(
      feature_thickness = pitch_params$line_thickness %or% 0,
      is_touchline = TRUE,
      is_goal_line = FALSE,
      depth = pitch_params$defensive_mark_depth %or% 0,
      separation_from_line =
        pitch_params$defensive_mark_separation_from_line %or% 0
    ),

    #### Goal Line Corner Defensive Marks ####
    goal_line_corner_defensive_mark = soccer_corner_defensive_marks(
      feature_thickness = pitch_params$line_thickness %or% 0,
      is_touchline = FALSE,
      is_goal_line = TRUE,
      depth = pitch_params$defensive_mark_depth %or% 0,
      separation_from_line =
        pitch_params$defensive_mark_separation_from_line %or% 0
    ),

    #### Goal ####
    goal = soccer_goal(
      feature_thickness = pitch_params$line_thickness %or% 0,
      goal_width = pitch_params$goal_width %or% 0,
      goal_depth = pitch_params$goal_depth %or% 0
    )
  )

  # Coordinate Transformations -------------------------------------------------

  # Convert the units as needed
  if (
    !is.null(pitch_units) &&
    tolower(pitch_params$pitch_units %or% "ft") != tolower(pitch_units)
  )  {
    pitch_features <- lapply(
      pitch_features,
      convert_units,
      from_unit = pitch_params$pitch_units %or% "ft",
      to_unit = pitch_units,
      conversion_columns = c("x", "y")
    )
  }

  # Generate the Plot ----------------------------------------------------------

  # Start by getting the colors to use to make the plot
  feature_colors <- soccer_features_set_colors()

  # Update the features' colors as specified by the user
  feature_colors <- utils::modifyList(feature_colors, color_updates)

  # Create the base of the plot
  pitch_plot <- create_plot_base(
    plot_background = feature_colors$plot_background
  )

  #### Defensive Half Pitch ####
  pitch_plot <- add_feature(
    pitch_plot,
    x_anchor = -0.25 * (pitch_params$pitch_length %or% 0),
    y_anchor = 0,
    feature_df = pitch_features$defensive_half_pitch,
    feature_color = feature_colors$defensive_half_pitch,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Offensive Half Pitch ####
  pitch_plot <- add_feature(
    pitch_plot,
    x_anchor = 0.25 * (pitch_params$pitch_length %or% 0),
    y_anchor = 0,
    feature_df = pitch_features$offensive_half_pitch,
    feature_color = feature_colors$offensive_half_pitch,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Pitch Apron ####
  pitch_plot <- add_feature(
    pitch_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = pitch_features$pitch_apron,
    feature_color = feature_colors$pitch_apron,
    feature_outline_color = feature_colors$pitch_apron,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Touchline ####
  pitch_plot <- add_feature(
    pitch_plot,
    x_anchor = 0,
    y_anchor = (pitch_params$pitch_width %or% 0) / 2,
    feature_df = pitch_features$touchline,
    feature_color = feature_colors$touchline,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Goal Line ####
  pitch_plot <- add_feature(
    pitch_plot,
    x_anchor = (pitch_params$pitch_length %or% 0) / 2,
    y_anchor = 0,
    feature_df = pitch_features$goal_line,
    feature_color = feature_colors$goal_line,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Corner Arc ####
  pitch_plot <- add_feature(
    pitch_plot,
    x_anchor = ((pitch_params$pitch_length %or% 0) / 2) -
      ((pitch_params$line_thickness %or% 0) / 2),
    y_anchor = ((pitch_params$pitch_width %or% 0) / 2) -
      ((pitch_params$line_thickness %or% 0) / 2),
    feature_df = pitch_features$corner_arc,
    feature_color = feature_colors$corner_arc,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Halfway Line ####
  pitch_plot <- add_feature(
    pitch_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = pitch_features$halfway_line,
    feature_color = feature_colors$halfway_line,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Penalty Box ####
  pitch_plot <- add_feature(
    pitch_plot,
    x_anchor = (pitch_params$pitch_length %or% 0) / 2,
    y_anchor = 0,
    feature_df = pitch_features$penalty_box,
    feature_color = feature_colors$penalty_box,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Goal Box ####
  pitch_plot <- add_feature(
    pitch_plot,
    x_anchor = (pitch_params$pitch_length %or% 0) / 2,
    y_anchor = 0,
    feature_df = pitch_features$goal_box,
    feature_color = feature_colors$goal_box,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Center Circle ####
  pitch_plot <- add_feature(
    pitch_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = pitch_features$center_circle,
    feature_color = feature_colors$center_circle,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Penalty Mark ####
  pitch_plot <- add_feature(
    pitch_plot,
    x_anchor = ((pitch_params$pitch_length %or% 0) / 2) -
      (pitch_params$penalty_mark_dist %or% 0),
    y_anchor = 0,
    feature_df = pitch_features$penalty_mark,
    feature_color = feature_colors$penalty_mark,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Center Mark ####
  pitch_plot <- add_feature(
    pitch_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = pitch_features$center_mark,
    feature_color = feature_colors$center_mark,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Touchline Corner Defensive Marks ####
  if (pitch_params$touchline_defensive_mark_visible) {
    pitch_plot <- add_feature(
      pitch_plot,
      x_anchor = ((pitch_params$pitch_length %or% 0) / 2) -
        (pitch_params$defensive_mark_distance %or% 0),
      y_anchor = (pitch_params$pitch_width %or% 0) / 2,
      feature_df = pitch_features$touchline_corner_defensive_mark,
      feature_color = feature_colors$corner_defensive_mark,
      reflect_x = TRUE,
      reflect_y = TRUE,
      x_trans = x_trans,
      y_trans = y_trans,
      rotation = rotation
    )
  }

  #### Goal Line Corner Defensive Marks ####
  if (pitch_params$goal_line_defensive_mark_visible) {
    pitch_plot <- add_feature(
      pitch_plot,
      x_anchor = (pitch_params$pitch_length %or% 0) / 2,
      y_anchor = ((pitch_params$pitch_width %or% 0) / 2) -
        (pitch_params$defensive_mark_distance %or% 0),
      feature_df = pitch_features$goal_line_corner_defensive_mark,
      feature_color = feature_colors$corner_defensive_mark,
      reflect_x = TRUE,
      reflect_y = TRUE,
      x_trans = x_trans,
      y_trans = y_trans,
      rotation = rotation
    )
  }

  #### Goal ####
  pitch_plot <- add_feature(
    pitch_plot,
    x_anchor = ((pitch_params$pitch_length %or% 0) / 2) -
      (pitch_params$line_thickness %or% 0),
    y_anchor = 0,
    feature_df = pitch_features$goal,
    feature_color = feature_colors$goal,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  # Set Display Range-----------------------------------------------------------
  half_pitch_length <- ((pitch_params$pitch_length %or% 0) / 2) + 5
  half_pitch_width <- ((pitch_params$pitch_width %or% 0) / 2) + 5

  if (is.null(xlims)) {
    xlims <- switch(tolower(display_range),
                    # Full surface
                    "full" = c(-half_pitch_length, half_pitch_length),

                    # Half-pitch plots
                    "offense" = c(0, half_pitch_length),
                    "offence" = c(0, half_pitch_length),
                    "offensivehalfpitch" = c(0, half_pitch_length),
                    "offensive_half_pitch" = c(0, half_pitch_length),
                    "offensive half pitch" = c(0, half_pitch_length),
                    "defense" = c(-half_pitch_length, 0),
                    "defence" = c(-half_pitch_length, 0),
                    "defensivehalfpitch" = c(-half_pitch_length, 0),
                    "defensive_half_pitch" = c(-half_pitch_length, 0),
                    "defensive half pitch" = c(-half_pitch_length, 0),

                    # Default case
                    c(-half_pitch_length, half_pitch_length)
    )

    # Adjust the x limits of the plot per the specified x translation
    xlims <- xlims + x_trans
  }

  if (is.null(ylims)) {
    ylims <- switch(tolower(display_range),
                    # Full surface
                    "full" = c(-half_pitch_width, half_pitch_width),

                    # Half-pitch plots
                    "offense" = c(-half_pitch_width, half_pitch_width),
                    "offence" = c(-half_pitch_width, half_pitch_width),
                    "offensivehalfpitch" = c(
                      -half_pitch_width,
                      half_pitch_width
                    ),
                    "offensive_half_pitch" = c(
                      -half_pitch_width,
                      half_pitch_width
                    ),
                    "offensive half pitch" = c(
                      -half_pitch_width,
                      half_pitch_width
                    ),
                    "defense" = c(-half_pitch_width, half_pitch_width),
                    "defence" = c(-half_pitch_width, half_pitch_width),
                    "defensivehalfpitch" = c(
                      -half_pitch_width,
                      half_pitch_width
                    ),
                    "defensive_half_pitch" = c(
                      -half_pitch_width,
                      half_pitch_width
                    ),
                    "defensive half pitch" = c(
                      -half_pitch_width,
                      half_pitch_width
                    ),

                    # Default case
                    c(-half_pitch_width, half_pitch_width)
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
  pitch_plot <- pitch_plot +
    ggplot2::coord_fixed(
      xlim = xlims,
      ylim = ylims
    )

  # Return the ggplot2 instance
  return(pitch_plot)
}
