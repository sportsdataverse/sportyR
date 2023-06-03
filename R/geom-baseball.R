#' Set the colors to be used for the plot. The values provided in the arguments
#' are the defaults, and, where specified, are the rule-book specified values.
#'
#' Hexadecimal values are the passed vales to this function by default, but it
#' is also possible to use string-named values (e.g. \code{"dodgerblue"}) when
#' specifying.
#'
#' @param plot_background A hexadecimal string representing the color to use for
#'   this feature
#' @param infield_dirt A hexadecimal string representing the color to use for
#'   this feature
#' @param infield_grass A hexadecimal string representing the color to use for
#'   this feature
#' @param pitchers_mound A hexadecimal string representing the color to use for
#'   this feature
#' @param base A hexadecimal string representing the color to use for this
#'   feature
#' @param pitchers_plate A hexadecimal string representing the color to use for
#'   this feature
#' @param batters_box A hexadecimal string representing the color to use for
#'   this feature
#' @param catchers_box A hexadecimal string representing the color to use for
#'   this feature
#' @param foul_line A hexadecimal string representing the color to use for this
#'   feature
#' @param running_lane A hexadecimal string representing the color to use for
#'   this feature
#'
#' @return A list of hexadecimal colors to use to color the features on the
#'   resulting plot
#'
#' @keywords internal
baseball_features_set_colors <- function(plot_background = "#395d33",
                                         infield_dirt = "#9b7653",
                                         infield_grass = "#395d33",
                                         pitchers_mound = "#9b7653",
                                         base = "#ffffff",
                                         pitchers_plate = "#ffffff",
                                         batters_box = "#ffffff",
                                         catchers_box = "#ffffff",
                                         foul_line = "#ffffff",
                                         running_lane = "#ffffff") {
  feature_colors <- list(
    plot_background = plot_background,
    infield_dirt = infield_dirt,
    infield_grass = infield_grass,
    pitchers_mound = pitchers_mound,
    base = base,
    pitchers_plate = pitchers_plate,
    batters_box = batters_box,
    catchers_box = catchers_box,
    foul_line = foul_line,
    running_lane = running_lane
  )

  return(feature_colors)
}

#' Generate a \code{ggplot2} instance containing a baseball field for a
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
#'     \item{\code{"infield"}}{The infield on the baseball field}
#'   }
#' @param field_updates A list of updates to the field's parameters. These will
#'   overwrite the parameters of the league
#' @param color_updates A list of updates to the field's default colors, which
#'   are set by [baseball_features_set_colors()]
#' @param rotation An angle, given in degrees, through which the plot should be
#'   rotated
#' @param x_trans The amount that the \code{x} coordinates are to be shifted. By
#'   convention, the +\code{x} axis extends from the back tip of home plate
#'   towards the left-handed batter's box (the first base side of the field)
#' @param y_trans The amount that the \code{y} coordinates are to be shifted. By
#'   convention, the +\code{y} axis extends from the back tip of home plate
#'   towards straight-away center field
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
#'   baseball field
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   geom_baseball(league = "MLB", rotation = 270, display_range = "infield")
#'   geom_baseball(league = "little league", field_units = "m")
#' }
geom_baseball <- function(league,
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
  field_params <- surface_dimensions[["baseball"]][[league]]

  # If the provided league is not currently supported, alert the user. This will
  # manifest by having the parameters list be NULL
  if (is.null(field_params)) {
    stop(
      glue::glue(
        "Sorry, {toupper(league)} is not a viable league to plot ",
        "at this time. Please create an issue on GitHub with the league's ",
        "playing surface specifications for the league to be added to the ",
        "package"
      )
    )
  }

  # Update the field parameters as necessary
  field_params <- utils::modifyList(field_params, field_updates)

  # Feature initialization -----------------------------------------------------
  field_features <- list(

    ## Surface Base Features ---------------------------------------------------

    #### Infield Dirt ####
    infield_dirt = baseball_infield_dirt(
      home_plate_circle_radius = field_params$home_plate_circle_radius %or% 0,
      foul_line_to_foul_grass = field_params$foul_line_to_foul_grass %or% 0,
      pitchers_plate_distance =
        field_params$pitchers_plate_front_to_home_plate %or% 0,
      infield_arc_radius = field_params$infield_arc_radius %or% 0
    ),

    #### Infield Grass ####
    infield_grass = baseball_infield_grass(
      home_plate_circle_radius = field_params$home_plate_circle_radius %or% 0,
      foul_line_to_infield_grass =
        field_params$foul_line_to_infield_grass %or% 0,
      baseline_distance = field_params$baseline_distance %or% 0,
      base_anchor_to_infield_grass =
        field_params$base_anchor_to_infield_grass_radius %or% 0
    ),

    #### Pitcher's Mound ####
    pitchers_mound = baseball_pitchers_mound(
      pitchers_mound_radius = field_params$pitchers_mound_radius %or% 0
    ),


    ## Surface Boundaries ------------------------------------------------------

    ## Surface Lines -----------------------------------------------------------

    #### Batter's Box ####
    batters_box = baseball_batters_box(
      batters_box_length = field_params$batters_box_length %or% 0,
      batters_box_width = field_params$batters_box_width %or% 0,
      batters_box_y_adj = field_params$batters_box_y_adj %or% 0,
      batters_box_thickness = field_params$line_width %or% 0
    ),

    #### Catcher's Box ####
    catchers_box = baseball_catchers_box(
      catchers_box_depth = field_params$catchers_box_depth %or% 0,
      catchers_box_width = field_params$catchers_box_width %or% 0,
      batters_box_length = field_params$batters_box_length %or% 0,
      batters_box_y_adj = field_params$batters_box_y_adj %or% 0,
      catchers_box_shape = field_params$catchers_box_shape %or% "rectangle",
      catchers_box_thickness = field_params$line_width %or% 0,
      home_plate_circle_radius = field_params$home_plate_circle_radius %or% 0
    ),

    #### First Base Line ####
    first_base_line = baseball_foul_line(
      is_line_1b = TRUE,
      line_distance = field_params$right_field_distance %or% 0,
      batters_box_length = field_params$batters_box_length %or% 0,
      batters_box_width = field_params$batters_box_width %or% 0,
      batters_box_y_adj = field_params$batters_box_y_adj %or% 0,
      home_plate_side_to_batters_box =
        field_params$home_plate_side_to_batters_box %or% 0,
      foul_line_thickness = field_params$line_width %or% 0
    ),

    #### Third Base Line ####
    third_base_line = baseball_foul_line(
      is_line_1b = FALSE,
      line_distance = field_params$left_field_distance %or% 0,
      batters_box_length = field_params$batters_box_length %or% 0,
      batters_box_width = field_params$batters_box_width %or% 0,
      batters_box_y_adj = field_params$batters_box_y_adj %or% 0,
      home_plate_side_to_batters_box =
        field_params$home_plate_side_to_batters_box %or% 0,
      foul_line_thickness = field_params$line_width %or% 0
    ),

    #### Running Lane ####
    running_lane = baseball_running_lane(
      running_lane_depth = field_params$running_lane_depth %or% 0,
      running_lane_length = field_params$running_lane_length %or% 0,
      running_lane_start_distance =
        field_params$running_lane_start_distance %or% 0,
      running_lane_thickness = field_params$line_width %or% 0
    ),

    ## Surface Features --------------------------------------------------------

    #### Home Plate ####
    home_plate = baseball_home_plate(
      home_plate_edge_length = field_params$home_plate_edge_length %or% 0
    ),

    #### First Base ####
    first_base = baseball_base(
      base_side_length = field_params$base_side_length,
      adjust_x_left = TRUE,
      adjust_x_right = FALSE
    ),

    #### Second Base ####
    second_base = baseball_base(
      base_side_length = field_params$base_side_length,
      adjust_x_left = FALSE,
      adjust_x_right = FALSE
    ),

    #### Third Base ####
    third_base = baseball_base(
      base_side_length = field_params$base_side_length,
      adjust_x_left = FALSE,
      adjust_x_right = TRUE
    ),

    #### Pitcher's Plate ####
    pitchers_plate = baseball_pitchers_plate(
      pitchers_plate_length = field_params$pitchers_plate_length %or% 0,
      pitchers_plate_width = field_params$pitchers_plate_width %or% 0
    )
  )


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
  feature_colors <- baseball_features_set_colors()

  # Update the features' colors as specified by the user
  feature_colors <- utils::modifyList(feature_colors, color_updates)

  # Create the base of the plot
  field_plot <- create_plot_base(
    plot_background = feature_colors$plot_background
  )

  #### Infield Dirt ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$infield_dirt,
    feature_color = feature_colors$infield_dirt,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Infield Grass ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$infield_grass,
    feature_color = feature_colors$infield_grass,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Pitcher's Mound ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = field_params$pitchers_mound_center_to_home_plate %or% 0,
    feature_df = field_features$pitchers_mound,
    feature_color = feature_colors$pitchers_mound,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Home Plate ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$home_plate,
    feature_color = feature_colors$base,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### First Base ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = (field_params$baseline_distance %or% 0) * cos(pi / 4),
    y_anchor = (field_params$baseline_distance %or% 0) * sin(pi / 4),
    feature_df = field_features$first_base,
    feature_color = feature_colors$base,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Second Base ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = (field_params$baseline_distance %or% 0) * sqrt(2),
    feature_df = field_features$second_base,
    feature_color = feature_colors$base,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Third Base ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = (field_params$baseline_distance %or% 0) * cos((3 * pi) / 4),
    y_anchor = (field_params$baseline_distance %or% 0) * sin((3 * pi) / 4),
    feature_df = field_features$third_base,
    feature_color = feature_colors$base,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Pitcher's Plate ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = field_params$pitchers_plate_front_to_home_plate %or% 0,
    feature_df = field_features$pitchers_plate,
    feature_color = feature_colors$pitchers_plate,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Batter's Box ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = ((field_params$home_plate_edge_length %or% 0) / 2) +
      (field_params$home_plate_side_to_batters_box %or% 0) +
      ((field_params$batters_box_width %or% 0) / 2),
    y_anchor = 0,
    feature_df = field_features$batters_box,
    feature_color = feature_colors$batters_box,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Catcher's Box ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$catchers_box,
    feature_color = feature_colors$catchers_box,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### First Base Line ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$first_base_line,
    feature_color = feature_colors$foul_line,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Third Base Line ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$third_base_line,
    feature_color = feature_colors$foul_line,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Running Lane ####
  field_plot <- add_feature(
    field_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = field_features$running_lane,
    feature_color = feature_colors$running_lane,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  # Set Display Range-----------------------------------------------------------
  left_field_distance_x <- (
    (field_params$left_field_distance %or% 0) * cos(3 * pi / 4)
  )
  right_field_distance_x <- (
    (field_params$right_field_distance %or% 0) * cos(pi / 4)
  )
  left_infield_distance_x <- -(field_params$infield_arc_radius %or% 0) - 5
  right_infield_distance_x <- (field_params$infield_arc_radius %or% 0) + 5
  backstop_radius_y <- -(field_params$backstop_radius %or% 0) - 5
  center_field_distance_y <- (field_params$center_field_distance %or% 0) + 5
  home_plate_circle_y <- -(field_params$home_plate_circle_radius %or% 0) - 5
  infield_arc_y <- (field_params$pitchers_plate_front_to_home_plate %or% 0) +
    (field_params$infield_arc_radius %or% 0) +
    5


  if (is.null(xlims)) {
    xlims <- switch(
      tolower(display_range),

      # Full surface
      "full" = c(left_field_distance_x, right_field_distance_x),

      # Infield
      "infield" = c(
        left_infield_distance_x,
        right_infield_distance_x
      ),

      # Default case
      c(left_field_distance_x, right_field_distance_x)
    )

    # Adjust the x limits of the plot per the specified x translation
    xlims <- xlims + x_trans
  }

  if (is.null(ylims)) {
    ylims <- switch(
      tolower(display_range),

      # Full surface
      "full" = c(backstop_radius_y, center_field_distance_y),

      # Infield
      "infield" = c(home_plate_circle_y, infield_arc_y),

      # Default case
      c(backstop_radius_y, center_field_distance_y)
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
