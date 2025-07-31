#' Set the colors to be used for the plot. The values provided in the arguments
#' are the defaults, and, where specified, are the rule-book specified values.
#'
#' Hexadecimal values are the passed vales to this function by default, but it
#' is also possible to use string-named values (e.g. \code{"dodgerblue"}) when
#' specifying.
#'
#' @param plot_background A hexadecimal string representing the color to use for
#'   this feature
#' @param back_boundary_line A hexadecimal string representing the color to use
#'   for this feature
#' @param singles_sideline A hexadecimal string representing the color to use
#'   for this feature
#' @param doubles_sideline A hexadecimal string representing the color to use
#'   for this feature
#' @param short_serviceline A hexadecimal string representing the color to use
#'   for this feature
#' @param backcourt A hexadecimal string representing the color to use for this
#'   feature
#' @param doubles_alley A hexadecimal string representing the color to use for
#'   this feature
#' @param court_apron A hexadecimal string representing the color to use for
#'   this feature
#' @param net A hexadecimal string representing the color to use for this
#'   feature
#'
#' @return A list of hexadecimal colors to use to color the features on the
#'   resulting plot
#'
#' @keywords internal
badminton_features_set_colors <- function(plot_background = "#ffffff",
                                          back_boundary_line = "#d3d3d3",
                                          singles_sideline = "#d3d3d3",
                                          doubles_sideline = "#d3d3d3",
                                          short_serviceline = "#d3d3d3",
                                          long_serviceline = "#d3d3d3",
                                          centerline = "#d3d3d3",
                                          backcourt = "#395d33",
                                          doubles_alley = "#395d33",
                                          court_apron = "#395d33",
                                          net = "#d3d3d3") {
  feature_colors <- list(
    plot_background = plot_background,
    back_boundary_line = back_boundary_line,
    singles_sideline = singles_sideline,
    doubles_sideline = doubles_sideline,
    short_serviceline = short_serviceline,
    long_serviceline = long_serviceline,
    centerline = centerline,
    backcourt = backcourt,
    doubles_alley = doubles_alley,
    court_apron = court_apron,
    net = net
  )

  return(feature_colors)
}

#' Generate a \code{ggplot2} instance containing a badminton court for a
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
#'     \item{\code{"full"}}{The full court. This is the default}
#'     \item{\code{"in_bounds_only"}}{The full in-bounds area of the court}
#'     \item{\code{"in bounds only"}}{The full in-bounds area of the court}
#'     \item{\code{"serve"}}{The serving half of the court}
#'     \item{\code{"serving"}}{The serving half of the court}
#'     \item{\code{"servicehalf"}}{The serving half of the court}
#'     \item{\code{"service_half"}}{The serving half of the court}
#'     \item{\code{"service half"}}{The serving half of the court}
#'     \item{\code{"servinghalf"}}{The serving half of the court}
#'     \item{\code{"serving_half"}}{The serving half of the court}
#'     \item{\code{"serving half"}}{The serving half of the court}
#'     \item{\code{"receive"}}{The receiving half of the court}
#'     \item{\code{"receiving"}}{The receiving half of the court}
#'     \item{\code{"receivicehalf"}}{The receiving half of the court}
#'     \item{\code{"receivice_half"}}{The receiving half of the court}
#'     \item{\code{"receivice half"}}{The receiving half of the court}
#'     \item{\code{"receivinghalf"}}{The receiving half of the court}
#'     \item{\code{"receiving_half"}}{The receiving half of the court}
#'     \item{\code{"receiving half"}}{The receiving half of the court}
#'   }
#' @param court_updates A list of updates to the courts' parameters. These will
#'   overwrite the parameters of the league
#' @param color_updates A list of updates to the courts' default colors, which
#'   are set by [badminton_features_set_colors()]
#' @param rotation An angle, given in degrees, through which the plot should be
#'   rotated
#' @param x_trans The amount that the \code{x} coordinates are to be shifted. By
#'   convention, the +\code{x} axis extends from the center of the court towards
#'   the right-hand serviceline when viewing the court in TV View
#' @param y_trans The amount that the \code{y} coordinates are to be shifted. By
#'   convention, the +\code{y} axis extends from the center of the court towards
#'   the sideline when viewing the court in TV view
#' @param court_units The units with which to draw the court. The default is
#'   \code{NULL}, which will apply the rule-book specified units
#' @param xlims The limits on the final display in the \code{x} direction. The
#'   default is \code{NULL}, which will utilize the \code{xlims} specified by
#'   the \code{display_range} parameter
#' @param ylims The limits on the final display in the \code{y} direction. The
#'   default is \code{NULL}, which will utilize the \code{ylims} specified by
#'   the \code{display_range} parameter
#'
#' @return A \code{ggplot2} instance with a full-surface representation of a
#'   badminton court
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   geom_badminton(league = "bwf", court_units = "m")
#' }
geom_badminton <- function(league,
                        display_range = "full",
                        court_updates = list(),
                        color_updates = list(),
                        rotation = 0,
                        x_trans = 0,
                        y_trans = 0,
                        court_units = NULL,
                        xlims = NULL,
                        ylims = NULL) {
  # Input cleansing and data gathering -----------------------------------------

  # If no league is supplied, error and alert user
  if (missing(league)) {
    stop(
      glue::glue(
        "league parameter must be supplied. \"custom\" is a valid league if ",
        "you wish to specify your own court parameterization, but you must ",
        "use the court_updates parameter to do so"
      )
    )
  }

  # Force the league to be all lower case
  league <- tolower(league)

  # Get the dimensions for the specified league
  court_params <- surface_dimensions[["badminton"]][[league]]

  # If the provided league is not currently supported, alert the user. This will
  # manifest by having the parameters list be NULL

  if (is.null(court_params)) {
    stop(
      glue::glue(
        "Sorry, {toupper(league)} is not a viable league to plot ",
        "at this time. Please create an issue on GitHub with the league's ",
        "playing surface specifications for the league to be added to the ",
        "package"
      )
    )
  }

  # Update the court parameters as necessary
  court_params <- utils::modifyList(court_params, court_updates)

  # Start by getting the colors to use to make the plot
  feature_colors <- badminton_features_set_colors()

  # Update the features' colors as specified by the user
  feature_colors <- utils::modifyList(feature_colors, color_updates)

  # Feature initialization -----------------------------------------------------
  court_features <- list(

    ## Surface Base Features ---------------------------------------------------

    #### Forecourt Half (Left or Right Court) ####
    forecourt_half = badminton_forecourt_half(
      short_serviceline_distance =
        court_params$short_serviceline_distance %or% 0,
      singles_width = court_params$singles_width %or% 0
    ),

    #### Backcourt ####
    backcourt = badminton_backcourt(
      court_length = court_params$court_length %or% 0,
      long_serviceline_distance = court_params$long_serviceline_distance %or% 0,
      singles_width = court_params$singles_width %or% 0
    ),

    #### Doubles Alley ####
    doubles_alley = badminton_doubles_alley(
      court_length = court_params$court_length %or% 0,
      feature_thickness = (
        (court_params$doubles_width %or% 0) -
          (court_params$singles_width %or% 0)
      ) / 2
    ),

    ## Surface Boundaries ------------------------------------------------------

    #### Court Apron ####
    court_apron = badminton_court_apron(
      court_length = court_params$court_length %or% 0,
      court_width = court_params$doubles_width %or% 0,
      backstop_distance = court_params$backstop_distance %or% 0,
      sidestop_distance = court_params$sidestop_distance %or% 0
    ),

    ## Surface Lines -----------------------------------------------------------

    #### back_boundary_line ####
    back_boundary_line = badminton_back_boundary_line(
      court_width = court_params$doubles_width %or% 0,
      feature_thickness = court_params$line_thickness %or% 0
    ),

    #### Short Serviceline ####
    short_serviceline = badminton_short_serviceline(
      doubles_width = court_params$doubles_width %or% 0,
      line_thickness = court_params$line_thickness %or% 0
    ),

    #### Long Serviceline ####
    long_serviceline = badminton_long_serviceline(
      doubles_width = court_params$doubles_width %or% 0,
      line_thickness = court_params$line_thickness %or% 0
    ),

    #### Sideline ####
    sideline = badminton_sideline(
      court_length = court_params$court_length %or% 0,
      feature_thickness = court_params$line_thickness %or% 0
    ),

    #### Center Line ####
    centerline = badminton_centerline(
      short_serviceline_distance =
        court_params$short_serviceline_distance %or% 0,
      court_length = court_params$court_length %or% 0,
      feature_thickness = court_params$line_thickness %or% 0
    ),

    ## Surface Features --------------------------------------------------------

    #### Net ####
    net = badminton_net(
      feature_thickness = court_params$line_thickness %or% 0,
      net_length = court_params$net_length %or% 0
    )
  )

  # Coordinate Transformations -------------------------------------------------

  # Convert the units as needed
  if (
    !is.null(court_units) &&
    tolower(court_params$court_units %or% "m") != tolower(court_units)
  )  {
    court_features <- lapply(
      court_features,
      convert_units,
      from_unit = court_params$court_units %or% "m",
      to_unit = court_units,
      conversion_columns = c("x", "y")
    )
  }

  # Generate the Plot ----------------------------------------------------------

  # Create the base of the plot
  court_plot <- create_plot_base(
    plot_background = feature_colors$plot_background
  )

  # Add the features to the plot

  #### Court Apron ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = court_features$court_apron,
    feature_color = feature_colors$court_apron,
    feature_outline_color = feature_colors$court_apron,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Doubles Alley ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = 0,
    y_anchor = (court_params$singles_width %or% 0) / 2,
    feature_df = court_features$doubles_alley,
    feature_color = feature_colors$doubles_alley,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Backcourt ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = court_params$long_serviceline_distance %or% 0,
    y_anchor = 0,
    feature_df = court_features$backcourt,
    feature_color = feature_colors$backcourt,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Back Boundary Line ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = (court_params$court_length %or% 0) / 2,
    y_anchor = 0,
    feature_df = court_features$back_boundary_line,
    feature_color = feature_colors$back_boundary_line,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Doubles Sideline ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = 0,
    y_anchor = (court_params$doubles_width %or% 0) / 2,
    feature_df = court_features$sideline,
    feature_color = feature_colors$doubles_sideline,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Singles Sideline ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = 0,
    y_anchor = (court_params$singles_width %or% 0),
    feature_df = court_features$sideline,
    feature_color = feature_colors$singles_sideline,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Short Serviceline ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = court_params$short_serviceline_distance %or% 0,
    y_anchor = 0,
    feature_df = court_features$short_serviceline,
    feature_color = feature_colors$short_serviceline,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Long Serviceline ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = court_params$long_serviceline_distance %or% 0,
    y_anchor = 0,
    feature_df = court_features$long_serviceline,
    feature_color = feature_colors$long_serviceline,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Centerline ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = 0,
    y_anchor = court_params$centerline_length %or% 0,
    feature_df = court_features$centerline,
    feature_color = feature_colors$centerline,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Net ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = court_features$net,
    feature_color = feature_colors$net,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  # Set Display Range ----------------------------------------------------------
  half_court_length <- ((court_params$court_length %or% 0) / 2) +
    (court_params$backstop_distance %or% 2) + 1

  half_court_width <- ((court_params$doubles_width %or% 0) / 2) +
    (court_params$sidestop_distance %or% 1) + 1

  if (is.null(xlims)) {
    xlims <- switch(
      tolower(display_range),

      # Full surface
      "full" = c(-half_court_length, half_court_length),
      "in_bounds_only" = c(
        -(
          ((court_params$court_length %or% 0) / 2) +
            (court_params$line_thickness %or% 0)
        ),
        ((court_params$court_length %or% 0) / 2) +
          (court_params$line_thickness %or% 0)
      ),
      "in bounds only" = c(
        -(
          ((court_params$court_length %or% 0) / 2) +
            (court_params$line_thickness %or% 0)
        ),
        ((court_params$court_length %or% 0) / 2) +
          (court_params$line_thickness %or% 0)
      ),

      # Service half
      "serve" = c(-half_court_length, 1.5),
      "serving" = c(-half_court_length, 1.5),
      "servicehalf" = c(-half_court_length, 1.5),
      "service_half" = c(-half_court_length, 1.5),
      "service half" = c(-half_court_length, 1.5),
      "servinghalf" = c(-half_court_length, 1.5),
      "serving_half" = c(-half_court_length, 1.5),
      "serving half" = c(-half_court_length, 1.5),

      # Receiving half
      "receive" = c(-1.5, half_court_length),
      "receiving" = c(-1.5, half_court_length),
      "receivicehalf" = c(-1.5, half_court_length),
      "receivice_half" = c(-1.5, half_court_length),
      "receivice half" = c(-1.5, half_court_length),
      "receivinghalf" = c(-1.5, half_court_length),
      "receiving_half" = c(-1.5, half_court_length),
      "receiving half" = c(-1.5, half_court_length),

      # Default case
      c(-half_court_length, half_court_length)
    )

    # Adjust the x limits of the plot per the specified x translation
    xlims <- xlims + x_trans
  }

  if (is.null(ylims)) {
    ylims <- switch(
      tolower(display_range),

      # Full surface
      "full" = c(-half_court_width, half_court_width),
      "in_bounds_only" = c(
        -(
          ((court_params$doubles_width %or% 0) / 2) +
            (court_params$line_thickness %or% 0)
        ),
        ((court_params$doubles_width %or% 0) / 2) +
          (court_params$line_thickness %or% 0)
      ),
      "in bounds only" = c(
        -(
          ((court_params$doubles_width %or% 0) / 2) +
            (court_params$line_thickness %or% 0)
        ),
        ((court_params$doubles_width %or% 0) / 2) +
          (court_params$line_thickness %or% 0)
      ),

      # Service half
      "serve" = c(-half_court_width, half_court_width),
      "serving" = c(-half_court_width, half_court_width),
      "servicehalf" = c(-half_court_width, half_court_width),
      "service_half" = c(-half_court_width, half_court_width),
      "service half" = c(-half_court_width, half_court_width),
      "servinghalf" = c(-half_court_width, half_court_width),
      "serving_half" = c(-half_court_width, half_court_width),
      "serving half" = c(-half_court_width, half_court_width),

      # Receiving half
      "receive" = c(-half_court_width, half_court_width),
      "receiving" = c(-half_court_width, half_court_width),
      "receiving_half" = c(-half_court_width, half_court_width),
      "receiving_half" = c(-half_court_width, half_court_width),
      "receiving half" = c(-half_court_width, half_court_width),
      "receivinghalf" = c(-half_court_width, half_court_width),
      "receiving_half" = c(-half_court_width, half_court_width),
      "receiving half" = c(-half_court_width, half_court_width),

      # Default case
      c(-half_court_width, half_court_width)
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
  court_plot <- court_plot +
    ggplot2::coord_fixed(
      xlim = xlims,
      ylim = ylims,
      expand = FALSE
    )

  # Return the ggplot2 instance
  return(court_plot)
}
