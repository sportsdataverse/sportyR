#' Set the colors to be used for the plot. The values provided in the arguments
#' are the defaults, and, where specified, are the rule-book specified values.
#'
#' Hexadecimal values are the passed vales to this function by default, but it
#' is also possible to use string-named values (e.g. \code{"dodgerblue"}) when
#' specifying.
#'
#' @param plot_background A hexadecimal string representing the color to use for
#'   this feature
#' @param baseline A hexadecimal string representing the color to use for this
#'   feature
#' @param singles_sideline A hexadecimal string representing the color to use
#'   for this feature
#' @param doubles_sideline A hexadecimal string representing the color to use
#'   for this feature
#' @param serviceline A hexadecimal string representing the color to use for
#'   this feature
#' @param center_serviceline A hexadecimal string representing the color to use
#'   for this feature
#' @param center_mark A hexadecimal string representing the color to use for
#'   this feature
#' @param ad_court A hexadecimal string representing the color to use for this
#'   feature
#' @param deuce_court A hexadecimal string representing the color to use for
#'   this feature
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
tennis_features_set_colors <- function(plot_background = NULL,
                                       baseline = "#ffffff",
                                       singles_sideline = "#ffffff",
                                       doubles_sideline = "#ffffff",
                                       serviceline = "#ffffff",
                                       center_serviceline = "#ffffff",
                                       center_mark = "#ffffff",
                                       ad_court = "#395d33",
                                       deuce_court = "#395d33",
                                       backcourt = "#395d33",
                                       doubles_alley = "#395d33",
                                       court_apron = "#395d33",
                                       net = "#d3d3d3") {
  feature_colors <- list(
    plot_background = plot_background,
    baseline = baseline,
    singles_sideline = singles_sideline,
    doubles_sideline = doubles_sideline,
    serviceline = serviceline,
    center_serviceline = center_serviceline,
    center_mark = center_mark,
    ad_court = ad_court,
    deuce_court = deuce_court,
    backcourt = backcourt,
    doubles_alley = doubles_alley,
    court_apron = court_apron,
    net = net
  )

  return(feature_colors)
}

#' Generate a \code{ggplot2} instance containing a tennis court for a specified
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
#'     \item{\code{"full"}}{The full court. This is the default}
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
#'   are set by [tennis_features_set_colors()]
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
#'   tennis court
#'
#' @export
#'
#' @examples
#' geom_tennis(league = "USTA", rotation = 270, display_range = "serving")
#' geom_tennis(league = "itf", court_units = "m")
geom_tennis <- function(league,
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
  court_params <- surface_dimensions[["tennis"]][[league]]

  # Update the court parameters as necessary
  court_params <- utils::modifyList(court_params, court_updates)

  # Feature initialization -----------------------------------------------------
  court_features <- list(

    ## Surface Base Features ---------------------------------------------------

    #### Frontcourt Half (Ad or Deuce Court) ####
    frontcourt_half = tennis_frontcourt_half(
      serviceline_distance = court_params$serviceline_distance %or% 0,
      singles_width = court_params$singles_width %or% 0
    ),

    #### Backcourt ####
    backcourt = tennis_backcourt(
      court_length = court_params$court_length %or% 0,
      serviceline_distance = court_params$serviceline_distance %or% 0,
      singles_width = court_params$singles_width %or% 0
    ),

    #### Doubles Alley ####
    doubles_alley = tennis_doubles_alley(
      court_length = court_params$court_length %or% 0,
      feature_thickness = (
        (court_params$doubles_width %or% 0) -
          (court_params$singles_width %or% 0)
      ) / 2
    ),

    ## Surface Boundaries ------------------------------------------------------

    #### Baseline ####
    baseline = tennis_baseline(
      court_width = court_params$doubles_width %or% 0,
      feature_thickness = court_params$line_thickness %or% 0
    ),

    #### Sideline ####
    sideline = tennis_sideline(
      court_length = court_params$court_length %or% 0,
      feature_thickness = court_params$line_thickness %or% 0
    ),

    #### Court Apron ####
    court_apron = tennis_court_apron(
      court_length = court_params$court_length %or% 0,
      court_width = court_params$doubles_width %or% 0,
      backstop_distance = court_params$backstop_distance %or% 0,
      sidestop_distance = court_params$sidestop_distance %or% 0
    ),

    ## Surface Lines -----------------------------------------------------------

    #### Serviceline ####
    serviceline = tennis_serviceline(
      singles_width = court_params$singles_width %or% 0,
      feature_thickness = court_params$line_thickness %or% 0
    ),

    #### Center Serviceline ####
    center_serviceline = tennis_center_serviceline(
      center_serviceline_length = court_params$serviceline_distance %or% 0,
      feature_thickness = court_params$line_thickness %or% 0
    ),

    ## Surface Features --------------------------------------------------------

    #### Center Mark ####
    center_mark = tennis_center_mark(
      center_mark_length = court_params$center_mark_length %or% 0,
      feature_thickness = court_params$line_thickness %or% 0
    ),

    #### Net ####
    net = tennis_net(
      feature_thickness = court_params$line_thickness %or% 0,
      net_length = court_params$net_length %or% 0
    )
  )

  # Coordinate Transformations -------------------------------------------------

  # Convert the units as needed
  if (
    !is.null(court_units) &&
    tolower(court_params$court_units %or% "ft") != tolower(court_units)
  )  {
    court_features <- lapply(
      court_features,
      convert_units,
      from_unit = court_params$court_units %or% "ft",
      to_unit = court_units,
      conversion_columns = c("x", "y")
    )
  }

  # Generate the Plot ----------------------------------------------------------

  # Start by getting the colors to use to make the plot
  feature_colors <- tennis_features_set_colors()

  # Update the features' colors as specified by the user
  feature_colors <- utils::modifyList(feature_colors, color_updates)

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
    x_anchor = court_params$serviceline_distance %or% 0,
    y_anchor = 0,
    feature_df = court_features$backcourt,
    feature_color = feature_colors$backcourt,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Ad Court ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = 0,
    y_anchor = 0.25 * (court_params$singles_width %or% 0),
    feature_df = court_features$frontcourt_half,
    feature_color = feature_colors$ad_court,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  court_plot <- add_feature(
    court_plot,
    x_anchor = -court_params$serviceline_distance %or% 0,
    y_anchor = -0.25 * (court_params$singles_width %or% 0),
    feature_df = court_features$frontcourt_half,
    feature_color = feature_colors$ad_court,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Deuce Court ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = 0,
    y_anchor = -0.25 * (court_params$singles_width %or% 0),
    feature_df = court_features$frontcourt_half,
    feature_color = feature_colors$deuce_court,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  court_plot <- add_feature(
    court_plot,
    x_anchor = -court_params$serviceline_distance %or% 0,
    y_anchor = 0.25 * (court_params$singles_width %or% 0),
    feature_df = court_features$frontcourt_half,
    feature_color = feature_colors$deuce_court,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Baseline ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = (court_params$court_length %or% 0) / 2,
    y_anchor = 0,
    feature_df = court_features$baseline,
    feature_color = feature_colors$baseline,
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
    y_anchor = (court_params$singles_width %or% 0) / 2,
    feature_df = court_features$sideline,
    feature_color = feature_colors$singles_sideline,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Serviceline ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = court_params$serviceline_distance %or% 0,
    y_anchor = 0,
    feature_df = court_features$serviceline,
    feature_color = feature_colors$serviceline,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Center Serviceline ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = court_features$center_serviceline,
    feature_color = feature_colors$center_serviceline,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Center Mark ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = (court_params$court_length %or% 0) / 2,
    y_anchor = 0,
    feature_df = court_features$center_mark,
    feature_color = feature_colors$center_mark,
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

  # Set Display Range-----------------------------------------------------------
  half_court_length <- ((court_params$court_length %or% 0) / 2) +
    (court_params$backstop_distance %or% 20) +
    5

  half_court_width <- ((court_params$doubles_width %or% 0) / 2) +
    (court_params$sidestop_distance %or% 10) +
    5

  if (is.null(xlims)) {
    xlims <- switch(tolower(display_range),
                    # Full surface
                    "full" = c(-half_court_length, half_court_length),

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
    ylims <- switch(tolower(display_range),
                    # Full surface
                    "full" = c(-half_court_width, half_court_width),

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
                    "receivicehalf" = c(-half_court_width, half_court_width),
                    "receivice_half" = c(-half_court_width, half_court_width),
                    "receivice half" = c(-half_court_width, half_court_width),
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
      ylim = ylims
    )

  # Return the ggplot2 instance
  return(court_plot)
}
