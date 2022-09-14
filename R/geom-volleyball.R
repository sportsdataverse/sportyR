#' Set the colors to be used for the plot. The values provided in the arguments
#' are the defaults, and, where specified, are the rule-book specified values.
#'
#' Hexadecimal values are the passed vales to this function by default, but it
#' is also possible to use string-named values (e.g. \code{"dodgerblue"}) when
#' specifying.
#'
#' @param plot_background A hexadecimal string representing the color to use for
#'   this feature
#' @param free_zone A hexadecimal string representing the color to use for this
#'   feature
#' @param front_zone A hexadecimal string representing the color to use for this
#'   feature
#' @param defensive_backcourt A hexadecimal string representing the color to use
#'   for this feature
#' @param offensive_backcourt A hexadecimal string representing the color to use
#'   for this feature
#' @param court_apron A hexadecimal string representing the color to use for
#'   this feature
#' @param end_line A hexadecimal string representing the color to use for this
#'   feature
#' @param sideline A hexadecimal string representing the color to use for this
#'   feature
#' @param attack_line A hexadecimal string representing the color to use for
#'   this feature
#' @param center_line A hexadecimal string representing the color to use for
#'   this feature
#' @param service_zone_mark A hexadecimal string representing the color to use
#'   for this feature
#' @param substitution_zone A hexadecimal string representing the color to use
#'   for this feature
#'
#' @return A list of hexadecimal colors to use to color the features on the
#'   resulting plot
#'
#' @keywords internal
volleyball_features_set_colors <- function(plot_background = NULL,
                                           free_zone = "#d2ab6f",
                                           front_zone = "#d2ab6f",
                                           defensive_backcourt = "#d2ab6f",
                                           offensive_backcourt = "#d2ab6f",
                                           court_apron = "#d2ab6f",
                                           end_line = "#000000",
                                           sideline = "#000000",
                                           attack_line = "#000000",
                                           center_line = "#000000",
                                           service_zone_mark = "#000000",
                                           substitution_zone = "#000000") {
  feature_colors <- list(
    plot_background = plot_background,
    free_zone = free_zone,
    front_zone = front_zone,
    defensive_backcourt = defensive_backcourt,
    offensive_backcourt = offensive_backcourt,
    court_apron = court_apron,
    end_line = end_line,
    sideline = sideline,
    attack_line = attack_line,
    center_line = center_line,
    service_zone_mark = service_zone_mark,
    substitution_zone = substitution_zone
  )

  return(feature_colors)
}

#' Generate a \code{ggplot2} instance containing a volleyball court for a
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
#'       The full court. This is the default
#'     }
#'     \item{\code{"offense"}}{
#'       The offensive half of the court. This is the right half of the court
#'       in TV view
#'     }
#'     \item{\code{"offence"}}{
#'       The offensive half of the court. This is the right half of the court
#'       in TV view
#'     }
#'     \item{\code{"offensivehalfcourt"}}{
#'       The offensive half of the court. This is the right half of the court
#'       in TV view
#'     }
#'     \item{\code{"offensive_half_court"}}{
#'       The offensive half of the court. This is the right half of the court
#'       in TV view
#'     }
#'     \item{\code{"offensive half court"}}{
#'       The offensive half of the court. This is the right half of the court
#'       in TV view
#'     }
#'     \item{\code{"defense"}}{
#'       The defensive half of the court. This is the left half of the court in
#'       TV view
#'     }
#'     \item{\code{"defence"}}{
#'       The defensive half of the court. This is the left half of the court in
#'       TV view
#'     }
#'     \item{\code{"defensivehalfcourt"}}{
#'       The defensive half of the court. This is the left half of the court in
#'       TV view
#'     }
#'     \item{\code{"defensive_half_court"}}{
#'       The defensive half of the court. This is the left half of the court in
#'       TV view
#'     }
#'     \item{\code{"defensive half court"}}{
#'       The defensive half of the court. This is the left half of the court in
#'       TV view
#'     }
#'   }
#'
#' @param court_updates A list of updates to the courts' parameters. These will
#'   overwrite the parameters of the league
#' @param color_updates A list of updates to the courts' default colors, which
#'   are set by [volleyball_features_set_colors()]
#' @param rotation An angle, given in degrees, through which the plot should be
#'   rotated
#' @param x_trans The amount that the \code{x} coordinates are to be shifted. By
#'   convention, the +\code{x} axis extends from the center of the court towards
#'   the right-hand basket when viewing the court in TV View
#' @param y_trans The amount that the \code{y} coordinates are to be shifted. By
#'   convention, the +\code{y} axis extends from the center of the court towards
#'   the top of the court when viewing the court in TV view
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
#'   volleyball court
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   geom_volleyball(league = "NCAA", rotation = 270, display_range = "offense")
#'   geom_volleyball(league = "FIVB", court_units = "ft")
#' }
geom_volleyball <- function(league,
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
  court_params <- surface_dimensions[["volleyball"]][[league]]

  # Update the court parameters as necessary
  court_params <- utils::modifyList(court_params, court_updates)

  # Start by getting the colors to use to make the plot
  feature_colors <- volleyball_features_set_colors()

  # Update the features' colors as specified by the user
  feature_colors <- utils::modifyList(feature_colors, color_updates)

  # Feature initialization -----------------------------------------------------
  court_features <- list(

    ## Surface Base Features ---------------------------------------------------

    #### Free Zone ####
    free_zone = volleyball_free_zone(
      court_length = court_params$court_length %or% 0,
      court_width = court_params$court_width %or% 0,
      free_zone_end_line = court_params$free_zone_end_line %or% 0,
      free_zone_sideline = court_params$free_zone_sideline %or% 0
    ),

    #### Front Zone ####
    front_zone = volleyball_front_zone(
      attack_line_edge_to_center_line =
        court_params$attack_line_edge_to_center_line %or% 0,
      court_width = court_params$court_width %or% 0
    ),

    #### Backcourt ####
    backcourt = volleyball_backcourt(
      attack_line_edge_to_center_line =
        court_params$attack_line_edge_to_center_line %or% 0,
      court_length = court_params$court_length %or% 0,
      court_width = court_params$court_width %or% 0
    ),

    ## Surface Boundaries ------------------------------------------------------

    #### Court Apron ####
    court_apron = volleyball_court_apron(
      court_length = court_params$court_length %or% 0,
      court_width = court_params$court_width %or% 0,
      court_apron_end_line = court_params$court_apron_end_line %or% 0,
      court_apron_sideline = court_params$court_apron_sideline %or% 0
    ),

    #### End Line ####
    end_line = volleyball_end_line(
      court_width = court_params$court_width %or% 0,
      line_thickness = court_params$line_thickness %or% 0
    ),

    #### Sideline ####
    sideline = volleyball_sideline(
      court_length = court_params$court_length %or% 0,
      line_thickness = court_params$line_thickness %or% 0
    ),

    ## Surface Lines -----------------------------------------------------------

    #### Attack Line ####
    attack_line = volleyball_attack_line(
      court_width = court_params$court_width %or% 0,
      line_thickness = court_params$line_thickness %or% 0
    ),

    #### Center Line ####
    center_line = volleyball_center_line(
      court_width = court_params$court_width %or% 0,
      line_thickness = court_params$line_thickness %or% 0
    ),

    ## Surface Features --------------------------------------------------------

    #### Service Zone Mark ####
    service_zone_mark = volleyball_service_zone_mark(
      service_zone_mark_length = court_params$service_zone_mark_length %or% 0,
      line_thickness = court_params$line_thickness %or% 0
    ),

    #### Substitution Zone ####
    substitution_zone_dash = volleyball_substitution_zone_dash(
      dash_length = court_params$substitution_zone_dash_length %or% 0,
      line_thickness = court_params$line_thickness %or% 0
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

  # Create the base of the plot
  court_plot <- create_plot_base(
    plot_background = feature_colors$plot_background
  )

  # Add the features to the plot

  #### Free Zone ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = court_features$free_zone,
    feature_color = feature_colors$free_zone,
    feature_outline_color = feature_colors$free_zone,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Front Zone ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = court_features$front_zone,
    feature_color = feature_colors$front_zone,
    feature_outline_color = feature_colors$front_zone,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Defensive Backcourt ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = -(court_params$attack_line_edge_to_center_line %or% 0) -
      (
        (
          ((court_params$court_length %or% 0) / 2) -
            (court_params$attack_line_edge_to_center_line %or% 0)
        ) / 2
      ),
    y_anchor = 0,
    feature_df = court_features$backcourt,
    feature_color = feature_colors$defensive_backcourt,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Offensive Backcourt ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = (court_params$attack_line_edge_to_center_line %or% 0) +
      (
        (
          ((court_params$court_length %or% 0) / 2) -
          (court_params$attack_line_edge_to_center_line %or% 0)
        ) / 2
      ),
    y_anchor = 0,
    feature_df = court_features$backcourt,
    feature_color = feature_colors$offensive_backcourt,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

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

  #### Service Zone Marks ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = ((court_params$court_length %or% 0) / 2) +
      ((court_params$service_zone_mark_to_baseline %or% 0)),
    y_anchor = (court_params$court_width %or% 0) / 2,
    feature_df = court_features$service_zone_mark,
    feature_color = feature_colors$service_zone_mark,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Attack Lines ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = (court_params$attack_line_edge_to_center_line %or% 0),
    y_anchor = 0,
    feature_df = court_features$attack_line,
    feature_color = feature_colors$attack_line,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Substitution Zone ####
  substitution_zone_y_anchor <- ((court_params$court_width %or% 0) / 2) +
    (court_params$substitution_zone_dash_breaks %or% 0)
  for (i in seq_len(court_params$substitution_zone_rep_pattern %or% 1)) {
    court_plot <- add_feature(
      court_plot,
      x_anchor = (court_params$attack_line_edge_to_center_line %or% 0),
      y_anchor = substitution_zone_y_anchor,
      feature_df = court_features$substitution_zone,
      feature_color = feature_colors$substitution_zone,
      reflect_x = TRUE,
      reflect_y = TRUE,
      x_trans = x_trans,
      y_trans = y_trans,
      rotation = rotation
    )

    substitution_zone_y_anchor <- substitution_zone_y_anchor +
      (court_params$substitution_zone_dash_breaks %or% 0) +
      (court_params$substitution_zone_dash_length %or% 0)
  }

  #### Center Line ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = court_features$center_line,
    feature_color = feature_colors$center_line,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### End Lines ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = ((court_params$court_length %or% 0) / 2),
    y_anchor = 0,
    feature_df = court_features$end_line,
    feature_color = feature_colors$end_line,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Sidelines ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = 0,
    y_anchor = ((court_params$court_width %or% 0) / 2),
    feature_df = court_features$sideline,
    feature_color = feature_colors$sideline,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  # Set Display Range-----------------------------------------------------------
  half_court_length <- ((court_params$court_length %or% 0) / 2) +
    (court_params$free_zone_end_line %or% 0)

  half_court_width <- ((court_params$court_width %or% 0) / 2) +
    (court_params$free_zone_sideline %or% 0)

  if (is.null(xlims)) {
    xlims <- switch(tolower(display_range),
                    # Full surface
                    "full" = c(-half_court_length, half_court_length),

                    # Half-court plots
                    "offense" = c(0, half_court_length),
                    "offence" = c(0, half_court_length),
                    "offensivehalfcourt" = c(0, half_court_length),
                    "offensive_half_court" = c(0, half_court_length),
                    "offensive half court" = c(0, half_court_length),
                    "defense" = c(-half_court_length, 0),
                    "defence" = c(-half_court_length, 0),
                    "defensivehalfcourt" = c(-half_court_length, 0),
                    "defensive_half_court" = c(-half_court_length, 0),
                    "defensive half court" = c(-half_court_length, 0),

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

                    # Half-court plots
                    "offense" = c(-half_court_width, half_court_width),
                    "offence" = c(-half_court_width, half_court_width),
                    "offensivehalfcourt" = c(
                      -half_court_width,
                      half_court_width
                    ),
                    "offensive_half_court" = c(
                      -half_court_width,
                      half_court_width
                    ),
                    "offensive half court" = c(
                      -half_court_width,
                      half_court_width
                    ),
                    "defense" = c(-half_court_width, half_court_width),
                    "defence" = c(-half_court_width, half_court_width),
                    "defensivehalfcourt" = c(
                      -half_court_width,
                      half_court_width
                    ),
                    "defensive_half_court" = c(
                      -half_court_width,
                      half_court_width
                    ),
                    "defensive half court" = c(
                      -half_court_width,
                      half_court_width
                    ),

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
