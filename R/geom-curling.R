#' Set the colors to be used for the plot. The values provided in the arguments
#' are the defaults, and, where specified, are the rule-book specified values.
#'
#' Hexadecimal values are the passed vales to this function by default, but it
#' is also possible to use string-named values (e.g. \code{"dodgerblue"}) when
#' specifying.
#'
#' @param plot_background A hexadecimal string representing the color to use for
#'   this feature
#' @param end_1 A hexadecimal string representing the color to use for this
#'   feature
#' @param centre_zone A hexadecimal string representing the color to use for
#'   this feature
#' @param end_2 A hexadecimal string representing the color to use for this
#'   feature
#' @param sheet_apron A hexadecimal string representing the color to use for
#'   this feature
#' @param centre_line A hexadecimal string representing the color to use for
#'   this feature
#' @param tee_line A hexadecimal string representing the color to use for this
#'   feature
#' @param back_line A hexadecimal string representing the color to use for this
#'   feature
#' @param hog_line A hexadecimal string representing the color to use for this
#'   feature
#' @param hack_line A hexadecimal string representing the color to use for this
#'   feature
#' @param courtesy_line A hexadecimal string representing the color to use for
#'   this feature
#' @param hack A hexadecimal string representing the color to use for this
#'   feature
#' @param button A hexadecimal string representing the color to use for this
#'   feature
#' @param house_rings A vector of hexadecimal strings representing the color(s)
#'   to use for this feature
#'
#' @return A list of hexadecimal colors to use to color the features on the
#'   resulting plot
#'
#' @keywords internal
curling_features_set_colors <- function(plot_background = NULL,
                                        end_1 = "#ffffff",
                                        centre_zone = "#ffffff",
                                        end_2 = "#ffffff",
                                        sheet_apron = "#0033a0",
                                        centre_line = "#000000",
                                        tee_line = "#000000",
                                        back_line = "#000000",
                                        hog_line = "#c8102e",
                                        hack_line = "#000000",
                                        courtesy_line = "#000000",
                                        hack = "#000000",
                                        button = "#ffffff",
                                        house_rings = c(
                                          "#c8102e",
                                          "#ffffff",
                                          "#0033a0"
                                        )) {
  feature_colors <- list(
    plot_background = plot_background,
    end_1 = end_1,
    centre_zone = centre_zone,
    end_2 = end_2,
    sheet_apron = sheet_apron,
    centre_line = centre_line,
    tee_line = tee_line,
    back_line = back_line,
    hog_line = hog_line,
    hack_line = hack_line,
    courtesy_line = courtesy_line,
    hack = hack,
    button = button,
    house_rings = house_rings
  )

  return(feature_colors)
}

#' Generate a \code{ggplot2} instance containing a curling sheet for a specified
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
#'     \item{\code{"full"}}{The full sheet. This is the default}
#'     \item{\code{"in_bounds_only"}}{The full in-bounds area of the sheet}
#'     \item{\code{"in bounds only"}}{The full in-bounds area of the sheet}
#'     \item{\code{"house"}}{
#'       A single house, which defaults to the top house in TV view
#'     }
#'   }
#' @param sheet_updates A list of updates to the sheet's parameters. These will
#'   overwrite the parameters of the league
#' @param color_updates A list of updates to the sheet's default colors, which
#'   are set by [curling_features_set_colors()]
#' @param rotation An angle, given in degrees, through which the plot should be
#'   rotated
#' @param x_trans The amount that the \code{x} coordinates are to be shifted. By
#'   convention, the +\code{x} axis extends from the center of the sheet towards
#'   the right-hand goal when viewing the sheet in TV View
#' @param y_trans The amount that the \code{y} coordinates are to be shifted. By
#'   convention, the +\code{y} axis extends from the center of the sheet towards
#'   the top of the sheet when viewing the sheet in TV view
#' @param sheet_units The units with which to draw the sheet. The default is
#'   \code{NULL}, which will apply the rule-book specified units
#' @param xlims The limits on the final display in the \code{x} direction. The
#'   default is \code{NULL}, which will utilize the \code{xlims} specified by
#'   the \code{display_range} parameter
#' @param ylims The limits on the final display in the \code{y} direction. The
#'   default is \code{NULL}, which will utilize the \code{ylims} specified by
#'   the \code{display_range} parameter
#'
#' @return A \code{ggplot2} instance with a full-surface representation of a
#'   curling sheet
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   geom_curling(league = "wcf", rotation = 270, display_range = "house")
#'   geom_curling(league = "wcf", sheet_units = "ft")
#' }
geom_curling <- function(league,
                         display_range = "full",
                         sheet_updates = list(),
                         color_updates = list(),
                         rotation = 0,
                         x_trans = 0,
                         y_trans = 0,
                         sheet_units = NULL,
                         xlims = NULL,
                         ylims = NULL) {
  # Input cleansing and data gathering -----------------------------------------

  # If no league is supplied, error and alert user
  if (missing(league)) {
    stop(
      glue::glue(
        "league parameter must be supplied. \"custom\" is a valid league if ",
        "you wish to specify your own sheet parameterization, but you must ",
        "use the sheet_updates parameter to do so"
      )
    )
  }

  # Force the league to be all lower case
  league <- tolower(league)

  # Get the dimensions for the specified league
  sheet_params <- surface_dimensions[["curling"]][[league]]

  # If the provided league is not currently supported, alert the user. This will
  # manifest by having the parameters list be NULL
  if (is.null(sheet_params)) {
    stop(
      glue::glue(
        "Sorry, {toupper(league)} is not a viable league to plot ",
        "at this time. Please create an issue on GitHub with the league's ",
        "playing surface specifications for the league to be added to the ",
        "package"
      )
    )
  }

  # Update the sheet parameters as necessary
  sheet_params <- utils::modifyList(sheet_params, sheet_updates)

  # Order the radii of the house rings
  house_ring_radii <- sheet_params$house_ring_radii[
    rev(
      order(
        sheet_params$house_ring_radii
      )
    )
  ]

  # Feature initialization -----------------------------------------------------
  sheet_features <- list(

    ## Surface Base Features ---------------------------------------------------

    #### Ends ####
    end_1 = curling_end(
      sheet_length = sheet_params$sheet_length %or% 0,
      sheet_width = sheet_params$sheet_width %or% 0,
      tee_line_to_center = sheet_params$tee_line_to_center %or% 0,
      hog_line_to_tee_line = sheet_params$hog_line_to_tee_line %or% 0,
      drawn_direction = "downward"
    ),

    end_2 = curling_end(
      sheet_length = sheet_params$sheet_length %or% 0,
      sheet_width = sheet_params$sheet_width %or% 0,
      tee_line_to_center = sheet_params$tee_line_to_center %or% 0,
      hog_line_to_tee_line = sheet_params$hog_line_to_tee_line %or% 0,
      drawn_direction = "upward"
    ),

    #### Centre Zone ####
    centre_zone = curling_centre_zone(
      sheet_width = sheet_params$sheet_width %or% 0,
      tee_line_to_center = sheet_params$tee_line_to_center %or% 0,
      hog_line_to_tee_line = sheet_params$hog_line_to_tee_line %or% 0
    ),

    ## Surface Boundaries ------------------------------------------------------

    #### Sheet Apron ####
    sheet_apron = curling_sheet_apron(
      sheet_length = sheet_params$sheet_length %or% 0,
      sheet_width = sheet_params$sheet_width %or% 0,
      apron_behind_back = sheet_params$apron_behind_back %or% 0,
      apron_along_side = sheet_params$apron_along_side %or% 0
    ),

    ## Surface Lines -----------------------------------------------------------

    #### Centre Line ####
    centre_line = curling_centre_line(
      line_thickness = sheet_params$centre_line_thickness %or% 0,
      tee_line_to_center = sheet_params$tee_line_to_center %or% 0,
      centre_line_extension = sheet_params$centre_line_extension %or% 0
    ),

    #### Tee Line ####
    tee_line = curling_tee_line(
      line_thickness = sheet_params$tee_line_thickness %or% 0,
      sheet_width = sheet_params$sheet_width %or% 0
    ),

    #### Back Line ####
    back_line = curling_back_line(
      line_thickness = sheet_params$back_line_thickness %or% 0,
      sheet_width = sheet_params$sheet_width %or% 0
    ),

    #### Hog Line ####
    hog_line = curling_hog_line(
      line_thickness = sheet_params$hog_line_thickness %or% 0,
      sheet_width = sheet_params$sheet_width %or% 0
    ),

    #### Hack Line ####
    hack_line = curling_hack_line(
      line_thickness = sheet_params$hack_line_thickness %or% 0,
      hack_width = (2 * (sheet_params$hack_foothold_width %or% 0)) +
        (sheet_params$hack_foothold_gap %or% 0)
    ),

    #### Courtesy Line ####
    courtesy_line = curling_courtesy_line(
      line_thickness = sheet_params$courtesy_line_thickness %or% 0,
      line_length = sheet_params$courtesy_line_length %or% 0
    ),

    ## Surface Features --------------------------------------------------------

    #### Hack ####
    hack = curling_hack_foothold(
      foothold_depth = sheet_params$hack_foothold_depth %or% 0,
      foothold_width = sheet_params$hack_foothold_width %or% 0
    ),

    #### Button ####
    button = curling_button(
      feature_radius = sheet_params$button_radius %or% 0
    )
  )

  #### House Rings ####
  for(i in seq_along(house_ring_radii)) {
    sheet_features[[glue::glue("house_ring_radius_{i}")]] <- curling_house_ring(
      feature_radius = house_ring_radii[i]
    )
  }

  # Coordinate Transformations -------------------------------------------------

  # Convert the units as needed
  if (
    !is.null(sheet_units) &&
    tolower(sheet_params$sheet_units %or% "ft") != tolower(sheet_units)
  )  {
    sheet_features <- lapply(
      sheet_features,
      convert_units,
      from_unit = sheet_params$sheet_units %or% "ft",
      to_unit = sheet_units,
      conversion_columns = c("x", "y")
    )
  }

  # Generate the Plot ----------------------------------------------------------

  # Start by getting the colors to use to make the plot
  feature_colors <- curling_features_set_colors()

  # Update the features' colors as specified by the user
  feature_colors <- utils::modifyList(feature_colors, color_updates)

  # Create the base of the plot
  sheet_plot <- create_plot_base(
    plot_background = feature_colors$plot_background
  )

  #### Sheet Apron ####
  sheet_plot <- add_feature(
    sheet_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = sheet_features$sheet_apron,
    feature_color = feature_colors$sheet_apron,
    feature_outline_color = feature_colors$sheet_apron,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### End 1 ####
  sheet_plot <- add_feature(
    sheet_plot,
    x_anchor = 0,
    y_anchor = -(sheet_params$tee_line_to_center %or% 0) +
      (sheet_params$hog_line_to_tee_line %or% 0),
    feature_df = sheet_features$end_1,
    feature_color = feature_colors$end_1,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Centre Zone ####
  sheet_plot <- add_feature(
    sheet_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = sheet_features$centre_zone,
    feature_color = feature_colors$centre_zone,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### End 2 ####
  sheet_plot <- add_feature(
    sheet_plot,
    x_anchor = 0,
    y_anchor = (sheet_params$tee_line_to_center %or% 0) -
      (sheet_params$hog_line_to_tee_line %or% 0),
    feature_df = sheet_features$end_2,
    feature_color = feature_colors$end_2,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### House Rings ####
  for(i in seq_along(house_ring_radii)) {
    house_ring_df <- sheet_features[[glue::glue("house_ring_radius_{i}")]]
    sheet_plot <- add_feature(
      sheet_plot,
      x_anchor = 0,
      y_anchor = sheet_params$tee_line_to_center %or% 0,
      feature_df = house_ring_df,
      feature_color = feature_colors$house_rings[i],
      reflect_x = FALSE,
      reflect_y = TRUE,
      x_trans = x_trans,
      y_trans = y_trans,
      rotation = rotation
    )
  }

  #### Button ####
  sheet_plot <- add_feature(
    sheet_plot,
    x_anchor = 0,
    y_anchor = sheet_params$tee_line_to_center %or% 0,
    feature_df = sheet_features$button,
    feature_color = feature_colors$button,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Tee Line ####
  sheet_plot <- add_feature(
    sheet_plot,
    x_anchor = 0,
    y_anchor = sheet_params$tee_line_to_center %or% 0,
    feature_df = sheet_features$tee_line,
    feature_color = feature_colors$tee_line,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Back Line ####
  sheet_plot <- add_feature(
    sheet_plot,
    x_anchor = 0,
    y_anchor = (sheet_params$tee_line_to_center %or% 0) +
      (sheet_params$back_line_to_tee_line %or% 0),
    feature_df = sheet_features$back_line,
    feature_color = feature_colors$back_line,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Hog Lines ####
  sheet_plot <- add_feature(
    sheet_plot,
    x_anchor = 0,
    y_anchor = (sheet_params$tee_line_to_center %or% 0) -
      (sheet_params$hog_line_to_tee_line %or% 0),
    feature_df = sheet_features$hog_line,
    feature_color = feature_colors$hog_line,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Centre Line ####
  sheet_plot <- add_feature(
    sheet_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = sheet_features$centre_line,
    feature_color = feature_colors$centre_line,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Hack Line ####
  sheet_plot <- add_feature(
    sheet_plot,
    x_anchor = 0,
    y_anchor = (sheet_params$tee_line_to_center %or% 0) +
      (sheet_params$centre_line_extension %or% 0),
    feature_df = sheet_features$hack_line,
    feature_color = feature_colors$hack_line,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Courtesy Lines ####
  sheet_plot <- add_feature(
    sheet_plot,
    x_anchor = (sheet_params$sheet_width %or% 0) / 2,
    y_anchor = (sheet_params$tee_line_to_center %or% 0) -
      (sheet_params$hog_line_to_tee_line %or% 0) -
      (sheet_params$courtesy_line_to_hog_line %or% 0),
    feature_df = sheet_features$courtesy_line,
    feature_color = feature_colors$courtesy_line,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Hack ####
  sheet_plot <- add_feature(
    sheet_plot,
    x_anchor = sheet_params$hack_foothold_gap %or% 0,
    y_anchor = (sheet_params$tee_line_to_center %or% 0) +
      (sheet_params$centre_line_extension %or% 0),
    feature_df = sheet_features$hack,
    feature_color = feature_colors$hack,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  # Set Display Range-----------------------------------------------------------
  half_sheet_length <- ((sheet_params$sheet_length %or% 0) / 2) +
    (sheet_params$apron_behind_back %or% 0) +
    5
  half_sheet_width <- ((sheet_params$sheet_width %or% 0) / 2) +
    (sheet_params$apron_along_side %or% 0) +
    5

  if (is.null(xlims)) {
    xlims <- switch(
      tolower(display_range),

      # Full surface
      "full" = c(-half_sheet_width, half_sheet_width),
      "in_bounds_only" = c(
        -(
          ((sheet_params$sheet_width %or% 0) / 2) +
            0.5
        ),
        ((sheet_params$sheet_width %or% 0) / 2) +
          0.5
      ),
      "in bounds only" = c(
        -(
          ((sheet_params$sheet_width %or% 0) / 2) +
            0.5
        ),
        ((sheet_params$sheet_width %or% 0) / 2) +
          0.5
      ),

      # House
      "house" = c(-half_sheet_width, half_sheet_width),

      # Default case
      c(-half_sheet_width, half_sheet_width)
    )

    # Adjust the x limits of the plot per the specified x translation
    xlims <- xlims + x_trans
  }

  if (is.null(ylims)) {
    end_length <- (sheet_params$tee_line_to_center %or% 0) -
      (sheet_params$hog_line_to_tee_line %or% 0) -
      (sheet_params$courtesy_line_to_hog_line %or% 0)
    ylims <- switch(
      tolower(display_range),

      # Full surface
      "full" = c(-half_sheet_length, half_sheet_length),
      "in_bounds_only" = c(
        -(
          ((sheet_params$sheet_length %or% 0) / 2) +
            0.5
        ),
        ((sheet_params$sheet_length %or% 0) / 2) +
          0.5
      ),
      "in bounds only" = c(
        -(
          ((sheet_params$sheet_length %or% 0) / 2) +
            0.5
        ),
        ((sheet_params$sheet_length %or% 0) / 2) +
          0.5
      ),

      # House
      "house" = c(end_length, half_sheet_length),

      # Default case
      c(-half_sheet_length, half_sheet_length)
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
  sheet_plot <- sheet_plot +
    ggplot2::coord_fixed(
      xlim = xlims,
      ylim = ylims,
      expand = FALSE
    )

  # Return the ggplot2 instance
  return(sheet_plot)
}
