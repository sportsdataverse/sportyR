#' Create the base for the \code{ggplot2} instance with the correct theme
#' elements
#'
#' @param plot_background A hexadecimal string representing the color to use
#'   for the plot's background. Default: \code{NULL}
#'
#' @return A \code{ggplot2} instance onto which the features will be added
create_plot_base <- function(plot_background = NULL) {
  if (!is.null(plot_background)) {
    # If the plot has a specific background color, set the element correctly
    background <- ggplot2::element_rect(fill = plot_background)
  } else {
    # Otherwise, use ggplot2::element_blank()
    background <- ggplot2::element_blank()
  }

  g <- ggplot2::ggplot() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(
        t = -1,
        r = 0,
        b = -1,
        l = 0,
        unit = "cm"
      ),
      plot.background = background,
      panel.border = ggplot2::element_blank(),
      panel.background = background,
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )

  # Return the ggplot2 instance
  return(g)
}

#' Add a surface's feature to a \code{ggplot2} instance
#'
#' @param g The \code{ggplot2} instance onto which the feature will be added
#' @param x_anchor The anchor point along the \code{x} axis for the feature
#' @param y_anchor The anchor point along the \code{y} axis for the feature
#' @param feature_df The data frame containing the points to add to the feature
#' @param feature_color A hexadecimal string with which to color the feature
#'   once added to the plot
#' @param feature_outline_color A hexadecimal string with which to color the
#'   outline of the feature added to the plot. The default value is
#'   \code{"#ffffff00"}, which is white with a 0% alpha value. This results in
#'   no outline being added, which is usually desirable, but may be overwritten
#'   to prevent "seams" from appearing in the resulting plot
#' @param reflect_x Whether or not to reflect the feature over the \code{x} axis
#' @param reflect_y Whether or not to reflect the feature over the \code{y} axis
#' @param group A grouping to pass along to [ggplot2::aes()]. This is used
#'   for speed in the NFL and NCAA Football plotting functions
#'
#' @return A \code{ggplot2} instance with the feature added to it
#'
#' @keywords internal
add_feature <- function(g,
                        x_anchor,
                        y_anchor,
                        feature_df,
                        feature_color,
                        feature_outline_color = "#ffffff00",
                        reflect_x = FALSE,
                        reflect_y = FALSE,
                        x_trans = 0,
                        y_trans = 0,
                        rotation = 0,
                        group = NULL) {
  # Define the four possible shifted and rotated data frames to be added to the
  # plot
  df_1 <- feature_df
  df_1["x"] <- df_1["x"] + x_anchor + x_trans
  df_1["y"] <- df_1["y"] + y_anchor + y_trans

  df_2 <- feature_df
  df_2["x"] <- -(df_2["x"] + x_anchor) + x_trans
  df_2["y"] <- df_2["y"] + y_anchor + y_trans

  df_3 <- feature_df
  df_3["x"] <- -(df_3["x"] + x_anchor) + x_trans
  df_3["y"] <- -(df_3["y"] + y_anchor) + y_trans

  df_4 <- feature_df
  df_4["x"] <- df_4["x"] + x_anchor + x_trans
  df_4["y"] <- -(df_4["y"] + y_anchor) + y_trans

  df_1 <- rotate_coords(df_1, angle = rotation)
  df_2 <- rotate_coords(df_2, angle = rotation)
  df_3 <- rotate_coords(df_3, angle = rotation)
  df_4 <- rotate_coords(df_4, angle = rotation)

  # Add the feature based on the reflections
  if (reflect_x && reflect_y) {
    g <- g +
      ggplot2::geom_polygon(
        ggplot2::aes(
          x = df_1$x,
          y = df_1$y,
          group = group
        ),
        fill = feature_color,
        color = feature_outline_color
      ) +
      ggplot2::geom_polygon(
        ggplot2::aes(
          x = df_2$x,
          y = df_2$y,
          group = group
        ),
        fill = feature_color,
        color = feature_outline_color
      ) +
      ggplot2::geom_polygon(
        ggplot2::aes(
          x = df_3$x,
          y = df_3$y,
          group = group
        ),
        fill = feature_color,
        color = feature_outline_color
      ) +
      ggplot2::geom_polygon(
        ggplot2::aes(
          x = df_4$x,
          y = df_4$y,
          group = group
        ),
        fill = feature_color,
        color = feature_outline_color
      )
  } else if (reflect_x && !reflect_y) {
    g <- g +
      ggplot2::geom_polygon(
        ggplot2::aes(
          x = df_1$x,
          y = df_1$y,
          group = group
        ),
        fill = feature_color,
        color = feature_outline_color
      ) +
      ggplot2::geom_polygon(
        ggplot2::aes(
          x = df_2$x,
          y = df_2$y,
          group = group
        ),
        fill = feature_color,
        color = feature_outline_color
      )
  } else if (!reflect_x && reflect_y) {
    g <- g +
      ggplot2::geom_polygon(
        ggplot2::aes(
          x = df_1$x,
          y = df_1$y,
          group = group
        ),
        fill = feature_color,
        color = feature_outline_color
      ) +
      ggplot2::geom_polygon(
        ggplot2::aes(
          x = df_4$x,
          y = df_4$y,
          group = group
        ),
        fill = feature_color,
        color = feature_outline_color
      )
  } else {
    g <- g +
      ggplot2::geom_polygon(
        ggplot2::aes(
          x = df_1$x,
          y = df_1$y,
          group = group
        ),
        fill = feature_color,
        color = feature_outline_color
      )
  }

  # Return the ggplot2 instance with the feature added appropriately
  return(g)
}
