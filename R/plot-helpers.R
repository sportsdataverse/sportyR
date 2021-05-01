#' Create the base for the ggplot2 instance with the correct theme elements
#'
#' @param rotate Whether or not the final output will be rotated (adjusts the
#'   margins). Default: \code{FALSE}
#' @param caption_color A hexadecimal string representing the color to use for
#'   the plot's caption. Default: '#707372' (grey)
#' @param background_color A hexadecimal string representing the color to use for
#'   the plot's background. Default: \code{NULL}
#'
#' @return A ggplot2 instance onto which the features will be added
create_plot_base = function(rotate = FALSE, caption_color = '#707372', background_color = NULL){
  if(is.null(caption_color)){
    # Ensure that the caption color is provided
    stop('Caption color must not be NULL')
  }

  if(!is.null(background_color)){
    # If the plot has a specific background color, set the element correctly
    background = ggplot2::element_rect(fill = background_color)
  }

  else {
    # Otherwise, use ggplot2::element_blank()
    background = ggplot2::element_blank()
  }

  if(rotate){
    g = ggplot2::ggplot() +
      ggplot2::coord_fixed() +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(color = caption_color, hjust = 0.5),
        plot.margin = ggplot2::margin(0, -1, 0, -1, "cm"),
        panel.border = ggplot2::element_blank(),
        panel.background = background,
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
      ) +
      ggplot2::labs(
        caption = "Plot made via sportyR"
      )
  }

  else {
    g = ggplot2::ggplot() +
      ggplot2::coord_fixed() +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(color = caption_color, hjust = 0.5),
        plot.margin = ggplot2::margin(-1, 0, -1, 0, "cm"),
        panel.border = ggplot2::element_blank(),
        panel.background = background,
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
      ) +
      ggplot2::labs(
        caption = "Plot made via sportyR"
      )
  }

  # Return the ggplot2 instance
  return(g)
}

#' Check a data frame to ensure it has the minimal features for plotting
#' @param df A data frame to check
#'
#' @return A boolean indicating whether the minimal features needed for plotting
#'   (a column called \code{x} and a column called \code{y}) are present
check_data_frame_for_plot = function(df){
  if(nrow(df) == 0){
    # If the data frame is empty, it's suitable for plotting since nothing will
    # actually be plotted
    return(TRUE)
  }
  if(sum(c('x', 'y') %in% names(df)) == 2){
    # If the columns 'x' and 'y' are present, the data frame is suitable for
    # plotting
    return(TRUE)
  }

  else {
    # Otherwise, the data frame is not suitable for a plot
    return(FALSE)
  }
}

#' Add a feature to a ggplot2 instance
#'
#' @param g The ggplot2 instance onto which the feature will be added
#' @param feature_df The data frame containing the points to add to the feature
#' @param feature_color A hexadecimal string with which to color the feature
#'   once added to the plot
#' @param group A grouping to pass along to \code{ggplot2::aes()}. This is used
#'   for speed in the NFL and NCAA Football plotting functions
#'
#' @return A ggplot2 instance with the feature added to it
add_feature = function(g, feature_df, feature_color, group = NULL, ...){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # First, check the data frame to ensure it has 'x' and 'y' columns
  data_frame_checked_and_passed = check_data_frame_for_plot(feature_df)

  if(data_frame_checked_and_passed){
    # So long as the input data frame has the correct features, add the feature
    # to the plot
    g = g +
      ggplot2::geom_polygon(data = feature_df, ggplot2::aes(x, y, group = group), fill = feature_color, ...)
  }

  else {
    stop('Incorrect column names. Data frame must have columns \'x\' and \'y\' to be plotted')
  }
}

#' Add a line feature to a ggplot2 instance
#'
#' @param g The ggplot2 instance onto which the feature will be added
#' @param feature_df The data frame containing the points to add to the feature
#' @param feature_color A hexadecimal string with which to color the feature
#'   once added to the plot
#' @param linetype a string containing a ggplot2 linetype
#'
#' @return A ggplot2 instance with the feature added to it
add_line_feature = function(g, feature_df, feature_color, linetype = 1, group = NULL, ...){
  # Initialize x and y (to pass checks)
  x = y = NULL

  # First, check the data frame to ensure it has 'x' and 'y' columns
  data_frame_checked_and_passed = check_data_frame_for_plot(feature_df)

  if(data_frame_checked_and_passed){
    # So long as the input data frame has the correct features, add the feature
    # to the plot
    g = g +
      ggplot2::geom_segment(data = feature_df, ggplot2::aes(x = x, y = y, xend = xend, yend = yend, group = group), color = feature_color, linetype = linetype, ...)
  }

  else {
    stop('Incorrect column names. Data frame must have columns \'x\', \'xend\', \'y\' and \'yend\' to be plotted')
  }
}
