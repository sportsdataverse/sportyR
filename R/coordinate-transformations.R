#' Perform a mathematical reflection of coordinates over a specified axis
#'
#' @param df The data frame to reflect. It must have \code{x} and \code{y}
#'   columns
#' @param over_x A boolean indicating whether or not to reflect over the x axis.
#'   Default: FALSE
#' @param over_y A boolean indicating whether or not to reflect over the y axis.
#'   Default: TRUE
#' @return The reflected data frame
reflect = function(df, over_x = FALSE, over_y = TRUE){
  if(over_y){
    df['x'] = -1 * df['x']
  }

  if(over_x){
    df['y'] = -1 * df['y']
  }

  return(df)
}

#' Perform a mathematical rotation about (0, 0) of coordinates. This rotation is
#' given as x' = x \* cos(theta) - y \* sin(theta) y' = x \* sin(theta) + y \*
#' cos(theta)
#'
#' @param df The data frame to rotate. It must have \code{x} and \code{y} columns
#' @param rotation_dir The direction in which to rotate the coordinates.
#'   \code{ccw} corresponds to counterclockwise
#' @param angle the angle (in radians, divided by pi) through which to rotate
#'   the coordinates
#' @return The rotated data frame
rotate_coords = function(df, rotation_dir = 'ccw', angle = .5){
  # If the data frame is empty, just give back the data frame
  if(nrow(df) == 0){
    return(df)
  }
  # If the rotation direction is clockwise, take the negative of the angle
  if(!tolower(rotation_dir) %in% c('ccw', 'pos', 'positive', 'counterclockwise', 'anticlockwise')){
    angle = angle * -1
  }

  # Set theta to be the angle of rotation
  theta = angle * pi

  # Copy the data frame so that the original data frame can be utilized for
  # vectorized calculations
  rotated_df = df

  # Perform the rotation
  rotated_df['x'] = df['x'] * cos(theta) - df['y'] * sin(theta)
  rotated_df['y'] = df['x'] * sin(theta) + df['y'] * cos(theta)

  return(rotated_df)
}

#' Perform a mathematical translation of coordinates
#'
#' @param df The data frame to translate It must have \code{x} and \code{y}
#'   columns
#' @param translate_x The number of units (in the input data frame's units) to
#'   translate the points in the +x direction. Default: 0
#' @param translate_y The number of units (in the input data frame's units) to
#'   translate the points in the +y direction. Default: 0
#' @return The translated data frame
translate = function(df, translate_x = 0, translate_y = 0){
  df['x'] = df['x'] + translate_x
  df['y'] = df['y'] + translate_y

  return(df)
}
