#' Perform a mathematical reflection of coordinates over a specified axis
#'
#' @param df The data frame to reflect. It must have \code{x} and \code{y}
#'   columns
#' @param over_x A boolean indicating whether or not to reflect over the x axis.
#'   Default: FALSE
#' @param over_y A boolean indicating whether or not to reflect over the y axis.
#'   Default: TRUE
#'
#' @return The reflected data frame
#'
#' @export
#'
#' @examples
#' reflect(data.frame(x = 1, y = 0))
reflect <- function(df,
                   over_x = FALSE,
                   over_y = TRUE) {
  if (over_y) {
    df["x"] <- -1 * df["x"]
  }

  if (over_x) {
    df["y"] <- -1 * df["y"]
  }

  return(df)
}

#' Perform a mathematical rotation about (0, 0) of coordinates. This rotation is
#' given as x' = x \* cos(theta) - y \* sin(theta) y' = x \* sin(theta) + y \*
#' cos(theta)
#'
#' @param df The data frame to rotate. It must have \code{x} and \code{y}
#'   columns
#' @param rotation_dir The direction in which to rotate the coordinates.
#'   \code{ccw} corresponds to counterclockwise
#' @param angle the angle (in degrees) through which to rotate the coordinates
#'
#' @return The rotated data frame
#'
#' @export
#'
#' @examples
#' rotate_coords(data.frame(x = 0, y = 1))
rotate_coords <- function(df,
                         rotation_dir = "ccw",
                         angle = 90) {
  # If the data frame is empty, just give back the data frame
  if (nrow(df) == 0) {
    return(df)
  }

  # Set theta to be the angle of rotation, converting from degrees to radians
  theta <- (angle / 180) * pi

  # Copy the data frame so that the original data frame can be utilized for
  # vectorized calculations
  rotated_df <- df

  # Perform the rotation
  rotated_df["x"] <- df["x"] * cos(theta) - df["y"] * sin(theta)
  rotated_df["y"] <- df["x"] * sin(theta) + df["y"] * cos(theta)

  return(rotated_df)
}
