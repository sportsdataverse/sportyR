#' Create a set of \code{x} and \code{y} coordinates that form a circle (or the
#' arc of a circle)
#'
#' @param center The (\code{x}, \code{y}) coordinates of the center of the
#'   circle. Default: \code{(0, 0)}
#' @param npoints The number of points with which to create the circle. This
#'   will also be the length of the resulting data frame. Default: 1000
#' @param r The radius of the circle IN THE UNITS OF THE PLOT. This default
#'   unit will be feet. Default: \code{1} (unit circle)
#' @param start The angle (in radians, divided by pi) at which to start drawing
#'   the circle, where zero runs along the +\code{x} axis. Default: \code{0}
#' @param end The angle (in radians, divided by pi) at which to stop drawing the
#'   circle, where zero runs along the +\code{x} axis. Default: \code{2}
#'
#' @return A data frame containing the points needed to draw the specified
#'   circle
#'
#' @keywords internal
create_circle <- function(center = c(0, 0),
                          npoints = 1000,
                          r = 1,
                          start = 0,
                          end = 2) {
  pts <- seq(start * pi, end * pi, length.out = npoints)
  circle_coords <- data.frame(
    x = center[1] + (r * cos(pts)),
    y = center[2] + (r * sin(pts))
  )

  return(circle_coords)
}

#' Create a set of \code{x} and \code{y} coordinates that form a rectangle
#'
#' @param x_min The minimum value of \code{x}
#' @param x_max The maximum value of \code{x}
#' @param y_min The minimum value of \code{y}
#' @param y_max The maximum value of \code{y}
#'
#' @return A data frame containing the points needed to draw the specified
#'   rectangle
#'
#' @keywords internal
create_rectangle <- function(x_min, x_max, y_min, y_max) {
  rectangle_coords <- data.frame(
    x = c(x_min, x_max, x_max, x_min, x_min),
    y = c(y_min, y_min, y_max, y_max, y_min)
  )

  return(rectangle_coords)
}

#' Create a set of \code{x} and \code{y} coordinates that form a square. This
#' function is a wrapper on \code{create_rectangle()} above
#'
#' @param side_length The length of the side of the square
#' @param center The center coordinates of the square
#'
#' @return A data frame containing the points needed to draw the specified
#'   square
#'
#' @keywords internal
create_square <- function(side_length, center = c(0, 0)) {
  square_coords <- create_rectangle(
    x_min = center[1] - (side_length / 2),
    x_max = center[1] + (side_length / 2),
    y_min = center[2] - (side_length / 2),
    y_max = center[2] + (side_length / 2)
  )

  return(square_coords)
}

#' Create a set of \code{x} and \code{y} coordinates that form a diamond. This
#' function is a wrapper on \code{create_rectangle()} above
#'
#' @param height The vertical height of the diamond
#' @param width The horizontal width of the diamond
#' @param center The center coordinates of the diamond
#'
#' @return A data frame containing the points needed to draw the specified
#'   diamond
#'
#' @keywords internal
create_diamond <- function(height, width, center = c(0, 0)) {
  diamond_coords <- data.frame(
    x = c(
      center[1] - (width / 2),
      center[1],
      center[1] + (width / 2),
      center[1],
      center[1] - (width / 2)
    ),
    y = c(
      center[2],
      center[2] - (height / 2),
      center[2],
      center[2] + (height / 2),
      center[2]
    )
  )

  return(diamond_coords)
}

#' Create a set of \code{x} and \code{y} coordinates that form an "X"-like shape
#'
#' @param bar_length The length of one of the bars that forms the "X" shape,
#'  from outer edge to outer edge
#' @param bar_width The width of one of the bars that forms the "X" shape, from
#'  outer edge to outer edge
#' @param rotation The angle of rotation (in degrees) that the resulting
#'  "X"-like shape should be rotated
#'
#' @return A data frame containing the points needed to draw the specified "X"
#'
#' @keywords internal
create_x_shape <- function(bar_length, bar_width, rotation = 45) {
  cross_df <- data.frame(
    x = c(
      0,
      bar_width / 2,
      bar_width / 2,
      bar_length / 2,
      bar_length / 2,
      bar_width / 2,
      bar_width / 2,
      -bar_width / 2,
      -bar_width / 2,
      -bar_length / 2,
      -bar_length / 2,
      -bar_width / 2,
      -bar_width / 2,
      0
    ),
    y = c(
      bar_length / 2,
      bar_length / 2,
      bar_width / 2,
      bar_width / 2,
      -bar_width / 2,
      -bar_width / 2,
      -bar_length / 2,
      -bar_length / 2,
      -bar_width / 2,
      -bar_width / 2,
      bar_width / 2,
      bar_width / 2,
      bar_length / 2,
      bar_length / 2
    )
  )

  x_shape_df <- rotate_coords(
    cross_df, angle = rotation
  )

  return(x_shape_df)
}
