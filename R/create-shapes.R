#' Create Circle
#' 
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
  # The angles are kept in units of pi so that cospi() and sinpi() can be used.
  # These return exact values at the quarter points (e.g. cospi(0.5) is exactly
  # 0, where cos(pi / 2) is 6.1e-17), so arcs meet on the axes without a seam
  pts <- seq(start, end, length.out = npoints)
  circle_coords <- data.frame(
    x = center[1] + (r * cospi(pts)),
    y = center[2] + (r * sinpi(pts))
  )

  return(circle_coords)
}

#' Create Rectangle
#' 
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

#' Create Square
#' 
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

#' Create Diamond
#' 
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

#' Create an "X" Shape
#' 
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

#' Split a Feature Into Rings
#'
#' A feature's data frame can hold several closed rings back to back, where each
#' ring ends on the point it started from. This splits the data frame at every
#' return to a ring's first point. Repeated consecutive points are dropped
#' first, so a ring that sits on a single point does not split into pieces
#'
#' @param df A data frame with \code{x} and \code{y} columns
#' @param tolerance The distance below which two points are the same point.
#'   Default: \code{1e-9}
#'
#' @return A list of two column matrices (\code{x} and \code{y}), one for each
#'   ring
#'
#' @keywords internal
split_rings <- function(df, tolerance = 1e-9) {
  coords <- as.matrix(df[, c("x", "y")])
  if (nrow(coords) < 2) {
    return(list(coords))
  }

  # Drop each point that repeats the one before it
  is_repeat <- rowSums(abs(diff(coords))) < tolerance
  coords <- coords[c(TRUE, !is_repeat), , drop = FALSE]

  rings <- list()
  ring_start <- 1
  for (i in seq_len(nrow(coords))[-1]) {
    # A ring ends when the path is back at the ring's first point
    if (i > ring_start &&
        all(abs(coords[i, ] - coords[ring_start, ]) < tolerance)) {
      rings[[length(rings) + 1]] <- coords[ring_start:i, , drop = FALSE]
      ring_start <- i + 1
    }
  }

  # Keep any trailing path that never closed on its first point
  if (ring_start < nrow(coords)) {
    rings[[length(rings) + 1]] <- coords[ring_start:nrow(coords), , drop = FALSE]
  }

  return(rings)
}

#' Check Whether a Feature Has Area
#'
#' Sums the unsigned area (shoelace formula) of each ring in a feature, so rings
#' that wind in opposite directions do not cancel. A feature with a zero-width
#' ring, a line, or a single point has no area
#'
#' @param df A data frame with \code{x} and \code{y} columns
#' @param tolerance The area at or below which a feature has no area. Default:
#'   \code{1e-9}
#'
#' @return \code{TRUE} if at least one ring in the feature has area, otherwise
#'   \code{FALSE}
#'
#' @keywords internal
has_area <- function(df, tolerance = 1e-9) {
  if (nrow(df) < 3) {
    return(FALSE)
  }

  ring_areas <- vapply(
    split_rings(df),
    function(ring) {
      n <- nrow(ring)
      abs(sum(ring[-n, 1] * ring[-1, 2] - ring[-1, 1] * ring[-n, 2])) / 2
    },
    numeric(1)
  )

  return(any(ring_areas > tolerance))
}
