#' Deploy the quadratic formula given inputs \code{a}, \code{b}, and \code{c}
#' such that they satisfy the equation \code{(a * (x ^ 2)) + bx + c = 0}
#'
#' @param a The coefficient of the squared term
#' @param b The coefficient of the linear term
#' @param c The coefficient of the constant term
#'
#' @return The solutions to the equation
quadratic_formula = function(a, b, c) {
  # The quadratic formula yields two solutions described by the formula:
  # x = (-b +/- sqrt((b ^2) - (4 * a * c))) / (2 * a)

  # First, find the solution using addition in the numerator
  x_plus = (-b + sqrt((b**2) - (4 * a * c))) / (2 * a)

  # Then, repeat using subtraction
  x_minus = (-b - sqrt((b**2) - (4 * a * c))) / (2 * a)

  # Combine the solutions into a single vector
  solutions = c(x_plus, x_minus)

  # Return the solution
  return(solutions)
}

#' Get the Euclidean distance between two points
#'
#' @param point_1_x A point's (or vector of points') x coordinate
#' @param point_1_y A point's (or vector of points') y coordinate
#' @param point_2_x A point's (or vector of points') x coordinate
#' @param point_2_y A point's (or vector of points') y coordinate
#'
#' @return The distance between the two supplied points
distance_formula = function(point_1_x, point_1_y, point_2_x = 0, point_2_y = 0) {
  # Calculate the distance between the points
  dist = sqrt(
    (
      ((point_2_x - point_1_x)^2) +
        ((point_2_y - point_1_y)^2)
    )
  )

  # Return the distance calculated above
  return(dist)
}
