#' Deploy the quadratic formula given inputs \code{a}, \code{b}, and \code{c}
#' such that they satisfy the equation \code{(a * (x ^ 2)) + bx + c = 0}
#'
#' @param a The coefficient of the squared term
#' @param b The coefficient of the linear term
#' @param c The coefficient of the constant term
#'
#' @return The solutions to the equation
quadratic_formula = function(a, b, c){
  # The quadratic formula yields two solutions described by the formula:
  # x = (-b +/- sqrt((b ^2) - (4 * a * c))) / (2 * a)

  # First, find the solution using addition in the numerator
  x_plus = (-b + sqrt((b ** 2) - (4 * a * c))) / (2 * a)

  # Then, repeat using subtraction
  x_minus = (-b - sqrt((b ** 2) - (4 * a * c))) / (2 * a)

  # Combine the solutions into a single vector
  solutions = c(x_plus, x_minus)

  # Return the solution
  return(solutions)
}

#' Get the Euclidean distance between two points
#'
#' @param point_1 A vector containing a point's coordinates
#' @param point_2 A vector containing a second point's coordinates
#'
#' @return The distance between the two supplied points
distance_formula = function(point_1_x, point_1_y, point_2_x = 0, point_2_y = 0){
  sqrt(
    (
      ((point_2_x - point_1_x) ^ 2) +
      ((point_2_y - point_1_y) ^ 2)
    )
  )
}
