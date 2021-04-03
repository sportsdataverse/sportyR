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
