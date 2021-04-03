#' Converts meters to feet
#'
#' @param m A number of meters to be converted to feet
#' @return The converted number of meters to feet
m_to_ft = function(m){
  ft = m * 100 / (2.54 * 12)
  return(ft)
}

#' Converts inches to yards
#'
#' @param inches A number of inches to be converted to yards
#' @return The converted number of inches to yards
inches_to_yd = function(inches){
  yd = inches / 36
  return(yd)
}

#' Converts feet to yards
#'
#' @param ft A number of feet to be converted to yards
#' @return The converted number of feet to yards
ft_to_yd = function(ft){
  yd = ft / 3
  return(yd)
}

#' Converts inches to meters
#'
#' @param inches A number of inches to be converted to meters
#' @return The converted number of inches to meters
in_to_m = function(inches){
  m = inches * 2.54 / 100
  return(m)
}
