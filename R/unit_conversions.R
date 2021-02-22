#' Converts meters to feet
#'
#' @param m A number of meters to be converted to feet
#' @return The converted number of meters to feet
m_to_ft = function(m){
  ft = m * 100 / (2.54 * 12)
  return(ft)
}
