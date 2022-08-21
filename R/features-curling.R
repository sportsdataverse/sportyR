# Surface Base Features --------------------------------------------------------

#' The curling sheet is the entire sheet, with the houses at either the top or
#' bottom ends. This draws the area of the sheet from the hog line to the back
#' board
#'
#' @param sheet_length The length of the sheet, from back board to back board
#' @param sheet_width The width of the curling sheet, from side wall to side
#'   wall
#' @param hog_line_to_tee_line The distance from the center of the tee line to
#'   the interior edge of the tee line
#' @param tee_line_to_center The distance from the tee line to the center of the
#'   sheet
#'
#' @returns A data frame containing the bounding box of the end of the ice sheet
#'
#' @keywords internal
curling_end <- function(sheet_length = 0,
                        sheet_width = 0,
                        tee_line_to_center = 0,
                        hog_line_to_tee_line = 0,
                        drawn_direction = "") {
  # Calculate the length of the end, from back board to interior edge of the hog
  # line
  end_length <- (sheet_length / 2) - tee_line_to_center + hog_line_to_tee_line

  if (tolower(drawn_direction) == "upward") {
    end_df <- create_rectangle(
      x_min = -sheet_width / 2,
      x_max = sheet_width / 2,
      y_min = 0,
      y_max = end_length
    )
  } else {
    end_df <- create_rectangle(
      x_min = -sheet_width / 2,
      x_max = sheet_width / 2,
      y_min = -end_length,
      y_max = 0
    )
  }

  return(end_df)
}

#' The curling sheet is the entire sheet, with the houses at either the top or
#' bottom ends. This draws the area between the hog lines
#'
#' @param sheet_width The width of the curling sheet, from side wall to side
#'   wall
#' @param tee_line_to_center The distance from the tee line to the center of the
#'   sheet
#' @param hog_line_to_tee_line The distance from the center of the tee line to
#'   the interior edge of the tee line
#'
#' @returns A data frame containing the bounding box of the end of the ice sheet
#'
#' @keywords internal
curling_centre_zone <- function(sheet_width = 0,
                                tee_line_to_center = 0,
                                hog_line_to_tee_line = 0) {
  half_centre_zone_length <- tee_line_to_center - hog_line_to_tee_line
  centre_zone_df <- create_rectangle(
    x_min = -sheet_width / 2,
    x_max = sheet_width / 2,
    y_min = -half_centre_zone_length,
    y_max = half_centre_zone_length
  )

  return(centre_zone_df)
}





# Surface Boundaries -----------------------------------------------------------

#' The apron of the sheet is what separates adjacent sheets, and in this context
#' provides a border around the outside of the sheet
#'
#' @param sheet_length The length of the sheet, from back board to back board
#' @param sheet_width The width of the curling sheet, from side wall to side
#'   wall
#' @param apron_behind_back The extension of the apron beyond the back board
#' @param apron_along_side The extension of the apron running along the side
#'   walls
#'
#' @returns A data frame containing the bounding coordinates of the sheet's
#'   apron
#'
#' @keywords internal
curling_sheet_apron <- function(sheet_length = 0,
                                sheet_width = 0,
                                apron_behind_back = 0,
                                apron_along_side = 0) {
  sheet_apron_df <- data.frame(
    x = c(
      0,
      sheet_width / 2,
      sheet_width / 2,
      (sheet_width / 2) + apron_along_side,
      (sheet_width / 2) + apron_along_side,
      -(sheet_width / 2) - apron_along_side,
      -(sheet_width / 2) - apron_along_side,
      -sheet_width / 2,
      -sheet_width / 2,
      0
    ),

    y = c(
      sheet_length / 2,
      sheet_length / 2,
      0,
      0,
      (sheet_length / 2) + apron_behind_back,
      (sheet_length / 2) + apron_behind_back,
      0,
      0,
      sheet_length / 2,
      sheet_length / 2
    )
  )

  return(sheet_apron_df)
}





# Surface Lines ----------------------------------------------------------------

#' The centre line is the line that runs the full length of the curling sheet,
#' or the line \code{x = 0} in TV view
#'
#' @param line_thickness The thickness of the centre line
#' @param tee_line_to_center The distance between the tee lines. (See
#'   [curling_tee_line()] for more information)
#' @param centre_line_extension The distance beyond the tee lines that the
#'   centre line extends
#'
#' @returns A data frame containing the bounding coordinates of the centre line
#'
#' @keywords internal
curling_centre_line <- function(line_thickness = 0,
                                tee_line_to_center = 0,
                                centre_line_extension = 0) {
  centre_line_df <- create_rectangle(
    x_min = -line_thickness / 2,
    x_max = line_thickness / 2,
    y_min = -tee_line_to_center - centre_line_extension,
    y_max = tee_line_to_center + centre_line_extension
  )

  return(centre_line_df)
}

#' The tee line is the line that runs through the center of the house. Its
#' midpoints are connected by the centre line (see [curling_centre_line()] for
#' more information)
#'
#' @param line_thickness The thickness of the tee line
#' @param sheet_width The width of the curling sheet, from side wall to side
#'   wall
#'
#' @returns A data frame containing the bounding box of the tee line
#'
#' @keywords internal
curling_tee_line <- function(line_thickness = 0, sheet_width = 0) {
  tee_line_df <- create_rectangle(
    x_min = -sheet_width / 2,
    x_max = sheet_width / 2,
    y_min = -line_thickness / 2,
    y_max = line_thickness / 2
  )

  return(tee_line_df)
}

#' The back line is the line in the back of the house. Its outer edge should be
#' used as its anchor point
#'
#' @param line_thickness The thickness of the back line
#' @param sheet_width The width of the curling sheet, from side wall to side
#'   wall
#'
#' @returns A data frame containing the bounding box of the back line
#'
#' @keywords internal
curling_back_line <- function(line_thickness = 0, sheet_width = 0) {
  back_line_df <- create_rectangle(
    x_min = -sheet_width / 2,
    x_max = sheet_width / 2,
    y_min = 0,
    y_max = line_thickness
  )

  return(back_line_df)
}


#' The hog line is the line that begins the Free Guard Zone at each end of the
#' ice. Its inner edge (relative to the nearest house) should be used as its
#' anchor point
#'
#' @param line_thickness The thickness of the hog line
#' @param sheet_width The width of the curling sheet, from side wall to side
#'   wall
#'
#' @returns A data frame containing the bounding box of the hog line
#'
#' @keywords internal
curling_hog_line <- function(line_thickness = 0, sheet_width = 0) {
  hog_line_df <- create_rectangle(
    x_min = -sheet_width / 2,
    x_max = sheet_width / 2,
    y_min = 0,
    y_max = line_thickness
  )

  return(hog_line_df)
}

#' The hack line connects the two footholds at each hack. It should be anchored
#' at the terminus of the centre line (see [curling_centre_line()] for more
#' information)
#'
#' @param line_thickness The thickness of the hack line
#' @param hack_width The width of the hack, measured from the outside of one
#'   foothold to the outside of the other
#'
#' @returns A data frame containing the bounding box of the hack line
#'
#' @keywords internal
curling_hack_line <- function(line_thickness = 0, hack_width = 0) {
  hack_line_df <- create_rectangle(
    x_min = -hack_width / 2,
    x_max = hack_width / 2,
    y_min = -line_thickness,
    y_max = 0
  )

  return(hack_line_df)
}

#' The courtesy lines are where players stand during the delivery process of
#' each stone when the opposing team is throwing
#'
#' @param line_thickness The thickness of the courtesy line
#' @param line_length The length of the courtesy line, from the side wall
#'   towards the centre line
#'
#' @returns A data frame containing the bounding box of the courtesy line
#'
#' @keywords internal
curling_courtesy_line <- function(line_thickness = 0, line_length = 0) {
  courtesy_line_df <- create_rectangle(
    x_min = -line_length,
    x_max = 0,
    y_min = -line_thickness,
    y_max = 0
  )

  return(courtesy_line_df)
}





# Surface Features -------------------------------------------------------------

#' The hack exits on both sides of the curling sheet between the back board and
#' the back line. This is where a curler pushes off from, and it should be
#' centered on the centre line (see [curling_centre_line()]). This function
#' draws one of the footholds of the hack
#'
#' @param foothold_depth The depth of each foothold in the hack, from the side
#'   nearest the house to the side nearest the back board
#' @param foothold_width The width of each foothold in the hack, from the side
#'   nearest the centre line to the side nearest the nearest side wall
#'
#' @returns A data frame containing the bounding box of one foothold of the hack
#'
#' @keywords internal
curling_hack_foothold <- function(foothold_depth = 0, foothold_width = 0) {
  foothold_df <- create_rectangle(
    x_min = 0,
    x_max = foothold_width,
    y_min = -foothold_depth,
    y_max = 0
  )

  return(foothold_df)
}

#' The inner-most of the concentric circles comprising the house is called the
#' button. This is the intersection of the tee line (see [curling_tee_line()])
#' and the centre line (see [curling_centre_line()])
#'
#' @param feature_radius The radius of the button
#'
#' @returns A data frame containing the bounding coordinates of the button
#'
#' @keywords internal
curling_button <- function(feature_radius = 0) {
  button_df <- create_circle(
    center = c(0, 0),
    start = 0,
    end = 2,
    r = feature_radius
  )

  return(button_df)
}

#' The house is comprised of three concentric circles outside of the button of
#' varying radii. This feature is designed to be each of the house rings
#' excluding the button
#'
#' @param feature_radius The radius of the ring
#'
#' @returns A data frame containing the bounding coordinates of the house ring
#'
#' @keywords internal
curling_house_ring <- function(feature_radius = 0) {
  ring_df <- create_circle(
    center = c(0, 0),
    start = 0,
    end = 2,
    r = feature_radius
  )

  return(ring_df)
}
