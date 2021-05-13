#' Function to convert all units, regardless of starting and ending units
#'
#' @param meas A measurement in any unit of length
#' @param from_unit A string containing the original unit of measure to be
#'   converted
#' @param to_unit A string containing the ending unit of measure
#' @param conversion_columns A vector containing the columns to convert if
#'   \code{meas} is of type \code{data.frame}
#'
#' @return The measurement in converted units
#'
#' @export
#'
#' @examples
#' convert_units(1, "in", "cm")
#' convert_units(100, "cm", "m")
convert_units = function(meas,
                         from_unit,
                         to_unit,
                         conversion_columns = NULL) {
  # Convert the from_unit and to_unit to be lower case
  from_unit = tolower(from_unit)
  to_unit = tolower(to_unit)

  # Create the conversion table
  conversion_table = data.frame(
    unit_name = c(
      "millimeters",
      "centimeters",
      "meters",
      "inches",
      "feet",
      "yards"
    ),
    unit_abbreviation = c(
      "mm",
      "cm",
      "m",
      "in",
      "ft",
      "yd"
    ),
    conversion_factor = c(
      3048,
      30.48,
      0.3048,
      12,
      1,
      1 / 3
    )
  )

  # Ensure that the from_unit and to_unit exist somewhere in the conversion table
  if (!(from_unit %in% conversion_table$unit_abbreviation) & !(from_unit %in% conversion_table$unit_name)) {
    stop(glue::glue("{from_unit} is not a viable unit at this time."))
  }

  if (!(to_unit %in% conversion_table$unit_abbreviation) & !(to_unit %in% conversion_table$unit_name)) {
    stop(glue::glue("{to_unit} is not a viable unit at this time."))
  }

  # This function should work over entire data frames as well as individual vectors
  if (class(meas) == "data.frame") {

    # If no columns are supplied, alert user of error
    if (is.null(conversion_columns)) {
      stop(glue::glue("Must supply columns to convert"))
    }

    # Iterate over the supplied columns and perform the conversion
    for (column in conversion_columns) {
      # Check to ensure the column name is in the data frame's column names
      if (!(column %in% names(meas))) {
        message(glue::glue("{column} is not a column in the supplied data frame. Skipping..."))
      }
      # Columns must be either integer or numeric in type
      else if (class(meas[, column]) %in% c("integer", "numeric")) {
        if (from_unit %in% conversion_table$unit_abbreviation & to_unit %in% conversion_table$unit_abbreviation) {
          meas[column] = meas[column] / conversion_table[conversion_table$unit_abbreviation == from_unit, "conversion_factor"]
          meas[column] = meas[column] * conversion_table[conversion_table$unit_abbreviation == to_unit, "conversion_factor"]
        }

        else if (from_unit %in% conversion_table$unit_abbreviation & to_unit %in% conversion_table$unit_name) {
          meas[column] = meas[column] / conversion_table[conversion_table$unit_abbreviation == from_unit, "conversion_factor"]
          meas[column] = meas[column] * conversion_table[conversion_table$unit_name == to_unit, "conversion_factor"]
        }

        else if (from_unit %in% conversion_table$unit_name & to_unit %in% conversion_table$unit_abbreviation) {
          meas[column] = meas[column] / conversion_table[conversion_table$unit_name == from_unit, "conversion_factor"]
          meas[column] = meas[column] * conversion_table[conversion_table$unit_abbreviation == to_unit, "conversion_factor"]
        }

        else {
          meas[column] = meas[column] / conversion_table[conversion_table$unit_name == from_unit, "conversion_factor"]
          meas[column] = meas[column] * conversion_table[conversion_table$unit_name == to_unit, "conversion_factor"]
        }
      }

      # If a supplied column is neither of type integer or numeric, alert user that the column is
      else {
        message(glue::glue("{column} is not convertable. Skipping..."))
      }
    }

    # Return the data frame
    return(meas)
  }

  else {
    # Perform the conversion by first converting the input into the base unit,
    # then converting to the desired unit
    if (from_unit %in% conversion_table$unit_abbreviation & to_unit %in% conversion_table$unit_abbreviation) {
      reduced = meas / conversion_table[conversion_table$unit_abbreviation == from_unit, "conversion_factor"]
      result = reduced * conversion_table[conversion_table$unit_abbreviation == to_unit, "conversion_factor"]
    }

    else if (from_unit %in% conversion_table$unit_abbreviation & to_unit %in% conversion_table$unit_name) {
      reduced = meas / conversion_table[conversion_table$unit_abbreviation == from_unit, "conversion_factor"]
      result = reduced * conversion_table[conversion_table$unit_name == to_unit, "conversion_factor"]
    }

    else if (from_unit %in% conversion_table$unit_name & to_unit %in% conversion_table$unit_abbreviation) {
      reduced = meas / conversion_table[conversion_table$unit_name == from_unit, "conversion_factor"]
      result = reduced * conversion_table[conversion_table$unit_abbreviation == to_unit, "conversion_factor"]
    }

    else {
      reduced = meas / conversion_table[conversion_table$unit_name == from_unit, "conversion_factor"]
      result = reduced * conversion_table[conversion_table$unit_name == to_unit, "conversion_factor"]
    }

    # Return the converted measurement
    return(result)
  }
}
