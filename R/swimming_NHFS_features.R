#' Generate the data frame for the points that comprise the pool deck background
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the pool deck
nfhs_swimming_feature_deck = function(course = "SCY",
                                      lane_width = 2,
                                      number_of_lanes = 8,
                                      overflow_channels = 0.5,
                                      rotate = FALSE,
                                      rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long

  # This gives the grey background of the deck
  deck = create_rectangle(
    x_min = (-pool_length / 2) - 3,
    x_max = (pool_length / 2) + 3,
    y_min = ((-lane_width * number_of_lanes) / 2) - overflow_channels - 3,
    y_max = ((lane_width * number_of_lanes) / 2) +  overflow_channels + 3
  )

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    deck = rotate_coords(deck,
                         rotation_dir)
  }

  # Return the feature's data frame
  return(deck)
}

#' Generate the data frame for the points that comprise the water background
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the pool
nfhs_swimming_feature_pool = function(course = "SCY",
                                      lane_width = 7 / 3,
                                      number_of_lanes = 8,
                                      overflow_channels = 0.5,
                                      rotate = FALSE,
                                      rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long

  # This gives the water background of the pool
  pool = create_rectangle(
    x_min = -pool_length / 2,
    x_max = pool_length / 2,
    y_min = ((-lane_width * number_of_lanes) / 2) - overflow_channels,
    y_max = ((lane_width * number_of_lanes) / 2) + overflow_channels
  )

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    pool = rotate_coords(pool,
                         rotation_dir)
  }

  # Return the feature's data frame
  return(pool)
}

#' Generate the data frame for the points that comprise the 15m line for the start end of the pool
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the 15m line from the start end
nfhs_swimming_feature_15m_start_line = function(course = "SCY",
                                                lane_width = 7 / 3,
                                                number_of_lanes = 8,
                                                overflow_channels = 0.5,
                                                rotate = FALSE,
                                                rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long
  m15_distance <-
    ifelse(course %in% c("SCY"), 16.40, 15) # 15m mark is always at 15m, so 16.40y
  line_thickness <-
    ifelse(course %in% c("SCY"), 6 / 12 / 3, 15 / 100) # line is approxmiately 6in (15cm) thick

  # 15 meter marks are 15 meters from the start and turn ends of the pool
  m15_line_start = create_rectangle(
    x_min = (-pool_length / 2) + m15_distance - (line_thickness / 2),
    x_max = (-pool_length / 2) + m15_distance + (line_thickness / 2),
    y_min = ((-lane_width * number_of_lanes) / 2) - overflow_channels,
    y_max = ((lane_width * number_of_lanes) / 2) + overflow_channels
  )

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    m15_line_start = rotate_coords(m15_line_start,
                                   rotation_dir)
  }

  # Return the feature's data frame
  return(m15_line_start)
}

#' Generate the data frame for the points that comprise the 15m marks for the start end of the pool
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the 15m marks from the start end
nfhs_swimming_feature_15m_start_markers = function(course = "SCY",
                                                   lane_width = 7 / 3,
                                                   number_of_lanes = 8,
                                                   overflow_channels = 0.5,
                                                   rotate = FALSE,
                                                   rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long
  m15_distance <-
    ifelse(course %in% c("SCY"), 16.40, 15) # 15m mark is always at 15m, so 16.40y
  mark_thickness <-
    ifelse(course %in% c("SCY"), 2 / 12 / 3, 5.8 / 100) # lines are 2in (5.08cm) thick

  pool_width <-
    ((lane_width * number_of_lanes)) + (overflow_channels * 2)
  centerlines <-
    c((-pool_width / 2) - (mark_thickness / 2),
      (pool_width / 2) + (mark_thickness / 2))

  m15_markers_fun <-
    function(centerline,
             pool_length,
             mark_thickness) {
      df = create_rectangle(
        x_min = (-pool_length / 2) + m15_distance - (mark_thickness / 2),
        x_max = (-pool_length / 2) + m15_distance + (mark_thickness / 2),
        y_min = centerline - (mark_thickness / 2),
        y_max = centerline + (mark_thickness / 2)
      )

      return(df)
    }

  m15_markers_start <-
    lapply(centerlines,
           m15_markers_fun,
           pool_length = pool_length,
           mark_thickness = mark_thickness)

  # convert to single data frame with id column for each separate marker
  id <- seq(1, length(m15_markers_start)) # id column
  m15_markers_start <-
    Map(cbind, m15_markers_start, group = id) # add id column to each data frame
  m15_markers_start <-
    do.call("rbind", m15_markers_start) # bind into single data frame

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    m15_markers_start = rotate_coords(m15_markers_start,
                                      rotation_dir)
  }

  # Return the feature's data frame
  return(m15_markers_start)
}

#' Generate the data frame for the points that comprise the 15m line for the turn end of the pool
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the 15m line from the turn end
nfhs_swimming_feature_15m_turn_line = function(course = "SCY",
                                               lane_width = 7 / 3,
                                               number_of_lanes = 8,
                                               overflow_channels = 0.5,
                                               rotate = FALSE,
                                               rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long
  m15_distance <-
    ifelse(course %in% c("SCY"), 16.40, 15) # 15m mark is always at 15m, so 16.40y
  line_thickness <-
    ifelse(course %in% c("SCY"), 6 / 12 / 3, 15 / 100) # line is approxmiately 6in (15cm) thick

  # 15 meter marks are 15 meters from the start and turn ends of the pool
  m15_line_turn = create_rectangle(
    x_min = (pool_length / 2) - m15_distance - (line_thickness / 2),
    x_max = (pool_length / 2) - m15_distance + (line_thickness / 2),
    y_min = ((-lane_width * number_of_lanes) / 2) - overflow_channels,
    y_max = ((lane_width * number_of_lanes) / 2) + overflow_channels
  )


  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    m15_line_turn = rotate_coords(m15_line_turn,
                                  rotation_dir)
  }

  # Return the feature's data frame
  return(m15_line_turn)
}

#' Generate the data frame for the points that comprise the 15m marks for the turn end of the pool
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the 15m marks from the turn end
nfhs_swimming_feature_15m_turn_markers = function(course = "SCY",
                                                  lane_width = 7 / 3,
                                                  number_of_lanes = 8,
                                                  overflow_channels = 0.5,
                                                  rotate = FALSE,
                                                  rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long
  m15_distance <-
    ifelse(course %in% c("SCY"), 16.40, 15) # 15m mark is always at 15m, so 16.40y
  mark_thickness <-
    ifelse(course %in% c("SCY"), 2 / 12 / 3, 5.8 / 100) # lines are 2in (5.08cm) thick

  pool_width <-
    ((lane_width * number_of_lanes)) + (overflow_channels * 2)
  centerlines <-
    c((-pool_width / 2) - (mark_thickness / 2),
      (pool_width / 2) + (mark_thickness / 2))

  m15_markers_fun <-
    function(centerline,
             pool_length,
             mark_thickness) {
      df = create_rectangle(
        x_min = (pool_length / 2) - m15_distance - (mark_thickness / 2),
        x_max = (pool_length / 2) - m15_distance + (mark_thickness / 2),
        y_min = centerline - (mark_thickness / 2),
        y_max = centerline + (mark_thickness / 2)
      )

      return(df)
    }

  m15_markers_turn <-
    lapply(centerlines,
           m15_markers_fun,
           pool_length = pool_length,
           mark_thickness = mark_thickness)

  # convert to single data frame with id column for each separate marker
  id <- seq(1, length(m15_markers_turn)) # id column
  m15_markers_turn <-
    Map(cbind, m15_markers_turn, group = id) # add id column to each data frame
  m15_markers_turn <-
    do.call("rbind", m15_markers_turn) # bind into single data frame

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    m15_markers_turn = rotate_coords(m15_markers_turn,
                                     rotation_dir)
  }

  # Return the feature's data frame
  return(m15_markers_turn)
}

#' Generate the data frame for the points that comprise the backstroke flags for the start end of the pool
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the backstroke flags for the start end
nfhs_swimming_feature_flags_start = function(course = "SCY",
                                             lane_width = 7 / 3,
                                             number_of_lanes = 8,
                                             overflow_channels = 0.5,
                                             rotate = FALSE,
                                             rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long

  flags_distance <- 5 # flags are 5y or 5m from wall

  # flags are 5m from the walls
  flags_start = create_line(
    x_start = (-pool_length / 2) + flags_distance,
    x_end = (-pool_length / 2) + flags_distance,
    y_start = ((-lane_width * number_of_lanes) / 2) - overflow_channels - 1.5,
    y_end = ((lane_width * number_of_lanes) / 2) + overflow_channels + 1.5
  )

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    flags_start = rotate_coords(flags_start,
                                rotation_dir)
  }

  # Return the feature's data frame
  return(flags_start)
}

#' Generate the data frame for the points that comprise the alternate color backstroke flags for the start end of the pool
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the alternate color backstroke flags for the start end
nfhs_swimming_feature_flags_start_2 = function(course = "SCY",
                                               lane_width = 7 / 3,
                                               number_of_lanes = 8,
                                               overflow_channels = 0.5,
                                               rotate = FALSE,
                                               rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long
  flags_distance <- 5 # flags are 5y or 5m from wall
  alternate_offset <-
    ifelse(course %in% c("SCY"), 10 / 12 / 3, 25.40 / 100)

  # flags are 5m from the walls
  flags_start_2 = create_line(
    x_start = (-pool_length / 2) + flags_distance,
    x_end = (-pool_length / 2) + flags_distance,
    y_start = ((-lane_width * number_of_lanes) / 2) - overflow_channels - 1.5 + alternate_offset,
    y_end = ((lane_width * number_of_lanes) / 2) + overflow_channels + 1.5 - alternate_offset
  )

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    flags_start_2 = rotate_coords(flags_start_2,
                                  rotation_dir)
  }

  # Return the feature's data frame
  return(flags_start_2)
}

#' Generate the data frame for the points that comprise the backstroke flags string for the start end of the pool
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the backstroke flags string for the start end
nfhs_swimming_feature_flags_start_string = function(course = "SCY",
                                                    lane_width = 7 / 3,
                                                    number_of_lanes = 8,
                                                    overflow_channels = 0.5,
                                                    rotate = FALSE,
                                                    rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long
  flags_distance <- 5 # flags are 5y or 5m from wall

  # flags are 5m from the walls
  flags_start_string = create_line(
    x_start = (-pool_length / 2) + flags_distance,
    x_end = (-pool_length / 2) + flags_distance,
    y_start = ((-lane_width * number_of_lanes) / 2) - overflow_channels - 1.5,
    y_end = ((lane_width * number_of_lanes) / 2) + overflow_channels + 1.5
  )

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    flags_start_string = rotate_coords(flags_start_string,
                                       rotation_dir)
  }

  # Return the feature's data frame
  return(flags_start_string)
}

#' Generate the data frame for the points that comprise the backstroke flags for the turn end of the pool
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the flags at the turn end
nfhs_swimming_feature_flags_turn = function(course = "SCY",
                                            lane_width = 7 / 3,
                                            number_of_lanes = 8,
                                            overflow_channels = 0.5,
                                            rotate = FALSE,
                                            rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long
  flags_distance <- 5 # flags are 5y or 5m from wall

  # flags are 5m from the walls
  flags_turn = create_line(
    x_start = (pool_length / 2) - flags_distance,
    x_end = (pool_length / 2) - flags_distance,
    y_start = ((-lane_width * number_of_lanes) / 2) - overflow_channels - 1.5,
    y_end = ((lane_width * number_of_lanes) / 2) + overflow_channels + 1.5
  )

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    flags_turn = rotate_coords(flags_turn,
                               rotation_dir)
  }

  # Return the feature's data frame
  return(flags_turn)
}

#' Generate the data frame for the points that comprise the alternate backstroke flags for the turn end of the pool
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the alternate flags at the turn end
nfhs_swimming_feature_flags_turn_2 = function(course = "SCY",
                                              lane_width = 7 / 3,
                                              number_of_lanes = 8,
                                              overflow_channels = 0.5,
                                              rotate = FALSE,
                                              rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long
  flags_distance <- 5 # flags are 5y or 5m from wall
  alternate_offset <-
    ifelse(course %in% c("SCY"), 10 / 12 / 3, 25.40 / 100)

  # flags are 5m from the walls
  flags_turn_2 = create_line(
    x_start = (pool_length / 2) - flags_distance,
    x_end = (pool_length / 2) - flags_distance,
    y_start = ((-lane_width * number_of_lanes) / 2) - overflow_channels - 1.5 + alternate_offset,
    y_end = ((lane_width * number_of_lanes) / 2) + overflow_channels + 1.5 - alternate_offset
  )

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    flags_turn_2 = rotate_coords(flags_turn_2,
                                 rotation_dir)
  }

  # Return the feature's data frame
  return(flags_turn_2)
}

#' Generate the data frame for the points that comprise the backstroke flags string for the turn end of the pool
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the flags string at the turn end
nfhs_swimming_feature_flags_turn_string = function(course = "SCY",
                                                   lane_width = 7 / 3,
                                                   number_of_lanes = 8,
                                                   overflow_channels = 0.5,
                                                   rotate = FALSE,
                                                   rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long
  flags_distance <- 5 # flags are 5y or 5m from wall

  # flags are 5m from the walls
  flags_turn_string = create_line(
    x_start = (pool_length / 2) - flags_distance,
    x_end = (pool_length / 2) - flags_distance,
    y_start = ((-lane_width * number_of_lanes) / 2) - overflow_channels - 1.5,
    y_end = ((lane_width * number_of_lanes) / 2) + overflow_channels + 1.5
  )

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    flags_turn_string = rotate_coords(flags_turn_string,
                                      rotation_dir)
  }

  # Return the feature's data frame
  return(flags_turn_string)
}

#' Generate the data frame for the points that comprise the lane markers
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the lane markers
nfhs_swimming_feature_lane_markers = function(course = "SCY",
                                              lane_width = 7 / 3,
                                              number_of_lanes = 8,
                                              overflow_channels = 0.5,
                                              rotate = FALSE,
                                              rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long
  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)
  t_offset <-
    5 / 3 # ts are between 60-80 inches (5-6.67ft) or 1.5-2m from the walls
  line_thickness <- 1 / 3 # lines are 10-12in thick

  offset <- overflow_channels + lane_width / 2
  lane_list <- seq(1, number_of_lanes, 1)
  centerlines <-
    (offset + ((lane_list - 1) * lane_width)) - pool_width / 2

  lane_markers_fun <-
    function(centerline,
             pool_length,
             t_offset,
             line_thickness) {
      df = create_rectangle(
        x_min = (-pool_length / 2) + t_offset,
        x_max = (pool_length / 2) - t_offset,
        y_min = centerline - (line_thickness / 2),
        y_max = centerline + (line_thickness / 2)
      )

      return(df)
    }

  lane_markers <-
    lapply(
      centerlines,
      lane_markers_fun,
      pool_length = pool_length,
      t_offset = t_offset,
      line_thickness = line_thickness
    )

  # convert to single data frame with id column for each separate marker
  id <- seq(1, length(lane_markers)) # id column
  lane_markers <-
    Map(cbind, lane_markers, group = id) # add id column to each data frame
  lane_markers <-
    do.call("rbind", lane_markers) # bind into single data frame

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    lane_markers = rotate_coords(lane_markers,
                                 rotation_dir)
  }

  # Return the feature's data frame
  return(lane_markers)
}

#' Generate the data frame for the points that comprise the lane marker crosses at the start end
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the lane marker crosses at the start end
nfhs_swimming_feature_lane_markers_cross_start = function(course = "SCY",
                                                          lane_width = 7 / 3,
                                                          number_of_lanes = 8,
                                                          overflow_channels = 0.5,
                                                          rotate = FALSE,
                                                          rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long
  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)
  t_offset <- 5 / 3 # ts are 5ft from the walls
  line_thickness <- 1 / 3 # ts are 1ft thick
  cross_length <-
    ifelse(course %in% c("SCY"), 1 / 3, 0.9144) # ts are 1y or 0.9144m thick

  offset <- overflow_channels + lane_width / 2
  lane_list <- seq(1, number_of_lanes, 1)
  centerlines <-
    (offset + ((lane_list - 1) * lane_width)) - pool_width / 2

  lane_markers_cross_fun <-
    function(centerline,
             pool_length,
             t_offset,
             line_thickness,
             cross_length) {
      df = create_rectangle(
        x_min = (-pool_length / 2) + t_offset,
        x_max = (-pool_length / 2) + (t_offset + line_thickness),
        y_min = centerline - (cross_length / 2),
        y_max = centerline + (cross_length / 2)
      )

      return(df)
    }

  lane_markers_cross_start <-
    lapply(
      centerlines,
      lane_markers_cross_fun,
      pool_length = pool_length,
      t_offset = t_offset,
      line_thickness = line_thickness,
      cross_length = cross_length
    )

  # convert to single data frame with id column for each separate marker
  id <- seq(1, length(lane_markers_cross_start)) # id column
  lane_markers_cross_start <-
    Map(cbind, lane_markers_cross_start, group = id) # add id column to each data frame
  lane_markers_cross_start <-
    do.call("rbind", lane_markers_cross_start) # bind into single data frame

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    lane_markers_cross_start = rotate_coords(lane_markers_cross_start,
                                             rotation_dir)
  }

  # Return the feature's data frame
  return(lane_markers_cross_start)
}

#' Generate the data frame for the points that comprise the lane marker crosses at the turn end
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the lane marker crosses at the turn end
nfhs_swimming_feature_lane_markers_cross_turn = function(course = "SCY",
                                                         lane_width = 7 / 3,
                                                         number_of_lanes = 8,
                                                         overflow_channels = 0.5,
                                                         rotate = FALSE,
                                                         rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long
  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)
  t_offset <- 5 / 3 # ts are 5ft from the walls
  line_thickness <-  1 / 3 # ts are 1ft or 1/3m thick
  cross_length <-
    ifelse(course %in% c("SCY"), 1 / 3, 0.9144) # ts are 1y or 0.9144m thick

  offset <- overflow_channels + lane_width / 2
  lane_list <- seq(1, number_of_lanes, 1)
  centerlines <-
    (offset + ((lane_list - 1) * lane_width)) - pool_width / 2

  lane_markers_cross_fun <-
    function(centerline,
             pool_length,
             t_offset,
             line_thickness,
             cross_length) {
      df = create_rectangle(
        x_min = (pool_length / 2) - t_offset,
        x_max = (pool_length / 2) - (t_offset + line_thickness),
        y_min = centerline - (cross_length / 2),
        y_max = centerline + (cross_length / 2)
      )

      return(df)
    }

  lane_markers_cross_turn <-
    lapply(
      centerlines,
      lane_markers_cross_fun,
      pool_length = pool_length,
      t_offset = t_offset,
      line_thickness = line_thickness,
      cross_length = cross_length
    )

  # convert to single data frame with id column for each separate marker
  id <- seq(1, length(lane_markers_cross_turn)) # id column
  lane_markers_cross_turn <-
    Map(cbind, lane_markers_cross_turn, group = id) # add id column to each data frame
  lane_markers_cross_turn <-
    do.call("rbind", lane_markers_cross_turn) # bind into single data frame

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    lane_markers_cross_turn = rotate_coords(lane_markers_cross_turn,
                                            rotation_dir)
  }

  # Return the feature's data frame
  return(lane_markers_cross_turn)
}

#' Generate the data frame for the points that comprise the blocks
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the blocks
nfhs_swimming_feature_blocks = function(course = "SCY",
                                        lane_width = 7 / 3,
                                        number_of_lanes = 8,
                                        overflow_channels = 0.5,
                                        rotate = FALSE,
                                        rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long
  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)
  blocks_depth <-
    ifelse(course %in% c("SCY"), 34 / 12 / 3, 86.36 / 100) # blocks are 34in (86.26cm) deep
  blocks_width <-
    ifelse(course %in% c("SCY"), 34 / 12 / 3, 86.36 / 100) # blocks are 34in (86.26cm) wide

  offset <- overflow_channels + lane_width / 2
  lane_list <- seq(1, number_of_lanes, 1)
  blocks_centerlines <-
    (offset + ((lane_list - 1) * lane_width)) - (pool_width / 2)

  blocks_fun <-
    function(centerline,
             pool_length,
             blocks_depth,
             blocks_width) {
      df = create_rectangle(
        x_min = (-pool_length / 2) - blocks_depth,
        x_max = (-pool_length / 2),
        y_min = centerline - (blocks_width / 2),
        y_max = centerline + (blocks_width / 2)
      )

      return(df)
    }

  blocks <-
    lapply(
      blocks_centerlines,
      blocks_fun,
      pool_length = pool_length,
      blocks_depth = blocks_depth,
      blocks_width = blocks_width
    )

  # convert to single data frame with id column for each separate marker
  id <- seq(1, length(blocks)) # id column
  blocks <-
    Map(cbind, blocks, group = id) # add id column to each data frame
  blocks <- do.call("rbind", blocks) # bind into single data frame

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    blocks = rotate_coords(blocks,
                           rotation_dir)
  }

  # Return the feature's data frame
  return(blocks)
}

#' Generate the data frame for the points that comprise the lane lines
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the lane lines
nfhs_swimming_feature_lane_lines = function(course = "SCY",
                                            lane_width = 7 / 3,
                                            number_of_lanes = 8,
                                            overflow_channels = 0.5,
                                            rotate = FALSE,
                                            rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long
  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)
  lane_line_width <-
    ifelse(course %in% c("SCY"), 6 / 12 / 3, 15.24 / 100) # 6in or 15.24cm

  offset_width <- overflow_channels / 2
  if (overflow_channels > 0) {
    lane_list <- seq(0, number_of_lanes , 1)
  } else {
    lane_list <- seq(1, number_of_lanes - 1, 1)
  }

  lane_line_centerlines <-
    overflow_channels + (lane_list * lane_width) - (pool_width / 2)

  lane_lines_fun <-
    function(lane_line_centerline,
             pool_length,
             lane_line_width) {
      df = create_rectangle(
        x_min = (-pool_length / 2) + 1,
        x_max = (pool_length / 2) - 1,
        y_min = lane_line_centerline - (lane_line_width / 2),
        y_max = lane_line_centerline + (lane_line_width / 2)
      )

      return(df)
    }

  lane_lines <-
    lapply(
      lane_line_centerlines,
      lane_lines_fun,
      pool_length = pool_length,
      lane_line_width = lane_line_width
    )

  # convert to single data frame with id column for each separate marker
  id <- seq(1, length(lane_lines)) # id column
  lane_lines <-
    Map(cbind, lane_lines, group = id) # add id column to each data frame
  lane_lines <-
    do.call("rbind", lane_lines) # bind into single data frame

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    lane_lines = rotate_coords(lane_lines,
                               rotation_dir)
  }

  # Return the feature's data frame
  return(lane_lines)
}

#' Generate the data frame for the points that comprise the lane lines at the start end
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the lane lines at the start end
nfhs_swimming_feature_lane_lines_start = function(course = "SCY",
                                                  lane_width = 7 / 3,
                                                  number_of_lanes = 8,
                                                  overflow_channels = 0.5,
                                                  rotate = FALSE,
                                                  rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long
  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)
  lane_line_width <-
    ifelse(course %in% c("SCY"), 6 / 12 / 3, 15.24 / 100) # 6in or 15.24cm
  color_length <- 5

  offset_width <- overflow_channels / 2
  if (overflow_channels > 0) {
    lane_list <- seq(0, number_of_lanes , 1)
  } else {
    lane_list <- seq(1, number_of_lanes - 1, 1)
  }

  lane_line_centerlines <-
    overflow_channels + (lane_list * lane_width) - (pool_width / 2)

  lane_lines_fun <-
    function(lane_line_centerline,
             pool_length,
             color_length,
             lane_line_width) {
      df = create_rectangle(
        x_min = (-pool_length / 2) + 1,
        x_max = (-pool_length / 2) + color_length,
        y_min = lane_line_centerline - (lane_line_width / 2),
        y_max = lane_line_centerline + (lane_line_width / 2)
      )

      return(df)
    }

  lane_lines_start <-
    lapply(
      lane_line_centerlines,
      lane_lines_fun,
      pool_length = pool_length,
      color_length = color_length,
      lane_line_width = lane_line_width
    )

  # convert to single data frame with id column for each separate marker
  id <- seq(1, length(lane_lines_start)) # id column
  lane_lines_start <-
    Map(cbind, lane_lines_start, group = id) # add id column to each data frame
  lane_lines_start <-
    do.call("rbind", lane_lines_start) # bind into single data frame

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    lane_lines_start = rotate_coords(lane_lines_start,
                                     rotation_dir)
  }

  # Return the feature's data frame
  return(lane_lines_start)
}

#' Generate the data frame for the points that comprise the lane lines at the turn end
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the lane lines at the turn end
nfhs_swimming_feature_lane_lines_turn = function(course = "SCY",
                                                 lane_width = 7 / 3,
                                                 number_of_lanes = 8,
                                                 overflow_channels = 0.5,
                                                 rotate = FALSE,
                                                 rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long
  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)
  lane_line_width <-
    ifelse(course %in% c("SCY"), 6 / 12 / 3, 15.24 / 100) # 6in or 15.24cm
  color_length <- 5

  offset_width <- overflow_channels / 2
  if (overflow_channels > 0) {
    lane_list <- seq(0, number_of_lanes , 1)
  } else {
    lane_list <- seq(1, number_of_lanes - 1, 1)
  }

  lane_line_centerlines <-
    overflow_channels + (lane_list * lane_width) - (pool_width / 2)

  lane_lines_fun <-
    function(lane_line_centerline,
             pool_length,
             color_length,
             lane_line_width) {
      df = create_rectangle(
        x_min = (pool_length / 2) - color_length,
        x_max = (pool_length / 2) - 1,
        y_min = lane_line_centerline - (lane_line_width / 2),
        y_max = lane_line_centerline + (lane_line_width / 2)
      )

      return(df)
    }

  lane_lines_turn <-
    lapply(
      lane_line_centerlines,
      lane_lines_fun,
      pool_length = pool_length,
      color_length = color_length,
      lane_line_width = lane_line_width
    )

  # convert to single data frame with id column for each separate marker
  id <- seq(1, length(lane_lines_turn)) # id column
  lane_lines_turn <-
    Map(cbind, lane_lines_turn, group = id) # add id column to each data frame
  lane_lines_turn <-
    do.call("rbind", lane_lines_turn) # bind into single data frame

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    lane_lines_turn = rotate_coords(lane_lines_turn,
                                    rotation_dir)
  }

  # Return the feature's data frame
  return(lane_lines_turn)
}

#' Generate the data frame for the points that comprise the lane line strings
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in the same units as \code{course}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the lane line strings
nfhs_swimming_feature_lane_line_strings = function(course = "SCY",
                                                   lane_width = 7 / 3,
                                                   number_of_lanes = 8,
                                                   overflow_channels = 0.5,
                                                   rotate = FALSE,
                                                   rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- 25 # all NHFS pools are 25y or 25m long
  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)
  lane_line_string_width <-
    ifelse(course %in% c("SCY"), 1 / 12 / 3, 2.54 / 100) # 1in or 2.54cm  - needs to be large enough to be visible

  offset_width <- overflow_channels / 2
  if (overflow_channels > 0) {
    lane_list <- seq(0, number_of_lanes , 1)
  } else {
    lane_list <- seq(1, number_of_lanes - 1, 1)
  }

  lane_line_centerlines <-
    overflow_channels + (lane_list * lane_width) - (pool_width / 2)

  # using rectangles sometimes results in rectangle not being shown because it's to thin
  # heavily dependent on viewer window
  # lane_line_strings_fun <- function(lane_line_centerline, pool_length, lane_line_string_width) {
  #   df = create_rectangle(
  #     x_min = (-pool_length / 2),
  #     x_max = (pool_length / 2),
  #     y_min = lane_line_centerline - (lane_line_string_width / 2),
  #     y_max = lane_line_centerline + (lane_line_string_width / 2)
  #   )
  #
  #   return(df)
  # }

  lane_line_strings_fun <-
    function(lane_line_centerline,
             pool_length,
             lane_line_string_width) {
      df = create_line(
        x_start = (-pool_length / 2),
        x_end = (pool_length / 2),
        y_start = lane_line_centerline,
        y_end = lane_line_centerline
      )

      return(df)
    }

  lane_line_strings <-
    lapply(
      lane_line_centerlines,
      lane_line_strings_fun,
      pool_length = pool_length,
      lane_line_string_width = lane_line_string_width
    )

  # convert to single data frame with id column for each separate marker
  id <- seq(1, length(lane_line_strings)) # id column
  lane_line_strings <-
    Map(cbind, lane_line_strings, group = id) # add id column to each data frame
  lane_line_strings <-
    do.call("rbind", lane_line_strings) # bind into single data frame

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    lane_line_strings = rotate_coords(lane_line_strings,
                                      rotation_dir)
  }

  # Return the feature's data frame
  return(lane_line_strings)
}

#' Generate the list of colors for a pool. The defaults can
#' be overwritten by supplying the names of the list elements to the
#' \code{geom_swimming()} function
#'
#' @param deck_color A hexadecimal string representing the color to use for
#'   this feature
#' @param pool_color A hexadecimal string representing the color to use for
#'   this feature
#' @param m15_start_color A hexadecimal string representing the color to use for
#'   this feature
#' @param m15_turn_color A hexadecimal string representing the color to use for
#'   this feature
#' @param m15_markers_color A hexadecimal string representing the color to use for
#'   this feature
#' @param flags_start_color A hexadecimal string representing the color to use for
#'   this feature
#' @param flags_start_alternate_color A hexadecimal string representing the color to use for
#'   this feature
#' @param flags_turn_color A hexadecimal string representing the color to use for
#'   this feature
#' @param flags_turn_alternate_color A hexadecimal string representing the color to use for
#'   this feature
#' @param flags_string_color A hexadecimal string representing the color to use for
#'   this feature
#' @param  lane_markers_color  A hexadecimal string representing the color to use for
#'   this feature
#' @param blocks_color A hexadecimal string representing the color to use for
#'   this feature
#' @param  lane_lines_color  A hexadecimal string representing the color to use for
#'   this feature
#' @param  lane_lines_ends_color  A hexadecimal string representing the color to use for
#'   this feature
#' @param  lane_lines_string_color  A hexadecimal string representing the color to use for
#'   this feature
#'
#' @author Gregory A. Pilgrim
#'
#' @return A list of hexadecimal colors to use to color the features on the
#'   resulting plot
nfhs_swimming_features_set_colors = function(deck_color = 'grey',
                                             pool_color = 'blue',
                                             m15_start_color = 'black',
                                             m15_turn_color = 'black',
                                             m15_markers_color = 'red',
                                             flags_start_color = 'red',
                                             flags_start_alternate_color = 'white',
                                             flags_turn_color = 'red',
                                             flags_turn_alternate_color = 'white',
                                             flags_string_color = 'black',
                                             lane_markers_color = 'black',
                                             blocks_color = 'white',
                                             lane_lines_color = 'white',
                                             lane_line_ends_color = 'red',
                                             lane_line_string_color = 'black') {
  # Create the colors to use for the plot
  feature_colors = list(
    deck_color = deck_color,
    pool_color = pool_color,
    m15_start_color = m15_start_color,
    m15_turn_color = m15_turn_color,
    m15_markers_color = m15_markers_color,
    flags_start_color = flags_start_color,
    flags_start_alternate_color = flags_start_alternate_color,
    flags_turn_color = flags_turn_color,
    flags_turn_alternate_color = flags_turn_alternate_color,
    flags_string_color = flags_string_color,
    lane_markers_color = lane_markers_color,
    blocks_color = blocks_color,
    lane_lines_color = lane_lines_color,
    lane_line_ends_color = lane_line_ends_color,
    lane_line_string_color = lane_line_string_color
  )

  if (flags_start_color == flags_start_alternate_color) {
    stop("Flag and alternate flag colors must be different.")
  }

  if (flags_turn_color == flags_turn_alternate_color) {
    stop("Flag and alternate flag colors must be different.")
  }

  # Return the list of colors
  return(feature_colors)
}

#' Create a ggplot2 instance that represents a regulation pool,
#' with the center of the pool corresponding to (0, 0)
#'
#' @param course The length of the pool as "SCM" or "SCY"
#' @param lane_width The width of an individual lane
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist)
#' @param rotate A boolean indicating whether or not the surface representation
#'   needs to be rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the surface
#'   representation. Default: \code{'ccw'}
#' @param caption_color A hexadecimal string representing the color to use for
#'   the plot's caption. Default: '#707372' (grey)
#' @param background_color A hexadecimal string representing the color to use
#'   for the plot's background. Default: \code{NULL}
#' @param ... Additional arguments to pass to the function. These should be the
#'   colors to pass to the \code{nfhs_swimming_features_set_colors()} function
#'
#' @author Gregory A. Pilgrim
#'
#' @return A ggplot2 instance that represents a regulation pool
geom_nfhs_swimming = function(course,
                              lane_width = 7 / 3,
                              number_of_lanes = 8,
                              overflow_channels = 1.5,
                              rotate = FALSE,
                              rotation_dir = 'ccw',
                              caption_color = '#707372',
                              background_color = NULL,
                              ...) {
  # Create the colors to use for the plot
  color_list = nfhs_swimming_features_set_colors(...)

  # Generate the data frames for the features of a pool + deck
  deck = nfhs_swimming_feature_deck(course,
                                    lane_width,
                                    number_of_lanes,
                                    overflow_channels,
                                    rotate,
                                    rotation_dir)
  pool = nfhs_swimming_feature_pool(course,
                                    lane_width,
                                    number_of_lanes,
                                    overflow_channels,
                                    rotate,
                                    rotation_dir)
  m15_start = nfhs_swimming_feature_15m_start_line(course,
                                                   lane_width,
                                                   number_of_lanes,
                                                   overflow_channels,
                                                   rotate,
                                                   rotation_dir)
  m15_markers_start = nfhs_swimming_feature_15m_start_markers(course,
                                                              lane_width,
                                                              number_of_lanes,
                                                              overflow_channels,
                                                              rotate,
                                                              rotation_dir)
  m15_turn = nfhs_swimming_feature_15m_turn_line(course,
                                                 lane_width,
                                                 number_of_lanes,
                                                 overflow_channels,
                                                 rotate,
                                                 rotation_dir)
  m15_markers_turn = nfhs_swimming_feature_15m_turn_markers(course,
                                                            lane_width,
                                                            number_of_lanes,
                                                            overflow_channels,
                                                            rotate,
                                                            rotation_dir)
  flags_start = nfhs_swimming_feature_flags_start(course,
                                                  lane_width,
                                                  number_of_lanes,
                                                  overflow_channels,
                                                  rotate,
                                                  rotation_dir)
  flags_start_alternate = nfhs_swimming_feature_flags_start_2(course,
                                                              lane_width,
                                                              number_of_lanes,
                                                              overflow_channels,
                                                              rotate,
                                                              rotation_dir)
  flags_turn = nfhs_swimming_feature_flags_turn(course,
                                                lane_width,
                                                number_of_lanes,
                                                overflow_channels,
                                                rotate,
                                                rotation_dir)
  flags_turn_alternate = nfhs_swimming_feature_flags_turn_2(course,
                                                            lane_width,
                                                            number_of_lanes,
                                                            overflow_channels,
                                                            rotate,
                                                            rotation_dir)
  flags_start_string = nfhs_swimming_feature_flags_start_string(course,
                                                                lane_width,
                                                                number_of_lanes,
                                                                overflow_channels,
                                                                rotate,
                                                                rotation_dir)
  flags_turn_string = nfhs_swimming_feature_flags_turn_string(course,
                                                              lane_width,
                                                              number_of_lanes,
                                                              overflow_channels,
                                                              rotate,
                                                              rotation_dir)
  lane_markers = nfhs_swimming_feature_lane_markers(course,
                                                    lane_width,
                                                    number_of_lanes,
                                                    overflow_channels,
                                                    rotate,
                                                    rotation_dir)
  lane_markers_cross_start = nfhs_swimming_feature_lane_markers_cross_start(course,
                                                                            lane_width,
                                                                            number_of_lanes,
                                                                            overflow_channels,
                                                                            rotate,
                                                                            rotation_dir)
  lane_markers_cross_turn = nfhs_swimming_feature_lane_markers_cross_turn(course,
                                                                          lane_width,
                                                                          number_of_lanes,
                                                                          overflow_channels,
                                                                          rotate,
                                                                          rotation_dir)
  blocks = nfhs_swimming_feature_blocks(course,
                                        lane_width,
                                        number_of_lanes,
                                        overflow_channels,
                                        rotate,
                                        rotation_dir)
  lane_lines = nfhs_swimming_feature_lane_lines(course,
                                                lane_width,
                                                number_of_lanes,
                                                overflow_channels,
                                                rotate,
                                                rotation_dir)
  lane_lines_start = nfhs_swimming_feature_lane_lines_start(course,
                                                            lane_width,
                                                            number_of_lanes,
                                                            overflow_channels,
                                                            rotate,
                                                            rotation_dir)
  lane_lines_turn = nfhs_swimming_feature_lane_lines_turn(course,
                                                          lane_width,
                                                          number_of_lanes,
                                                          overflow_channels,
                                                          rotate,
                                                          rotation_dir)
  lane_line_strings = nfhs_swimming_feature_lane_line_strings(course,
                                                              lane_width,
                                                              number_of_lanes,
                                                              overflow_channels,
                                                              rotate,
                                                              rotation_dir)


  unit <- ifelse(course %in% c("SCM"), "meters", "yards")

  # Convert units as necessary
  # if(!(unit %in% c('m', 'meters'))){
  #   deck = convert_units(deck, 'm', unit, conversion_columns = c('x', 'y'))
  #   pool = convert_units(pool, 'm', unit, conversion_columns = c('x', 'y'))
  #   m15_start = convert_units(m15_start, 'm', unit, conversion_columns = c('x', 'y'))
  #   m15_turn = convert_units(m15_turn, 'm', unit, conversion_columns = c('x', 'y'))
  #   flags_start = convert_units(flags_start, 'm', unit, conversion_columns = c('x', 'y'))
  #   flags_turn = convert_units(flags_turn, 'm', unit, conversion_columns = c('x', 'y'))
  #   lane_markers = convert_units(lane_markers, 'm', unit, conversion_columns = c('x', 'y'))
  #   lane_markers_cross_start = convert_units(lane_markers_cross_start, 'm', unit, conversion_columns = c('x', 'y'))
  #   lane_markers_cross_turn = convert_units(lane_markers_cross_turn, 'm', unit, conversion_columns = c('x', 'y'))
  #   blocks = convert_units(blocks, 'm', unit, conversion_columns = c('x', 'y'))
  #   lane_lines = convert_units(lane_lines, 'm', unit, conversion_columns = c('x', 'y'))
  #
  # }

  # Create the initial ggplot2 instance onto which the features will be added
  g = create_plot_base(rotate, caption_color, background_color)

  # Add the features to the ggplot2 instance
  g = add_feature(g, deck, color_list$deck_color)
  g = add_feature(g, pool, color_list$pool_color)
  g = add_feature(g, m15_start, color_list$m15_start_color, alpha = 0.5)
  g = add_feature(g, m15_markers_start, group = group, color_list$m15_markers)
  g = add_feature(g, m15_turn, color_list$m15_turn_color, alpha = 0.5)
  g = add_feature(g, m15_markers_turn, group = group, color_list$m15_markers)
  g = add_feature(g,
                  lane_markers,
                  group = group,
                  color_list$lane_markers,
                  alpha = 0.75)
  g = add_feature(
    g,
    lane_markers_cross_start,
    group = group,
    color_list$lane_markers,
    alpha = 0.75
  )
  g = add_feature(
    g,
    lane_markers_cross_turn,
    group = group,
    color_list$lane_markers,
    alpha = 0.75
  )
  g = add_feature(g, blocks, group = group, color_list$blocks)
  # g = add_feature(g, lane_line_strings, group = group, color_list$lane_line_string)
  g = add_line_feature(
    g,
    lane_line_strings,
    group = group,
    color_list$lane_line_string,
    size = 0.25
  )
  g = add_feature(g, lane_lines, group = group, color_list$lane_lines)
  g = add_feature(g, lane_lines_start, group = group, color_list$lane_line_ends)
  g = add_feature(g, lane_lines_turn, group = group, color_list$lane_line_ends)
  g = add_line_feature(g, flags_start_string, color_list$flags_string, size = 0.25)
  g = add_line_feature(g, flags_turn_string, color_list$flags_string, size = 0.25)
  g = add_line_feature(
    g,
    flags_start,
    color_list$flags_start_color,
    size = 0.75,
    linetype = "dashed"
  )
  g = add_line_feature(
    g,
    flags_start_alternate,
    color_list$flags_start_alternate_color,
    size = 0.75,
    linetype = "dashed"
  )
  g = add_line_feature(g,
                       flags_turn,
                       color_list$flags_turn_color,
                       size = 0.75,
                       linetype = "dashed")
  g = add_line_feature(
    g,
    flags_turn_alternate,
    color_list$flags_turn_alternate_color,
    size = 0.75,
    linetype = "dashed"
  )

  # Return the ggplot2 instance that contains the NHFS swimming pool plot
  return(g)
}
