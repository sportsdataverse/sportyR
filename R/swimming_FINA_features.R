#' Generate the data frame for the points that comprise the pool deck background
#'
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the pool deck
fina_swimming_feature_deck = function(course = "LCM",
                                      number_of_lanes = 8,
                                      overflow_channels = 0.2,
                                      rotate = FALSE,
                                      rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m

  # This gives the grey background of the deck
  deck = create_rectangle(
    x_min = (-pool_length / 2) - 3,
    x_max = (pool_length / 2) + 3,
    y_min = ((-lane_width * number_of_lanes) / 2) - (overflow_channels) - 3,
    y_max = ((lane_width * number_of_lanes) / 2) +  (overflow_channels) + 3
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
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the pool
fina_swimming_feature_pool = function(course = "LCM",
                                      number_of_lanes = 8,
                                      overflow_channels = 0.2,
                                      rotate = FALSE,
                                      rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m

  # This gives the blue background of the pool
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
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the 15m line from the start end
fina_swimming_feature_15m_start_line = function(course = "LCM",
                                                number_of_lanes = 8,
                                                overflow_channels = 0.2,
                                                rotate = FALSE,
                                                rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m
  m15_distance <- 15 # 15m mark is always at 15m
  line_thickness <- 0.25 # line is 0.25m thick
  cross_length <- 0.5 # crosses are 0.5m long

  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)

  offset <- overflow_channels + lane_width / 2
  lane_list <- seq(1, number_of_lanes, 1)
  centerlines <-
    (offset + ((lane_list - 1) * lane_width)) - pool_width / 2

  # 15 meter marks are 15 meters from the start and turn ends of the pool
  m15_line_start_fun <-
    function(centerline,
             pool_length,
             m15_distance,
             line_thickness,
             cross_length) {
      df = create_rectangle(
        x_min = (-pool_length / 2) + m15_distance - (line_thickness / 2),
        x_max = (-pool_length / 2) + m15_distance + (line_thickness / 2),
        y_min = centerline - (cross_length / 2),
        y_max = centerline + (cross_length / 2)
      )

      return(df)
    }

  m15_line_start <-
    lapply(
      centerlines,
      m15_line_start_fun,
      pool_length = pool_length,
      m15_distance = m15_distance,
      line_thickness = line_thickness,
      cross_length = cross_length
    )

  # convert to single data frame with id column for each separate marker
  id <- seq(1, length(m15_line_start)) # id column
  m15_line_start <-
    Map(cbind, m15_line_start, group = id) # add id column to each data frame
  m15_line_start <-
    do.call("rbind", m15_line_start) # bind into single data frame

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    m15_line_start = rotate_coords(m15_line_start,
                                   rotation_dir)
  }

  # Return the feature's data frame
  return(m15_line_start)
}

#' Generate the data frame for the points that comprise the 15m line for the turn end of the pool
#'
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the 15m line from the turn end
fina_swimming_feature_15m_turn_line = function(course = "LCM",
                                               number_of_lanes = 8,
                                               overflow_channels = 0.2,
                                               rotate = FALSE,
                                               rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m
  m15_distance <- 15 # 15m mark is always at 15m
  line_thickness <- 0.25 # line is 0.25m thick
  cross_length <- 0.5 # crosses are 0.5m long

  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)

  offset <- overflow_channels + lane_width / 2
  lane_list <- seq(1, number_of_lanes, 1)
  centerlines <-
    (offset + ((lane_list - 1) * lane_width)) - pool_width / 2

  # 15 meter marks are 15 meters from the start and turn ends of the pool
  m15_line_turn_fun <-
    function(centerline,
             pool_length,
             m15_distance,
             line_thickness,
             cross_length) {
      df = create_rectangle(
        x_min = (pool_length / 2) - m15_distance - (line_thickness / 2),
        x_max = (pool_length / 2) - m15_distance + (line_thickness / 2),
        y_min = centerline - (cross_length / 2),
        y_max = centerline + (cross_length / 2)
      )

      return(df)
    }

  m15_line_turn <-
    lapply(
      centerlines,
      m15_line_turn_fun,
      pool_length = pool_length,
      m15_distance = m15_distance,
      line_thickness = line_thickness,
      cross_length = cross_length
    )

  # convert to single data frame with id column for each separate marker
  id <- seq(1, length(m15_line_turn)) # id column
  m15_line_turn <-
    Map(cbind, m15_line_turn, group = id) # add id column to each data frame
  m15_line_turn <-
    do.call("rbind", m15_line_turn) # bind into single data frame


  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    m15_line_turn = rotate_coords(m15_line_turn,
                                  rotation_dir)
  }

  # Return the feature's data frame
  return(m15_line_turn)
}

#' Generate the data frame for the points that comprise the center line for the pool
#'
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise center line for the pool
fina_swimming_feature_center_line = function(course = "LCM",
                                             number_of_lanes = 8,
                                             overflow_channels = 0.2,
                                             rotate = FALSE,
                                             rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m
  line_thickness <- 0.25 # line is 0.25m thick
  cross_length <- 1 # crosses are 1m long

  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)

  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)

  offset <- overflow_channels + lane_width / 2
  lane_list <- seq(1, number_of_lanes, 1)
  centerlines <-
    (offset + ((lane_list - 1) * lane_width)) - pool_width / 2

  # center line is at x = 0
  center_line_fun <-
    function(centerline,
             pool_length,
             t_offset,
             line_thickness,
             cross_length) {
      df = create_rectangle(
        x_min = 0 - (line_thickness / 2),
        x_max = 0 + (line_thickness / 2),
        y_min = centerline - (cross_length / 2),
        y_max = centerline + (cross_length / 2)
      )

      return(df)
    }

  center_line <-
    lapply(
      centerlines,
      center_line_fun,
      pool_length = pool_length,
      line_thickness = line_thickness,
      cross_length = cross_length
    )

  # convert to single data frame with id column for each separate marker
  id <- seq(1, length(center_line)) # id column
  center_line <-
    Map(cbind, center_line, group = id) # add id column to each data frame
  center_line <-
    do.call("rbind", center_line) # bind into single data frame


  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    center_line = rotate_coords(center_line,
                                rotation_dir)
  }

  # Return the feature's data frame
  return(center_line)
}

#' Generate the data frame for the points that comprise the center markers on lane lines
#'
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the center markers on lane lines
fina_swimming_feature_lane_lines_center = function(course = "LCM",
                                                   number_of_lanes = 8,
                                                   overflow_channels = 0.2,
                                                   rotate = FALSE,
                                                   rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m
  lane_line_width <- 0.125 # 0.125m
  bouy_thickness <- 10 / 100

  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)

  lane_list <- seq(0, number_of_lanes, 1)

  lane_line_centerlines <-
    overflow_channels + (lane_list * lane_width) - (pool_width / 2)

  # center line is at x = 0
  center_line_fun <-
    function(lane_line_centerline,
             bouy_thickness,
             lane_line_width) {
      df = create_rectangle(
        x_min = 0 - (3 * bouy_thickness),
        x_max = 0 + (3 * bouy_thickness),
        y_min = lane_line_centerline - (lane_line_width / 2),
        y_max = lane_line_centerline + (lane_line_width / 2)
      )

      return(df)
    }

  lane_line_center <-
    lapply(
      lane_line_centerlines,
      center_line_fun,
      bouy_thickness = bouy_thickness,
      lane_line_width = lane_line_width
    )

  # convert to single data frame with id column for each separate marker
  id <- seq(1, length(lane_line_center)) # id column
  lane_line_center <-
    Map(cbind, lane_line_center, group = id) # add id column to each data frame
  lane_line_center <-
    do.call("rbind", lane_line_center) # bind into single data frame


  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    lane_line_center = rotate_coords(lane_line_center,
                                     rotation_dir)
  }

  # Return the feature's data frame
  return(lane_line_center)
}

#' Generate the data frame for the points that comprise the backstroke flags for the start end of the pool
#'
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the backstroke flags for the start end
fina_swimming_feature_flags_start = function(course = "LCM",
                                             number_of_lanes = 8,
                                             overflow_channels = 0.2,
                                             rotate = FALSE,
                                             rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)

  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m
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

#' Generate the data frame for the points that comprise the backstroke flags string for the start end of the pool
#'
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the backstroke flags string for the start end
fina_swimming_feature_flags_start_string = function(course = "LCM",
                                                    number_of_lanes = 8,
                                                    overflow_channels = 0.2,
                                                    rotate = FALSE,
                                                    rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)

  # course = "LCM"
  # lane_width = 2.5
  # number_of_lanes = 8
  # overflow_channels = 1
  # rotate = FALSE
  # rotation_dir = 'ccw'

  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m
  # flags_distance <- ifelse(course %in% c("SCY"), 5.468, 5)
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
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#'  @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the flags at the turn end
fina_swimming_feature_flags_turn = function(course = "LCM",
                                            number_of_lanes = 8,
                                            overflow_channels = 0.2,
                                            rotate = FALSE,
                                            rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m
  # flags_distance <- ifelse(course %in% c("SCY"), 5.468, 5)
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

#' Generate the data frame for the points that comprise the backstroke flags string for the turn end of the pool
#'
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#'  @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the flags string at the turn end
fina_swimming_feature_flags_turn_string = function(course = "LCM",
                                                   number_of_lanes = 8,
                                                   overflow_channels = 0.2,
                                                   rotate = FALSE,
                                                   rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m
  # flags_distance <- ifelse(course %in% c("SCY"), 5.468, 5)
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
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#'  @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the lane markers
fina_swimming_feature_lane_markers = function(course = "LCM",
                                              number_of_lanes = 8,
                                              overflow_channels = 0.2,
                                              rotate = FALSE,
                                              rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m
  t_offset <- 2 # ts are 2m from the walls
  line_thickness <- 0.25 # ts are 0.25m thick
  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)

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
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#'  @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the lane marker crosses at the start end
fina_swimming_feature_lane_markers_cross_start = function(course = "LCM",
                                                          number_of_lanes = 8,
                                                          overflow_channels = 0.2,
                                                          rotate = FALSE,
                                                          rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m
  t_offset <- 2 # ts are 2m from the walls
  line_thickness <- 0.25 # ts are 0.25m thick
  cross_length <- 1 # crosses are 1m long

  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)

  offset <- overflow_channels + lane_width / 2
  lane_list <- seq(1, number_of_lanes, 1)
  centerlines <-
    (offset + ((lane_list - 1) * lane_width)) - (pool_width / 2)

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
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the lane marker crosses at the turn end
fina_swimming_feature_lane_markers_cross_turn = function(course = "LCM",
                                                         number_of_lanes = 8,
                                                         overflow_channels = 0.2,
                                                         rotate = FALSE,
                                                         rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m
  t_offset <- 2 # ts are 2m from the walls
  line_thickness <- 0.25 # ts are 0.25m thick
  cross_length <- 1 # crosses are 1m long

  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)

  offset <- overflow_channels + lane_width / 2
  lane_list <- seq(1, number_of_lanes, 1)
  centerlines <-
    (offset + ((lane_list - 1) * lane_width)) - (pool_width / 2)

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
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the blocks
fina_swimming_feature_blocks = function(course = "LCM",
                                        number_of_lanes = 8,
                                        overflow_channels = 0.2,
                                        rotate = FALSE,
                                        rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m
  blocks_depth <- 86.36 / 100 # blocks are 34in (86.26cm) deep
  blocks_width <- 86.36 / 100 # blocks are 34in (86.26cm) wide

  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)

  offset <- overflow_channels + lane_width / 2
  lane_list <- seq(1, number_of_lanes, 1)
  blocks_centerlines <-
    (offset + ((lane_list - 1) * lane_width)) - pool_width / 2

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
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the lane lines
fina_swimming_feature_lane_lines = function(course = "LCM",
                                            number_of_lanes = 8,
                                            overflow_channels = 0.2,
                                            rotate = FALSE,
                                            rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m
  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)
  lane_line_width <- 0.125 # 0.125m
  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)

  lane_list <- seq(0, number_of_lanes, 1)

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

#' Generate the data frame for the points that comprise the lane lines at the start end.  FINA lane lines must extend to the end of the course
#'
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the lane lines at the start end
fina_swimming_feature_lane_lines_start = function(course = "LCM",
                                                  number_of_lanes = 8,
                                                  overflow_channels = 0.2,
                                                  rotate = FALSE,
                                                  rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m
  lane_line_width <- 0.125 # 0.125m
  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)

  lane_list <- seq(0, number_of_lanes, 1)

  lane_line_centerlines <-
    overflow_channels + (lane_list * lane_width) - (pool_width / 2)

  lane_lines_fun <-
    function(lane_line_centerline,
             pool_length,
             lane_line_width) {
      df = create_rectangle(
        x_min = (-pool_length / 2),
        x_max = (-pool_length / 2 + 5),
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

#' Generate the data frame for the points that comprise the lane line bouy marking 15m from the start end.
#'
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the lane line bouy marking 15m at the start end
fina_swimming_feature_lane_lines_start_15m = function(course = "LCM",
                                                      number_of_lanes = 8,
                                                      overflow_channels = 0.2,
                                                      rotate = FALSE,
                                                      rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m
  lane_line_width <- 0.125 # 0.125m
  m15_distance <- 15
  bouy_thickness <- 10 / 100

  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)

  lane_list <- seq(0, number_of_lanes, 1)

  lane_line_centerlines <-
    overflow_channels + (lane_list * lane_width) - (pool_width / 2)

  lane_lines_fun <-
    function(lane_line_centerline,
             pool_length,
             m15_distance,
             bouy_thickness,
             lane_line_width) {
      df = create_rectangle(
        x_min = (-pool_length / 2) + m15_distance,
        x_max = (-pool_length / 2) + m15_distance + bouy_thickness,
        y_min = lane_line_centerline - (lane_line_width / 2),
        y_max = lane_line_centerline + (lane_line_width / 2)
      )

      return(df)
    }

  lane_lines_start_15m <-
    lapply(
      lane_line_centerlines,
      lane_lines_fun,
      pool_length = pool_length,
      m15_distance = m15_distance,
      bouy_thickness = bouy_thickness,
      lane_line_width = lane_line_width
    )

  # convert to single data frame with id column for each separate marker
  id <- seq(1, length(lane_lines_start_15m)) # id column
  lane_lines_start_15m <-
    Map(cbind, lane_lines_start_15m, group = id) # add id column to each data frame
  lane_lines_start_15m <-
    do.call("rbind", lane_lines_start_15m) # bind into single data frame

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    lane_lines_start_15m = rotate_coords(lane_lines_start_15m,
                                         rotation_dir)
  }

  # Return the feature's data frame
  return(lane_lines_start_15m)
}

#' Generate the data frame for the points that comprise the lane lines at the turn end.  FINA lane lines must extend to the end of the course
#'
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the lane lines at the turn end
fina_swimming_feature_lane_lines_turn = function(course = "LCM",
                                                 number_of_lanes = 8,
                                                 overflow_channels = 0.2,
                                                 rotate = FALSE,
                                                 rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m
  lane_line_width <- 0.125 # 0.125m

  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)

  lane_list <- seq(0, number_of_lanes, 1)

  lane_line_centerlines <-
    overflow_channels + (lane_list * lane_width) - (pool_width / 2)

  lane_lines_fun <-
    function(lane_line_centerline,
             pool_length,
             lane_line_width) {
      df = create_rectangle(
        x_min = (pool_length / 2 - 5),
        x_max = (pool_length / 2),
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

#' Generate the data frame for the points that comprise the lane line bouy marking 15m from the turn end.
#'
#' @param course The length of the pool as "SCM" or "LCM"
#' @param number_of_lanes The number of lanes in the pool
#' @param overflow_channels Width of overflow channels (if they exist) in meters
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @author Gregory A. Pilgrim
#'
#' @return A data frame containing the points that comprise the lane line bouy marking 15m at the turn end
fina_swimming_feature_lane_lines_turn_15m = function(course = "LCM",
                                                     number_of_lanes = 8,
                                                     overflow_channels = 0.2,
                                                     rotate = FALSE,
                                                     rotation_dir = 'ccw') {
  # Initialize x and y (to pass checks)
  x = y = NULL

  pool_length <- ifelse(course %in% c("SCM"), 25, 50)
  lane_width <- 2.5 # must be 2.5m
  lane_line_width <- 0.125 # 0.125m
  bouy_thickness <- 10 / 100
  m15_distance <- 15

  pool_width <-
    (lane_width * number_of_lanes) + (overflow_channels * 2)

  lane_list <- seq(0, number_of_lanes, 1)

  lane_line_centerlines <-
    overflow_channels + (lane_list * lane_width) - (pool_width / 2)

  lane_lines_fun <-
    function(lane_line_centerline,
             m15_distance,
             bouy_thickness,
             lane_line_width) {
      df = create_rectangle(
        x_min = (pool_length / 2) - m15_distance - bouy_thickness,
        x_max = (pool_length / 2) - m15_distance,
        y_min = lane_line_centerline - (lane_line_width / 2),
        y_max = lane_line_centerline + (lane_line_width / 2)
      )

      return(df)
    }

  lane_lines_start_15m <-
    lapply(
      lane_line_centerlines,
      lane_lines_fun,
      m15_distance = m15_distance,
      bouy_thickness = bouy_thickness,
      lane_line_width = lane_line_width
    )

  # convert to single data frame with id column for each separate marker
  id <- seq(1, length(lane_lines_start_15m)) # id column
  lane_lines_start_15m <-
    Map(cbind, lane_lines_start_15m, group = id) # add id column to each data frame
  lane_lines_start_15m <-
    do.call("rbind", lane_lines_start_15m) # bind into single data frame

  if (rotate) {
    # If the desired output needs to be rotated, rotate the coordinates
    lane_lines_start_15m = rotate_coords(lane_lines_start_15m,
                                         rotation_dir)
  }

  # Return the feature's data frame
  return(lane_lines_start_15m)
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
#' @param center_line_color A hexadecimal string representing the color to use for
#'   this feature
#' @param flags_start_color A hexadecimal string representing the color to use for
#'   this feature
#' @param flags_turn_color A hexadecimal string representing the color to use for
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
#'
#' @return A list of hexadecimal colors to use to color the features on the
#'   resulting plot
fina_swimming_features_set_colors = function(deck_color = 'grey',
                                             pool_color = 'blue',
                                             m15_start_color = 'black',
                                             m15_turn_color = 'black',
                                             center_line_color = 'black',
                                             flags_start_color = 'red',
                                             flags_turn_color = 'red',
                                             flags_string_color = 'black',
                                             lane_markers_color = 'black',
                                             blocks_color = 'white',
                                             lane_lines_color = 'white',
                                             lane_line_ends_color = 'red',
                                             lane_line_15m_color = 'red') {
  # Create the colors to use for the plot
  feature_colors = list(
    deck_color = deck_color,
    pool_color = pool_color,
    m15_start_color = m15_start_color,
    m15_turn_color = m15_turn_color,
    center_line_color = center_line_color,
    flags_start_color = flags_start_color,
    flags_turn_color = flags_turn_color,
    flags_string_color = flags_string_color,
    lane_markers_color = lane_markers_color,
    blocks_color = blocks_color,
    lane_lines_color = lane_lines_color,
    lane_line_ends_color = lane_line_ends_color,
    lane_line_15m_color = lane_line_15m_color


  )

  # Return the list of colors
  return(feature_colors)
}

#' Create a ggplot2 instance that represents a regulation pool,
#' with the center of the pool corresponding to (0, 0)
#'
#' @param course The length of the pool as "SCM" or "LCM"
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
#'   colors to pass to the \code{fina_swimming_features_set_colors()} function
#'
#' @return A ggplot2 instance that represents a regulation pool
geom_fina_swimming = function(course,
                              number_of_lanes = 8,
                              overflow_channels = 1.5,
                              rotate = FALSE,
                              rotation_dir = 'ccw',
                              caption_color = '#707372',
                              background_color = NULL,
                              ...) {
  # Create the colors to use for the plot
  color_list = fina_swimming_features_set_colors(...)

  # Generate the data frames for the features of a pool + deck
  deck = fina_swimming_feature_deck(course,
                                    number_of_lanes,
                                    overflow_channels,
                                    rotate,
                                    rotation_dir)
  pool = fina_swimming_feature_pool(course,
                                    number_of_lanes,
                                    overflow_channels,
                                    rotate,
                                    rotation_dir)
  m15_start = fina_swimming_feature_15m_start_line(course,
                                                   number_of_lanes,
                                                   overflow_channels,
                                                   rotate,
                                                   rotation_dir)
  m15_turn = fina_swimming_feature_15m_turn_line(course,
                                                 number_of_lanes,
                                                 overflow_channels,
                                                 rotate,
                                                 rotation_dir)
  center_line = fina_swimming_feature_center_line(course,
                                                  number_of_lanes,
                                                  overflow_channels,
                                                  rotate,
                                                  rotation_dir)
  flags_start = fina_swimming_feature_flags_start(course,
                                                  number_of_lanes,
                                                  overflow_channels,
                                                  rotate,
                                                  rotation_dir)
  flags_turn = fina_swimming_feature_flags_turn(course,
                                                number_of_lanes,
                                                overflow_channels,
                                                rotate,
                                                rotation_dir)
  flags_start_string = fina_swimming_feature_flags_start_string(course,
                                                                number_of_lanes,
                                                                overflow_channels,
                                                                rotate,
                                                                rotation_dir)
  flags_turn_string = fina_swimming_feature_flags_turn_string(course,
                                                              number_of_lanes,
                                                              overflow_channels,
                                                              rotate,
                                                              rotation_dir)
  lane_markers = fina_swimming_feature_lane_markers(course,
                                                    number_of_lanes,
                                                    overflow_channels,
                                                    rotate,
                                                    rotation_dir)
  lane_markers_cross_start = fina_swimming_feature_lane_markers_cross_start(course,
                                                                            number_of_lanes,
                                                                            overflow_channels,
                                                                            rotate,
                                                                            rotation_dir)
  lane_markers_cross_turn = fina_swimming_feature_lane_markers_cross_turn(course,
                                                                          number_of_lanes,
                                                                          overflow_channels,
                                                                          rotate,
                                                                          rotation_dir)
  blocks = fina_swimming_feature_blocks(course,
                                        number_of_lanes,
                                        overflow_channels,
                                        rotate,
                                        rotation_dir)
  lane_lines = fina_swimming_feature_lane_lines(course,
                                                number_of_lanes,
                                                overflow_channels,
                                                rotate,
                                                rotation_dir)

  # FINA requires these lane line color schemes for pools with 8 and 10 lanes
  if (number_of_lanes == 8) {
    green_lane_lines <- lane_lines[lane_lines$group %in% c(1, 9), ]
    blue_lane_lines <-
      lane_lines[lane_lines$group %in% c(2, 3, 7, 8), ]
    yellow_lane_lines <-
      lane_lines[lane_lines$group %in% c(4, 5, 6), ]
  }

  if (number_of_lanes == 10) {
    green_lane_lines <- lane_lines[lane_lines$group %in% c(1, 11), ]
    blue_lane_lines <-
      lane_lines[lane_lines$group %in% c(2, 3, 4, 8, 9, 10), ]
    yellow_lane_lines <-
      lane_lines[lane_lines$group %in% c(5, 6, 7), ]
  }

  lane_lines_start = fina_swimming_feature_lane_lines_start(course,
                                                            number_of_lanes,
                                                            overflow_channels,
                                                            rotate,
                                                            rotation_dir)
  lane_lines_start_15m = fina_swimming_feature_lane_lines_start_15m(course,
                                                                    number_of_lanes,
                                                                    overflow_channels,
                                                                    rotate,
                                                                    rotation_dir)
  lane_lines_turn = fina_swimming_feature_lane_lines_turn(course,
                                                          number_of_lanes,
                                                          overflow_channels,
                                                          rotate,
                                                          rotation_dir)
  lane_lines_turn_15m = fina_swimming_feature_lane_lines_turn_15m(course,
                                                                  number_of_lanes,
                                                                  overflow_channels,
                                                                  rotate,
                                                                  rotation_dir)
  lane_lines_center = fina_swimming_feature_lane_lines_center(course,
                                                              number_of_lanes,
                                                              overflow_channels,
                                                              rotate,
                                                              rotation_dir)

  unit <- ifelse(course %in% c("SCM", "LCM"), "meters", "yards")

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
  g = add_feature(g, m15_turn, color_list$m15_turn_color, alpha = 0.5)
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
  g = add_feature(g,
                  center_line,
                  group = group,
                  color_list$center_line,
                  alpha = 0.75)
  g = add_feature(g, blocks, group = group, color_list$blocks)
  if (number_of_lanes == 8) {
    g = add_feature(g, green_lane_lines, group = group, 'green')
    g = add_feature(g, blue_lane_lines, group = group, 'lightblue')
    g = add_feature(g, yellow_lane_lines, group = group, 'yellow')
  } else if (number_of_lanes == 10) {
    g = add_feature(g, green_lane_lines, group = group, 'green')
    g = add_feature(g, blue_lane_lines, group = group, 'lightblue')
    g = add_feature(g, yellow_lane_lines, group = group, 'yellow')
  } else {
    g = add_feature(g, lane_lines, group = group, color_list$lane_lines)
  }
  g = add_feature(g, lane_lines_start, group = group, color_list$lane_line_ends)
  g = add_feature(g, lane_lines_start_15m, group = group, color_list$lane_line_ends)
  g = add_feature(g, lane_lines_turn, group = group, color_list$lane_line_ends)
  g = add_feature(g, lane_lines_turn_15m, group = group, color_list$lane_line_ends)
  g = add_feature(g, lane_lines_center, group = group, color_list$lane_line_ends)
  g = add_line_feature(g, flags_start_string, color_list$flags_string, size = 0.25)
  g = add_line_feature(g, flags_turn_string, color_list$flags_string, size = 0.25)
  g = add_line_feature(
    g,
    flags_start,
    color_list$flags_start_color,
    size = 0.75,
    linetype = "dashed"
  )
  g = add_line_feature(g,
                       flags_turn,
                       color_list$flags_turn_color,
                       size = 0.75,
                       linetype = "dashed")

  # g = add_feature(g, lane_markers, color_list$lane_markers_color)


  # Return the ggplot2 instance that contains the swimming pool plot
  return(g)
}
