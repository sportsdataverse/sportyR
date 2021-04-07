#' Generate the data frame for the points that comprise the infield dirt and the
#' dirt circles around home plate
#'
#' @return A list of data frames containing the points that comprise the infield
#'   dirt and the dirt circles around home plate
mlb_feature_infield_dirt = function(){
  # The infield dirt follows an arc of radius 95' from the front-center point on
  # the pitcher's mound, then goes down the base paths to a circle of diameter
  # 26' centered at home plate, then back out along the third base line. The
  # base paths themselves are 6' wide, with the outer edge of the foul line (y =
  # x) in its center.
  a = 2
  b = (2 * (((45 - (3 * sin(pi/4))) - (45 + (3 * cos(pi/4)))) - 60.5))
  c =  (((((45 - (3 * sin(pi/4))) - (45 + (3 * cos(pi/4)))) - 60.5) ** 2) - (95 ** 2))

  angle_start = atan2(
    y = quadratic_formula(a, b, c)[1] - 60.5,
    x = quadratic_formula(a, b, c)[1]
  )/pi

  angle_end = 1 - angle_start

  infield_dirt = rbind(
    data.frame(
      x = 0,
      y = (45 - (3 * sin(pi / 4))) - (45 + (3 * cos(pi / 4)))
    ),

    data.frame(
      x = quadratic_formula(a, b, c)[1],
      y = quadratic_formula(a, b, c)[1] + ((45 - (3 * sin(pi/4))) - (45 + (3 * cos(pi/4))))
    ),

    create_circle(
      center = c(0, 60.5),
      start = angle_start,
      end = angle_end,
      d = 190
    ),

    data.frame(
      x = -quadratic_formula(a, b, c)[1],
      y = quadratic_formula(a, b, c)[1] + ((45 - (3 * sin(pi/4))) - (45 + (3 * cos(pi/4))))
    ),

    data.frame(
      x = 0,
      y = (45 - (3 * sin(pi/4))) - (45 + (3 * cos(pi/4)))
    )
  )

  # Add the dirt circle around home plate
  home_dirt = create_circle(
    center = c(0, 0),
    start = 0,
    end = 2,
    d = 26
  )

  # Return the feature's data frames as a list
  home_and_infield_dirt = list(
    home_dirt = home_dirt,
    infield_dirt = infield_dirt
  )

  return(home_and_infield_dirt)
}

#' Generate the data frame for the points that comprise the infield grass
#'
#' @return A data frame containing the points that comprise the infield grass
mlb_feature_infield_grass = function(){
  # Following a similar process to how the infield dirt was created, the
  # infield grass can also be created

  # The circle around home plate has diameter 26' (r = 13) and is centered at
  # (0, 0). The infield grass must intersect the line given by y = -x + 3
  # ((x - 0) ^ 2) + ((y - 0) ^ 2) = 13 ^ 2
  # (x ^ 2) + ((-x + 3) ^ 2) = 13 ^ 2
  # (x ^ 2) + (x ^ 2) - 6x + 9 = 169
  # (2x ^ 2) - 6x - 160 = 0
  a = 2
  b = -6
  c = -160
  quadratic_results = quadratic_formula(a, b, c)

  # Since this point is on the third base side of the field, x must be
  # negative
  x_start = quadratic_results[which(quadratic_results < 0)]

  # Now the angle can be determined (and subsequently divided by pi)
  start_theta_home_plate = acos(x_start / 13) / pi
  end_theta_home_plate = 1 - start_theta_home_plate

  # Lastly, create the coordinate points
  home_plate_dirt_circle = create_circle(
    center = c(0, 0),
    start = start_theta_home_plate,
    end = end_theta_home_plate,
    d = 26
  )

  # First base, second base, and third base are just rotations and
  # translations of these coordinates
  first_base_dirt_circle = rotate_coords(
    home_plate_dirt_circle,
    rotation_dir = 'ccw'
  )

  second_base_dirt_circle = rotate_coords(
    first_base_dirt_circle,
    rotation_dir = 'ccw'
  )

  third_base_dirt_circle = rotate_coords(
    second_base_dirt_circle,
    rotation_dir = 'ccw'
  )

  first_base_dirt_circle = translate(
    first_base_dirt_circle,
    translate_x = 90 * cos(pi/4),
    translate_y = 90 * sin(pi/4)
  )

  second_base_dirt_circle = translate(
    second_base_dirt_circle,
    translate_x = 0,
    translate_y = 127 + (3/12) + ((3/8)/12)
  )

  third_base_dirt_circle = translate(
    third_base_dirt_circle,
    translate_x = -90 * cos(pi/4),
    translate_y = 90 * sin(pi/4)
  )

  infield_grass = rbind(
    home_plate_dirt_circle,
    first_base_dirt_circle,
    second_base_dirt_circle,
    third_base_dirt_circle,
    home_plate_dirt_circle[1, ]
  )

  # Return the feature's data frame
  return(infield_grass)
}

#' Generate the data frame for the points that comprise the pitcher's mound and
#' pitcher's plate
#'
#' @return A list of data frames containing the points that comprise the
#'   pitcher's mound and pitcher's plate
mlb_feature_mound = function(){
  # The mound is an 18' circle centered 59' from the back tip of home plate
  mound = create_circle(
    center = c(0, 59),
    start = 0,
    end = 2,
    d = 18
  )

  pitchers_plate = create_rectangle(
    x_min = -1,
    x_max = 1,
    y_min = 60.5,
    y_max = 61
  )

  # Return the features' data frames as a list
  mound_and_pitchers_plate = list(
    mound = mound,
    pitchers_plate = pitchers_plate
  )

  return(mound_and_pitchers_plate)
}

#' Generate the data frame for the points that comprise the bases
#'
#' @return A list of data frames containing the points that comprise the bases
mlb_feature_bases = function(){
  # Home plate is 17" across the front edge, and its back tip is (0, 0)
  home_plate = data.frame(
    x = c(
      0,
      -8.5/12,
      -8.5/12,
      8.5/12,
      8.5/12,
      0
    ),

    y = c(
      0,
      sqrt(1 - (8.5/12) ** 2),
      sqrt(1 - (8.5/12) ** 2) + (8.5/12),
      sqrt(1 - (8.5/12) ** 2) + (8.5/12),
      sqrt(1 - (8.5/12) ** 2),
      0
    )
  )

  # First base and third base are 15" squares with their furthest corners
  # (lying on the foul line) from the tip of home plate a distance of 90'
  first_base_bag = data.frame(
    x = c(
      44.375 * sqrt(2),
      45 * sqrt(2),
      44.375 * sqrt(2),
      45 * sqrt(2) - sqrt(2 * (15 / 12) * (15 / 12)),
      44.375 * sqrt(2)
    ),

    y = c(
      44.375 * sqrt(2),
      45 * sqrt(2),
      (44.375 * sqrt(2)) + sqrt(2 * (15 / 12) * (15 / 12)),
      45 * sqrt(2),
      44.375 * sqrt(2)
    )
  )

  third_base_bag = data.frame(
    x = c(
      -44.375 * sqrt(2),
      -45 * sqrt(2),
      -44.375 * sqrt(2),
      -45 * sqrt(2) + sqrt(2 * (15 / 12) * (15 / 12)),
      -44.375 * sqrt(2)
    ),

    y = c(
      44.375 * sqrt(2),
      45 * sqrt(2),
      (44.375 * sqrt(2)) + sqrt(2 * (15 / 12) * (15 / 12)),
      45 * sqrt(2),
      44.375 * sqrt(2)
    )
  )

  # The center of the second base bag is 127' 3 3/8" from the back tip of home
  # plate. It is also a 15" square
  second_base_center = 127 + (3/12) + ((3/8)/12)
  second_base_bag = data.frame(
    x = c(
      0,
      (15/12) * cos(pi/4),
      0,
      (15/12) * cos(3 * pi/4),
      0
    ),

    y = c(
      second_base_center - ((15/12) * cos(pi/4)),
      second_base_center,
      second_base_center + ((15/12) * cos(pi/4)),
      second_base_center,
      second_base_center - ((15/12) * cos(pi/4))
    )
  )

  # Return the feature's data frames as a list
  bases = list(
    home_plate = home_plate,
    first_base_bag = first_base_bag,
    second_base_bag = second_base_bag,
    third_base_bag = third_base_bag
  )

  return(bases)
}

#' Generate the data frame for the points that comprise the batter's boxes and
#' catcher's box
#'
#' @return A list of data frames containing the points that comprise the
#'   batter's boxes and catcher's box added
mlb_feature_batters_boxes = function(){
  # The lefty's batter's box are 4' wide and 6' tall
  lefty_batters_box = data.frame(
    x = c(
      38.5 / 12,
      14.5 / 12,
      14.5 / 12,
      38.5 / 12,
      38.5 / 12,
      17.5 / 12,
      17.5 / 12,
      38.5 / 12,
      38.5 / 12,
      62.5 / 12,
      62.5 / 12,
      38.5 / 12,
      38.5 / 12,
      59.5 / 12,
      59.5 / 12,
      38.5 / 12,
      38.5 / 12
    ),

    y = c(
      sqrt(1 - (8.5 / 12) ^ 2) - 3,
      sqrt(1 - (8.5 / 12) ^ 2) - 3,
      sqrt(1 - (8.5 / 12) ^ 2) + 3,
      sqrt(1 - (8.5 / 12) ^ 2) + 3,
      sqrt(1 - (8.5 / 12) ^ 2) + 2.75,
      sqrt(1 - (8.5 / 12) ^ 2) + 2.75,
      sqrt(1 - (8.5 / 12) ^ 2) - 2.75,
      sqrt(1 - (8.5 / 12) ^ 2) - 2.75,
      sqrt(1 - (8.5 / 12) ^ 2) - 3,
      sqrt(1 - (8.5 / 12) ^ 2) - 3,
      sqrt(1 - (8.5 / 12) ^ 2) + 3,
      sqrt(1 - (8.5 / 12) ^ 2) + 3,
      sqrt(1 - (8.5 / 12) ^ 2) + 2.75,
      sqrt(1 - (8.5 / 12) ^ 2) + 2.75,
      sqrt(1 - (8.5 / 12) ^ 2) - 2.75,
      sqrt(1 - (8.5 / 12) ^ 2) - 2.75,
      sqrt(1 - (8.5 / 12) ^ 2) - 3
    )
  )

  # The righty's batter's box is the same as the lefty's, but reflected over
  # the y axis
  righty_batters_box = reflect(
    lefty_batters_box,
    over_y = TRUE
  )

  # The catcher's box is 43" and centered along the y axis
  catchers_box = data.frame(
    x = c(
      -23.5 / 12,
      -23.5 / 12,
      23.5 / 12,
      23.5 / 12,
      20.5 / 12,
      20.5 / 12,
      -20.5 / 12,
      -20.5 / 12,
      -23.5 / 12
    ),

    y = c(
      sqrt(1 - (8.5 / 12) ^ 2) - 3,
      -8,
      -8,
      sqrt(1 - (8.5 / 12) ^ 2) - 3,
      sqrt(1 - (8.5 / 12) ^ 2) - 3,
      -7.75,
      -7.75,
      sqrt(1 - (8.5 / 12) ^ 2) - 3,
      sqrt(1 - (8.5 / 12) ^ 2) - 3
    )
  )

  # Return the feature's data frames as a list
  batters_boxes = list(
    lefty_batters_box = lefty_batters_box,
    righty_batters_box = righty_batters_box,
    catchers_box = catchers_box
  )
  return(batters_boxes)
}

#' Generate the data frame for the points that comprise the baselines and the
#' running lane
#'
#' @return A list of data frames containing the points that comprise the
#'   baselines and running lane
mlb_feature_baselines = function(){
  # The right field line
  rf_line = data.frame(
    x = c(
      sqrt(1 - (8.5/12) ** 2) + 3,
      155.5,
      155.25,
      sqrt(1 - (8.5/12) ** 2) + 2.75,
      sqrt(1 - (8.5/12) ** 2) + 3
    ),

    y = c(
      sqrt(1 - (8.5/12) ** 2) + 3,
      155.5,
      155.5,
      sqrt(1 - (8.5/12) ** 2) + 3,
      sqrt(1 - (8.5/12) ** 2) + 3
    )
  )

  # The left field line is the reflection of the right field line over the y
  # axis
  lf_line = reflect(
    rf_line,
    over_y = TRUE
  )

  # The running lane
  running_lane = data.frame(
    x = c(
      22.5 * sqrt(2),
      (22.5 * sqrt(2)) + (3 * cos(pi/4)),
      ((22.5 * sqrt(2)) + (3 * cos(pi/4))) + (45 * cos(pi/4)),
      (((22.5 * sqrt(2)) + (3 * cos(pi/4))) + (45 * cos(pi/4))) - ((3/12) * cos(pi/4)),
      ((((22.5 * sqrt(2)) + (3 * cos(pi/4))) + (45 * cos(pi/4))) - ((3/12) * cos(pi/4))) - (44.75 * cos(pi/4)),
      (((((22.5 * sqrt(2)) + (3 * cos(pi/4))) + (45 * cos(pi/4))) - ((3/12) * cos(pi/4))) - (44.75 * cos(pi/4))) - (3 * cos(pi/4))
    ),

    y = c(
      22.5 * sqrt(2),
      (22.5 * sqrt(2)) - (3 * sin(pi/4)),
      ((22.5 * sqrt(2)) - (3 * sin(pi/4))) + (45 * sin(pi/4)),
      (((22.5 * sqrt(2)) - (3 * sin(pi/4))) + (45 * sin(pi/4))) + ((3/12) * sin(pi/4)),
      ((((22.5 * sqrt(2)) - (3 * sin(pi/4))) + (45 * sin(pi/4))) + ((3/12) * sin(pi/4))) - (44.75 * cos(pi/4)),
      (((((22.5 * sqrt(2)) - (3 * sin(pi/4))) + (45 * sin(pi/4))) + ((3/12) * sin(pi/4))) - (44.75 * cos(pi/4))) + (3 * sin(pi/4))
    )
  )

  # Return the feature's data frames as a list
  baselines = list(
    rf_line = rf_line,
    lf_line = lf_line,
    running_lane = running_lane
  )

  return(baselines)
}

#' Generate the list of colors for an MLB field plot. The defaults can be
#' overwritten by supplying the names of the list elements to the
#' \code{geom_mlb()} function (or its wrapper \code{geom_baseball()})
#'
#' @param infield_dirt_color A hexadecimal string representing the color to use
#'   for this feature
#' @param home_dirt_color A hexadecimal string representing the color to use for
#'   this feature
#' @param infield_grass_color A hexadecimal string representing the color to use
#'   for this feature
#' @param mound_color A hexadecimal string representing the color to use for
#'   this feature
#' @param pitchers_plate_color A hexadecimal string representing the color to
#'   use for this feature
#' @param home_plate_color A hexadecimal string representing the color to use
#'   for this feature
#' @param first_base_bag_color A hexadecimal string representing the color to
#'   use for this feature
#' @param second_base_bag_color A hexadecimal string representing the color to
#'   use for this feature
#' @param third_base_bag_color A hexadecimal string representing the color to
#'   use for this feature
#' @param lefty_batters_box_color A hexadecimal string representing the color to
#'   use for this feature
#' @param righty_batters_box_color A hexadecimal string representing the color
#'   to use for this feature
#' @param catchers_box_color A hexadecimal string representing the color to use
#'   for this feature
#' @param rf_line_color A hexadecimal string representing the color to use for
#'   this feature
#' @param lf_line_color A hexadecimal string representing the color to use for
#'   this feature
#' @param running_lane_color A hexadecimal string representing the color to use
#'   for this feature
#'
#' @return A list of hexadecimal colors to use to color the features on the
#'   resulting plot
mlb_features_set_colors = function(infield_dirt_color = '#9b7653',
                                   home_dirt_color = '#9b7653',
                                   infield_grass_color = '#395d33',
                                   mound_color = '#9b7653',
                                   pitchers_plate_color = '#ffffff',
                                   home_plate_color = '#ffffff',
                                   first_base_bag_color = '#ffffff',
                                   second_base_bag_color = '#ffffff',
                                   third_base_bag_color = '#ffffff',
                                   lefty_batters_box_color = '#ffffff',
                                   righty_batters_box_color = '#ffffff',
                                   catchers_box_color = '#ffffff',
                                   rf_line_color = '#ffffff',
                                   lf_line_color = '#ffffff',
                                   running_lane_color = '#ffffff'
){
  # Create the colors to use for the plot
  feature_colors = list(
    infield_dirt_color = infield_dirt_color,
    home_dirt_color = home_dirt_color,
    infield_grass_color = infield_grass_color,
    mound_color = mound_color,
    pitchers_plate_color = pitchers_plate_color,
    home_plate_color = home_plate_color,
    first_base_bag_color = first_base_bag_color,
    second_base_bag_color = second_base_bag_color,
    third_base_bag_color = third_base_bag_color,
    lefty_batters_box_color = lefty_batters_box_color,
    righty_batters_box_color = righty_batters_box_color,
    catchers_box_color = catchers_box_color,
    rf_line_color = rf_line_color,
    lf_line_color = lf_line_color,
    running_lane_color = running_lane_color
  )

  # Return the list of colors
  return(feature_colors)
}

#' Create a ggplot2 instance that represents a regulation NBA court, with the
#' back tip of home plate corresponding to (0, 0)
#'
#' @param caption_color A hexadecimal string representing the color to use for
#'   the plot's caption. Default: '#ffffff' (white)
#' @param background_color A hexadecimal string representing the color to use
#'   for the plot's background. Default: '#395d33' (green)
#' @param unit A string indicating the units with which to make the plot.
#'   Default: \code{'ft'}
#' @param ... Additional arguments to pass to the function. These should be the
#'   colors to pass to the \code{mlb_features_set_colors()} function
#'
#' @return A ggplot2 instance that represents a regulation NBA court
geom_mlb = function(caption_color = '#707372',
                    background_color = '#395d33',
                    unit = 'ft',
                    ...
){
  # Force the plot unit to be lower case
  unit = tolower(unit)

  # Create the colors to use for the plot
  color_list = mlb_features_set_colors(...)

  # Generate the data frames for the features of an MLB field
  infield_dirt = mlb_feature_infield_dirt()
  infield_grass = mlb_feature_infield_grass()
  mound = mlb_feature_mound()
  bases = mlb_feature_bases()
  batters_boxes = mlb_feature_batters_boxes()
  baselines = mlb_feature_baselines()

  # Convert between units as necessary
  if(!(unit %in% c('ft', 'feet'))){
    infield_dirt$infield_dirt = convert_units(infield_dirt$infield_dirt, 'ft', unit, conversion_columns = c('x', 'y'))
    infield_dirt$home_dirt = convert_units(infield_dirt$home_dirt, 'ft', unit, conversion_columns = c('x', 'y'))
    infield_grass = convert_units(infield_grass, 'ft', unit, conversion_columns = c('x', 'y'))
    mound$mound = convert_units(mound$mound, 'ft', unit, conversion_columns = c('x', 'y'))
    mound$pitchers_plate = convert_units(mound$pitchers_plate, 'ft', unit, conversion_columns = c('x', 'y'))
    bases$home_plate = convert_units(bases$home_plate, 'ft', unit, conversion_columns = c('x', 'y'))
    bases$first_base_bag = convert_units(bases$first_base_bag, 'ft', unit, conversion_columns = c('x', 'y'))
    bases$second_base_bag = convert_units(bases$second_base_bag, 'ft', unit, conversion_columns = c('x', 'y'))
    bases$third_base_bag = convert_units(bases$third_base_bag, 'ft', unit, conversion_columns = c('x', 'y'))
    batters_boxes$lefty_batters_box = convert_units(batters_boxes$lefty_batters_box, 'ft', unit, conversion_columns = c('x', 'y'))
    batters_boxes$righty_batters_box = convert_units(batters_boxes$righty_batters_box, 'ft', unit, conversion_columns = c('x', 'y'))
    batters_boxes$catchers_box = convert_units(batters_boxes$catchers_box, 'ft', unit, conversion_columns = c('x', 'y'))
    baselines$rf_line = convert_units(baselines$rf_line, 'ft', unit, conversion_columns = c('x', 'y'))
    baselines$lf_line = convert_units(baselines$lf_line, 'ft', unit, conversion_columns = c('x', 'y'))
    baselines$running_lane = convert_units(baselines$running_lane, 'ft', unit, conversion_columns = c('x', 'y'))
  }

  # Create the initial ggplot2 instance onto which the features will be added
  g = create_plot_base(rotate = FALSE, caption_color, background_color)

  # Add the features to the ggplot2 instance
  g = add_feature(g, infield_dirt$infield_dirt, color_list$infield_dirt_color)
  g = add_feature(g, infield_dirt$home_dirt, color_list$home_dirt_color)
  g = add_feature(g, infield_grass, color_list$infield_grass_color)
  g = add_feature(g, mound$mound, color_list$mound_color)
  g = add_feature(g, mound$pitchers_plate, color_list$pitchers_plate_color)
  g = add_feature(g, bases$home_plate, color_list$home_plate_color)
  g = add_feature(g, bases$first_base_bag, color_list$first_base_bag_color)
  g = add_feature(g, bases$second_base_bag, color_list$second_base_bag_color)
  g = add_feature(g, bases$third_base_bag, color_list$third_base_bag_color)
  g = add_feature(g, batters_boxes$lefty_batters_box, color_list$lefty_batters_box_color)
  g = add_feature(g, batters_boxes$righty_batters_box, color_list$righty_batters_box_color)
  g = add_feature(g, batters_boxes$catchers_box, color_list$catchers_box_color)
  g = add_feature(g, baselines$rf_line, color_list$rf_line)
  g = add_feature(g, baselines$lf_line, color_list$lf_line)
  g = add_feature(g, baselines$running_lane, color_list$running_lane)

  # Return the ggplot2 instance that contains the field plot
  return(g)
}
