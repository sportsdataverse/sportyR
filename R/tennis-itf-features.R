#' Generate the data frame for the points that comprise the court background
#'
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the court background
itf_feature_court_background = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # The court is 78' long and 36' wide (doubles), with the net posts 3' outside
  # of the doubles line. The backstop should be at minimum 21' from the outer
  # edge of the baseline
  background = create_rectangle(
    x_min = -60,
    x_max = 0,
    y_min = -24,
    y_max = 24
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the x axis
    background = rbind(
      background,
      reflect(
        background,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    background = rotate_coords(
      background,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(background)
}

#' Generate the data frame for the points that comprise the baseline(s)
#'
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the baseline(s)
itf_feature_baseline = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # The baseline spans the entire width of the court, which is 36' between the
  # outer edges of the doubles line, and is 39' from the center of the net to
  # the furthest edge of the baseline. It is 2" in width
  baseline = create_rectangle(
    x_min = -39,
    x_max = -39 + convert_units(2, 'in', 'ft'),
    y_min = -18,
    y_max = 18
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the x axis
    baseline = rbind(
      baseline,
      reflect(
        baseline,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    baseline = rotate_coords(
      baseline,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(baseline)
}

#' Generate the data frame for the points that comprise the doubles sideline(s)
#'
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the doubles
#'   sideline(s)
itf_feature_doubles_sideline = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # The doubles sideline spans the entire length of the court, which is 78'
  # between the outer edges of the baselines, and is 18' from the center of the
  # net to the furthest edge of the doubles line. It is 2" in width
  sideline_1 = create_rectangle(
    x_min = -39,
    x_max = 0,
    y_min = -18,
    y_max = -18 + convert_units(2, 'in', 'ft')
  )

  sideline_2 = create_rectangle(
    x_min = -39,
    x_max = 0,
    y_min = 18 - convert_units(2, 'in', 'ft'),
    y_max = 18
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the x axis
    sideline_1 = rbind(
      sideline_1,
      reflect(
        sideline_1,
        over_y = TRUE
      )
    )

    sideline_2 = rbind(
      sideline_2,
      reflect(
        sideline_2,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    sideline_1 = rotate_coords(
      sideline_1,
      rotation_dir
    )

    sideline_2 = rotate_coords(
      sideline_2,
      rotation_dir
    )
  }

  # Return the feature's data frame as a list
  sidelines = list(
    sideline_1 = sideline_1,
    sideline_2 = sideline_2
  )

  return(sidelines)
}

#' Generate the data frame for the points that comprise the doubles sideline(s)
#'
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the doubles
#'   sideline(s)
itf_feature_singles_sideline = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # The singles sideline spans the entire length of the court, which is 78'
  # between the outer edges of the baselines, and is 13.5' from the center of the
  # net to the furthest edge of the singles line. It is 2" in width
  sideline_1 = create_rectangle(
    x_min = -39,
    x_max = 0,
    y_min = -13.5,
    y_max = -13.5 + convert_units(2, 'in', 'ft')
  )

  sideline_2 = create_rectangle(
    x_min = -39,
    x_max = 0,
    y_min = 13.5 - convert_units(2, 'in', 'ft'),
    y_max = 13.5
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the x axis
    sideline_1 = rbind(
      sideline_1,
      reflect(
        sideline_1,
        over_y = TRUE
      )
    )

    sideline_2 = rbind(
      sideline_2,
      reflect(
        sideline_2,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    sideline_1 = rotate_coords(
      sideline_1,
      rotation_dir
    )

    sideline_2 = rotate_coords(
      sideline_2,
      rotation_dir
    )
  }

  # Return the feature's data frame as a list
  sidelines = list(
    sideline_1 = sideline_1,
    sideline_2 = sideline_2
  )

  return(sidelines)
}

#' Generate the data frame for the points that comprise the service line(s)
#'
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the service line(s)
itf_feature_service_line = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # The service line is a 2" thick line that is 21' (outer edge) from the center
  # of the court. It extends the 27' within the singles sidelines
  service_line = create_rectangle(
    x_min = -21,
    x_max = -21 + convert_units(2, 'in', 'ft'),
    y_min = -13.5,
    y_max = 13.5
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the x axis
    service_line = rbind(
      service_line,
      reflect(
        service_line,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    service_line = rotate_coords(
      service_line,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(service_line)
}

#' Generate the data frame for the points that comprise the center service
#' line(s)
#'
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the center service
#'   line(s)
itf_feature_center_service_line = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # The center service line is a 2" thick line that extends 21' from the center
  # of the net
  center_service_line = create_rectangle(
    x_min = -21,
    x_max = 0,
    y_min = convert_units(1, 'in', 'ft'),
    y_max = -convert_units(1, 'in', 'ft')
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the x axis
    center_service_line = rbind(
      center_service_line,
      reflect(
        center_service_line,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    center_service_line = rotate_coords(
      center_service_line,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(center_service_line)
}

#' Generate the data frame for the points that comprise the center mark(s)
#'
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the center mark(s)
itf_feature_center_mark = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # The center mark line is a 2" thick line that extends 4" onto the court from
  # the inner edge of each baseline
  center_mark = create_rectangle(
    x_min = -39 + convert_units(2, 'in', 'ft'),
    x_max = -39 + convert_units(6, 'in', 'ft'),
    y_min = convert_units(1, 'in', 'ft'),
    y_max = -convert_units(1, 'in', 'ft')
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the x axis
    center_mark = rbind(
      center_mark,
      reflect(
        center_mark,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    center_mark = rotate_coords(
      center_mark,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(center_mark)
}

#' Generate the data frame for the points that comprise the net
#'
#' @param full_surf A boolean indicating whether or not to plot a full surface
#'   representation of the surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not this feature needs to be
#'   rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the
#'   feature. Default: \code{'ccw'}
#'
#' @return A data frame containing the points that comprise the center mark(s)
itf_feature_net = function(full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # The net splits the court in half, running along x = 0. It can vary in length
  # based on whether or not the match is a singles or doubles match, however
  # sportyR will always assume a doubles match (thus the net will be 42' in
  # length). Although no thickness is directly specified, sportyR will use a net
  # thickness of 4.5" total, as the net posts may range between 3" and 6"
  net = create_rectangle(
    x_min = convert_units(-2.25, 'in', 'ft'),
    x_max = 0,
    y_min = -21,
    y_max = 21
  )

  if(full_surf){
    # If the surface being drawn is a full-surface representation, reflect over
    # the x axis
    net = rbind(
      net,
      reflect(
        net,
        over_y = TRUE
      )
    )
  }

  if(rotate){
    # If the desired output needs to be rotated, rotate the coordinates
    net = rotate_coords(
      net,
      rotation_dir
    )
  }

  # Return the feature's data frame
  return(net)
}

#' Generate the list of colors for an ITF tennis court plot. The defaults can be
#' overwritten by supplying the names of the list elements to the
#' \code{geom_itf()} function (or its wrapper \code{geom_tennis()})
#'
#' @param court_background_color A hexadecimal string representing the color to use for
#'   this feature
#' @param baseline_color A hexadecimal string representing the color to use for
#'   this feature
#' @param doubles_sideline_1_color A hexadecimal string representing the color
#'   to use for this feature
#' @param doubles_sideline_2_color A hexadecimal string representing the color
#'   to use for this feature
#' @param singles_sideline_1_color A hexadecimal string representing the color
#'   to use for this feature
#' @param singles_sideline_2_color A hexadecimal string representing the color
#'   to use for this feature
#' @param service_line_color A hexadecimal string representing the color to use
#'   for this feature
#' @param center_service_line_color A hexadecimal string representing the color
#'   to use for this feature
#' @param center_mark_color A hexadecimal string representing the color to use
#'   for this feature
#' @param net_color A hexadecimal string representing the color to use for this
#'   feature
#'
#' @return A list of hexadecimal colors to use to color the features on the
#'   resulting plot
itf_features_set_colors = function(court_background_color = '#196f0c',
                                   baseline_color = '#ffffff',
                                   doubles_sideline_1_color = '#ffffff',
                                   doubles_sideline_2_color = '#ffffff',
                                   singles_sideline_1_color = '#ffffff',
                                   singles_sideline_2_color = '#ffffff',
                                   service_line_color = '#ffffff',
                                   center_service_line_color = '#ffffff',
                                   center_mark_color = '#ffffff',
                                   net_color = '#ffffff'
){

  # Create the colors to use for the plot
  feature_colors = list(
    court_background_color = court_background_color,
    baseline_color = baseline_color,
    doubles_sideline_1_color = doubles_sideline_1_color,
    doubles_sideline_2_color = doubles_sideline_2_color,
    singles_sideline_1_color = singles_sideline_1_color,
    singles_sideline_2_color = singles_sideline_2_color,
    service_line_color = service_line_color,
    center_service_line_color = center_service_line_color,
    center_mark_color = center_mark_color,
    net_color = net_color
  )

  # Return the list of colors
  return(feature_colors)
}

#' Create a ggplot2 instance that represents a regulation ITF tennis court, with
#' the center of the net corresponding to (0, 0)
#'
#' @param full_surf A boolean indicating whether or not to draw a full-surface
#'   representation of the playing surface. Default: \code{TRUE}
#' @param rotate A boolean indicating whether or not the surface representation
#'   needs to be rotated. Default: \code{FALSE}
#' @param rotation_dir A string indicating which direction to rotate the surface
#'   representation. Default: \code{'ccw'}
#' @param unit A string indicating the units with which to make the plot.
#'   Default: \code{'yd'}
#' @param caption_color A hexadecimal string representing the color to use for
#'   the plot's caption. Default: '#707372' (grey)
#' @param background_color A hexadecimal string representing the color to use
#'   for the plot's background. Default: \code{NULL}
#' @param ... Additional arguments to pass to the function. These should be the
#'   colors to pass to the \code{itf_features_set_colors()} function
#'
#' @return A ggplot2 instance that represents a regulation NFL field
geom_itf = function(full_surf = TRUE,
                    rotate = FALSE,
                    rotation_dir = 'ccw',
                    unit = 'ft',
                    caption_color = '#707372',
                    background_color = NULL,
                    ...
){
  # Force the plot unit to be lower case
  unit = tolower(unit)

  # Create the colors to use for the plot
  color_list = itf_features_set_colors(...)

  # Generate the data frames for the features of an ITF tennis court
  court_background = itf_feature_court_background(full_surf, rotate, rotation_dir)
  baseline = itf_feature_baseline(full_surf, rotate, rotation_dir)
  doubles_sideline = itf_feature_doubles_sideline(full_surf, rotate, rotation_dir)
  singles_sideline = itf_feature_singles_sideline(full_surf, rotate, rotation_dir)
  service_line = itf_feature_service_line(full_surf, rotate, rotation_dir)
  center_service_line = itf_feature_center_service_line(full_surf, rotate, rotation_dir)
  center_mark = itf_feature_center_mark(full_surf, rotate, rotation_dir)
  net = itf_feature_net(full_surf, rotate, rotation_dir)

  # Convert between units as necessary
  if(!(unit %in% c('ft', 'feet'))){
    court_background = convert_units(court_background, 'ft', unit, conversion_columns = c('x', 'y'))
    baseline = convert_units(baseline, 'ft', unit, conversion_columns = c('x', 'y'))
    doubles_sideline$sideline_1 = convert_units(doubles_sideline$sideline_1, 'ft', unit, conversion_columns = c('x', 'y'))
    doubles_sideline$sideline_2 = convert_units(doubles_sideline$sideline_2, 'ft', unit, conversion_columns = c('x', 'y'))
    singles_sideline$sideline_1 = convert_units(singles_sideline$sideline_1, 'ft', unit, conversion_columns = c('x', 'y'))
    singles_sideline$sideline_2 = convert_units(singles_sideline$sideline_2, 'ft', unit, conversion_columns = c('x', 'y'))
    service_line = convert_units(service_line, 'ft', unit, conversion_columns = c('x', 'y'))
    center_service_line = convert_units(center_service_line, 'ft', unit, conversion_columns = c('x', 'y'))
    center_mark = convert_units(center_mark, 'ft', unit, conversion_columns = c('x', 'y'))
    net = convert_units(net, 'ft', unit, conversion_columns = c('x', 'y'))
  }

  # Create the initial ggplot2 instance onto which the features will be added
  g = create_plot_base(rotate, caption_color, background_color)

  # Add the features to the ggplot2 instance
  g = add_feature(g, court_background, color_list$court_background_color)
  g = add_feature(g, baseline, color_list$baseline_color)
  g = add_feature(g, doubles_sideline$sideline_1, color_list$doubles_sideline_1_color)
  g = add_feature(g, doubles_sideline$sideline_2, color_list$doubles_sideline_2_color)
  g = add_feature(g, singles_sideline$sideline_1, color_list$singles_sideline_1_color)
  g = add_feature(g, singles_sideline$sideline_2, color_list$singles_sideline_2_color)
  g = add_feature(g, service_line, color_list$service_line_color)
  g = add_feature(g, center_service_line, color_list$center_service_line_color)
  g = add_feature(g, center_mark, color_list$center_mark_color)
  g = add_feature(g, net, color_list$net_color)

  # Return the ggplot2 instance that contains the court plot
  return(g)
}
