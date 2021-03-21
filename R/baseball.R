usethis::use_package("ggplot2")
usethis::use_package("glue")

#' This draws a baseball diamond in its standard coordinate system, with (0, 0)
#' being the back tip of home plate. The positive x direction extends to the
#' first-base side of the field, and the positive y direction extends towards
#' the pitcher's mound. The entire field will always be provided and will not
#' rotate, although this may change in a future iteration

#' Generate the dataframe for the points that comprise home plate
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @return A ggplot2 instance with home plate added to it
baseball_infield_dirt = function(g, league){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('MLB', 'NCAA')){
    # The infield dirt follows an arc of radius 95' from the front-center point
    # on the pitcher's mound, then goes down the base paths to a circle of
    # diameter 26' centered at home plate, then back out along the third base
    # line. The base paths themselves are 6' wide, with the outer edge of the
    # foul line (y = x) in its center.
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

    # Add the infield dirt to the ggplot2 instance. It will be tan in color
    g = g +
      ggplot2::geom_polygon(data = infield_dirt, ggplot2::aes(x, y), fill = '#9b7653') +
      ggplot2::geom_polygon(data = home_dirt, ggplot2::aes(x, y), fill = '#9b7653')

    # Return the ggplot2 instance
    return(g)
  }

  else{
    # If the league isn't valid (i.e. MLB, NCAA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise home plate
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @return A ggplot2 instance with the infield grass added to it
baseball_infield_grass = function(g, league){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('MLB', 'NCAA')){
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

    # Add the infield grass to the ggplot2 instance. It will be green in color
    g = g +
      ggplot2::geom_polygon(data = infield_grass, ggplot2::aes(x, y), fill = '#395d33')

    # Return the ggplot2 instance
    return(g)
  }

  else{
    # If the league isn't valid (i.e. MLB, NCAA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the pitcher's mound and
#' pitcher's plate
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @return A ggplot2 instance with the pitcher's mound and pitcher's plate added
#'   to it
baseball_mound = function(g, league){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('MLB', 'NCAA')){
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

    # Add the mound and pitcher's plate to the ggplot2 instance. The mound will
    # be tan in color, and the pitcher's plate will be white
    g = g +
      ggplot2::geom_polygon(data = mound, ggplot2::aes(x, y), fill = '#9b7653') +
      ggplot2::geom_polygon(data = pitchers_plate, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else{
    # If the league isn't valid (i.e. MLB, NCAA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the bases
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @return A ggplot2 instance with the bases added to it
baseball_bases = function(g, league){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('MLB', 'NCAA')){
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

    # Add the bases to the ggplot2 instance. They will be white in color
    g = g +
      ggplot2::geom_polygon(data = home_plate, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = first_base_bag, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = second_base_bag, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = third_base_bag, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else{
    # If the league isn't valid (i.e. MLB, NCAA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the batter's boxes and
#' catcher's box
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @return A ggplot2 instance with the batter's boxes and catcher's box added to
#'   it
baseball_batters_boxes = function(g, league){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('MLB', 'NCAA')){
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

    # Add the batters' boxes and catcher's box to the ggplot2 instance. They
    # will be white in color
    g = g +
      ggplot2::geom_polygon(data = lefty_batters_box, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = righty_batters_box, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = catchers_box, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else{
    # If the league isn't valid (i.e. MLB, NCAA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the baselines and the
#' running lane
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @return A ggplot2 instance with the baselines and running lane added to it
baseball_lines = function(g, league){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('MLB', 'NCAA')){
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

    # Add the left field line, right field line, and running lane to the ggplot2
    # instance. They will be white in color
    g = g +
      ggplot2::geom_polygon(data = rf_line, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = lf_line, ggplot2::aes(x, y), fill = '#ffffff') +
      ggplot2::geom_polygon(data = running_lane, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else{
    # If the league isn't valid (i.e. MLB, NCAA), return the ggplot2 instance
    return(g)
  }
}

#' Generate a ggplot2 instance containing a regulation baseball field for a
#' specified league
#'
#' @param league The league for which to draw the surface
#'
#' @return A ggplot2 instance with a full-surface representation of a baseball
#'   field
#'
#' @export
#'
#' @examples
#' geom_baseball(league = "MLB")
geom_baseball = function(league){
  # Force the league to be all upper case
  league = toupper(league)

  if(league %in% c('MLB', 'NCAA')){
    # Create the initial ggplot2 instance onto which the features will be added
    g = ggplot2::ggplot() +
      ggplot2::coord_fixed() +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = '#395d33'),
        panel.border = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )

    # Add the infield dirt
    g = baseball_infield_dirt(g, league)

    # Add the infield grass
    g = baseball_infield_grass(g, league)

    # Add the pitcher's mound and pitcher's plate
    g = baseball_mound(g, league)

    # Add home plate
    g = baseball_bases(g, league)

    # Add the batters' boxes and catcher's box
    g = baseball_batters_boxes(g, league)

    # Add the right and left field lines and the running lane
    g = baseball_lines(g, league)


    # Return the ggplot2 instance that contains the field plot
    return(g)
  }

  else {
    stop(message(glue::glue("Sorry, {league} is not a valid league.")))
  }
}
