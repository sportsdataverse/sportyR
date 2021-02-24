usethis::use_package("ggplot2")

#' Generate the dataframe for the points that comprise the court
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw'
#' @return A ggplot2 instance with the court added to it
court = function(g, league, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NBA', 'WNBA')){
    # A regulation (W)NBA court is 94' long and 50' wide, but this will only draw the half-court, so it will measure 47' long and 25' wide
    court = create_rectangle(
      x_min = -47,
      x_max = 0,
      y_min = -25,
      y_max = 25
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      court = rbind(
        court,
        reflect(
          court,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      court = rotate_coords(
        court,
        rotation_dir
      )
    }

    # Add the court to the ggplot2 instance. It will be tan in color
    g = g +
      ggplot2::geom_polygon(data = court, ggplot2::aes(x, y), fill = '#d2ab6f')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league %in% c('NCAA', 'NCAAM', 'NCAAW', 'COLLEGE')){
    # A regulation college court is 94' long and 50' wide, but this will only draw the half-court, so it will measure 47' long and 25' wide
    court = create_rectangle(
      x_min = -47,
      x_max = 0,
      y_min = -25,
      y_max = 25
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      court = rbind(
        court,
        reflect(
          court,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      court = rotate_coords(
        court,
        rotation_dir
      )
    }

    # Add the court to the ggplot2 instance. It will be tan in color
    g = g +
      ggplot2::geom_polygon(data = court, ggplot2::aes(x, y), fill = '#d2ab6f')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'FIBA'){
    # A regulation college court is 28m long and 15m wide, but this will only draw the half-court, so it will measure 14m long and 7.5m wide
    court = create_rectangle(
      x_min = m_to_ft(-14),
      x_max = m_to_ft(0),
      y_min = m_to_ft(-7.5),
      y_max = m_to_ft(7.5)
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      court = rbind(
        court,
        reflect(
          court,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      court = rotate_coords(
        court,
        rotation_dir
      )
    }

    # Add the court to the ggplot2 instance. It will be tan in color
    g = g +
      ggplot2::geom_polygon(data = court, ggplot2::aes(x, y), fill = '#d2ab6f')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NBA, WNBA, NCAA, or FIBA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the center circle
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw'
#' @return A ggplot2 instance with the center circle added to it
center_circle = function(g, league, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NBA', 'WNBA')){
    # A (W)NBA court has two circles in the center: one of radius 2' (interior), and one of radius 6' (exterior). The lines are 2" thick

    # Inner circle is 2' in radius (interior)
    inner_circle = rbind(
      create_circle(
        center = c(0, 0),
        start = .5,
        end = 1.5,
        d = 4
      ),
      data.frame(
        x = 0,
        y = -4
      ),
      create_circle(
        center = c(0, 0),
        start = 1.5,
        end = .5,
        d = 4 + (4/12)
      )
    )

    # Outer circle is 6' in radius (exterior)
    outer_circle = rbind(
      create_circle(
        center = c(0, 0),
        start = .5,
        end = 1.5,
        d = 12
      ),
      data.frame(
        x = 0,
        y = -12
      ),
      create_circle(
        center = c(0, 0),
        start = 1.5,
        end = .5,
        d = 12 - (4/12)
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      inner_circle = rbind(
        inner_circle,
        reflect(
          inner_circle,
          over_y = TRUE
        )
      )

      outer_circle = rbind(
        outer_circle,
        reflect(
          outer_circle,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      inner_circle = rotate_coords(
        inner_circle,
        rotation_dir
      )

      outer_circle = rotate_coords(
        outer_circle,
        rotation_dir
      )
    }

    # Add the center circles to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = inner_circle, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = outer_circle, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league %in% c('NCAA', 'NCAAM', 'NCAAW', 'COLLEGE')){
    # The NCAA court's center circle is 12' in diameter (exterior) with thickness 2"
    center_circle = rbind(
      create_circle(
        center = c(0, 0),
        start = .5,
        end = 1.5,
        d = 12
      ),
      data.frame(
        x = 0,
        y = -6 + (2/12)
      ),
      create_circle(
        center = c(0, 0),
        start = 1.5,
        end = .5,
        d = 12 - (4/12)
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      center_circle = rbind(
        center_circle,
        reflect(
          center_circle,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      center_circle = rotate_coords(
        center_circle,
        rotation_dir
      )
    }

    # Add the center circle to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = center_circle, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'FIBA'){
    # A FIBA court's center circle is 1.8m in radius (exterior) with thickness 5cm
    center_circle = rbind(
      create_circle(
        center = c(0, 0),
        start = .5,
        end = 1.5,
        d = m_to_ft(3.6)
      ),
      data.frame(
        x = m_to_ft(0),
        y = m_to_ft(-3.5)
      ),
      create_circle(
        center = c(0, 0),
        start = 1.5,
        end = .5,
        d = m_to_ft(3.5)
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      center_circle = rbind(
        center_circle,
        reflect(
          center_circle,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      center_circle = rotate_coords(
        center_circle,
        rotation_dir
      )
    }

    # Add the center circle to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = center_circle, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NBA, WNBA, NCAA, or FIBA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the division line
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw'
#' @return A ggplot2 instance with the division line added to it
division_line = function(g, league, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NBA', 'WNBA')){
    # The line is 2" thick, goes right through the middle of the court (1" of its width on each side of it), and spans the width of the court
    division_line = create_rectangle(
      x_min = -1/12,
      x_max = 0,
      y_min = -25,
      y_max = 25
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      division_line = rbind(
        division_line,
        reflect(
          division_line,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      division_line = rotate_coords(
        division_line,
        rotation_dir
      )
    }

    # Add the division line to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = division_line, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league %in% c('NCAA', 'NCAAM', 'NCAAW', 'COLLEGE')){
    # The line is 2" thick, goes right through the middle of the court (1" of its width on each side of it), and spans the width of the court
    division_line = create_rectangle(
      x_min = -1/12,
      x_max = 0,
      y_min = -25,
      y_max = 25
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      division_line = rbind(
        division_line,
        reflect(
          division_line,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      division_line = rotate_coords(
        division_line,
        rotation_dir
      )
    }

    # Add the division line to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = division_line, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'FIBA'){
    # The line is 5cm thick, goes right through the middle of the court (2.5cm of its width on each side of it), and extends .15m beyond each sideline
    division_line = create_rectangle(
      x_min = -1/12,
      x_max = 0,
      y_min = m_to_ft(-7.55),
      y_max = m_to_ft(7.70)
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect
      # over the y axis
      division_line = rbind(
        division_line,
        reflect(
          division_line,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      division_line = rotate_coords(
        division_line,
        rotation_dir
      )
    }

    # Add the division line to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = division_line, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NBA, WNBA, NCAA, or FIBA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the endline(s) line
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw'
#' @return A ggplot2 instance with the endline(s) added to it
endline = function(g, league, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NBA', 'WNBA')){
    # The endline is 47' (interior) from the center of the court, spans the width of the court, and is 2" in thickness
    endline = create_rectangle(
      x_min = -47 - (2/12),
      x_max = -47,
      y_min = -25,
      y_max = 25
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      endline = rbind(
        endline,
        reflect(
          endline,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      endline = rotate_coords(
        endline,
        rotation_dir
      )
    }

    # Add the endline(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = endline, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league %in% c('NCAA', 'NCAAM', 'NCAAW', 'COLLEGE')){
    # The endline is 47' (interior) from the center of the court, spans the width of the court, and is 2" in thickness
    endline = create_rectangle(
      x_min = -47 - (2/12),
      x_max = -47,
      y_min = -25,
      y_max = 25
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      endline = rbind(
        endline,
        reflect(
          endline,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      endline = rotate_coords(
        endline,
        rotation_dir
      )
    }

    # Add the endline(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = endline, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'FIBA'){
    # The endline is 14m (interior) from the center of the court, spans the width of the court, and is 5cm in thickness
    endline = create_rectangle(
      x_min = m_to_ft(-14.05),
      x_max = m_to_ft(-14),
      y_min = m_to_ft(-7.5),
      y_max = m_to_ft(7.5)
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      endline = rbind(
        endline,
        reflect(
          endline,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      endline = rotate_coords(
        endline,
        rotation_dir
      )
    }

    # Add the endline(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = endline, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NBA, WNBA, NCAA, or FIBA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the sideine(s) line
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw''
#' @return A ggplot2 instance with the sideline(s) added to it
sideline = function(g, league, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NBA', 'WNBA')){
    # The sideline is 25' (interior) from the center of the court, spans the length of the court, and is 2" in thickness

    # First, the lower sideline
    sideline_lower = create_rectangle(
      x_min = -47 - (2/12),
      x_max = 0,
      y_min = -25 - (2/12),
      y_max = -25
    )

    # Then, the upper sideline
    sideline_upper = create_rectangle(
      x_min = -47 - (2/12),
      x_max = 0,
      y_min = 25,
      y_max = 25 + (2/12)
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      sideline_lower = rbind(
        sideline_lower,
        reflect(
          sideline_lower,
          over_y = TRUE
        )
      )

      sideline_upper = rbind(
        sideline_upper,
        reflect(
          sideline_upper,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      sideline_lower = rotate_coords(
        sideline_lower,
        rotation_dir
      )

      sideline_upper = rotate_coords(
        sideline_upper,
        rotation_dir
      )
    }

    # Add the sideline(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = sideline_lower, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = sideline_upper, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league %in% c('NCAA', 'NCAAM', 'NCAAW', 'COLLEGE')){
    # The sideline is 25' (interior) from the center of the court, spans the length of the court, and is 2" in thickness

    # First, the lower sideline
    sideline_lower = create_rectangle(
      x_min = -47 - (2/12),
      x_max = 0,
      y_min = -25 - (2/12),
      y_max = -25
    )

    # Then, the upper sideline
    sideline_upper = create_rectangle(
      x_min = -47 - (2/12),
      x_max = 0,
      y_min = 25,
      y_max = 25 + (2/12)
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      sideline_lower = rbind(
        sideline_lower,
        reflect(
          sideline_lower,
          over_y = TRUE
        )
      )

      sideline_upper = rbind(
        sideline_upper,
        reflect(
          sideline_upper,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      sideline_lower = rotate_coords(
        sideline_lower,
        rotation_dir
      )

      sideline_upper = rotate_coords(
        sideline_upper,
        rotation_dir
      )
    }

    # Add the sideline(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = sideline_lower, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = sideline_upper, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'FIBA'){
    # The sideline is 7.5m (interior) from the center of the court, spans the length of the court, and is 5cm in thickness

    # First, the lower sideline
    sideline_lower = create_rectangle(
      x_min = m_to_ft(-14.05),
      x_max = m_to_ft(0),
      y_min = m_to_ft(-7.55),
      y_max = m_to_ft(-7.5)
    )

    # Then, the upper sideline
    sideline_upper = create_rectangle(
      x_min = m_to_ft(-14.05),
      x_max = m_to_ft(0),
      y_min = m_to_ft(7.5),
      y_max = m_to_ft(7.55)
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      sideline_lower = rbind(
        sideline_lower,
        reflect(
          sideline_lower,
          over_y = TRUE
        )
      )

      sideline_upper = rbind(
        sideline_upper,
        reflect(
          sideline_upper,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      sideline_lower = rotate_coords(
        sideline_lower,
        rotation_dir
      )

      sideline_upper = rotate_coords(
        sideline_upper,
        rotation_dir
      )
    }

    # Add the sideline(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = sideline_lower, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = sideline_upper, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NBA, WNBA, NCAA, or FIBA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the team bench
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw'
#' @return A ggplot2 instance with the team bench added to it
team_bench = function(g, league, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NBA', 'WNBA')){
    # The team bench is 28' (interior) from the nearest endline sideline, protrudes 3' into the court, and is 2" in thickness

    # First, the lower sideline
    team_bench_lower = create_rectangle(
      x_min = -19,
      x_max = -19 + (2/12),
      y_min = -25,
      y_max = -22
    )

    # Then, the upper sideline
    team_bench_upper = create_rectangle(
      x_min = -19,
      x_max = -19 + (2/12),
      y_min = 22,
      y_max = 25
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      team_bench_lower = rbind(
        team_bench_lower,
        reflect(
          team_bench_lower,
          over_y = TRUE
        )
      )

      team_bench_upper = rbind(
        team_bench_upper,
        reflect(
          team_bench_upper,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      team_bench_lower = rotate_coords(
        team_bench_lower,
        rotation_dir
      )

      team_bench_upper = rotate_coords(
        team_bench_upper,
        rotation_dir
      )
    }

    # Add the team bench lines to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = team_bench_lower, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = team_bench_upper, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league %in% c('NCAA', 'NCAAM', 'NCAAW', 'COLLEGE')){
    # The team bench is 28' (interior) from the nearest endline sideline, protrudes 3' into the court, and is 2" in thickness

    # First, the lower sideline
    team_bench_lower = create_rectangle(
      x_min = -19,
      x_max = -19 + (2/12),
      y_min = -28,
      y_max = -22
    )

    # Then, the upper sideline
    team_bench_upper = create_rectangle(
      x_min = -19,
      x_max = -19 + (2/12),
      y_min = 22,
      y_max = 28
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      team_bench_lower = rbind(
        team_bench_lower,
        reflect(
          team_bench_lower,
          over_y = TRUE
        )
      )

      team_bench_upper = rbind(
        team_bench_upper,
        reflect(
          team_bench_upper,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      team_bench_lower = rotate_coords(
        team_bench_lower,
        rotation_dir
      )

      team_bench_upper = rotate_coords(
        team_bench_upper,
        rotation_dir
      )
    }

    # Add the team bench lines to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = team_bench_lower, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = team_bench_upper, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'FIBA'){
    # On a FIBA court, a throw-in line is .15m in length, with its outer edge 8.325m from the inner edge of the nearest endline, and with thickness of 5cm. They only exist on one side of the court. Team benches are 5m from the center of the division line, extend 2m towards the bench, and are 5cm in width

    # First, draw the throw-in line
    throw_in_line = create_rectangle(
      x_min = m_to_ft(-5.725),
      x_max = m_to_ft(-5.675),
      y_min = m_to_ft(-7.70),
      y_max = m_to_ft(-7.55)
    )

    # Then, the bench line
    team_bench = create_rectangle(
      x_min = m_to_ft(-5.05),
      x_max = m_to_ft(-5),
      y_min = m_to_ft(7.55),
      y_max = m_to_ft(9.55)
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      throw_in_line = rbind(
        throw_in_line,
        reflect(
          throw_in_line,
          over_y = TRUE
        )
      )

      team_bench = rbind(
        team_bench,
        reflect(
          team_bench,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      throw_in_line = rotate_coords(
        throw_in_line,
        rotation_dir
      )

      team_bench = rotate_coords(
        team_bench,
        rotation_dir
      )
    }

    # Add the throw-in line(s) and team bench line(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = throw_in_line, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = team_bench, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NBA, WNBA, NCAA, or FIBA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the substitution area
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw'
#' @return A ggplot2 instance with the substitution area added to it
substitution_area = function(g, league, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NBA', 'WNBA')){
    # The substitution areas are 8' 2" (interior) apart, or 4' 1" from the center of the division line. They extend 4' from the boundary line, and are 2" thick

    # Substitution area
    substitution_area = create_rectangle(
      x_min = -4 - (3/12),
      x_max = -4 - (1/12),
      y_min = 25 + (2/12),
      y_max = 29 + (2/12)
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      substitution_area = rbind(
        substitution_area,
        reflect(
          substitution_area,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      substitution_area = rotate_coords(
        substitution_area,
        rotation_dir
      )
    }

    # Add the substitution area line(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = substitution_area, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league %in% c('NCAA', 'NCAAM', 'NCAAW', 'COLLEGE')){
    # The substitution areas are 8' 2" (interior) apart, or 4' 1" from the center of the division line. They extend 4' from the boundary line, and are 2" thick

    # Substitution area
    substitution_area = create_rectangle(
      x_min = -9,
      x_max = -9 + (2/12),
      y_min = 25 + (2/12),
      y_max = 27 + (2/12)
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      substitution_area = rbind(
        substitution_area,
        reflect(
          substitution_area,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      substitution_area = rotate_coords(
        substitution_area,
        rotation_dir
      )
    }

    # Add the substitution area line(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = substitution_area, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'FIBA'){
    # FIBA courts do not require a substitution area

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NBA, WNBA, NCAA, or FIBA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the three-point line
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param include_m_line A boolean indicating whether or not to plot the men's three-point line (NCAA only)
#' @param include_w_line A boolean indicating whether or not to plot the women's three-point line (NCAA only)
#' @param full_surf A boolean indicating whether or not this feature is needed for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw'
#' @return A ggplot2 instance with the three-point line added to it
three_point_line = function(g, league, include_m_line = TRUE, include_w_line = TRUE, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league == 'NBA'){
    # First, a bit of math is needed to determine the starting and ending angles of the three-point arc, relative to 0 radians. Since in the end, the angle is what matters, the units of measure do not. Inches are easier to use for this calculation. The angle begins 3' from the interior edge of the sideline
    start_y = (22 * 12)

    # The rule book describes the arc as having a radius of 23' 9"
    radius_outer = (23 * 12) + 9

    # From here, the calculation is relatively straightforward. To determine the angle, the inverse sine is needed. It will be multiplied by pi so that it can be passed to the create_circle() function
    start_angle_outer = asin(start_y / radius_outer) / pi
    end_angle_outer = -start_angle_outer

    # The same method can be used for the inner angles, however, since the inner radius will be traced from bottom to top, the angle must be negative to start
    radius_inner = (23 * 12) + 9 - 2
    start_angle_inner = -asin((start_y - 2) / radius_inner) / pi
    end_angle_inner = -start_angle_inner

    # NBA three-point line
    three_point_line = rbind(
      data.frame(
        x = -47,
        y = 22
      ),

      create_circle(
        center = c(-41.75, 0),
        start = start_angle_outer,
        end = end_angle_outer,
        d = 2 * (radius_outer / 12)
      ),

      data.frame(
        x = c(-47, -47),
        y = c(-22, -22 + (2/12))
      ),

      create_circle(
        center = c(-41.75, 0),
        start = start_angle_inner,
        end = end_angle_inner,
        d = 2 * (radius_inner / 12)
      ),

      data.frame(
        x = c(-47, -47),
        y = c(22 - (2/12), 22)
      )
    )

    # Sometimes, the inside of the three-point arc (aka 2-point range) will be a different color than the floor. This section allows for this to happen in a future iteration
    two_point_range = rbind(
      data.frame(
        x = -47,
        y = -22 + (2/12)
      ),

      create_circle(
        center = c(-41.75, 0),
        start = start_angle_inner,
        end = end_angle_inner,
        d = 2 * (radius_inner / 12)
      ),

      data.frame(
        x = c(-47, -47),
        y = c(22 - (2/12), -22 + (2/12))
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      three_point_line = rbind(
        three_point_line,
        reflect(
          three_point_line,
          over_y = TRUE
        )
      )

      two_point_range = rbind(
        two_point_range,
        reflect(
          two_point_range,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      three_point_line = rotate_coords(
        three_point_line,
        rotation_dir
      )

      two_point_range = rotate_coords(
        two_point_range,
        rotation_dir
      )
    }

    # Add the three-point line(s) to the ggplot2 instance. The lines will be black in color, and the 2-point range will be tan in color
    g = g +
      ggplot2::geom_polygon(data = three_point_line, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = two_point_range, ggplot2::aes(x, y), fill = '#d2ab6f')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'WNBA'){
    # First, a bit of math is needed to determine the starting and ending angles of the three-point arc, relative to 0 radians. Since in the end, the angle is what matters, the units of measure do not. Inches are easier to use for this calculation. The angle begins 3' from the interior edge of the sideline
    start_y = (22 * 12)

    # The rule book describes the arc as having a radius of 22' 1.75"
    radius_outer = (22 * 12) + 1.75

    # From here, the calculation is relatively straightforward. To determine the angle, the inverse sine is needed. It will be multiplied by pi so that it can be passed to the create_circle() function
    start_angle_outer = asin(start_y / radius_outer) / pi
    end_angle_outer = -start_angle_outer

    # The same method can be used for the inner angles, however, since the inner radius will be traced from bottom to top, the angle must be negative to start
    radius_inner = (22 * 12) + 1.75 - 2
    start_angle_inner = -asin((start_y - 2) / radius_inner) / pi
    end_angle_inner = -start_angle_inner

    # WNBA three-point line
    three_point_line = rbind(
      data.frame(
        x = -47,
        y = 22
      ),

      create_circle(
        center = c(-41.75, 0),
        start = start_angle_outer,
        end = end_angle_outer,
        d = 2 * (radius_outer / 12)
      ),

      data.frame(
        x = c(-47, -47),
        y = c(-22, -22 + (2/12))
      ),

      create_circle(
        center = c(-41.75, 0),
        start = start_angle_inner,
        end = end_angle_inner,
        d = 2 * (radius_inner / 12)
      ),

      data.frame(
        x = c(-47, -47),
        y = c(22 - (2/12), 22)
      )
    )

    # Sometimes, the inside of the three-point arc (aka 2-point range) will be a different color than the floor. This section allows for this to happen in a future iteration
    two_point_range = rbind(
      data.frame(
        x = -47,
        y = -22 + (2/12)
      ),

      create_circle(
        center = c(-41.75, 0),
        start = start_angle_inner,
        end = end_angle_inner,
        d = 2 * (radius_inner / 12)
      ),

      data.frame(
        x = c(-47, -47),
        y = c(22 - (2/12), -22 + (2/12))
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      three_point_line = rbind(
        three_point_line,
        reflect(
          three_point_line,
          over_y = TRUE
        )
      )

      two_point_range = rbind(
        two_point_range,
        reflect(
          two_point_range,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      three_point_line = rotate_coords(
        three_point_line,
        rotation_dir
      )

      two_point_range = rotate_coords(
        two_point_range,
        rotation_dir
      )
    }

    # Add the three-point line(s) to the ggplot2 instance. The lines will be black in color, and the 2-point range will be tan in color
    g = g +
      ggplot2::geom_polygon(data = three_point_line, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = two_point_range, ggplot2::aes(x, y), fill = '#d2ab6f')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league %in% c('NCAA', 'NCAAM', 'NCAAW', 'COLLEGE')){
    if(include_m_line){
      # First, a bit of math is needed to determine the starting and ending angles of the three-point arc, relative to 0 radians. Since in the end, the angle is what matters, the units of measure do not. Inches are easier to use for this calculation. The angle begins 9' 10 3/8" from the interior edge of the endline
      start_x = (9 * 12) + 10 + (3/8)

      # However, the rule book describes the arc as having a radius of 22' 1.75" from the center of the basket. The basket's center is 63" away from the interior of the endline, so this must be subtracted from our starting x position to get the starting x position *relative to the center of the basket*
      start_x = start_x - 63
      radius_outer = (22 * 12) + 1.75

      # From here, the calculation is relatively straightforward. To determine the angle, the inverse cosine is needed. It will be multiplied by pi so that it can be passed to the create_circle() function
      start_angle_outer = acos(start_x / radius_outer) / pi
      end_angle_outer = -start_angle_outer

      # The same method can be used for the inner angles, however, since the inner radius will be traced from bottom to top, the angle must be negative to start
      radius_inner = (22 * 12) + 1.75 - 2
      start_angle_inner = -acos(start_x / radius_inner) / pi
      end_angle_inner = -start_angle_inner

      # According to the rulebook, the men's three-point line is 21' 7 7/8" in the corners
      m_three_point_line = rbind(
        data.frame(
          x = -47,
          y = 21 + ((7 + (7/8)) / 12)
        ),

        create_circle(
          center = c(-41.75, 0),
          start = start_angle_outer,
          end = end_angle_outer,
          d = 2 * (((22 * 12) + 1.75) / 12)
        ),

        data.frame(
          x = c(
            -47,
            -47,
            -47 + (((9 * 12) + 10 + (3/8))/12)
          ),

          y = c(
            -21 - ((7 + (7/8))/12),
            -21 - ((7 + (7/8))/12) + (2/12),
            -21 - ((7 + (7/8))/12) + (2/12)
          )
        ),

        create_circle(
          center = c(-41.75, 0),
          start = start_angle_inner,
          end = end_angle_inner,
          d = 2 * ((((22 * 12) + 1.75)/12) - (2/12))
        ),

        data.frame(
          x = c(
            -47 + (((9 * 12) + 10 + (3/8))/12),
            -47,
            -47
          ),

          y = c(
            21 + ((7 + (7/8))/12) - (2/12),
            21 + ((7 + (7/8))/12) - (2/12),
            21 + ((7 + (7/8))/12)
          )
        )
      )

      m_two_point_range = rbind(
        data.frame(
          x = c(
            -47,
            -47 + (((9 * 12) + 10 + (3/8))/12)
          ),

          y = c(
            -21 - ((7 + (7/8))/12) + (2/12),
            -21 - ((7 + (7/8))/12) + (2/12)
          )
        ),

        create_circle(
          center = c(-41.75, 0),
          start = start_angle_inner,
          end = end_angle_inner,
          d = 2 * ((((22 * 12) + 1.75)/12) - (2/12))
        ),

        data.frame(
          x = c(
            -47 + (((9 * 12) + 10 + (3/8))/12),
            -47,
            -47
          ),

          y = c(
            21 + ((7 + (7/8))/12) - (2/12),
            21 + ((7 + (7/8))/12) - (2/12),
            -21 - ((7 + (7/8))/12) + (2/12)
          )
        )
      )

      if(full_surf){
        # If the surface being drawn is a full-surface representation, reflect over the y axis
        m_three_point_line = rbind(
          m_three_point_line,
          reflect(
            m_three_point_line,
            over_y = TRUE
          )
        )

        m_two_point_range = rbind(
          m_two_point_range,
          reflect(
            m_two_point_range,
            over_y = TRUE
          )
        )
      }

      if(rotate){
        # If the desired output needs to be rotated, rotate the coordinates
        m_three_point_line = rotate_coords(
          m_three_point_line,
          rotation_dir
        )

        m_two_point_range = rotate_coords(
          m_two_point_range,
          rotation_dir
        )
      }

      # Add the three-point line(s) to the ggplot2 instance. The lines will be black in color, and the 2-point range will be tan in color
      g = g +
        ggplot2::geom_polygon(data = m_three_point_line, ggplot2::aes(x, y), fill = '#000000') +
        ggplot2::geom_polygon(data = m_two_point_range, ggplot2::aes(x, y), fill = '#d2ab6f')
    }

    if(include_w_line){
      # The women's three-point has a distance of 20' 9" in the corners, and an arc of radius 20' 9"
      w_three_point_line = rbind(
        data.frame(
          x = -47,
          y = -20.75
        ),

        create_circle(
          center = c(-41.75, 0),
          start = -.5,
          end = .5,
          d = 41.5
        ),

        data.frame(
          x = c(-47, -47),
          y = c(20.75, 20.75 - (2/12))
        ),

        create_circle(
          center = c(-41.75, 0),
          start = .5,
          end = -.5,
          d = 41.5 - (4/12)
        ),

        data.frame(
          x = c(-47, -47),
          y = c(-20.75 + (2/12), -20.75)
        )
      )

      w_two_point_range = rbind(
        data.frame(
          x = -47,
          y = 20.75 - (2/12)
        ),

        create_circle(
          center = c(-41.75, 0),
          start = .5,
          end = -.5,
          d = 41.5 - (4/12)
        ),

        data.frame(
          x = c(-47, -47),
          y = c(-20.75 + (2/12), 20.75 - (2/12))
        )
      )

      if(full_surf){
        # If the surface being drawn is a full-surface representation, reflect over the y axis
        w_three_point_line = rbind(
          w_three_point_line,
          reflect(
            w_three_point_line,
            over_y = TRUE
          )
        )

        w_two_point_range = rbind(
          w_two_point_range,
          reflect(
            w_two_point_range,
            over_y = TRUE
          )
        )
      }

      if(rotate){
        # If the desired output needs to be rotated, rotate the coordinates
        w_three_point_line = rotate_coords(
          w_three_point_line,
          rotation_dir
        )

        w_two_point_range = rotate_coords(
          w_two_point_range,
          rotation_dir
        )
      }

      # Add the three-point line(s) to the ggplot2 instance. The lines will be black in color, and the 2-point range will be tan in color
      g = g +
        ggplot2::geom_polygon(data = w_three_point_line, ggplot2::aes(x, y), fill = '#000000') +
        ggplot2::geom_polygon(data = w_two_point_range, ggplot2::aes(x, y), fill = '#d2ab6f')
    }

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'FIBA'){
    # First, a bit of math is needed to determine the starting and ending angles of the three-point arc, relative to 0 radians. Since in the end, the angle is what matters, the units of measure do not. Inches are easier to use for this calculation. The angle begins .90m from the interior edge of the sideline
    start_y = m_to_ft(6.6)

    # The rule book describes the arc as having a radius of 6.75m
    radius_outer = m_to_ft(6.75)

    # From here, the calculation is relatively straightforward. To determine the angle, the inverse sine is needed. It will be multiplied by pi so that it can be passed to the create_circle() function
    start_angle_outer = asin(start_y / radius_outer) / pi
    end_angle_outer = -start_angle_outer

    # The same method can be used for the inner angles, however, since the inner radius will be traced from bottom to top, the angle must be negative to start
    radius_inner = m_to_ft(6.70)
    start_angle_inner = -asin((start_y - m_to_ft(.05)) / radius_inner) / pi
    end_angle_inner = -start_angle_inner

    # FIBA three-point line
    three_point_line = rbind(
      data.frame(
        x = m_to_ft(-14),
        y = m_to_ft(6.6)
      ),

      create_circle(
        center = c(m_to_ft(-12.425), m_to_ft(0)),
        start = start_angle_outer,
        end = end_angle_outer,
        d = 2 * radius_outer
      ),

      data.frame(
        x = c(m_to_ft(-14), m_to_ft(-14)),
        y = c(m_to_ft(-6.6), m_to_ft(-6.55))
      ),

      create_circle(
        center = c(m_to_ft(-12.425), m_to_ft(0)),
        start = start_angle_inner,
        end = end_angle_inner,
        d = 2 * radius_inner
      ),

      data.frame(
        x = c(m_to_ft(-14), m_to_ft(-14)),
        y = c(m_to_ft(6.55), m_to_ft(6.6))
      )
    )

    # Sometimes, the inside of the three-point arc (aka 2-point range) will be a different color than the floor. This section allows for this to happen in a future iteration
    two_point_range = rbind(
      data.frame(
        x = m_to_ft(-14),
        y =  m_to_ft(-6.55)
      ),

      create_circle(
        center = c(m_to_ft(-12.425), m_to_ft(0)),
        start = start_angle_inner,
        end = end_angle_inner,
        d = 2 * radius_inner
      ),

      data.frame(
        x = c(m_to_ft(-14), m_to_ft(-14)),
        y = c(m_to_ft(6.55), m_to_ft(-6.55))
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      three_point_line = rbind(
        three_point_line,
        reflect(
          three_point_line,
          over_y = TRUE
        )
      )

      two_point_range = rbind(
        two_point_range,
        reflect(
          two_point_range,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      three_point_line = rotate_coords(
        three_point_line,
        rotation_dir
      )

      two_point_range = rotate_coords(
        two_point_range,
        rotation_dir
      )
    }

    # Add the three-point line(s) to the ggplot2 instance. The lines will be black in color, and the 2-point range will be tan in color
    g = g +
      ggplot2::geom_polygon(data = three_point_line, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = two_point_range, ggplot2::aes(x, y), fill = '#d2ab6f')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NBA, WNBA, NCAA, or FIBA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the free-throw lane
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param include_amateur A boolean indicating whether or not to include amateur free-throw lane markings (some professional (W)NBA courts include amateur free-throw lane markings)
#' @param full_surf A boolean indicating whether or not this feature is needed for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw'
#' @return A ggplot2 instance with the free-throw lane added to it
free_throw_lane = function(g, league, include_amateur = FALSE, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NBA', 'WNBA')){
    # The free-throw lane measures 19' from the inside edge of the endline to the outside edge of the free-throw line, measures 8' wide (exterior) and has lines of width of 2"
    pro_free_throw_lane = data.frame(
      x = c(
        -47,
        -28,
        -28,
        -47,
        -47,
        -28 - (2/12),
        -28 - (2/12),
        -47,
        -47
      ),

      y = c(
        -8,
        -8,
        8,
        8,
        8 - (2/12),
        8 - (2/12),
        -8 + (2/12),
        -8 + (2/12),
        -8
      )
    )

    pro_painted_area = create_rectangle(
      x_min = -47,
      x_max = -28 - (2/12),
      y_min = -8 + (2/12),
      y_max = 8 - (2/12)
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      pro_free_throw_lane = rbind(
        pro_free_throw_lane,
        reflect(
          pro_free_throw_lane,
          over_y = TRUE
        )
      )

      pro_painted_area = rbind(
        pro_painted_area,
        reflect(
          pro_painted_area,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      pro_free_throw_lane = rotate_coords(
        pro_free_throw_lane,
        rotation_dir
      )

      pro_painted_area = rotate_coords(
        pro_painted_area,
        rotation_dir
      )
    }

    # Add the professional free-throw lane to the ggplot2 instance. The lines will be black in color, and the painted area will be tan in color
    g = g +
      ggplot2::geom_polygon(data = pro_free_throw_lane, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = pro_painted_area, ggplot2::aes(x, y), fill = '#d2ab6f')

    if(include_amateur){
      # Some courts have the amateur (NCAA) free-throw lane included as well
      amateur_free_throw_lane = data.frame(
        x = c(
          -47,
          -28,
          -28,
          -47,
          -47,
          -28 - (2/12),
          -28 - (2/12),
          -47,
          -47
        ),

        y = c(
          -6,
          -6,
          6,
          6,
          6 - (2/12),
          6 - (2/12),
          -6 + (2/12),
          -6 + (2/12),
          -6
        )
      )

      if(full_surf){
        # If the surface being drawn is a full-surface representation, reflect over the y axis
        amateur_free_throw_lane = rbind(
          amateur_free_throw_lane,
          reflect(
            amateur_free_throw_lane,
            over_y = TRUE
          )
        )
      }

      if(rotate){
        # If the desired output needs to be rotated, rotate the coordinates
        amateur_free_throw_lane = rotate_coords(
          amateur_free_throw_lane,
          rotation_dir
        )
      }

      # Add the amateur (college) free-throw lane to the ggplot2 instance. They will be black in color
      g = g +
        ggplot2::geom_polygon(data = amateur_free_throw_lane, ggplot2::aes(x, y), fill = '#000000')
    }

    # Return the ggplot2 instance
    return(g)
  }

  else if(league %in% c('NCAA', 'NCAAM', 'NCAAW', 'COLLEGE')){
    # The free-throw lane measures 19' from the inside edge of the endline to the outside edge of the free-throw line, measures 6' wide (exterior) and has lines of width of 2"
    free_throw_lane = data.frame(
      x = c(
        -47,
        -28,
        -28,
        -47,
        -47,
        -28 - (2/12),
        -28 - (2/12),
        -47,
        -47
      ),

      y = c(
        -6,
        -6,
        6,
        6,
        6 - (2/12),
        6 - (2/12),
        -6 + (2/12),
        -6 + (2/12),
        -6
      )
    )

    painted_area = create_rectangle(
      x_min = -47,
      x_max = -28 - (2/12),
      y_min = -6 + (2/12),
      y_max = 6 - (2/12)
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      free_throw_lane = rbind(
        free_throw_lane,
        reflect(
          free_throw_lane,
          over_y = TRUE
        )
      )

      painted_area = rbind(
        painted_area,
        reflect(
          painted_area,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      free_throw_lane = rotate_coords(
        free_throw_lane,
        rotation_dir
      )

      painted_area = rotate_coords(
        painted_area,
        rotation_dir
      )
    }

    # Add the professional free-throw lane to the ggplot2 instance. The lines will be black in color, and the painted area will be tan in color
    g = g +
      ggplot2::geom_polygon(data = free_throw_lane, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = painted_area, ggplot2::aes(x, y), fill = '#d2ab6f')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'FIBA'){
    # The free-throw lane measures 5.8m from the inside edge of the endline to the outside edge of the free-throw line, has a width of 4.9m, and has lines of width of 5cm
    free_throw_lane = data.frame(
      x = c(
        m_to_ft(-14),
        m_to_ft(-8.15),
        m_to_ft(-8.15),
        m_to_ft(-14),

        m_to_ft(-14),
        m_to_ft(-8.2),
        m_to_ft(-8.2),
        m_to_ft(-14),

        m_to_ft(-14)
      ),

      y = c(
        m_to_ft(-2.45),
        m_to_ft(-2.45),
        m_to_ft(2.45),
        m_to_ft(2.45),

        m_to_ft(2.40),
        m_to_ft(2.40),
        m_to_ft(-2.40),
        m_to_ft(-2.40),

        m_to_ft(-2.45)
      )
    )

    painted_area = create_rectangle(
      x_min = m_to_ft(-14),
      x_max = m_to_ft(-8.2),
      y_min = m_to_ft(-2.40),
      y_max = m_to_ft(2.40)
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      free_throw_lane = rbind(
        free_throw_lane,
        reflect(
          free_throw_lane,
          over_y = TRUE
        )
      )

      painted_area = rbind(
        painted_area,
        reflect(
          painted_area,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      free_throw_lane = rotate_coords(
        free_throw_lane,
        rotation_dir
      )

      painted_area = rotate_coords(
        painted_area,
        rotation_dir
      )
    }

    # Add the professional free-throw lane to the ggplot2 instance. The lines will be black in color, and the painted area will be tan in color
    g = g +
      ggplot2::geom_polygon(data = free_throw_lane, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = painted_area, ggplot2::aes(x, y), fill = '#d2ab6f')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NBA, WNBA, NCAA, or FIBA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the free-throw lane lines
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param include_amateur A boolean indicating whether or not to include amateur free-throw lane markings (some professional (W)NBA courts include amateur free-throw lane markings)
#' @param full_surf A boolean indicating whether or not this feature is needed for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw'
#' @return A ggplot2 instance with the free-throw lane lines added to it
free_throw_lane_lines = function(g, league, include_amateur = FALSE, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NBA', 'WNBA')){
    # The first block is 7' (interior) from the endline, measures 2" in width, and extends 6" towards the nearest sideline
    professional_lane_line_1 = create_rectangle(
      x_min = -40,
      x_max = -40 + (2 / 12),
      y_min = -8.5,
      y_max = -8
    )

    # The second lane line is 8" (interior) from the first lane line, measures 2" in width, and extend 6" towards the nearest sideline
    professional_lane_line_2 = create_rectangle(
      x_min = -40 + (10 / 12),
      x_max = -39,
      y_min = -8.5,
      y_max = -8
    )

    # The third lane line is 3' (interior) from the second lane line, measures 2" in width, and extend 6" towards the nearest sideline
    professional_lane_line_3 = create_rectangle(
      x_min = -36,
      x_max = -36 + (2 / 12),
      y_min = -8.5,
      y_max = -8
    )

    # The fourth lane line is 3' (interior) from the third lane line, measures 2" in width, and extend 6" towards the nearest sideline
    professional_lane_line_4 = create_rectangle(
      x_min = -33 + (2 / 12),
      x_max = -33 + (4 / 12),
      y_min = -8.5,
      y_max = -8
    )

    # Lane line 4 but on the other side of the free-throw lane
    professional_lane_line_5 = reflect(
      professional_lane_line_4,
      over_x = TRUE,
      over_y = FALSE
    )

    # Lane line 3 but on the other side of the free-throw lane
    professional_lane_line_6 = reflect(
      professional_lane_line_3,
      over_x = TRUE,
      over_y = FALSE
    )

    # Lane line 2 but on the other side of the free-throw lane
    professional_lane_line_7 = reflect(
      professional_lane_line_2,
      over_x = TRUE,
      over_y = FALSE
    )

    # Lane line 1 but on the other side of the free-throw lane
    professional_lane_line_8 = reflect(
      professional_lane_line_1,
      over_x = TRUE,
      over_y = FALSE
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect
      # over the y axis
      professional_lane_line_1 = rbind(
        professional_lane_line_1,
        reflect(
          professional_lane_line_1,
          over_y = TRUE
        )
      )

      professional_lane_line_2 = rbind(
        professional_lane_line_2,
        reflect(
          professional_lane_line_2,
          over_y = TRUE
        )
      )

      professional_lane_line_3 = rbind(
        professional_lane_line_3,
        reflect(
          professional_lane_line_3,
          over_y = TRUE
        )
      )

      professional_lane_line_4 = rbind(
        professional_lane_line_4,
        reflect(
          professional_lane_line_4,
          over_y = TRUE
        )
      )

      professional_lane_line_5 = rbind(
        professional_lane_line_5,
        reflect(
          professional_lane_line_5,
          over_y = TRUE
        )
      )

      professional_lane_line_6 = rbind(
        professional_lane_line_6,
        reflect(
          professional_lane_line_6,
          over_y = TRUE
        )
      )

      professional_lane_line_7 = rbind(
        professional_lane_line_7,
        reflect(
          professional_lane_line_7,
          over_y = TRUE
        )
      )

      professional_lane_line_8 = rbind(
        professional_lane_line_8,
        reflect(
          professional_lane_line_8,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      professional_lane_line_1 = rotate_coords(
        professional_lane_line_1,
        rotation_dir
      )

      professional_lane_line_2 = rotate_coords(
        professional_lane_line_2,
        rotation_dir
      )

      professional_lane_line_3 = rotate_coords(
        professional_lane_line_3,
        rotation_dir
      )

      professional_lane_line_4 = rotate_coords(
        professional_lane_line_4,
        rotation_dir
      )

      professional_lane_line_5 = rotate_coords(
        professional_lane_line_5,
        rotation_dir
      )

      professional_lane_line_6 = rotate_coords(
        professional_lane_line_6,
        rotation_dir
      )

      professional_lane_line_7 = rotate_coords(
        professional_lane_line_7,
        rotation_dir
      )

      professional_lane_line_8 = rotate_coords(
        professional_lane_line_8,
        rotation_dir
      )
    }

    # Add the professional lane lines to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = professional_lane_line_1, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = professional_lane_line_2, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = professional_lane_line_3, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = professional_lane_line_4, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = professional_lane_line_5, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = professional_lane_line_6, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = professional_lane_line_7, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = professional_lane_line_8, ggplot2::aes(x, y), fill = '#000000')

    if(include_amateur){
      # The first set of lane lines are 7' from the interior of the baseline, measure 1' in width, and extend 8" towards the nearest sideline
      amateur_lane_line_1 = create_rectangle(
        x_min = -40,
        x_max = -39,
        y_min = -6 - (8 / 12),
        y_max = -6
      )

      # The second set of lane lines are 3' from the first lane line, measure 2" in width, and extend 8" towards the nearest sideline
      amateur_lane_line_2 = create_rectangle(
        x_min = -36,
        x_max = -36 + (2 / 12),
        y_min = -6 - (8 / 12),
        y_max = -6
      )

      # The third set of lane lines are 3' from the second lane line, measure 2" in width, and extend 8" towards the nearest sideline
      amateur_lane_line_3 = create_rectangle(
        x_min = -33 + (2 / 12),
        x_max = -33 + (4 / 12),
        y_min = -6 - (8 / 12),
        y_max = -6
      )

      # The fourth set of lane lines are 3' from the third lane line, measure 2" in width, and extend 8" towards the nearest sideline
      amateur_lane_line_4 = create_rectangle(
        x_min = -30 + (4 / 12),
        x_max = -30 + (6 / 12),
        y_min = -6 - (8 / 12),
        y_max = -6
      )

      # Lane line 4 but on the other side of the free-throw lane
      amateur_lane_line_5 = reflect(
        amateur_lane_line_4,
        over_x = TRUE,
        over_y = FALSE
      )

      # Lane line 3 but on the other side of the free-throw lane
      amateur_lane_line_6 = reflect(
        amateur_lane_line_3,
        over_x = TRUE,
        over_y = FALSE
      )

      # Lane line 2 but on the other side of the free-throw lane
      amateur_lane_line_7 = reflect(
        amateur_lane_line_2,
        over_x = TRUE,
        over_y = FALSE
      )

      # Lane line 1 but on the other side of the free-throw lane
      amateur_lane_line_8 = reflect(
        amateur_lane_line_1,
        over_x = TRUE,
        over_y = FALSE
      )

      if(full_surf){
        # If the surface being drawn is a full-surface representation, reflect
        # over the y axis
        amateur_lane_line_1 = rbind(
          amateur_lane_line_1,
          reflect(
            amateur_lane_line_1,
            over_y = TRUE
          )
        )

        amateur_lane_line_2 = rbind(
          amateur_lane_line_2,
          reflect(
            amateur_lane_line_2,
            over_y = TRUE
          )
        )

        amateur_lane_line_3 = rbind(
          amateur_lane_line_3,
          reflect(
            amateur_lane_line_3,
            over_y = TRUE
          )
        )

        amateur_lane_line_4 = rbind(
          amateur_lane_line_4,
          reflect(
            amateur_lane_line_4,
            over_y = TRUE
          )
        )

        amateur_lane_line_5 = rbind(
          amateur_lane_line_5,
          reflect(
            amateur_lane_line_5,
            over_y = TRUE
          )
        )

        amateur_lane_line_6 = rbind(
          amateur_lane_line_6,
          reflect(
            amateur_lane_line_6,
            over_y = TRUE
          )
        )

        amateur_lane_line_7 = rbind(
          amateur_lane_line_7,
          reflect(
            amateur_lane_line_7,
            over_y = TRUE
          )
        )

        amateur_lane_line_8 = rbind(
          amateur_lane_line_8,
          reflect(
            amateur_lane_line_8,
            over_y = TRUE
          )
        )
      }

      if(rotate){
        # If the desired output needs to be rotated, rotate the coordinates
        amateur_lane_line_1 = rotate_coords(
          amateur_lane_line_1,
          rotation_dir
        )

        amateur_lane_line_2 = rotate_coords(
          amateur_lane_line_2,
          rotation_dir
        )

        amateur_lane_line_3 = rotate_coords(
          amateur_lane_line_3,
          rotation_dir
        )

        amateur_lane_line_4 = rotate_coords(
          amateur_lane_line_4,
          rotation_dir
        )

        amateur_lane_line_5 = rotate_coords(
          amateur_lane_line_5,
          rotation_dir
        )

        amateur_lane_line_6 = rotate_coords(
          amateur_lane_line_6,
          rotation_dir
        )

        amateur_lane_line_7 = rotate_coords(
          amateur_lane_line_7,
          rotation_dir
        )

        amateur_lane_line_8 = rotate_coords(
          amateur_lane_line_8,
          rotation_dir
        )
      }

      # Add the amateur lane lines to the ggplot2 instance. They will be black in color
      g = g +
        ggplot2::geom_polygon(data = amateur_lane_line_1, ggplot2::aes(x, y), fill = '#000000') +
        ggplot2::geom_polygon(data = amateur_lane_line_2, ggplot2::aes(x, y), fill = '#000000') +
        ggplot2::geom_polygon(data = amateur_lane_line_3, ggplot2::aes(x, y), fill = '#000000') +
        ggplot2::geom_polygon(data = amateur_lane_line_4, ggplot2::aes(x, y), fill = '#000000') +
        ggplot2::geom_polygon(data = amateur_lane_line_5, ggplot2::aes(x, y), fill = '#000000') +
        ggplot2::geom_polygon(data = amateur_lane_line_6, ggplot2::aes(x, y), fill = '#000000') +
        ggplot2::geom_polygon(data = amateur_lane_line_7, ggplot2::aes(x, y), fill = '#000000') +
        ggplot2::geom_polygon(data = amateur_lane_line_8, ggplot2::aes(x, y), fill = '#000000')
    }

    return(g)
  }

  else if(league %in% c('NCAA', 'NCAAM', 'NCAAW', 'COLLEGE')){
    # The first set of lane lines are 7' from the interior of the baseline, measure 1' in width, and extend 8" towards the nearest sideline
    lane_line_1 = create_rectangle(
      x_min = -40,
      x_max = -39,
      y_min = -6 - (8 / 12),
      y_max = -6
    )

    # The second set of lane lines are 3' from the first lane line, measure 2" in width, and extend 8" towards the nearest sideline
    lane_line_2 = create_rectangle(
      x_min = -36,
      x_max = -36 + (2 / 12),
      y_min = -6 - (8 / 12),
      y_max = -6
    )

    # The third set of lane lines are 3' from the second lane line, measure 2" in width, and extend 8" towards the nearest sideline
    lane_line_3 = create_rectangle(
      x_min = -33 + (2 / 12),
      x_max = -33 + (4 / 12),
      y_min = -6 - (8 / 12),
      y_max = -6
    )

    # The fourth set of lane lines are 3' from the third lane line, measure 2" in width, and extend 8" towards the nearest sideline
    lane_line_4 = create_rectangle(
      x_min = -30 + (4 / 12),
      x_max = -30 + (6 / 12),
      y_min = -6 - (8 / 12),
      y_max = -6
    )

    # Lane line 4 but on the other side of the free-throw lane
    lane_line_5 = reflect(
      lane_line_4,
      over_x = TRUE,
      over_y = FALSE
    )

    # Lane line 3 but on the other side of the free-throw lane
    lane_line_6 = reflect(
      lane_line_3,
      over_x = TRUE,
      over_y = FALSE
    )

    # Lane line 2 but on the other side of the free-throw lane
    lane_line_7 = reflect(
      lane_line_2,
      over_x = TRUE,
      over_y = FALSE
    )

    # Lane line 1 but on the other side of the free-throw lane
    lane_line_8 = reflect(
      lane_line_1,
      over_x = TRUE,
      over_y = FALSE
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect
      # over the y axis
      lane_line_1 = rbind(
        lane_line_1,
        reflect(
          lane_line_1,
          over_y = TRUE
        )
      )

      lane_line_2 = rbind(
        lane_line_2,
        reflect(
          lane_line_2,
          over_y = TRUE
        )
      )

      lane_line_3 = rbind(
        lane_line_3,
        reflect(
          lane_line_3,
          over_y = TRUE
        )
      )

      lane_line_4 = rbind(
        lane_line_4,
        reflect(
          lane_line_4,
          over_y = TRUE
        )
      )

      lane_line_5 = rbind(
        lane_line_5,
        reflect(
          lane_line_5,
          over_y = TRUE
        )
      )

      lane_line_6 = rbind(
        lane_line_6,
        reflect(
          lane_line_6,
          over_y = TRUE
        )
      )

      lane_line_7 = rbind(
        lane_line_7,
        reflect(
          lane_line_7,
          over_y = TRUE
        )
      )

      lane_line_8 = rbind(
        lane_line_8,
        reflect(
          lane_line_8,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      lane_line_1 = rotate_coords(
        lane_line_1,
        rotation_dir
      )

      lane_line_2 = rotate_coords(
        lane_line_2,
        rotation_dir
      )

      lane_line_3 = rotate_coords(
        lane_line_3,
        rotation_dir
      )

      lane_line_4 = rotate_coords(
        lane_line_4,
        rotation_dir
      )

      lane_line_5 = rotate_coords(
        lane_line_5,
        rotation_dir
      )

      lane_line_6 = rotate_coords(
        lane_line_6,
        rotation_dir
      )

      lane_line_7 = rotate_coords(
        lane_line_7,
        rotation_dir
      )

      lane_line_8 = rotate_coords(
        lane_line_8,
        rotation_dir
      )
    }

    # Add the amateur lane lines to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = lane_line_1, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = lane_line_2, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = lane_line_3, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = lane_line_4, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = lane_line_5, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = lane_line_6, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = lane_line_7, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = lane_line_8, ggplot2::aes(x, y), fill = '#000000')
  }

  else if(league == 'FIBA'){
    # The first set of lane lines are 1.75m from the interior of the baseline, measure 5cm in width, and extend 10cm towards the nearest sideline
    lane_line_1 = create_rectangle(
      x_min = m_to_ft(-12.25),
      x_max = m_to_ft(-12.20),
      y_min = m_to_ft(-2.55),
      y_max = m_to_ft(-2.45)
    )

    # The second set of lane lines are 85cm from the first lane line, and measure 40cm in width, and extend 10cm towards the nearest sideline
    lane_line_2 = create_rectangle(
      x_min = m_to_ft(-11.35),
      x_max = m_to_ft(-10.95),
      y_min = m_to_ft(-2.55),
      y_max = m_to_ft(-2.45)
    )

    # The third set of lane lines are 85cm from the second lane line, and measure 5cm in width, and extend 10cm towards the nearest sideline
    lane_line_3 = create_rectangle(
      x_min = m_to_ft(-10.10),
      x_max = m_to_ft(-10.05),
      y_min = m_to_ft(-2.55),
      y_max = m_to_ft(-2.45)
    )

    # The fourth set of lane lines are 85cm from the third lane line, and measure 5cm in width, and extend 10cm towards the nearest sideline
    lane_line_4 = create_rectangle(
      x_min = m_to_ft(-9.20),
      x_max = m_to_ft(-9.15),
      y_min = m_to_ft(-2.55),
      y_max = m_to_ft(-2.45)
    )

    # Lane line 4 but on the other side of the free-throw lane
    lane_line_5 = reflect(
      lane_line_4,
      over_x = TRUE,
      over_y = FALSE
    )

    # Lane line 3 but on the other side of the free-throw lane
    lane_line_6 = reflect(
      lane_line_3,
      over_x = TRUE,
      over_y = FALSE
    )

    # Lane line 2 but on the other side of the free-throw lane
    lane_line_7 = reflect(
      lane_line_2,
      over_x = TRUE,
      over_y = FALSE
    )

    # Lane line 1 but on the other side of the free-throw lane
    lane_line_8 = reflect(
      lane_line_1,
      over_x = TRUE,
      over_y = FALSE
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect
      # over the y axis
      lane_line_1 = rbind(
        lane_line_1,
        reflect(
          lane_line_1,
          over_y = TRUE
        )
      )

      lane_line_2 = rbind(
        lane_line_2,
        reflect(
          lane_line_2,
          over_y = TRUE
        )
      )

      lane_line_3 = rbind(
        lane_line_3,
        reflect(
          lane_line_3,
          over_y = TRUE
        )
      )

      lane_line_4 = rbind(
        lane_line_4,
        reflect(
          lane_line_4,
          over_y = TRUE
        )
      )

      lane_line_5 = rbind(
        lane_line_5,
        reflect(
          lane_line_5,
          over_y = TRUE
        )
      )

      lane_line_6 = rbind(
        lane_line_6,
        reflect(
          lane_line_6,
          over_y = TRUE
        )
      )

      lane_line_7 = rbind(
        lane_line_7,
        reflect(
          lane_line_7,
          over_y = TRUE
        )
      )

      lane_line_8 = rbind(
        lane_line_8,
        reflect(
          lane_line_8,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      lane_line_1 = rotate_coords(
        lane_line_1,
        rotation_dir
      )

      lane_line_2 = rotate_coords(
        lane_line_2,
        rotation_dir
      )

      lane_line_3 = rotate_coords(
        lane_line_3,
        rotation_dir
      )

      lane_line_4 = rotate_coords(
        lane_line_4,
        rotation_dir
      )

      lane_line_5 = rotate_coords(
        lane_line_5,
        rotation_dir
      )

      lane_line_6 = rotate_coords(
        lane_line_6,
        rotation_dir
      )

      lane_line_7 = rotate_coords(
        lane_line_7,
        rotation_dir
      )

      lane_line_8 = rotate_coords(
        lane_line_8,
        rotation_dir
      )
    }

    # Add the amateur lane lines to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = lane_line_1, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = lane_line_2, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = lane_line_3, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = lane_line_4, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = lane_line_5, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = lane_line_6, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = lane_line_7, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = lane_line_8, ggplot2::aes(x, y), fill = '#000000')
  }

  else {
    # If the league isn't valid (i.e. either NBA, WNBA, NCAA, or FIBA), return
    # the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the free-throw semi-circle
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw'
#' @return A ggplot2 instance with the free-throw semi-circle added to it
free_throw_semi_circle = function(g, league, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NBA', 'WNBA')){
    # The free-throw semi-circle has radius 6' and is centered at the midpoint of the free-throw line (18' 11" from the interior edge of the baseline)
    free_throw_semi_circle = rbind(
      create_circle(
        center = c(-28 - (1/12), 0),
        start = .5,
        end = -.5,
        d = 12
      ),

      data.frame(
        x = -28 - (1/12),
        y = -6
      ),

      create_circle(
        center = c(-28 - (1/12), 0),
        start = -.5,
        end = .5,
        d = 12 - (4/12)
      ),

      data.frame(
        x = -28 - (1/12),
        y = 6
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      free_throw_semi_circle = rbind(
        free_throw_semi_circle,
        reflect(
          free_throw_semi_circle,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      free_throw_semi_circle = rotate_coords(
        free_throw_semi_circle,
        rotation_dir
      )
    }

    # Add the free-throw semi-circle to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = free_throw_semi_circle, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league %in% c('NCAA', 'NCAAM', 'NCAAW', 'COLLEGE')){
    # The free-throw semi-circle has radius 6' and is centered at the midpoint of the free-throw line (18' 11" from the interior edge of the baseline)
    free_throw_semi_circle = rbind(
      create_circle(
        center = c(-28 - (1/12), 0),
        start = .5,
        end = -.5,
        d = 12
      ),

      data.frame(
        x = -28 - (1/12),
        y = -6
      ),

      create_circle(
        center = c(-28 - (1/12), 0),
        start = -.5,
        end = .5,
        d = 12 - (4/12)
      ),

      data.frame(
        x = -28 - (1/12),
        y = 6
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      free_throw_semi_circle = rbind(
        free_throw_semi_circle,
        reflect(
          free_throw_semi_circle,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      free_throw_semi_circle = rotate_coords(
        free_throw_semi_circle,
        rotation_dir
      )
    }

    # Add the free-throw semi-circle to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = free_throw_semi_circle, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'FIBA'){
    # The free-throw semi-circle has radius 1.8m and is centered at the center of the free-throw line (5.8m from the interior edge of the baseline)
    free_throw_semi_circle = rbind(
      create_circle(
        center = c(m_to_ft(-8.2), m_to_ft(0)),
        start = .5,
        end = -.5,
        d = m_to_ft(3.6)
      ),

      data.frame(
        x = m_to_ft(-8.2),
        y = m_to_ft(-1.8)
      ),

      create_circle(
        center = c(m_to_ft(-8.2), m_to_ft(0)),
        start = -.5,
        end = .5,
        d = m_to_ft(3.5)
      ),

      data.frame(
        x = m_to_ft(-8.2),
        y = m_to_ft(1.8)
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      free_throw_semi_circle = rbind(
        free_throw_semi_circle,
        reflect(
          free_throw_semi_circle,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      free_throw_semi_circle = rotate_coords(
        free_throw_semi_circle,
        rotation_dir
      )
    }

    # Add the free-throw semi-circle to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = free_throw_semi_circle, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NBA, WNBA, NCAA, or FIBA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the dashed part of the free-throw circle
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw'
#' @return A ggplot2 instance with the dashed part of the free-throw circle added to it
free_throw_dashed_semi_circle = function(g, league, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NBA', 'WNBA')){
    # Per the NBA rule book, the solid portion of the circle extends along an arc of length 12.29" behind the free-throw line. The angle theta must be calculated to determine where to start. It can be determined via the relationship s = r*theta, where s is the arc length, r is the radius, and theta is the angle (in radians)

    # First, define s to be the arc length in feet
    s = 12.29 / 12

    # The outer radius is 6'
    r = 6

    # Theta is therefore s/r, but since the create_circle() function takes an angle in radians/pi, this must be divided out as well
    theta = (s / r) / pi

    # Since the circle must extend theta radians past +/-pi/2, theta must be added/subtracted from 1/2 accordingly
    start_angle = .5
    end_angle = .5 + theta

    # Create the first dash
    dash_1 = rbind(
      create_circle(
        center = c(-28 - (1/12), 0),
        start = start_angle,
        end = end_angle,
        d = 12
      ),

      create_circle(
        center = c(-28 - (1/12), 0),
        start = end_angle,
        end = start_angle,
        d = 12 - (4/12)
      ),

      create_circle(
        center = c(-28 - (1/12), 0),
        start = start_angle,
        end = end_angle,
        d = 12
      )[1, ]
    )

    # The dashed sections of the free-throw circle are all of length 15.5", and are spaced 15.5" from each other. Following a similar process as above, the starting and ending angles can be computed

    # First, compute the arc length in feet
    s = 15.5 / 12

    # The outer radius is 6'
    r = 6

    # Finally, compute the angle traced by the dashed lines
    theta_dashes = (s / r) / pi

    # This theta must be added to start_angle above to get the starting angle for each dash, and added twice to get the ending angle for each dash. This pattern can be repeated, taking the end angle of the previous dash as the start angle for the following dash
    dash_2_start_angle = end_angle + theta_dashes
    dash_2_end_angle = end_angle + (2 * theta_dashes)

    dash_3_start_angle = dash_2_end_angle + theta_dashes
    dash_3_end_angle = dash_2_end_angle + (2 * theta_dashes)

    dash_4_start_angle = dash_3_end_angle + theta_dashes
    dash_4_end_angle = dash_3_end_angle + (2 * theta_dashes)

    # Create the remaining dashes to complete the quarter-circle
    dash_2 = rbind(
      create_circle(
        center = c(-28 - (1/12), 0),
        start = dash_2_start_angle,
        end = dash_2_end_angle,
        d = 12
      ),

      create_circle(
        center = c(-28 - (1/12), 0),
        start = dash_2_end_angle,
        end = dash_2_start_angle,
        d = 12 - (4/12)
      ),

      create_circle(
        center = c(-28 - (1/12), 0),
        start = dash_2_start_angle,
        end = dash_2_end_angle,
        d = 12
      )[1, ]
    )

    dash_3 = rbind(
      create_circle(
        center = c(-28 - (1/12), 0),
        start = dash_3_start_angle,
        end = dash_3_end_angle,
        d = 12
      ),

      create_circle(
        center = c(-28 - (1/12), 0),
        start = dash_3_end_angle,
        end = dash_3_start_angle,
        d = 12 - (4/12)
      ),

      create_circle(
        center = c(-28 - (1/12), 0),
        start = dash_3_start_angle,
        end = dash_3_end_angle,
        d = 12
      )[1, ]
    )

    dash_4 = rbind(
      create_circle(
        center = c(-28 - (1/12), 0),
        start = dash_4_start_angle,
        end = dash_4_end_angle,
        d = 12
      ),

      create_circle(
        center = c(-28 - (1/12), 0),
        start = dash_4_end_angle,
        end = dash_4_start_angle,
        d = 12 - (4/12)
      ),

      create_circle(
        center = c(-28 - (1/12), 0),
        start = dash_4_start_angle,
        end = dash_4_end_angle,
        d = 12
      )[1, ]
    )

    # Reflect these over the x axis to get the remaining quarter-circle
    dash_5 = reflect(
      dash_4,
      over_x = TRUE,
      over_y = FALSE
    )

    dash_6 = reflect(
      dash_3,
      over_x = TRUE,
      over_y = FALSE
    )

    dash_7 = reflect(
      dash_2,
      over_x = TRUE,
      over_y = FALSE
    )

    dash_8 = reflect(
      dash_1,
      over_x = TRUE,
      over_y = FALSE
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      dash_1 = rbind(
        dash_1,
        reflect(
          dash_1,
          over_y = TRUE
        )
      )

      dash_2 = rbind(
        dash_2,
        reflect(
          dash_2,
          over_y = TRUE
        )
      )

      dash_3 = rbind(
        dash_3,
        reflect(
          dash_3,
          over_y = TRUE
        )
      )

      dash_4 = rbind(
        dash_4,
        reflect(
          dash_4,
          over_y = TRUE
        )
      )

      dash_5 = rbind(
        dash_5,
        reflect(
          dash_5,
          over_y = TRUE
        )
      )

      dash_6 = rbind(
        dash_6,
        reflect(
          dash_6,
          over_y = TRUE
        )
      )

      dash_7 = rbind(
        dash_7,
        reflect(
          dash_7,
          over_y = TRUE
        )
      )

      dash_8 = rbind(
        dash_8,
        reflect(
          dash_8,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      dash_1 = rotate_coords(
        dash_1,
        rotation_dir
      )

      dash_2 = rotate_coords(
        dash_2,
        rotation_dir
      )

      dash_3 = rotate_coords(
        dash_3,
        rotation_dir
      )

      dash_4 = rotate_coords(
        dash_4,
        rotation_dir
      )

      dash_5 = rotate_coords(
        dash_5,
        rotation_dir
      )

      dash_6 = rotate_coords(
        dash_6,
        rotation_dir
      )

      dash_7 = rotate_coords(
        dash_7,
        rotation_dir
      )

      dash_8 = rotate_coords(
        dash_8,
        rotation_dir
      )
    }

    # Add the dashed part of the free-throw circle to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = dash_1, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = dash_2, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = dash_3, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = dash_4, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = dash_5, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = dash_6, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = dash_7, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = dash_8, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league %in% c('NCAA', 'NCAAM', 'NCAAW', 'COLLEGE')){
    # This feature is not needed on an NCAA court

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'FIBA'){
    # This feature is not needed on a FIBA court

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NBA, WNBA, NCAA, or FIBA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the lower defensive box
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw'
#' @return A ggplot2 instance with the lower defensive box added to it
lower_defensive_box = function(g, league, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NBA', 'WNBA')){
    # The lower defensive box is comprised of four dashes: two along the baseline that are 3' (interior) from the edges of the free-throw lane, protrude 6" into the court, and have a 2" width, and the other is located 13' (interior) from the baseline and 3' (exterior) from the edge of the free-throw lane, has a length of 6", and a width of 2"

    # Dash 1 is the line along the baseline
    dash_1 = create_rectangle(
      x_min = -47,
      x_max = -46.5,
      y_min = 11,
      y_max = 11 + (2/12)
    )

    # Dash 2 is the line in the painted area
    dash_2 = create_rectangle(
      x_min = -34,
      x_max = -34 + (2/12),
      y_min = 5,
      y_max = 5.5
    )

    # Dash 3 is the reflection of dash 2 over the x axis
    dash_3 = reflect(
      dash_2,
      over_x = TRUE,
      over_y = FALSE
    )

    # Dash 4 is the reflection of dash 1 over the x axis
    dash_4 = reflect(
      dash_1,
      over_x = TRUE,
      over_y = FALSE
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      dash_1 = rbind(
        dash_1,
        reflect(
          dash_1,
          over_y = TRUE
        )
      )

      dash_2 = rbind(
        dash_2,
        reflect(
          dash_2,
          over_y = TRUE
        )
      )

      dash_3 = rbind(
        dash_3,
        reflect(
          dash_3,
          over_y = TRUE
        )
      )

      dash_4 = rbind(
        dash_4,
        reflect(
          dash_4,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      dash_1 = rotate_coords(
        dash_1,
        rotation_dir
      )

      dash_2 = rotate_coords(
        dash_2,
        rotation_dir
      )

      dash_3 = rotate_coords(
        dash_3,
        rotation_dir
      )

      dash_4 = rotate_coords(
        dash_4,
        rotation_dir
      )
    }

    # Add the lower defensive box to the ggplot2 instance. The markings will be black in color
    g = g +
      ggplot2::geom_polygon(data = dash_1, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = dash_2, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = dash_3, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = dash_4, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league %in% c('NCAA', 'NCAAM', 'NCAAW', 'COLLEGE')){
    # The lower defensive box is comprised of two dashes along the baseline that are 3' (interior) from the edges of the free-throw lane, protrude 12" into the court, and have a 2" width

    # Dash 1 is the line along the baseline
    dash_1 = create_rectangle(
      x_min = -47,
      x_max = -46,
      y_min = 9,
      y_max = 9 + (2/12)
    )

    # Dash 2 is the reflection of dash 1 over the x axis
    dash_2 = reflect(
      dash_1,
      over_x = TRUE,
      over_y = FALSE
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      dash_1 = rbind(
        dash_1,
        reflect(
          dash_1,
          over_y = TRUE
        )
      )

      dash_2 = rbind(
        dash_2,
        reflect(
          dash_2,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      dash_1 = rotate_coords(
        dash_1,
        rotation_dir
      )

      dash_2 = rotate_coords(
        dash_2,
        rotation_dir
      )
    }

    # Add the lower defensive box to the ggplot2 instance. The markings will be black in color
    g = g +
      ggplot2::geom_polygon(data = dash_1, ggplot2::aes(x, y), fill = '#000000') +
      ggplot2::geom_polygon(data = dash_2, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'FIBA'){
    # This feature is not needed on a FIBA court

    # Return the ggplot2 instance
    return(g)
  }

  else {
    # If the league isn't valid (i.e. either NBA, WNBA, NCAA, or FIBA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the restricted area arc
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw'
#' @return A ggplot2 instance with the restricted area arc added to it
restricted_area_arc = function(g, league, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NBA', 'WNBA')){
    # Following the same process as for the three-point line, the restricted area arc's starting and ending angle can be computed
    start_y = -4 - (2/12)

    # The rule book describes the arc as having a radius of 4'
    radius_outer = 4 + (2/12)

    # From here, the calculation is relatively straightforward. To determine the angle, the inverse sine is needed. It will be multiplied by pi so that it can be passed to the create_circle() function
    start_angle_outer = asin(start_y / radius_outer) / pi
    end_angle_outer = -start_angle_outer

    # The same method can be used for the inner angles, however, since the inner radius will be traced from bottom to top, the angle must be negative to start
    radius_inner = 4
    start_angle_inner = -asin((start_y + (2/12)) / radius_inner) / pi
    end_angle_inner = -start_angle_inner

    # The restricted area arc is an arc of radius 4' from the center of the basket, and extending in a straight line to the front face of the backboard, and having thickness of 2"
    restricted_area_arc = rbind(
      data.frame(
        x = -43,
        y = -4 - (2/12)
      ),

      create_circle(
        center = c(-41.75, 0),
        start = start_angle_outer,
        end = end_angle_outer,
        d = 8 + (4/12)
      ),

      data.frame(
        x = c(-43, -43),
        y = c(4 + (2/12), 4)
      ),

      create_circle(
        center = c(-41.75, 0),
        start = start_angle_inner,
        end = end_angle_inner,
        d = 8
      ),

      data.frame(
        x = c(-43, -43),
        y = c(-4, -4 - (2/12))
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      restricted_area_arc = rbind(
        restricted_area_arc,
        reflect(
          restricted_area_arc,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      restricted_area_arc = rotate_coords(
        restricted_area_arc,
        rotation_dir
      )
    }

    # Add the restricted area arc(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = restricted_area_arc, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league %in% c('NCAA', 'NCAAM', 'NCAAW', 'COLLEGE')){
    # Following the same process as for the three-point line, the restricted area arc's starting and ending angle can be computed
    start_y = -4 - (2/12)

    # The rule book describes the arc as having a radius of 4'
    radius_outer = 4 + (2/12)

    # From here, the calculation is relatively straightforward. To determine the angle, the inverse sine is needed. It will be multiplied by pi so that it can be passed to the create_circle() function
    start_angle_outer = asin(start_y / radius_outer) / pi
    end_angle_outer = -start_angle_outer

    # The same method can be used for the inner angles, however, since the inner radius will be traced from bottom to top, the angle must be negative to start
    radius_inner = 4
    start_angle_inner = -asin((start_y + (2/12)) / radius_inner) / pi
    end_angle_inner = -start_angle_inner

    # The restricted area arc is an arc of radius 4' from the center of the basket, and extending in a straight line to the front face of the backboard, and having thickness of 2"
    restricted_area_arc = rbind(
      data.frame(
        x = -43,
        y = -4 - (2/12)
      ),

      create_circle(
        center = c(-41.75, 0),
        start = start_angle_outer,
        end = end_angle_outer,
        d = 8 + (4/12)
      ),

      data.frame(
        x = c(-43, -43),
        y = c(4 + (2/12), 4)
      ),

      create_circle(
        center = c(-41.75, 0),
        start = start_angle_inner,
        end = end_angle_inner,
        d = 8
      ),

      data.frame(
        x = c(-43, -43),
        y = c(-4, -4 - (2/12))
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      restricted_area_arc = rbind(
        restricted_area_arc,
        reflect(
          restricted_area_arc,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      restricted_area_arc = rotate_coords(
        restricted_area_arc,
        rotation_dir
      )
    }

    # Add the restricted area arc(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = restricted_area_arc, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'FIBA'){
    # Following the same process as for the three-point line, the restricted area arc's starting and ending angle can be computed
    start_y = m_to_ft(-1.3)

    # The rule book describes the arc as having a radius of 1.25m
    radius_outer = m_to_ft(1.3)

    # From here, the calculation is relatively straightforward. To determine the angle, the inverse sine is needed. It will be multiplied by pi so that it can be passed to the create_circle() function
    start_angle_outer = asin(start_y / radius_outer) / pi
    end_angle_outer = -start_angle_outer

    # The same method can be used for the inner angles, however, since the inner radius will be traced from bottom to top, the angle must be negative to start
    radius_inner = m_to_ft(1.25)
    start_angle_inner = -asin(m_to_ft(-1.25) / radius_inner) / pi
    end_angle_inner = -start_angle_inner

    # The restricted area arc is an arc of radius 1.25m from the center of the basket, and extending in a straight line to the front face of the backboard, and having thickness of 5cm
    restricted_area_arc = rbind(
      data.frame(
        x = m_to_ft(-12.8),
        y = m_to_ft(-1.3)
      ),

      create_circle(
        center = c(m_to_ft(-12.425), m_to_ft(0)),
        start = start_angle_outer,
        end = end_angle_outer,
        d = m_to_ft(2.6)
      ),

      data.frame(
        x = c(m_to_ft(-12.8), m_to_ft(-12.8)),
        y = c(m_to_ft(1.3), m_to_ft(1.25))
      ),

      create_circle(
        center = c(m_to_ft(-12.425), m_to_ft(0)),
        start = start_angle_inner,
        end = end_angle_inner,
        d = m_to_ft(2.5)
      ),

      data.frame(
        x = c(m_to_ft(-12.8), m_to_ft(-12.8)),
        y = c(m_to_ft(-1.25), m_to_ft(-1.3))
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      restricted_area_arc = rbind(
        restricted_area_arc,
        reflect(
          restricted_area_arc,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      restricted_area_arc = rotate_coords(
        restricted_area_arc,
        rotation_dir
      )
    }

    # Add the restricted area arc(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = restricted_area_arc, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else{
    # If the league isn't valid (i.e. either NBA, WNBA, NCAA, or FIBA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the backboard
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw'
#' @return A ggplot2 instance with the backboard added to it
backboard = function(g, league, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NBA', 'WNBA')){
    # In the (W)NBA, the backboard is 6' in width, with its front edge 4' from the interior of the baseline
    backboard = create_rectangle(
      x_min = -43 - (4/12),
      x_max = -43,
      y_min = -3,
      y_max = 3
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      backboard = rbind(
        backboard,
        reflect(
          backboard,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      backboard = rotate_coords(
        backboard,
        rotation_dir
      )
    }

    # Add the backboard(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = backboard, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league %in% c('NCAA', 'NCAAM', 'NCAAW', 'COLLEGE')){
    # In college, the backboard is 6' in width, with its front edge 4' from the interior of the baseline
    backboard = create_rectangle(
      x_min = -43 - (4/12),
      x_max = -43,
      y_min = -3,
      y_max = 3
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      backboard = rbind(
        backboard,
        reflect(
          backboard,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      backboard = rotate_coords(
        backboard,
        rotation_dir
      )
    }

    # Add the backboard(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = backboard, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'FIBA'){
    # In FIBA, the backboard is 6' in width, with its front edge 1.2m from the interior of the baseline
    backboard = create_rectangle(
      x_min = m_to_ft(-12.8),
      x_max = m_to_ft(-12.9),
      y_min = m_to_ft(-.9),
      y_max = m_to_ft(.9)
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      backboard = rbind(
        backboard,
        reflect(
          backboard,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      backboard = rotate_coords(
        backboard,
        rotation_dir
      )
    }

    # Add the backboard(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = backboard, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else{
    # If the league isn't valid (i.e. either NBA, WNBA, NCAA, or FIBA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the basket ring
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw'
#' @return A ggplot2 instance with the basket ring added to it
basket_ring = function(g, league, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NBA', 'WNBA')){
    # The connector has a width of 5.5", so 2.75" are on either side of the x axis. The ring has a radius of 9", so the arcsine of these measurements should give the angle at which point they connect
    start_angle = pi - asin(2.75/9)

    # The ending angle of the ring would be the negative of the starting angle
    end_angle = -start_angle

    # Get the ring
    basket_ring = rbind(
      data.frame(
        x = c(-43, -41.75 - ((9/12) * cos(start_angle))),
        y = c(2.75/12, 2.75/12)
      ),

      create_circle(
        center = c(-41.75, 0),
        start = start_angle,
        end = end_angle,
        d = 1.5 + (4/12)
      ),

      data.frame(
        x = c(
          -41.75 - ((9/12) * cos(start_angle)),
          -43,
          -43
        ),

        y = c(
          -2.75/12,
          -2.75/12,
          2.75/12
        )
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      basket_ring = rbind(
        basket_ring,
        reflect(
          basket_ring,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      basket_ring = rotate_coords(
        basket_ring,
        rotation_dir
      )
    }

    # Add the basket ring(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = basket_ring, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league %in% c('NCAA', 'NCAAM', 'NCAAW', 'COLLEGE')){
    # The connector has a width of 5.5", so 2.75" are on either side of the x axis. The ring has a radius of 9", so the arcsine of these measurements should give the angle at which point they connect
    start_angle = pi - asin(2.75/9)

    # The ending angle of the ring would be the negative of the starting angle
    end_angle = -start_angle

    # Get the ring
    basket_ring = rbind(
      data.frame(
        x = c(-43, -41.75 - ((9/12) * cos(start_angle))),
        y = c(2.75/12, 2.75/12)
      ),

      create_circle(
        center = c(-41.75, 0),
        start = start_angle,
        end = end_angle,
        d = 1.5 + (4/12)
      ),

      data.frame(
        x = c(
          -41.75 - ((9/12) * cos(start_angle)),
          -43,
          -43
        ),

        y = c(
          -2.75/12,
          -2.75/12,
          2.75/12
        )
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      basket_ring = rbind(
        basket_ring,
        reflect(
          basket_ring,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      basket_ring = rotate_coords(
        basket_ring,
        rotation_dir
      )
    }

    # Add the basket ring(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = basket_ring, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'FIBA'){
    # The connector has a width of .126m, so .063m are on either side of the x axis. The ring has a radius of .225m, so the arcsine of these measurements should give the angle at which point they connect
    start_angle = pi - asin(m_to_ft(.063)/m_to_ft(.225))

    # The ending angle of the ring would be the negative of the starting angle
    end_angle = -start_angle

    # Get the ring
    basket_ring = rbind(
      data.frame(
        x = c(
          m_to_ft(-12.8),
          m_to_ft(-12.8) - (m_to_ft(.45) * cos(start_angle))
        ),

        y = c(
          m_to_ft(.063),
          m_to_ft(.063)
        )
      ),

      create_circle(
        center = c(m_to_ft(-12.425), m_to_ft(0)),
        start = start_angle,
        end = end_angle,
        d = m_to_ft(.55)
      ),

      data.frame(
        x = c(
          m_to_ft(-12.8) - (m_to_ft(.45) * cos(start_angle)),
          m_to_ft(-12.8),
          m_to_ft(-12.8)
        ),

        y = c(
          m_to_ft(-.063),
          m_to_ft(-.063),
          m_to_ft(.063)
        )
      )
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
      basket_ring = rbind(
        basket_ring,
        reflect(
          basket_ring,
          over_y = TRUE
        )
      )
    }

    if(rotate){
      # If the desired output needs to be rotated, rotate the coordinates
      basket_ring = rotate_coords(
        basket_ring,
        rotation_dir
      )
    }

    # Add the basket ring(s) to the ggplot2 instance. They will be black in color
    g = g +
      ggplot2::geom_polygon(data = basket_ring, ggplot2::aes(x, y), fill = '#000000')

    # Return the ggplot2 instance
    return(g)
  }

  else{
    # If the league isn't valid (i.e. either NBA, WNBA, NCAA, or FIBA), return the ggplot2 instance
    return(g)
  }
}

#' Generate the dataframe for the points that comprise the net
#'
#' @param g A ggplot2 instance on which to add the feature
#' @param league The league for which to draw the surface
#' @param full_surf A boolean indicating whether or not this feature is needed for a full-surface representation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw'
#' @return A ggplot2 instance with the net added to it
net = function(g, league, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Initialize x and y (to pass checks)
  x = y = NULL

  if(league %in% c('NBA', 'WNBA')){
    # The ring's center is 15" from the backboard, and 63" from the baseline, which means it is centered at (+/-41.75, 0). The ring has an interior diameter of 18", which is where the net is visible from above
    net = create_circle(
      center = c(-41.75, 0),
      start = 0,
      end = 2,
      d = 1.5
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
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

    # Add the net(s) to the ggplot2 instance. They will be white in color
    g = g +
      ggplot2::geom_polygon(data = net, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league %in% c('NCAA', 'NCAAM', 'NCAAW', 'COLLEGE')){
    # The ring's center is 15" from the backboard, and 63" from the baseline, which means it is centered at (+/-41.75, 0). The ring has an interior diameter of 18", which is where the net is visible from above
    net = create_circle(
      center = c(-41.75, 0),
      start = 0,
      end = 2,
      d = 1.5
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
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

    # Add the net(s) to the ggplot2 instance. They will be white in color
    g = g +
      ggplot2::geom_polygon(data = net, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else if(league == 'FIBA'){
    # The ring's center is .375m from the backboard, and 1.575m from the baseline, which means it is centered at (+/-12.425m, 0). The ring has an interior diameter of 450mm, which is where the net is visible from above
    net = create_circle(
      center = c(m_to_ft(-12.425), m_to_ft(0)),
      start = 0,
      end = 2,
      d = m_to_ft(.450)
    )

    if(full_surf){
      # If the surface being drawn is a full-surface representation, reflect over the y axis
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

    # Add the net(s) to the ggplot2 instance. They will be white in color
    g = g +
      ggplot2::geom_polygon(data = net, ggplot2::aes(x, y), fill = '#ffffff')

    # Return the ggplot2 instance
    return(g)
  }

  else{
    # If the league isn't valid (i.e. either NBA, WNBA, NCAA, or FIBA), return the ggplot2 instance
    return(g)
  }
}

#' Generate a ggplot2 instance containing a regulation basketball court for a specified league
#'
#' @param league The league for which to draw the surface
#' @param include_amateur A boolean indicating whether or not to include amateur free-throw lane markings (some professional (W)NBA courts include amateur free-throw lane markings)
#' @param include_m_line A boolean indicating whether or not to plot the men's three-point line (NCAA only)
#' @param include_w_line A boolean indicating whether or not to plot the women's three-point line (NCAA only)
#' @param full_surf A boolean indicating whether or not to plot a full surface represenation of the surface. Default: TRUE
#' @param rotate A boolean indicating whether or not this feature needs to be rotated. Default: FALSE
#' @param rotation_dir A string indicating which direction to rotate the feature. Default: 'ccw'
#'
#' @return A ggplot2 instance with the boards added to it
#'
#' @export
#'
#' @examples
#' geom_basketball(league = "NBA")
#' geom_basketball(league = "NCAA", full_surf = FALSE)
#' geom_basketball(league = "FIBA", rotate = TRUE, rotation_dir = "ccw")
geom_basketball = function(league, include_amateur = TRUE, include_m_line = TRUE, include_w_line = TRUE, full_surf = TRUE, rotate = FALSE, rotation_dir = 'ccw'){
  # Force the league to be all upper case
  league = toupper(league)

  if(league %in% c('NBA', 'WNBA', 'NCAA', 'NCAAM', 'NCAAW', 'COLLEGE', 'FIBA')){
    # Create the initial ggplot2 instance onto which the features will be added
    g = ggplot2::ggplot() +
      ggplot2::coord_fixed() +
      ggplot2::theme(
        plot.margin = ggplot2::margin(0, 0, 0, 0),
        plot.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )

    # Add the court
    g = court(g, league, full_surf, rotate, rotation_dir)

    # Add the center circle
    g = center_circle(g, league, full_surf, rotate, rotation_dir)

    # Add the division line
    g = division_line(g, league, full_surf, rotate, rotation_dir)

    # Add the endline(s)
    g = endline(g, league, full_surf, rotate, rotation_dir)

    # Add the sidelines
    g = sideline(g, league, full_surf, rotate, rotation_dir)

    # Add the team bench(es)
    g = team_bench(g, league, full_surf, rotate, rotation_dir)

    # Add the substitution area(s)
    g = substitution_area(g, league, full_surf, rotate, rotation_dir)

    # Add the three-point line
    g = three_point_line(g, league, include_m_line, include_w_line, full_surf, rotate, rotation_dir)

    # Add the free-throw lane(s)
    g = free_throw_lane(g, league, include_amateur, full_surf, rotate, rotation_dir)

    # Add the free-throw lane lines
    g = free_throw_lane_lines(g, league, include_amateur, full_surf, rotate, rotation_dir)

    # Add the free-throw semi-circle(s)
    g = free_throw_semi_circle(g, league, full_surf, rotate, rotation_dir)

    # Add the dashed free-throw semi-circle(s) (NBA only)
    g = free_throw_dashed_semi_circle(g, league, full_surf, rotate, rotation_dir)

    # Add the restricted area arc(s)
    g = restricted_area_arc(g, league, full_surf, rotate, rotation_dir)

    # Add the lower defensive box(es)
    g = lower_defensive_box(g, league, full_surf, rotate, rotation_dir)

    # Add the backboard(s)
    g = backboard(g, league, full_surf, rotate, rotation_dir)

    # Add the basket ring(s)
    g = basket_ring(g, league, full_surf, rotate, rotation_dir)

    # Add the net(s)
    g = net(g, league, full_surf, rotate, rotation_dir)

    # Return the ggplot2 instance that contains the court plot
    return(g)
  }

  else {
    message(paste('Sorry,', league, 'does not exist yet.'))
    return(0)
  }
}

# geom_basketball('NBA')
# geom_basketball('WNBA')
# geom_basketball('NCAA')
# geom_basketball('FIBA')
