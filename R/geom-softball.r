#' Softball Feature Colors
#'
#' Set the default colors for a softball field visualization.
#'
#' @param plot_background Background color for the plot area.
#' @param outfield_grass Color of the outfield grass area.
#' @param infield_dirt Color of the infield dirt area.
#' @param infield_grass Color of the inner infield grass.
#' @param pitching_circle Color of the pitching circle outline.
#' @param bases Color of the bases and home plate.
#' @param foul_lines Color of the foul lines.
#' @param fence Color of the outfield fence.
#'
#' @return A named list of feature colors.
#' @export
softball_features_set_colors <- function(
    plot_background = "#395d33",
    outfield_grass  = "#395d33",
    infield_dirt    = "#9b7653",
    infield_grass   = "#395d33",
    pitching_circle = "#ffffff",
    bases           = "#ffffff",
    foul_lines      = "#ffffff",
    fence           = "#000000"
) {
  list(
    plot_background = plot_background,
    outfield_grass  = outfield_grass,
    infield_dirt    = infield_dirt,
    infield_grass   = infield_grass,
    pitching_circle = pitching_circle,
    bases           = bases,
    foul_lines      = foul_lines,
    fence           = fence
  )
}

#' Default Dimensions for Softball Surface
#'
#' Provides regulation dimensions (in feet) for major softball leagues.
#'
#' @param league Case-insensitive league string (e.g., "NCAA", "USA", "WBSC", "AUSL").
#' @return List of dimension specifications.
#' @keywords internal
get_softball_dimensions <- function(league) {
  if (missing(league) || is.null(league) || nchar(trimws(league)) == 0) {
    stop("Parameter 'league' must be specified.")
  }
  
  league_upper <- toupper(league)
  
  # Valid supported leagues
  valid_leagues <- c("NCAA", "USA", "WBSC", "AUSL", "NFHS", "CUSTOM")
  
  if (!league_upper %in% valid_leagues) {
    stop(sprintf("League '%s' is not supported for softball.", league))
  }
  
  # Standard Fastpitch specifications (in feet)
  list(
    field_units            = "ft",
    base_distance          = 60,
    pitching_distance      = 43,
    pitchers_circle_radius = 8,
    pitchers_plate_length  = 2,
    pitchers_plate_width   = 0.5,
    home_plate_to_backstop = 25,
    outfield_fence_radius  = 220,
    skinned_infield_radius = 60,
    batters_box_length     = 7,
    batters_box_width      = 3,
    batters_box_offset     = 0.5,
    catchers_box_length    = 10,
    catchers_box_width     = 8.42,
    base_side_length       = 1.25
  )
}

#' Helper to rotate and translate coordinates
#'
#' @keywords internal
transform_coords <- function(df, rotation = 0, x_trans = 0, y_trans = 0) {
  if (is.null(df) || nrow(df) == 0) return(df)
  
  rad <- rotation * (pi / 180)
  x_rot <- df$x * cos(rad) - df$y * sin(rad)
  y_rot <- df$x * sin(rad) + df$y * cos(rad)
  
  df$x <- x_rot + x_trans
  df$y <- y_rot + y_trans
  return(df)
}

#' Draw Softball Field
#'
#' Generates a ggplot2 instance containing a softball field for a specified league.
#'
#' @param league The league for which to draw the surface (e.g., "NCAA", "USA", "WBSC", "AUSL").
#' @param display_range String indicating display region: "full" or "infield".
#' @param field_updates List of parameter updates to overwrite standard dimensions.
#' @param color_updates List of color updates to overwrite default feature colors.
#' @param rotation Rotation angle in degrees (counter-clockwise).
#' @param x_trans Shift amount in the x direction.
#' @param y_trans Shift amount in the y direction.
#' @param field_units Standard unit string to force plot output (e.g., "ft", "m").
#' @param xlims Custom x-axis limits.
#' @param ylims Custom y-axis limits.
#'
#' @return A ggplot2 plot representing the softball surface.
#' @export
geom_softball <- function(
    league,
    display_range   = "full",
    field_updates   = list(),
    color_updates   = list(),
    rotation        = 0,
    x_trans         = 0,
    y_trans         = 0,
    field_units     = NULL,
    xlims           = NULL,
    ylims           = NULL
) {
  if (missing(league) || is.null(league) || nchar(trimws(league)) == 0) {
    stop("Parameter 'league' must be specified.")
  }
  
  # 1. Load Defaults & Updates
  dims <- get_softball_dimensions(league)
  if (length(field_updates) > 0) {
    dims <- utils::modifyList(dims, field_updates)
  }
  
  colors <- softball_features_set_colors()
  if (length(color_updates) > 0) {
    colors <- utils::modifyList(colors, color_updates)
  }
  
  # 2. Build Feature Geometries
  theta_outfield <- seq(-pi/4, pi/4, length.out = 100)
  outfield_arc <- data.frame(
    x = dims$outfield_fence_radius * sin(theta_outfield),
    y = dims$outfield_fence_radius * cos(theta_outfield)
  )
  outfield_poly <- rbind(data.frame(x = 0, y = 0), outfield_arc, data.frame(x = 0, y = 0))
  
  theta_infield <- seq(-pi/2, pi/2, length.out = 100)
  infield_dirt_arc <- data.frame(
    x = dims$skinned_infield_radius * sin(theta_infield),
    y = dims$pitching_distance + dims$skinned_infield_radius * cos(theta_infield)
  )
  infield_dirt_poly <- rbind(data.frame(x = 0, y = 0), infield_dirt_arc, data.frame(x = 0, y = 0))
  
  theta_circle <- seq(0, 2 * pi, length.out = 100)
  pitchers_circle <- data.frame(
    x = dims$pitchers_circle_radius * cos(theta_circle),
    y = dims$pitching_distance + dims$pitchers_circle_radius * sin(theta_circle)
  )
  
  pitchers_plate <- data.frame(
    x = c(-dims$pitchers_plate_length / 2, dims$pitchers_plate_length / 2, 
          dims$pitchers_plate_length / 2, -dims$pitchers_plate_length / 2),
    y = dims$pitching_distance + c(0, 0, dims$pitchers_plate_width, dims$pitchers_plate_width)
  )
  
  home_plate <- data.frame(
    x = c(0, 0.708, 0.708, -0.708, -0.708),
    y = c(0, 0.708, 1.416, 1.416, 0.708)
  )
  
  foul_line_left <- data.frame(
    x = c(0, -dims$outfield_fence_radius * sin(pi/4)),
    y = c(0, dims$outfield_fence_radius * cos(pi/4))
  )
  foul_line_right <- data.frame(
    x = c(0, dims$outfield_fence_radius * sin(pi/4)),
    y = c(0, dims$outfield_fence_radius * cos(pi/4))
  )
  
  box_w <- dims$batters_box_width
  box_l <- dims$batters_box_length
  offset <- dims$batters_box_offset
  
  batters_box_left <- data.frame(
    x = c(-offset, -offset - box_w, -offset - box_w, -offset),
    y = c(box_l / 2, box_l / 2, -box_l / 2, -box_l / 2)
  )
  
  batters_box_right <- data.frame(
    x = c(offset, offset + box_w, offset + box_w, offset),
    y = c(box_l / 2, box_l / 2, -box_l / 2, -box_l / 2)
  )
  
  # 3. Apply Transformations
  features <- list(
    outfield_poly     = outfield_poly,
    infield_dirt_poly = infield_dirt_poly,
    pitchers_circle   = pitchers_circle,
    pitchers_plate    = pitchers_plate,
    home_plate        = home_plate,
    foul_line_left    = foul_line_left,
    foul_line_right   = foul_line_right,
    batters_box_left  = batters_box_left,
    batters_box_right = batters_box_right
  )
  
  tf <- lapply(features, transform_coords, rotation = rotation, x_trans = x_trans, y_trans = y_trans)
  
  # 4. Construct Plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = tf$outfield_poly, 
      ggplot2::aes(x = .data$x, y = .data$y), 
      fill = colors$outfield_grass, 
      color = NA
    ) +
    ggplot2::geom_polygon(
      data = tf$infield_dirt_poly, 
      ggplot2::aes(x = .data$x, y = .data$y), 
      fill = colors$infield_dirt, 
      color = NA
    ) +
    ggplot2::geom_path(
      data = tf$foul_line_left, 
      ggplot2::aes(x = .data$x, y = .data$y), 
      color = colors$foul_lines, 
      linewidth = 1
    ) +
    ggplot2::geom_path(
      data = tf$foul_line_right, 
      ggplot2::aes(x = .data$x, y = .data$y), 
      color = colors$foul_lines, 
      linewidth = 1
    ) +
    ggplot2::geom_path(
      data = tf$pitchers_circle, 
      ggplot2::aes(x = .data$x, y = .data$y), 
      color = colors$pitching_circle, 
      linewidth = 0.8
    ) +
    ggplot2::geom_polygon(
      data = tf$pitchers_plate, 
      ggplot2::aes(x = .data$x, y = .data$y), 
      fill = colors$bases, 
      color = NA
    ) +
    ggplot2::geom_polygon(
      data = tf$home_plate, 
      ggplot2::aes(x = .data$x, y = .data$y), 
      fill = colors$bases, 
      color = NA
    ) +
    ggplot2::geom_polygon(
      data = tf$batters_box_left, 
      ggplot2::aes(x = .data$x, y = .data$y), 
      fill = NA, 
      color = colors$foul_lines, 
      linewidth = 0.8
    ) +
    ggplot2::geom_polygon(
      data = tf$batters_box_right, 
      ggplot2::aes(x = .data$x, y = .data$y), 
      fill = NA, 
      color = colors$foul_lines, 
      linewidth = 0.8
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = colors$plot_background, color = NA),
      plot.background  = ggplot2::element_rect(fill = colors$plot_background, color = NA)
    )
  
  if (!is.null(xlims) && !is.null(ylims)) {
    p <- p + ggplot2::coord_fixed(xlim = xlims, ylim = ylims)
  } else if (tolower(display_range) == "infield") {
    r <- dims$skinned_infield_radius + 15
    p <- p + ggplot2::coord_fixed(xlim = c(-r, r), ylim = c(-10, dims$pitching_distance + r))
  }
  
  return(p)
}