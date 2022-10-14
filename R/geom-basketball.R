#' Set the colors to be used for the plot. The values provided in the arguments
#' are the defaults, and, where specified, are the rule-book specified values.
#'
#' Hexadecimal values are the passed vales to this function by default, but it
#' is also possible to use string-named values (e.g. \code{"dodgerblue"}) when
#' specifying.
#'
#' @param plot_background A hexadecimal string representing the color to use for
#'   this feature
#' @param defensive_half_court A hexadecimal string representing the color to
#'   use for this feature
#' @param offensive_half_court A hexadecimal string representing the color to
#'   use for this feature
#' @param court_apron A hexadecimal string representing the color to use for
#'   this feature
#' @param center_circle_outline A hexadecimal string representing the color to
#'   use for this feature
#' @param center_circle_fill A hexadecimal string representing the color to use
#'   for this feature
#' @param division_line A hexadecimal string representing the color to use for
#'   this feature
#' @param endline A hexadecimal string representing the color to use for this
#'   feature
#' @param sideline A hexadecimal string representing the color to use for this
#'   feature
#' @param two_point_range A hexadecimal string representing the color to use for
#'   this feature
#' @param three_point_line A hexadecimal string representing the color to use
#'   for this feature
#' @param painted_area A hexadecimal string representing the color to use for
#'   this feature
#' @param lane_boundary A hexadecimal string representing the color to use for
#'   this feature
#' @param free_throw_circle_outline A hexadecimal string representing the color
#'   to use for this feature
#' @param free_throw_circle_fill A hexadecimal string representing the color to
#'   use for this feature
#' @param free_throw_circle_dash A hexadecimal string representing the color to
#'   use for this feature
#' @param lane_space_mark A hexadecimal string representing the color to use for
#'   this feature
#' @param inbounding_line A hexadecimal string representing the color to use for
#'   this feature
#' @param substitution_line A hexadecimal string representing the color to use
#'   for this feature
#' @param baseline_lower_defensive_box A hexadecimal string representing the
#'   color to use for this feature
#' @param lane_lower_defensive_box A hexadecimal string representing the color
#'   to use for this feature
#' @param team_bench_line A hexadecimal string representing the color to use for
#'   this feature
#' @param restricted_arc A hexadecimal string representing the color to use for
#'   this feature
#' @param backboard A hexadecimal string representing the color to use for this
#'   feature
#' @param basket_ring A hexadecimal string representing the color to use for
#'   this feature
#' @param net A hexadecimal string representing the color to use for this
#'   feature
#'
#' @return A list of hexadecimal colors to use to color the features on the
#'   resulting plot
#'
#' @keywords internal
basketball_features_set_colors <- function(plot_background = NULL,
                                           defensive_half_court = "#d2ab6f",
                                           offensive_half_court = "#d2ab6f",
                                           court_apron = "#d2ab6f",
                                           center_circle_outline = "#000000",
                                           center_circle_fill = "#d2ab6f",
                                           division_line = "#000000",
                                           endline = "#000000",
                                           sideline = "#000000",
                                           two_point_range = "#d2ab6f",
                                           three_point_line = "#000000",
                                           painted_area = "#d2ab6f",
                                           lane_boundary = "#000000",
                                           free_throw_circle_outline =
                                             "#000000",
                                           free_throw_circle_fill = "#d2ab6f",
                                           free_throw_circle_dash = "#000000",
                                           lane_space_mark = "#000000",
                                           inbounding_line = "#000000",
                                           substitution_line = "#000000",
                                           baseline_lower_defensive_box =
                                             "#000000",
                                           lane_lower_defensive_box = "#000000",
                                           team_bench_line = "#000000",
                                           restricted_arc = "#000000",
                                           backboard = "#000000",
                                           basket_ring = "#f55b33",
                                           net = "#ffffff") {
  feature_colors <- list(
    plot_background = plot_background,
    defensive_half_court = defensive_half_court,
    offensive_half_court = offensive_half_court,
    court_apron = court_apron,
    center_circle_outline = center_circle_outline,
    center_circle_fill = center_circle_fill,
    division_line = division_line,
    endline = endline,
    sideline = sideline,
    two_point_range = two_point_range,
    three_point_line = three_point_line,
    painted_area = painted_area,
    lane_boundary = lane_boundary,
    free_throw_circle_outline = free_throw_circle_outline,
    free_throw_circle_fill = free_throw_circle_fill,
    free_throw_circle_dash = free_throw_circle_dash,
    lane_space_mark = lane_space_mark,
    inbounding_line = inbounding_line,
    substitution_line = substitution_line,
    baseline_lower_defensive_box = baseline_lower_defensive_box,
    lane_lower_defensive_box = lane_lower_defensive_box,
    team_bench_line = team_bench_line,
    restricted_arc = restricted_arc,
    backboard = backboard,
    basket_ring = basket_ring,
    net = net
  )

  return(feature_colors)
}

#' Generate a \code{ggplot2} instance containing a basketball court for a
#' specified league
#'
#' @param league The league for which to draw the surface. This is
#'   case-insensitive
#' @param display_range A case-insensitive string indicating the display range
#'   to use for the plot. The default is \code{"full"}, which will be returned
#'   when either an invalid or no value is passed to the function.
#'
#'   The possible display ranges are:
#'   \describe{
#'     \item{\code{"full"}}{
#'       The full court. This is the default
#'     }
#'     \item{\code{"in_bounds_only"}}{The full in-bounds area of the court}
#'     \item{\code{"in bounds only"}}{The full in-bounds area of the court}
#'     \item{\code{"offense"}}{
#'       The TV-right half of the court half-court. This is considered the
#'       offensive half of the court
#'     }
#'     \item{\code{"offence"}}{
#'       The TV-right half of the court half-court. This is considered the
#'       offensive half of the court
#'     }
#'     \item{\code{"offensivehalfcourt"}}{
#'       The TV-right half of the court half-court. This is considered the
#'       offensive half of the court
#'     }
#'     \item{\code{"offensive_half_court"}}{
#'       The TV-right half of the court half-court. This is considered the
#'       offensive half of the court
#'     }
#'     \item{\code{"offensive half court"}}{
#'       The TV-right half of the court half-court. This is considered the
#'       offensive half of the court
#'     }
#'     \item{\code{"defense"}}{
#'       The TV-left half of the court half-court. This is considered the
#'       defensive half of the court
#'     }
#'     \item{\code{"defence"}}{
#'       The TV-left half of the court half-court. This is considered the
#'       defensive half of the court
#'     }
#'     \item{\code{"defensivehalfcourt"}}{
#'       The TV-left half of the court half-court. This is considered the
#'       defensive half of the court
#'     }
#'     \item{\code{"defensive_half_court"}}{
#'       The TV-left half of the court half-court. This is considered the
#'       defensive half of the court
#'     }
#'     \item{\code{"defensive half court"}}{
#'       The TV-left half of the court half-court. This is considered the
#'       defensive half of the court
#'     }
#'     \item{\code{"offensivekey"}}{
#'       The TV-right offensive key (three-point line and two-point range)
#'     }
#'     \item{\code{"offensive_key"}}{
#'       The TV-right offensive key (three-point line and two-point range)
#'     }
#'     \item{\code{"offensive key"}}{
#'       The TV-right offensive key (three-point line and two-point range)
#'     }
#'     \item{\code{"attackingkey"}}{
#'       The TV-right offensive key (three-point line and two-point range)
#'     }
#'     \item{\code{"attacking_key"}}{
#'       The TV-right offensive key (three-point line and two-point range)
#'     }
#'     \item{\code{"attacking key"}}{
#'       The TV-right offensive key (three-point line and two-point range)
#'     }
#'     \item{\code{"defensivekey"}}{
#'       The TV-left defensive key (three-point line and two-point range)
#'     }
#'     \item{\code{"defensive_key"}}{
#'       The TV-left defensive key (three-point line and two-point range)
#'     }
#'     \item{\code{"defensive key"}}{
#'       The TV-left defensive key (three-point line and two-point range)
#'     }
#'     \item{\code{"defendingkey"}}{
#'       The TV-left defensive key (three-point line and two-point range)
#'     }
#'     \item{\code{"defending_key"}}{
#'       The TV-left defensive key (three-point line and two-point range)
#'     }
#'     \item{\code{"defending key"}}{
#'       The TV-left defensive key (three-point line and two-point range)
#'     }
#'     \item{\code{"offensivepaint"}}{
#'       The TV-right offensive free-throw lane
#'     }
#'     \item{\code{"offensive_paint"}}{
#'       The TV-right offensive free-throw lane
#'     }
#'     \item{\code{"offensive paint"}}{
#'       The TV-right offensive free-throw lane
#'     }
#'     \item{\code{"attackingpaint"}}{
#'       The TV-right offensive free-throw lane
#'     }
#'     \item{\code{"attacking_paint"}}{
#'       The TV-right offensive free-throw lane
#'     }
#'     \item{\code{"attacking paint"}}{
#'       The TV-right offensive free-throw lane
#'     }
#'     \item{\code{"offensivelane"}}{
#'       The TV-right offensive free-throw lane
#'     }
#'     \item{\code{"offensive_lane"}}{
#'       The TV-right offensive free-throw lane
#'     }
#'     \item{\code{"offensive lane"}}{
#'       The TV-right offensive free-throw lane
#'     }
#'     \item{\code{"attackinglane"}}{
#'       The TV-right offensive free-throw lane
#'     }
#'     \item{\code{"attacking_lane"}}{
#'       The TV-right offensive free-throw lane
#'     }
#'     \item{\code{"attacking lane"}}{
#'       The TV-right offensive free-throw lane
#'     }
#'     \item{\code{"defensivepaint"}}{
#'       The TV-left defensive free-throw lane
#'     }
#'     \item{\code{"defensive_paint"}}{
#'       The TV-left defensive free-throw lane
#'     }
#'     \item{\code{"defensive paint"}}{
#'       The TV-left defensive free-throw lane
#'     }
#'     \item{\code{"defendingpaint"}}{
#'       The TV-left defensive free-throw lane
#'     }
#'     \item{\code{"defending_paint"}}{
#'       The TV-left defensive free-throw lane
#'     }
#'     \item{\code{"defending paint"}}{
#'       The TV-left defensive free-throw lane
#'     }
#'     \item{\code{"defensivelane"}}{
#'       The TV-left defensive free-throw lane
#'     }
#'     \item{\code{"defensive_lane"}}{
#'       The TV-left defensive free-throw lane
#'     }
#'     \item{\code{"defensive lane"}}{
#'       The TV-left defensive free-throw lane
#'     }
#'     \item{\code{"defendinglane"}}{
#'       The TV-left defensive free-throw lane
#'     }
#'     \item{\code{"defending_lane"}}{
#'       The TV-left defensive free-throw lane
#'     }
#'     \item{\code{"defending lane"}}{
#'       The TV-left defensive free-throw lane
#'     }
#'   }
#' @param court_updates A list of updates to the courts' parameters. These will
#'   overwrite the parameters of the league
#' @param color_updates A list of updates to the courts' default colors, which
#'   are set by [basketball_features_set_colors()]
#' @param rotation An angle, given in degrees, through which the plot should be
#'   rotated
#' @param x_trans The amount that the \code{x} coordinates are to be shifted. By
#'   convention, the +\code{x} axis extends from the center of the court towards
#'   the right-hand basket when viewing the court in TV View
#' @param y_trans The amount that the \code{y} coordinates are to be shifted. By
#'   convention, the +\code{y} axis extends from the center of the court towards
#'   the top of the court when viewing the court in TV view
#' @param court_units The units with which to draw the court. The default is
#'   \code{NULL}, which will apply the rule-book specified units
#' @param xlims The limits on the final display in the \code{x} direction. The
#'   default is \code{NULL}, which will utilize the \code{xlims} specified by
#'   the \code{display_range} parameter
#' @param ylims The limits on the final display in the \code{y} direction. The
#'   default is \code{NULL}, which will utilize the \code{ylims} specified by
#'   the \code{display_range} parameter
#'
#' @return A \code{ggplot2} instance with a full-surface representation of a
#'   basketball court
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   geom_basketball(league = "NBA", rotation = 270, display_range = "offense")
#'   geom_basketball(league = "fiba", court_units = "ft")
#' }
geom_basketball <- function(league,
                            display_range = "full",
                            court_updates = list(),
                            color_updates = list(),
                            rotation = 0,
                            x_trans = 0,
                            y_trans = 0,
                            court_units = NULL,
                            xlims = NULL,
                            ylims = NULL) {
  # Input cleansing and data gathering -----------------------------------------

  # If no league is supplied, error and alert user
  if (missing(league)) {
    stop(
      glue::glue(
        "league parameter must be supplied. \"custom\" is a valid league if ",
        "you wish to specify your own court parameterization, but you must ",
        "use the court_updates parameter to do so"
      )
    )
  }

  # Force the league to be all lower case
  league <- tolower(league)

  # Get the dimensions for the specified league
  court_params <- surface_dimensions[["basketball"]][[league]]

  # Update the court parameters as necessary
  court_params <- utils::modifyList(court_params, court_updates)

  # If the lane lower defensive box marks are not needed, they should not be
  # plotted. This sets their visibility, defaulting to FALSE
  lane_lower_defensive_box_marks_visibility <-
    court_params$lane_lower_defensive_box_marks_visibility %or% FALSE

  # Start by getting the colors to use to make the plot
  feature_colors <- basketball_features_set_colors()

  # Update the features' colors as specified by the user
  feature_colors <- utils::modifyList(feature_colors, color_updates)

  # Organize the features that may have multiple instances
  center_circles <- data.frame(
    radius = court_params$center_circle_radius %or% 0,
    outline_color = feature_colors$center_circle_outline,
    fill_color = feature_colors$center_circle_fill
  )

  center_circles <- center_circles[order(-center_circles$radius), ]

  three_point_arcs <- data.frame(
    radius = court_params$basket_center_to_three_point_arc %or% 0,
    corner = court_params$basket_center_to_corner_three %or% 0,
    line_color = feature_colors$three_point_line,
    two_point_range_color = feature_colors$two_point_range
  )

  three_point_arcs <- three_point_arcs[order(-three_point_arcs$radius), ]

  lanes <- data.frame(
    length = court_params$lane_length %or% 0,
    width = court_params$lane_width %or% 0,
    paint_margin = court_params$paint_margin %or% 0,
    boundary_color = feature_colors$lane_boundary,
    painted_area = feature_colors$painted_area,
    boundary_visibility =
      court_params$lane_boundary_visibility %or% rep(
        FALSE,
        length(court_params$lane_length %or% 0)
      ),
    painted_area_visibility =
      court_params$painted_area_visibility %or% rep(
        FALSE,
        length(court_params$lane_length %or% 0)
      )
  )

  lanes <- lanes[order(-lanes$length, -lanes$width), ]

  lane_space_marks <- data.frame(
    y_anchor = c(),
    length = c(),
    depth = c(),
    separation = c(),
    color = c(),
    lane_space_mark_set = c(),
    lane_space_mark_visibility = c()
  )

  for(i in seq_len(nrow(court_params$lane_space_mark_lengths))) {
    lane_space_marks <- rbind(
      lane_space_marks,
      data.frame(
        y_anchor = rep(
          court_params$lane_width[i] / 2,
          length(court_params$lane_space_mark_lengths[i, ])
        ),
        length = court_params$lane_space_mark_lengths[i, ],
        width = rep(
          court_params$lane_space_mark_widths[i],
          length(court_params$lane_space_mark_lengths[i, ])
        ),
        separation = court_params$lane_space_mark_separations[i, ],
        color = ifelse(
          nrow(court_params$lane_space_mark_lengths) > 1,
          rep(
            ifelse(
              length(feature_colors$lane_space_mark) >= i,
              feature_colors$lane_space_mark[i],
              feature_colors$lane_space_mark[1]
            ),
            length(court_params$lane_space_mark_lengths[i, ])
          ),
          rep(
            feature_colors$lane_space_mark,
            length(court_params$lane_space_mark_lengths[i, ])
          )
        ),
        lane_space_mark_visibility = court_params$lane_space_mark_visibility[i],
        lane_space_mark_set = rep(
          glue::glue("lane_space_mark_set_{i}"),
          length(court_params$lane_space_mark_lengths[i, ])
        )
      )
    )
  }

  # The overhanging portion of the free throw circle rely on the arc length s
  # and the radius r. These are defined here since they aren't court features,
  # but will be added to the court_features list
  overhang_s <- court_params$free_throw_circle_overhang %or% 0
  overhang_r <- court_params$free_throw_circle_radius %or% 0
  if (overhang_r != 0) {
    overhang_end_theta <- 0.5 - ((overhang_s / overhang_r) / pi)
  } else {
    overhang_end_theta <- 0.5
  }

  # The inbounding lines may extend solely into the court, solely out of the
  # court, both into and out of the court, and these may be mixed within a court
  # (e.g. one side of the court may be fully out, and the other may be in and
  # out, like a college basketball court)
  inbounding_line_to_baseline <- court_params$inbounding_line_to_baseline %or% 0
  inbounding_line_in_play_ext <- court_params$inbounding_line_in_play_ext %or% 0
  inbounding_line_out_of_bounds_ext <-
    court_params$inbounding_line_out_of_bounds_ext %or% 0
  symmetric_inbounding_line <- court_params$symmetric_inbounding_line %or% FALSE
  inbounding_line_anchor_side <- court_params$inbounding_line_anchor_side %or% 1

  inbounding_lines <- data.frame(
    inbounding_line_to_baseline = inbounding_line_to_baseline,
    in_play_ext = inbounding_line_in_play_ext,
    out_of_bounds_ext = inbounding_line_out_of_bounds_ext,
    symmetric_inbounding_line = symmetric_inbounding_line,
    anchor_side = inbounding_line_anchor_side
  )

  if (tolower(court_params$bench_side %or% "top") != "top") {
    inbounding_lines[, "anchor_side"] <- -1 * inbounding_lines[, "anchor_side"]
  }

  inbounding_lines["drawn_direction"] <- "top_down"
  inbounding_lines[inbounding_lines$anchor_side < 1, "drawn_direction"] <-
    "bottom_up"

  # The substitution lines extend fully out of the court and must be on the side
  # of the court that has the team bench areas
  if (tolower(court_params$bench_side %or% "top") == "top") {
    bench_side <- 1
    substitution_line_drawn_direction <- "bottom_up"
    team_bench_line_drawn_direction <- "bottom_up"
  } else {
    bench_side <- -1
    substitution_line_drawn_direction <- "top_down"
    team_bench_line_drawn_direction <- "top_down"
  }

  # Feature initialization -----------------------------------------------------
  court_features <- list(

    ## Surface Base Features ---------------------------------------------------

    #### Defensive Half Court ####
    defensive_half_court = basketball_half_court(
      court_length = court_params$court_length %or% 0,
      court_width = court_params$court_width %or% 0
    ),

    #### Offensive Half Court ####
    offensive_half_court = basketball_half_court(
      court_length = court_params$court_length %or% 0,
      court_width = court_params$court_width %or% 0
    ),

    #### Free Throw Circle Fill ####
    free_throw_circle_fill = basketball_free_throw_circle_fill(
      free_throw_circle_radius = court_params$free_throw_circle_radius %or% 0,
      line_thickness = court_params$line_thickness %or% 0
    ),

    ## Surface Boundaries ------------------------------------------------------

    #### Court Apron ####
    court_apron = basketball_court_apron(
      court_length = court_params$court_length %or% 0,
      court_width = court_params$court_width %or% 0,
      court_apron_endline = court_params$court_apron_endline %or% 0,
      court_apron_sideline = court_params$court_apron_sideline %or% 0,
      court_apron_to_boundary = court_params$court_apron_to_boundary %or% 0,
      line_thickness = court_params$line_thickness %or% 0
    ),

    #### Endline ####
    endline = basketball_endline(
      court_width = court_params$court_width %or% 0,
      line_thickness = court_params$line_thickness %or% 0
    ),

    #### Sideline ####
    sideline = basketball_sideline(
      court_length = court_params$court_length %or% 0,
      line_thickness = court_params$line_thickness %or% 0
    ),

    ## Surface Lines -----------------------------------------------------------

    #### Division Line ####
    division_line = basketball_division_line(
      court_width = court_params$court_width %or% 0,
      line_thickness = court_params$line_thickness %or% 0,
      division_line_extension = court_params$division_line_extension %or% 0
    ),

    #### Free Throw Circle Outline ####
    free_throw_circle_outline = basketball_free_throw_circle(
      free_throw_circle_radius = court_params$free_throw_circle_radius %or% 0,
      line_thickness = court_params$line_thickness %or% 0
    ),

    #### Free Throw Circle Overhang ####
    free_throw_circle_overhang = basketball_free_throw_circle_dash(
      feature_radius = court_params$free_throw_circle_radius %or% 0,
      line_thickness = court_params$line_thickness %or% 0,
      start_angle = 0.5,
      end_angle = overhang_end_theta
    ),

    #### Substitution Line ####
    substitution_line = basketball_substitution_line(
      line_thickness = court_params$line_thickness %or% 0,
      substitution_line_width = court_params$substitution_line_width %or% 0,
      drawn_direction = substitution_line_drawn_direction
    ),

    #### Team Bench Line ####
    team_bench_line = basketball_team_bench_line(
      line_thickness = court_params$line_thickness %or% 0,
      extension = court_params$team_bench_line_ext %or% 0,
      drawn_direction = team_bench_line_drawn_direction
    ),

    ## Surface Features --------------------------------------------------------

    #### Lower Defensive Box ####
    baseline_lower_defensive_box = basketball_lower_defensive_box_mark(
      drawn_direction = "left_to_right",
      extension = court_params$lower_defensive_box_mark_extension %or% 0,
      line_thickness = court_params$line_thickness %or% 0
    ),

    lane_lower_defensive_box = basketball_lower_defensive_box_mark(
      drawn_direction = "top_down",
      extension = court_params$lower_defensive_box_mark_extension %or% 0,
      line_thickness = court_params$line_thickness %or% 0
    ),

    #### Restricted Arc ####
    restricted_arc = basketball_restricted_arc(
      feature_radius = court_params$restricted_arc_radius %or% 0,
      line_thickness = court_params$line_thickness %or% 0,
      backboard_to_center_of_basket = (
        (court_params$basket_center_to_baseline %or% 0) -
          (court_params$backboard_face_to_baseline %or% 0)
      )
    ),

    #### Backboard ####
    backboard = basketball_backboard(
      backboard_width = court_params$backboard_width %or% 0,
      backboard_thickness = court_params$backboard_thickness %or% 0
    ),

    #### Basket Ring ####
    basket_ring = basketball_basket_ring(
      basket_ring_connector_width =
        court_params$basket_ring_connector_width %or% 0,
      backboard_face_to_ring_cent =
        (court_params$basket_center_to_baseline %or% 0) -
        (court_params$backboard_face_to_baseline %or% 0),
      basket_ring_inner_radius = court_params$basket_ring_inner_radius %or% 0,
      basket_ring_thickness = court_params$basket_ring_thickness %or% 0
    ),

    #### Net ####
    net = basketball_net(
      basket_ring_inner_radius = court_params$basket_ring_inner_radius %or% 0
    )
  )

  #### Three Point Line(s) and Two Point Range(s) ####
  for(i in seq_len(nrow(three_point_arcs))) {
    court_features[[glue::glue("two_point_range_{i}")]] <-
      basketball_two_point_range(
        basket_center_to_baseline =
          court_params$basket_center_to_baseline %or% 0,
        basket_center_to_corner_three =
          court_params$basket_center_to_corner_three[i] %or% 0,
        line_thickness = court_params$line_thickness %or% 0,
        two_point_range_radius =
          court_params$basket_center_to_three_point_arc[i] %or% 0
      )

    court_features[[glue::glue("three_point_arc_{i}")]] <-
      basketball_three_point_line(
        basket_center_to_baseline =
          court_params$basket_center_to_baseline %or% 0,
        basket_center_to_corner_three =
          court_params$basket_center_to_corner_three[i] %or% 0,
        line_thickness = court_params$line_thickness %or% 0,
        three_point_line_radius =
          court_params$basket_center_to_three_point_arc[i] %or% 0
      )
  }

  #### Free Throw Lane(s) and Painted Area(s) ####
  for(i in seq_len(nrow(lanes))) {
    court_features[[glue::glue("lane_boundary_{i}")]] <-
      basketball_free_throw_lane_boundary(
        lane_length = lanes[i, "length"] %or% 0,
        lane_width = lanes[i, "width"] %or% 0,
        line_thickness = court_params$line_thickness %or% 0
      )

    court_features[[glue::glue("painted_area_{i}")]] <-
      basketball_painted_area(
        lane_length = lanes[i, "length"] %or% 0,
        lane_width = lanes[i, "width"] %or% 0,
        paint_margin = lanes[i, "paint_margin"] %or% 0,
        line_thickness = court_params$line_thickness %or% 0
      )
  }

  #### Center Circle(s) ####
  for(i in seq_len(nrow(center_circles))) {
    court_features[[glue::glue("center_circle_fill_{i}")]] <-
      basketball_center_circle_fill(
        center_circle_radius = center_circles[i, "radius"] %or% 0,
        line_thickness = court_params$line_thickness[i] %or% 0
      )

    court_features[[glue::glue("center_circle_outline_{i}")]] <-
      basketball_center_circle_outline(
        center_circle_radius = center_circles[i, "radius"] %or% 0,
        line_thickness = court_params$line_thickness %or% 0
      )
  }

  #### Lane Space Marks (Blocks) ####
  for(i in seq_len(nrow(lane_space_marks))) {
    court_features[[glue::glue("lane_space_mark_{i}")]] <-
      basketball_lane_space_mark(
        feature_thickness = lane_space_marks[i, "length"],
        mark_depth = lane_space_marks[i, "width"]
      )
  }

  #### Free Throw Circle Dashes ####
  n_dashes <- court_params$n_free_throw_circle_dashes %or% 0
  if (n_dashes > 0) {
    free_throw_circle_s <- court_params$free_throw_dash_length %or% 0
    free_throw_circle_r <- court_params$free_throw_circle_radius %or% 0
    free_throw_dash_spacing_s <- court_params$free_throw_dash_spacing %or% 0
    free_throw_dash_spacing_r <- court_params$free_throw_circle_radius %or% 0

    if (free_throw_circle_r != 0) {
      theta_dashes <- (free_throw_circle_s / free_throw_circle_r) / pi
    } else {
      theta_dashes <- 0
    }

    if (free_throw_dash_spacing_r != 0) {
      theta_spaces <- (free_throw_dash_spacing_s / free_throw_dash_spacing_r)
      theta_spaces <- theta_spaces / pi
    } else {
      theta_spaces <- 0
    }

    start_s <- (court_params$free_throw_circle_overhang %or% 0)

    if (free_throw_circle_r != 0) {
      start_angle <- 0.5 - ((start_s / free_throw_circle_r) / pi) - theta_spaces
    } else {
      start_angle <- 0.5 - theta_spaces
    }

    for(dash_no in 1:n_dashes) {
      court_features[[glue::glue("free_throw_circle_dash_{dash_no}")]] <-
        basketball_free_throw_circle_dash(
          feature_radius = court_params$free_throw_circle_radius %or% 0,
          line_thickness = court_params$line_thickness %or% 0,
          start_angle = start_angle,
          end_angle = start_angle - theta_spaces
        )

      start_angle <- start_angle - theta_dashes - theta_spaces
    }
  }

  #### Inbounding Line ####
  for(inbounding_line_no in seq_len(nrow(inbounding_lines))) {
    court_features[[glue::glue("inbounding_line_{inbounding_line_no}")]] <-
      basketball_inbounding_line(
        line_thickness = court_params$line_thickness %or% 0,
        in_play_ext = inbounding_lines[inbounding_line_no, "in_play_ext"],
        out_of_bounds_ext = inbounding_lines[
          inbounding_line_no, "out_of_bounds_ext"
        ],
        drawn_direction = inbounding_lines[
          inbounding_line_no, "drawn_direction"
        ]
      )
  }

  # Coordinate Transformations -------------------------------------------------

  # Convert the units as needed
  if (
    !is.null(court_units) &&
    tolower(court_params$court_units %or% "ft") != tolower(court_units)
  )  {
    court_features <- lapply(
      court_features,
      convert_units,
      from_unit = court_params$court_units %or% "ft",
      to_unit = court_units,
      conversion_columns = c("x", "y")
    )
  }

  # Generate the Plot ----------------------------------------------------------

  # Create the base of the plot
  court_plot <- create_plot_base(
    plot_background = feature_colors$plot_background
  )

  # Add the features to the plot

  #### Defensive Half Court ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = -0.25 * (court_params$court_length %or% 0),
    y_anchor = 0,
    feature_df = court_features$defensive_half_court,
    feature_color = feature_colors$defensive_half_court,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Offensive Half Court ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = 0.25 * (court_params$court_length %or% 0),
    y_anchor = 0,
    feature_df = court_features$offensive_half_court,
    feature_color = feature_colors$offensive_half_court,
    reflect_x = FALSE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Court Apron ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = court_features$court_apron,
    feature_color = feature_colors$court_apron,
    feature_outline_color = feature_colors$court_apron,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Center Circle ####
  for(i in seq_len(nrow(center_circles))) {
    court_plot <- add_feature(
      court_plot,
      x_anchor = 0,
      y_anchor = 0,
      feature_df = court_features[[glue::glue("center_circle_fill_{i}")]],
      feature_color = center_circles$fill_color[i],
      reflect_x = TRUE,
      reflect_y = FALSE,
      x_trans = x_trans,
      y_trans = y_trans,
      rotation = rotation
    )

    court_plot <- add_feature(
      court_plot,
      x_anchor = 0,
      y_anchor = 0,
      feature_df = court_features[[glue::glue("center_circle_outline_{i}")]],
      feature_color = center_circles$outline_color[i],
      reflect_x = TRUE,
      reflect_y = FALSE,
      x_trans = x_trans,
      y_trans = y_trans,
      rotation = rotation
    )
  }

  #### Three Point Line and Two Point Range ####
  for(i in seq_len(nrow(three_point_arcs))) {
    court_plot <- add_feature(
      court_plot,
      x_anchor = ((court_params$court_length %or% 0) / 2) -
        court_params$basket_center_to_baseline %or% 0,
      y_anchor = 0,
      feature_df = court_features[[glue::glue("two_point_range_{i}")]],
      feature_color = three_point_arcs$two_point_range_color[i],
      reflect_x = TRUE,
      reflect_y = FALSE,
      x_trans = x_trans,
      y_trans = y_trans,
      rotation = rotation
    )

    court_plot <- add_feature(
      court_plot,
      x_anchor = ((court_params$court_length %or% 0) / 2) -
        court_params$basket_center_to_baseline %or% 0,
      y_anchor = 0,
      feature_df = court_features[[glue::glue("three_point_arc_{i}")]],
      feature_color = three_point_arcs$line_color[i],
      reflect_x = TRUE,
      reflect_y = FALSE,
      x_trans = x_trans,
      y_trans = y_trans,
      rotation = rotation
    )
  }

  #### Free Throw Circles ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = ((court_params$court_length %or% 0) / 2) -
      (court_params$backboard_face_to_baseline %or% 0) -
      (court_params$free_throw_line_to_backboard %or% 0),
    y_anchor = 0,
    feature_df = court_features$free_throw_circle_fill,
    feature_color = feature_colors$free_throw_circle_fill,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  court_plot <- add_feature(
    court_plot,
    x_anchor = ((court_params$court_length %or% 0) / 2) -
      (court_params$backboard_face_to_baseline %or% 0) -
      (court_params$free_throw_line_to_backboard %or% 0) +
      ((court_params$line_thickness %or% 0) / 2),
    y_anchor = 0,
    feature_df = court_features$free_throw_circle_outline,
    feature_color = feature_colors$free_throw_circle_outline,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Free Throw Lanes ####
  for(lane_no in seq_len(nrow(lanes))) {
    court_plot <- add_feature(
      court_plot,
      x_anchor = ((court_params$court_length %or% 0) / 2) -
        lanes[lane_no, "length"] +
        (court_params$line_thickness %or% 0),
      y_anchor = 0,
      feature_df = court_features[[glue::glue("painted_area_{lane_no}")]],
      feature_color = ifelse(
        lanes[lane_no, "painted_area_visibility"] == TRUE,
        lanes$painted_area[lane_no],
        "#00000000"
      ),
      reflect_x = TRUE,
      reflect_y = FALSE,
      x_trans = x_trans,
      y_trans = y_trans,
      rotation = rotation
    )

    court_plot <- add_feature(
      court_plot,
      x_anchor = ((court_params$court_length %or% 0) / 2),
      y_anchor = 0,
      feature_df = court_features[[glue::glue("lane_boundary_{lane_no}")]],
      feature_color = ifelse(
        lanes[lane_no, "boundary_visibility"] == TRUE,
        lanes$boundary_color[lane_no],
        "#00000000"
      ),
      reflect_x = TRUE,
      reflect_y = FALSE,
      x_trans = x_trans,
      y_trans = y_trans,
      rotation = rotation
    )
  }

  #### Free Throw Circle Dashes ####
  if (n_dashes > 0) {
    court_plot <- add_feature(
      court_plot,
      x_anchor = ((court_params$court_length %or% 0) / 2) -
        (court_params$backboard_face_to_baseline %or% 0) -
        (court_params$free_throw_line_to_backboard %or% 0),
      y_anchor = 0,
      feature_df = court_features$free_throw_circle_overhang,
      feature_color = feature_colors$free_throw_circle_dash,
      reflect_x = TRUE,
      reflect_y = TRUE,
      x_trans = x_trans,
      y_trans = y_trans,
      rotation = rotation
    )

    for(dash_no in 1:n_dashes) {
      court_plot <- add_feature(
        court_plot,
        x_anchor = ((court_params$court_length %or% 0) / 2) -
          (court_params$backboard_face_to_baseline %or% 0) -
          (court_params$free_throw_line_to_backboard %or% 0) +
          ((court_params$line_thickness %or% 0) / 2),
        y_anchor = 0,
        feature_df = court_features[[
          glue::glue("free_throw_circle_dash_{dash_no}")
        ]],
        feature_color = feature_colors$free_throw_circle_dash,
        reflect_x = TRUE,
        reflect_y = FALSE,
        x_trans = x_trans,
        y_trans = y_trans,
        rotation = rotation
      )
    }
  }

  #### Lane Space Marks (Blocks) ####
  lane_space_mark <- 0
  for(i in seq_len(nrow(lanes))) {
    # Start by defining the starting x_anchor for the set of lane space marks
    x_anchor <- ((court_params$court_length %or% 0) / 2) -
      (court_params$backboard_face_to_baseline %or% 0)

    lane_space_mark_set <- lane_space_marks[
      lane_space_marks$lane_space_mark_set ==
        glue::glue("lane_space_mark_set_{i}")
      ,
    ]

    for(j in seq_len(nrow(lane_space_mark_set))) {
      lane_space_mark <- lane_space_mark + 1
      x_anchor <- x_anchor - lane_space_mark_set[j, "separation"]

      court_plot <- add_feature(
        court_plot,
        x_anchor = x_anchor,
        y_anchor = lane_space_mark_set[j, "y_anchor"],
        feature_df = court_features[[
          glue::glue("lane_space_mark_{lane_space_mark}")
        ]],
        feature_color = ifelse(
          lane_space_mark_set[j, "lane_space_mark_visibility"] == TRUE,
          lane_space_mark_set[i, "color"],
          "#00000000"
        ),
        reflect_x = TRUE,
        reflect_y = TRUE,
        x_trans = x_trans,
        y_trans = y_trans,
        rotation = rotation
      )
    }
  }

  #### Lower Defensive Box Marks ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = (court_params$court_length %or% 0) / 2,
    y_anchor = (
      court_params$baseline_lower_defensive_box_marks_int_sep %or% 0
    ) / 2,
    feature_df = court_features$baseline_lower_defensive_box,
    feature_color = feature_colors$baseline_lower_defensive_box,
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  court_plot <- add_feature(
    court_plot,
    x_anchor = ((court_params$court_length %or% 0) / 2) -
      (court_params$baseline_to_lane_lower_defensive_box_marks %or% 0),
    y_anchor = (
      court_params$lane_lower_defensive_box_marks_int_sep %or% 0
    ) / 2,
    feature_df = court_features$lane_lower_defensive_box,
    feature_color = ifelse(
      lane_lower_defensive_box_marks_visibility == TRUE,
      feature_colors$lane_lower_defensive_box,
      # If invisible, set alpha to 0
      "#00000000"
    ),
    reflect_x = TRUE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Endlines ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = ((court_params$court_length %or% 0) / 2),
    y_anchor = 0,
    feature_df = court_features$endline,
    feature_color = feature_colors$endline,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Sidelines ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = 0,
    y_anchor = ((court_params$court_width %or% 0) / 2),
    feature_df = court_features$sideline,
    feature_color = feature_colors$sideline,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Inbounding Lines ####
  for(inbounding_line_no in seq_len(nrow(inbounding_lines))) {
    court_plot <- add_feature(
      court_plot,
      x_anchor = ((court_params$court_length %or% 0) / 2) -
        inbounding_lines[inbounding_line_no, "inbounding_line_to_baseline"],
      y_anchor = ((court_params$court_width %or% 0) / 2) *
        inbounding_lines[inbounding_line_no, "anchor_side"],
      feature_df =
        court_features[[glue::glue("inbounding_line_{inbounding_line_no}")]],
      feature_color = feature_colors$inbounding_line,
      reflect_x = TRUE,
      reflect_y =
        inbounding_lines[inbounding_line_no, "symmetric_inbounding_line"],
      x_trans = x_trans,
      y_trans = y_trans,
      rotation = rotation
    )
  }

  #### Substitution Lines ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = (court_params$substitution_line_ext_sep %or% 0) / 2,
    y_anchor = bench_side * (court_params$court_width %or% 0) / 2,
    feature_df = court_features$substitution_line,
    feature_color = feature_colors$substitution_line,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Team Bench Lines ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = (court_params$court_length %or% 0) / 2,
    y_anchor = bench_side *
      (
        ((court_params$court_width %or% 0) / 2) +
          (court_params$line_thickness %or% 0)
      ),
    feature_df = court_features$team_bench_line,
    feature_color = feature_colors$team_bench_line,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Division Line ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = 0,
    y_anchor = 0,
    feature_df = court_features$division_line,
    feature_color = feature_colors$division_line,
    reflect_x = FALSE,
    reflect_y = TRUE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Restricted Arc ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = ((court_params$court_length %or% 0) / 2) -
      (court_params$backboard_face_to_baseline %or% 0),
    y_anchor = 0,
    feature_df = court_features$restricted_arc,
    feature_color = feature_colors$restricted_arc,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Basket Ring ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = ((court_params$court_length %or% 0) / 2) -
      (court_params$backboard_face_to_baseline %or% 0),
    y_anchor = 0,
    feature_df = court_features$basket_ring,
    feature_color = feature_colors$basket_ring,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Net ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = ((court_params$court_length %or% 0) / 2) -
      (court_params$basket_center_to_baseline %or% 0),
    y_anchor = 0,
    feature_df = court_features$net,
    feature_color = feature_colors$net,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  #### Backboard ####
  court_plot <- add_feature(
    court_plot,
    x_anchor = ((court_params$court_length %or% 0) / 2) -
      (court_params$backboard_face_to_baseline %or% 0),
    y_anchor = 0,
    feature_df = court_features$backboard,
    feature_color = feature_colors$backboard,
    reflect_x = TRUE,
    reflect_y = FALSE,
    x_trans = x_trans,
    y_trans = y_trans,
    rotation = rotation
  )

  # Set Display Range-----------------------------------------------------------
  half_court_length <- ((court_params$court_length %or% 0) / 2) +
    (court_params$court_apron_endline %or% 0)

  half_court_width <- ((court_params$court_width %or% 0) / 2) +
    (court_params$court_apron_sideline %or% 0)

  if (is.null(xlims)) {
    three_point_arc_distance <- max(
      court_params$basket_center_to_three_point_arc
    ) %or% 0

    lane_length <- max(court_params$lane_length) %or% 0

    xlims <- switch(tolower(display_range),
                    # Full surface
                    "full" = c(-half_court_length, half_court_length),
                    "in_bounds_only" = c(
                      -(
                        ((court_params$court_length %or% 0) / 2) +
                          (court_params$line_thickness %or% 0)
                      ),
                      ((court_params$court_length %or% 0) / 2) +
                        (court_params$line_thickness %or% 0)
                    ),
                    "in bounds only" = c(
                      -(
                        ((court_params$court_length %or% 0) / 2) +
                          (court_params$line_thickness %or% 0)
                      ),
                      ((court_params$court_length %or% 0) / 2) +
                        (court_params$line_thickness %or% 0)
                    ),

                    # Half-court plots
                    "offense" = c(0, half_court_length),
                    "offence" = c(0, half_court_length),
                    "offensivehalfcourt" = c(0, half_court_length),
                    "offensive_half_court" = c(0, half_court_length),
                    "offensive half court" = c(0, half_court_length),
                    "defense" = c(-half_court_length, 0),
                    "defence" = c(-half_court_length, 0),
                    "defensivehalfcourt" = c(-half_court_length, 0),
                    "defensive_half_court" = c(-half_court_length, 0),
                    "defensive half court" = c(-half_court_length, 0),

                    # Offensive Key
                    "offensivekey" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        (court_params$basket_center_to_baseline %or% 0) -
                        three_point_arc_distance -
                        3,
                      half_court_length
                    ),
                    "offensive_key" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        (court_params$basket_center_to_baseline %or% 0) -
                        three_point_arc_distance -
                        3,
                      half_court_length
                    ),
                    "offensive key" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        (court_params$basket_center_to_baseline %or% 0) -
                        three_point_arc_distance -
                        3,
                      half_court_length
                    ),
                    "attackingkey" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        (court_params$basket_center_to_baseline %or% 0) -
                        three_point_arc_distance -
                        3,
                      half_court_length
                    ),
                    "attacking_key" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        (court_params$basket_center_to_baseline %or% 0) -
                        three_point_arc_distance -
                        3,
                      half_court_length
                    ),
                    "attacking key" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        (court_params$basket_center_to_baseline %or% 0) -
                        three_point_arc_distance -
                        3,
                      half_court_length
                    ),

                    # Defensive Key
                    "defensivekey" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        (court_params$basket_center_to_baseline %or% 0) -
                        three_point_arc_distance -
                        3)
                    ),
                    "defensive_key" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        (court_params$basket_center_to_baseline %or% 0) -
                        three_point_arc_distance -
                        3)
                    ),
                    "defensive key" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        (court_params$basket_center_to_baseline %or% 0) -
                        three_point_arc_distance -
                        3)
                    ),
                    "defendingkey" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        (court_params$basket_center_to_baseline %or% 0) -
                        three_point_arc_distance -
                        3)
                    ),
                    "defending_key" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        (court_params$basket_center_to_baseline %or% 0) -
                        three_point_arc_distance -
                        3)
                    ),
                    "defending key" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        (court_params$basket_center_to_baseline %or% 0) -
                        three_point_arc_distance -
                        3)
                    ),

                    # Offensive Painted Area
                    "offensivepaint" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius,
                      half_court_length
                    ),
                    "offensive_paint" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius,
                      half_court_length
                    ),
                    "offensive paint" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius,
                      half_court_length
                    ),
                    "attackingpaint" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius,
                      half_court_length
                    ),
                    "attacking_paint" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius,
                      half_court_length
                    ),
                    "attacking paint" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius,
                      half_court_length
                    ),
                    "offensivelane" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius,
                      half_court_length
                    ),
                    "offensive_lane" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius,
                      half_court_length
                    ),
                    "offensive lane" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius,
                      half_court_length
                    ),
                    "attackinglane" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius,
                      half_court_length
                    ),
                    "attacking_lane" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius,
                      half_court_length
                    ),
                    "attacking lane" = c(
                      ((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius,
                      half_court_length
                    ),

                    # Defensive Painted Area
                    "defensivepaint" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius)
                    ),
                    "defensive_paint" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius)
                    ),
                    "defensive paint" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius)
                    ),
                    "defendingpaint" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius)
                    ),
                    "defending_paint" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius)
                    ),
                    "defending paint" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius)
                    ),
                    "defensivelane" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius)
                    ),
                    "defensive_lane" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius)
                    ),
                    "defensive lane" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius)
                    ),
                    "defendinglane" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius)
                    ),
                    "defending_lane" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius)
                    ),
                    "defending lane" = c(
                      -half_court_length,
                      -(((court_params$court_length %or% 0) / 2) -
                        lane_length -
                        court_params$free_throw_circle_radius)
                    ),

                    # Default case
                    c(-half_court_length, half_court_length)
    )

    # Adjust the x limits of the plot per the specified x translation
    xlims <- xlims + x_trans
  }

  if (is.null(ylims)) {
    lane_width <- (max(court_params$lane_width) %or% 0) / 2

    ylims <- switch(tolower(display_range),
                    # Full surface
                    "full" = c(-half_court_width, half_court_width),
                    "in_bounds_only" = c(
                      -(
                        ((court_params$court_width %or% 0) / 2) +
                          (court_params$line_thickness %or% 0)
                      ),
                      ((court_params$court_width %or% 0) / 2) +
                        (court_params$line_thickness %or% 0)
                    ),
                    "in bounds only" = c(
                      -(
                        ((court_params$court_width %or% 0) / 2) +
                          (court_params$line_thickness %or% 0)
                      ),
                      ((court_params$court_width %or% 0) / 2) +
                        (court_params$line_thickness %or% 0)
                    ),

                    # Half-court plots
                    "offense" = c(-half_court_width, half_court_width),
                    "offence" = c(-half_court_width, half_court_width),
                    "offensivehalfcourt" = c(
                      -half_court_width,
                      half_court_width
                    ),
                    "offensive_half_court" = c(
                      -half_court_width,
                      half_court_width
                    ),
                    "offensive half court" = c(
                      -half_court_width,
                      half_court_width
                    ),
                    "defense" = c(-half_court_width, half_court_width),
                    "defence" = c(-half_court_width, half_court_width),
                    "defensivehalfcourt" = c(
                      -half_court_width,
                      half_court_width
                    ),
                    "defensive_half_court" = c(
                      -half_court_width,
                      half_court_width
                    ),
                    "defensive half court" = c(
                      -half_court_width,
                      half_court_width
                    ),

                    # Offensive Key
                    "offensivekey" = c(-half_court_width, half_court_width),
                    "offensive_key" = c(-half_court_width, half_court_width),
                    "offensive key" = c(-half_court_width, half_court_width),
                    "attackingkey" = c(-half_court_width, half_court_width),
                    "attacking_key" = c(-half_court_width, half_court_width),
                    "attacking key" = c(-half_court_width, half_court_width),

                    # Defensive Key
                    "defensivekey" = c(-half_court_width, half_court_width),
                    "defensive_key" = c(-half_court_width, half_court_width),
                    "defensive key" = c(-half_court_width, half_court_width),
                    "defendingkey" = c(-half_court_width, half_court_width),
                    "defending_key" = c(-half_court_width, half_court_width),
                    "defending key" = c(-half_court_width, half_court_width),

                    # Offensive Painted Area
                    "offensivepaint" = c(-lane_width - 1.5, lane_width + 1.5),
                    "offensive_paint" = c(-lane_width - 1.5, lane_width + 1.5),
                    "offensive paint" = c(-lane_width - 1.5, lane_width + 1.5),
                    "attackingpaint" = c(-lane_width - 1.5, lane_width + 1.5),
                    "attacking_paint" = c(-lane_width - 1.5, lane_width + 1.5),
                    "attacking paint" = c(-lane_width - 1.5, lane_width + 1.5),
                    "offensivelane" = c(-lane_width - 1.5, lane_width + 1.5),
                    "offensive_lane" = c(-lane_width - 1.5, lane_width + 1.5),
                    "offensive lane" = c(-lane_width - 1.5, lane_width + 1.5),
                    "attackinglane" = c(-lane_width - 1.5, lane_width + 1.5),
                    "attacking_lane" = c(-lane_width - 1.5, lane_width + 1.5),
                    "attacking lane" = c(-lane_width - 1.5, lane_width + 1.5),

                    # Defensive Painted Area
                    "defensivepaint" = c(-lane_width - 1.5, lane_width + 1.5),
                    "defensive_paint" = c(-lane_width - 1.5, lane_width + 1.5),
                    "defensive paint" = c(-lane_width - 1.5, lane_width + 1.5),
                    "defendingpaint" = c(-lane_width - 1.5, lane_width + 1.5),
                    "defending_paint" = c(-lane_width - 1.5, lane_width + 1.5),
                    "defending paint" = c(-lane_width - 1.5, lane_width + 1.5),
                    "defensivelane" = c(-lane_width - 1.5, lane_width + 1.5),
                    "defensive_lane" = c(-lane_width - 1.5, lane_width + 1.5),
                    "defensive lane" = c(-lane_width - 1.5, lane_width + 1.5),
                    "defendinglane" = c(-lane_width - 1.5, lane_width + 1.5),
                    "defending_lane" = c(-lane_width - 1.5, lane_width + 1.5),
                    "defending lane" = c(-lane_width - 1.5, lane_width + 1.5),

                    # Default case
                    c(-half_court_width, half_court_width)
    )

    # Adjust the y limits of the plot per the specified y translation
    ylims <- ylims + y_trans
  }

  # Rotate the limits of the plot. First, create the bounding box, then rotate
  # the vertices of the bounding box accordingly
  if (rotation != 0) {
    plot_lims <- rotate_coords(
      create_rectangle(
        x_min = min(xlims),
        x_max = max(xlims),
        y_min = min(ylims),
        y_max = max(ylims)
      ),
      angle = rotation
    )

    xlims <- c(min(plot_lims$x), max(plot_lims$x))
    ylims <- c(min(plot_lims$y), max(plot_lims$y))
  }

  ## Force the aspect ratio to be 1/1, and the x and y limits to be set
  court_plot <- court_plot +
    ggplot2::coord_fixed(
      xlim = xlims,
      ylim = ylims,
      expand = FALSE
    )

  # Return the ggplot2 instance
  return(court_plot)
}
