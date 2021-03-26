# Test that all plotting functions and helper functions are working correctly.
# This is done through the vdiffr package. To validate new images, call
# vdiffr::manage_cases()
test_that('Basic league plots match expected outputs', {
  ## Baseball
  baseball_plot = geom_baseball('mlb')
  vdiffr::expect_doppelganger('mlb plot', baseball_plot)

  # Basketball
  nba_plot = geom_basketball('nba')
  wnba_plot = geom_basketball('wnba')
  ncaa_basketball_plot = geom_basketball('ncaa')
  fiba_plot = geom_basketball('fiba')

  nba_plot_rotated = geom_basketball('nba', rotate = TRUE)
  wnba_plot_rotated = geom_basketball('wnba', rotate = TRUE)
  ncaa_basketball_plot_rotated = geom_basketball('ncaa', rotate = TRUE)
  fiba_plot_rotated = geom_basketball('fiba', rotate = TRUE)

  vdiffr::expect_doppelganger('nba plot', nba_plot)
  vdiffr::expect_doppelganger('wnba plot', wnba_plot)
  vdiffr::expect_doppelganger('ncaa_basketball plot', ncaa_basketball_plot)
  vdiffr::expect_doppelganger('fiba plot', fiba_plot)

  vdiffr::expect_doppelganger('nba plot rotated', nba_plot_rotated)
  vdiffr::expect_doppelganger('wnba plot rotated', wnba_plot_rotated)
  vdiffr::expect_doppelganger('ncaa_basketball plot rotated', ncaa_basketball_plot_rotated)
  vdiffr::expect_doppelganger('fiba plot rotated', fiba_plot_rotated)

  # Football
  nfl_plot = geom_football('nfl')
  ncaa_plot = geom_football('ncaa')

  nfl_plot_rotated_ccw = geom_football('nfl', rotate = TRUE)
  ncaa_plot_rotated_ccw = geom_football('ncaa', rotate = TRUE)

  nfl_plot_rotated_cw = geom_football('nfl', rotate = TRUE, rotation_dir = 'cw')
  ncaa_plot_rotated_cw = geom_football('ncaa', rotate = TRUE, rotation_dir = 'cw')

  vdiffr::expect_doppelganger('nfl plot', nfl_plot)
  vdiffr::expect_doppelganger('ncaa_football plot', ncaa_plot)

  vdiffr::expect_doppelganger('nfl plot rotated ccw', nfl_plot_rotated_ccw)
  vdiffr::expect_doppelganger('nfl plot rotated cw', nfl_plot_rotated_cw)
  vdiffr::expect_doppelganger('ncaa_football plot rotated ccw', ncaa_plot_rotated_ccw)
  vdiffr::expect_doppelganger('ncaa_football plot rotated cw', ncaa_plot_rotated_cw)

  # Hockey
  nhl_plot = geom_hockey('nhl')
  iihf_plot = geom_hockey('iihf')
  ncaa_hockey_plot = geom_hockey('ncaa')

  nhl_plot_rotated = geom_hockey('nhl', rotate = TRUE)
  iihf_plot_rotated = geom_hockey('iihf', rotate = TRUE)
  ncaa_hockey_plot_rotated = geom_hockey('ncaa', rotate = TRUE)

  vdiffr::expect_doppelganger('nhl plot', nhl_plot)
  vdiffr::expect_doppelganger('iihf plot', iihf_plot)
  vdiffr::expect_doppelganger('ncaa_hockey plot', ncaa_hockey_plot)

  vdiffr::expect_doppelganger('nhl plot rotated', nhl_plot_rotated)
  vdiffr::expect_doppelganger('iihf plot rotated', iihf_plot_rotated)
  vdiffr::expect_doppelganger('ncaa_hockey plot rotated', ncaa_hockey_plot_rotated)

  # Soccer
  fifa_plot = geom_soccer('fifa')
  fifa_plot_rotated = geom_soccer('fifa', rotate = TRUE)

  vdiffr::expect_doppelganger('fifa plot', fifa_plot)
  vdiffr::expect_doppelganger('fifa plot rotated', fifa_plot_rotated)
})

test_that('Invalid leagues return the original ggplot2 instance for feature-creating functions', {
  # Make up a league name to use for these tests
  bad_league = 'badleague'

  # Create the base image for baseball
  baseball_base_plot = ggplot2::ggplot() +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(color = '#707372'),
      plot.background = ggplot2::element_rect(fill = '#395d33'),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      caption = "Plot made via sportyR"
    )

  # Test the baseball feature plotting functions when passed a bad league
  vdiffr::expect_doppelganger('baseball base plot', baseball_infield_dirt(baseball_base_plot, bad_league))
  vdiffr::expect_doppelganger('baseball base plot', baseball_infield_grass(baseball_base_plot, bad_league))
  vdiffr::expect_doppelganger('baseball base plot', baseball_mound(baseball_base_plot, bad_league))
  vdiffr::expect_doppelganger('baseball base plot', baseball_bases(baseball_base_plot, bad_league))
  vdiffr::expect_doppelganger('baseball base plot', baseball_batters_boxes(baseball_base_plot, bad_league))
  vdiffr::expect_doppelganger('baseball base plot', baseball_lines(baseball_base_plot, bad_league))

  # Create the base image for basketball
  basketball_base_plot = ggplot2::ggplot() +
    ggplot2::coord_fixed() +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(color = '#707372'),
      plot.margin = ggplot2::margin(-1, 0, -1, 0, "cm"),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
    ) +
    ggplot2::labs(
      caption = "Plot made via sportyR"
    )

  # Test the basketball feature plotting functions when passed a bad league
  vdiffr::expect_doppelganger('basketball base plot', basketball_court(basketball_base_plot, bad_league))
  vdiffr::expect_doppelganger('basketball base plot', basketball_center_circle(basketball_base_plot, bad_league))
  vdiffr::expect_doppelganger('basketball base plot', basketball_division_line(basketball_base_plot, bad_league))
  vdiffr::expect_doppelganger('basketball base plot', basketball_endline(basketball_base_plot, bad_league))
  vdiffr::expect_doppelganger('basketball base plot', basketball_sideline(basketball_base_plot, bad_league))
  vdiffr::expect_doppelganger('basketball base plot', basketball_team_bench(basketball_base_plot, bad_league))
  vdiffr::expect_doppelganger('basketball base plot', basketball_substitution_area(basketball_base_plot, bad_league))
  vdiffr::expect_doppelganger('basketball base plot', basketball_court_apron(basketball_base_plot, bad_league))
  vdiffr::expect_doppelganger('basketball base plot', basketball_three_point_line(basketball_base_plot, bad_league))
  vdiffr::expect_doppelganger('basketball base plot', basketball_free_throw_lane(basketball_base_plot, bad_league))
  vdiffr::expect_doppelganger('basketball base plot', basketball_free_throw_lane_lines(basketball_base_plot, bad_league))
  vdiffr::expect_doppelganger('basketball base plot', basketball_free_throw_semi_circle(basketball_base_plot, bad_league))
  vdiffr::expect_doppelganger('basketball base plot', basketball_free_throw_dashed_semi_circle(basketball_base_plot, bad_league))
  vdiffr::expect_doppelganger('basketball base plot', basketball_lower_defensive_box(basketball_base_plot, bad_league))
  vdiffr::expect_doppelganger('basketball base plot', basketball_restricted_area_arc(basketball_base_plot, bad_league))
  vdiffr::expect_doppelganger('basketball base plot', basketball_backboard(basketball_base_plot, bad_league))
  vdiffr::expect_doppelganger('basketball base plot', basketball_basket_ring(basketball_base_plot, bad_league))
  vdiffr::expect_doppelganger('basketball base plot', basketball_net(basketball_base_plot, bad_league))

  # Create the base image for football
  football_base_plot = ggplot2::ggplot() +
    ggplot2::coord_fixed() +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(color = '#707372'),
      plot.margin = ggplot2::margin(-1, 0, -1, 0, "cm"),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
    ) +
    ggplot2::labs(
      caption = "Plot made via sportyR"
    )

  # Test the football feature plotting functions when passed a bad league
  vdiffr::expect_doppelganger('football base plot', football_grass(football_base_plot, bad_league))
  vdiffr::expect_doppelganger('football base plot', football_sideline(football_base_plot, bad_league))
  vdiffr::expect_doppelganger('football base plot', football_endline(football_base_plot, bad_league))
  vdiffr::expect_doppelganger('football base plot', football_goal_line(football_base_plot, bad_league))
  vdiffr::expect_doppelganger('football base plot', football_yard_markings(football_base_plot, bad_league))
  vdiffr::expect_doppelganger('football base plot', football_try_markings(football_base_plot, bad_league))
  vdiffr::expect_doppelganger('football base plot', football_directional_arrows(football_base_plot, bad_league))
  vdiffr::expect_doppelganger('football base plot', football_yard_numbers(football_base_plot, bad_league))

  # Create the base plot for hockey
  hockey_base_plot = ggplot2::ggplot() +
    ggplot2::coord_fixed() +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(color = '#707372'),
      plot.margin = ggplot2::margin(-1, 0, -1, 0, "cm"),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
    ) +
    ggplot2::labs(
      caption = "Plot made via sportyR"
    )

  # Test the hockey feature plotting functions when passed a bad league
  vdiffr::expect_doppelganger('hockey base plot', hockey_boards(hockey_base_plot, bad_league))
  vdiffr::expect_doppelganger('hockey base plot', hockey_center_line(hockey_base_plot, bad_league))
  vdiffr::expect_doppelganger('hockey base plot', hockey_blue_line(hockey_base_plot, bad_league))
  vdiffr::expect_doppelganger('hockey base plot', hockey_goal_line(hockey_base_plot, bad_league))
  vdiffr::expect_doppelganger('hockey base plot', hockey_goalkeepers_restricted_area(hockey_base_plot, bad_league))
  vdiffr::expect_doppelganger('hockey base plot', hockey_goal_crease(hockey_base_plot, bad_league))
  vdiffr::expect_doppelganger('hockey base plot', hockey_referee_crease(hockey_base_plot, bad_league))
  vdiffr::expect_doppelganger('hockey base plot', hockey_faceoff_spot(hockey_base_plot, bad_league))
  vdiffr::expect_doppelganger('hockey base plot', hockey_faceoff_circle(hockey_base_plot, bad_league))
  vdiffr::expect_doppelganger('hockey base plot', hockey_faceoff_lines(hockey_base_plot, bad_league))

  # Create the base plot for soccer
  soccer_base_plot = ggplot2::ggplot() +
    ggplot2::coord_fixed() +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(color = '#707372'),
      plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
      plot.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
    ) +
    ggplot2::labs(
      caption = "Plot made via sportyR"
    )

  # Test the soccer feature plotting functions when passed a bad league
  vdiffr::expect_doppelganger('soccer base plot', soccer_grass(soccer_base_plot, bad_league))
  vdiffr::expect_doppelganger('soccer base plot', soccer_touchlines(soccer_base_plot, bad_league))
  vdiffr::expect_doppelganger('soccer base plot', soccer_goal_lines(soccer_base_plot, bad_league))
  vdiffr::expect_doppelganger('soccer base plot', soccer_halfway_line(soccer_base_plot, bad_league))
  vdiffr::expect_doppelganger('soccer base plot', soccer_center_circle(soccer_base_plot, bad_league))
  vdiffr::expect_doppelganger('soccer base plot', soccer_corner_circle(soccer_base_plot, bad_league))
  vdiffr::expect_doppelganger('soccer base plot', soccer_box_6yd(soccer_base_plot, bad_league))
  vdiffr::expect_doppelganger('soccer base plot', soccer_box_18yd(soccer_base_plot, bad_league))
  vdiffr::expect_doppelganger('soccer base plot', soccer_penalty_arc(soccer_base_plot, bad_league))
  vdiffr::expect_doppelganger('soccer base plot', soccer_penalty_mark(soccer_base_plot, bad_league))
  vdiffr::expect_doppelganger('soccer base plot', soccer_center_mark(soccer_base_plot, bad_league))
  vdiffr::expect_doppelganger('soccer base plot', soccer_goal(soccer_base_plot, bad_league))
})

test_that('Football df_maker functions work properly', {
  # If the yardage is divisible by 5, only one data frame should be created.
  # This results in a type of "data.frame"
  expect_s3_class(football_yard_markings_df_maker(20, league = 'NFL'), "data.frame")

  # Otherwise, it should return a list of data frames
  expect_type(football_yard_markings_df_maker(21, league = 'NFL', rotate = TRUE), "list")

  # The directional arrows data frame function should always return a list of
  # two dataframes: one for the upper arrow, and one for the lower
  expect_type(football_directional_arrows_df_maker(20, league = 'NFL'), "list")

  # The two functions should return NULL values if the league is not NFL or NCAA
  expect_null(football_yard_markings_df_maker(20, 'testleague'))
  expect_null(football_directional_arrows_df_maker(20, 'testleague'))
})

test_that('geom_{sport}() functions produce correct error messages for bad leagues', {
  # Make fake league
  bad_league = 'testleague'

  # Test baseball
  expect_error(geom_baseball(bad_league))

  # Test basketball
  expect_error(geom_basketball(bad_league))

  # Test football
  expect_error(geom_football(bad_league))

  # Test hockey
  expect_error(geom_hockey(bad_league))

  # Test soccer
  expect_error(geom_soccer(bad_league))

})
