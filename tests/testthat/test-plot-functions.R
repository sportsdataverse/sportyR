# Test that all plotting functions and helper functions are working correctly.
# This is done through the vdiffr package. To validate new images, call
# vdiffr::manage_cases()
test_that('Non-rotated league plots match expected outputs', {
  # Baseball
  mlb_plot = geom_baseball('mlb')
  vdiffr::expect_doppelganger('mlb plot', mlb_plot)

  # Basketball
  fiba_ft_plot = geom_basketball('fiba', unit = 'ft')
  fiba_m_plot = geom_basketball('fiba', unit = 'm')
  ncaa_bb_plot = geom_basketball('ncaa')
  nba_plot = geom_basketball('nba')
  wnba_plot = geom_basketball('wnba')
  vdiffr::expect_doppelganger('fiba ft plot', fiba_ft_plot)
  vdiffr::expect_doppelganger('fiba m plot', fiba_m_plot)
  vdiffr::expect_doppelganger('ncaa bb plot', ncaa_bb_plot)
  vdiffr::expect_doppelganger('nba plot', nba_plot)
  vdiffr::expect_doppelganger('wnba plot', wnba_plot)

  # Hockey
  iihf_plot = geom_hockey('iihf')
  ncaa_hockey_plot = geom_hockey('ncaa')
  nhl_plot = geom_hockey('nhl')
  nwhl_plot = geom_hockey('nwhl')
  vdiffr::expect_doppelganger('iihf plot', iihf_plot)
  vdiffr::expect_doppelganger('ncaa hockey plot', ncaa_hockey_plot)
  vdiffr::expect_doppelganger('nhl plot', nhl_plot)
  vdiffr::expect_doppelganger('nwhl plot', nwhl_plot)

  # Football
  nfl_plot = geom_football('nfl')
  ncaa_football_plot = geom_football('ncaa')
  vdiffr::expect_doppelganger('nfl plot', nfl_plot)
  vdiffr::expect_doppelganger('ncaa football plot', ncaa_football_plot)

  # Soccer
  fifa_plot = geom_soccer('fifa')
  mls_plot = geom_soccer('mls')
  ncaa_soccer_plot = geom_soccer('ncaa')
  nwsl_plot = geom_soccer('nwsl')
  premier_league_plot = geom_soccer('premier')
  vdiffr::expect_doppelganger('fifa plot', fifa_plot)
  vdiffr::expect_doppelganger('mls plot', mls_plot)
  vdiffr::expect_doppelganger('ncaa soccer plot', ncaa_soccer_plot)
  vdiffr::expect_doppelganger('nwsl plot', nwsl_plot)
  vdiffr::expect_doppelganger('premier league plot', premier_league_plot)
})

test_that('Rotated league plots match expected outputs', {
  # Basketball
  fiba_ft_plot = geom_basketball('fiba', unit = 'ft', rotate = TRUE)
  fiba_m_plot = geom_basketball('fiba', unit = 'm', rotate = TRUE)
  # NOTE: the professional free-throw lane features are added for additional
  # testing
  ncaa_bb_plot = geom_basketball('ncaa', rotate = TRUE, include_professional_free_throw_lane = TRUE, include_professional_free_throw_lane_lines = TRUE)
  nba_plot = geom_basketball('nba', rotate = TRUE)
  wnba_plot = geom_basketball('wnba', rotate = TRUE)
  vdiffr::expect_doppelganger('fiba ft rotated plot', fiba_ft_plot)
  vdiffr::expect_doppelganger('fiba m rotated plot', fiba_m_plot)
  vdiffr::expect_doppelganger('ncaa bb rotated plot', ncaa_bb_plot)
  vdiffr::expect_doppelganger('nba rotated plot', nba_plot)
  vdiffr::expect_doppelganger('wnba rotated plot', wnba_plot)

  # Hockey
  iihf_plot = geom_hockey('iihf', rotate = TRUE)
  ncaa_hockey_plot = geom_hockey('ncaa', rotate = TRUE)
  nhl_plot = geom_hockey('nhl', rotate = TRUE)
  nwhl_plot = geom_hockey('nwhl', rotate = TRUE)
  vdiffr::expect_doppelganger('iihf rotated plot', iihf_plot)
  vdiffr::expect_doppelganger('ncaa hockey rotated plot', ncaa_hockey_plot)
  vdiffr::expect_doppelganger('nhl rotated plot', nhl_plot)
  vdiffr::expect_doppelganger('nwhl rotated plot', nwhl_plot)

  # Football
  nfl_ccw_plot = geom_football('nfl', rotate = TRUE)
  nfl_cw_plot = geom_football('nfl', rotate = TRUE, rotation_dir = 'cw')
  ncaa_football_ccw_plot = geom_football('ncaa', rotate = TRUE)
  ncaa_football_cw_plot = geom_football('ncaa', rotate = TRUE, rotation_dir = 'cw')
  vdiffr::expect_doppelganger('nfl rotated ccw plot', nfl_ccw_plot)
  vdiffr::expect_doppelganger('nfl rotated cw plot', nfl_cw_plot)
  vdiffr::expect_doppelganger('ncaa football rotated ccw plot', ncaa_football_ccw_plot)
  vdiffr::expect_doppelganger('ncaa football rotated cw plot', ncaa_football_cw_plot)

  # Soccer
  # NOTE: only half the pitch is plotted for additional testing of features
  fifa_plot = geom_soccer('fifa', rotate = TRUE, full_surf = FALSE)
  mls_plot = geom_soccer('mls', rotate = TRUE, full_surf = FALSE)
  ncaa_soccer_plot = geom_soccer('ncaa', rotate = TRUE, full_surf = FALSE)
  nwsl_plot = geom_soccer('nwsl', rotate = TRUE, full_surf = FALSE)
  premier_league_plot = geom_soccer('premier', rotate = TRUE, full_surf = FALSE)
  vdiffr::expect_doppelganger('fifa rotated plot', fifa_plot)
  vdiffr::expect_doppelganger('mls rotated plot', mls_plot)
  vdiffr::expect_doppelganger('nwsl rotated plot', nwsl_plot)
  vdiffr::expect_doppelganger('premier league rotated plot', premier_league_plot)
})

test_that('Plot must have non-null caption color', {
  # This is done to ensure that the plot has correct attribution
  expect_error(create_plot_base(caption_color = NULL))
})

test_that('A data frame without columns \'x\' and \'y\' should not be plottable', {
  # This is done to ensure that the column names passed to add_feature() will
  # pass
  bad_df = data.frame('badx' = 1:3, 'bady' = 1:3 ^ 2)
  expect_false(check_data_frame_for_plot(bad_df))

  # Passing this to the add_feature() function should result in an error
  g = create_plot_base()
  expect_error(add_feature(g, bad_df, '#000000'))
})
