context('Plots')

test_that('Basic league plots match expected outputs', {
  ## Baseball
  baseball_plot = geom_baseball('mlb')
  vdiffr::expect_doppelganger('mlb plot', baseball_plot)

  # Basketball
  nba_plot = geom_basketball('nba')
  wnba_plot = geom_basketball('wnba')
  ncaa_basketball_plot = geom_basketball('ncaa')

  vdiffr::expect_doppelganger('nba plot', nba_plot)
  vdiffr::expect_doppelganger('wnba plot', wnba_plot)
  vdiffr::expect_doppelganger('ncaa_basketball plot', ncaa_basketball_plot)

  # Football
  nfl_plot = geom_football('nfl')
  ncaa_plot = geom_football('ncaa')

  vdiffr::expect_doppelganger('nfl plot', nfl_plot)
  vdiffr::expect_doppelganger('ncaa_football plot', nfl_plot)

  # Hockey
  nhl_plot = geom_hockey('nhl')
  iihf_plot = geom_hockey('iihf')
  ncaa_hockey_plot = geom_hockey('ncaa')

  vdiffr::expect_doppelganger('nhl plot', nhl_plot)
  vdiffr::expect_doppelganger('iihf plot', iihf_plot)
  vdiffr::expect_doppelganger('ncaa_hockey plot', ncaa_hockey_plot)

  # Soccer
  fifa_plot = geom_soccer('fifa')
  vdiffr::expect_doppelganger('fifa plot', fifa_plot)
})
