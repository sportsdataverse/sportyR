# Test the cani_plot functions to ensure that they work properly. They should
# each return a specific message about what leagues/sports are available to be
# plotted

test_that("cani_plot_() functions are working correctly for multiple leagues/sports", {
  # Test to make sure proper messages are returned when multiple leagues/sports
  # are possible
  expect_message(cani_plot_league('ncaa'), fixed = TRUE, regexp = 'NCAA can be used in the following functions: geom_basketball(), geom_football(), geom_hockey(), or geom_soccer()')
  expect_message(cani_plot_sport('basketball'), fixed = TRUE, regexp = 'geom_basketball() can be used to plot for the following leagues: COLLEGE, NBA, NCAA, NCAAM, NCAAW, WNBA')
})

test_that("cani_plot_() functions return correct messages for no leagues/sports", {
  # Test to make sure proper messages are returned if no league/sport is
  # supplied
  expect_message(cani_plot_league('testleague'), fixed = TRUE, regexp = 'Sorry, TESTLEAGUE is not a viable league to plot at this time. Please create an issue on GitHub with the league\'s playing surface specifications for the league to be added to the package')
  expect_message(cani_plot_sport('testsport'), fixed = TRUE, regexp = 'Sorry, TESTSPORT is not a viable sport to plot at this time. Please create an issue on GitHub with the sport\'s playing surface specifications for the league to be added to the package')
})

test_that("cani_plot_() functions return correct messages for single leagues/sports", {
  # Test to make sure proper messages are returned if no league/sport is
  # supplied
  expect_message(cani_plot_league('nfl'), fixed = TRUE, regexp = 'A plot for NFL can be created via the geom_football() function')
  expect_message(cani_plot_sport('baseball'), fixed = TRUE, regexp = 'A plot for BASEBALL can be created via the geom_baseball() function for the following league: MLB')
})

test_that("cani_color_league_features() returns message on success", {
  # Test to make sure that cani_color_league_features() returns a message for
  # each league
  expect_message(cani_color_league_features('FIBA'))
  expect_message(cani_color_league_features('FIFA'))
  expect_message(cani_color_league_features('IIHF'))
  expect_message(cani_color_league_features('MLB'))
  expect_message(cani_color_league_features('MLS'))
  expect_message(cani_color_league_features('NBA'))
  expect_message(cani_color_league_features('NFL'))
  expect_message(cani_color_league_features('NHL'))
  expect_message(cani_color_league_features('NWHL'))
  expect_message(cani_color_league_features('NWSL'))
  expect_message(cani_color_league_features('PREMIER'))
  expect_message(cani_color_league_features('WNBA'))
  expect_message(cani_color_league_features('NCAA', 'basketball'))
  expect_message(cani_color_league_features('NCAA', 'football'))
  expect_message(cani_color_league_features('NCAA', 'hockey'))
  expect_message(cani_color_league_features('NCAA', 'soccer'))
})

test_that("cani_color_league_features() returns error with league code NCAA and either NULL or unavailable sport", {
  # Test to make sure that an error message is returned
  expect_error(cani_color_league_features('NCAA'))
  expect_error(cani_color_league_features('NCAA', 'baseball'))
})

test_that("cani_color_league_features() returns error with bad league", {
  # Test to make sure that proper error messages are returned when a bad league
  # code is supplied
  expect_error(cani_color_league_features('badleague'))
})
