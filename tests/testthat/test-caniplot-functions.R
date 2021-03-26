# Test the caniplot functions to ensure that they work properly. They should
# each return a specific message about what leagues/sports are available to be
# plotted

test_that("caniplot_() functions are working correctly for multiple leagues/sports", {
  # Test to make sure proper messages are returned when multiple leagues/sports
  # are possible
  expect_message(caniplot_league('ncaa'), fixed = TRUE, regexp = 'NCAA can be used in the following functions: geom_baseball(), geom_basketball(), geom_football(), or geom_hockey()')
  expect_message(caniplot_sport('basketball'), fixed = TRUE, regexp = 'geom_basketball() can be used to plot for the following leagues: COLLEGE, NBA, NCAA, NCAAM, NCAAW, WNBA')
})

test_that("caniplot_() functions return correct messages for no leagues/sports", {
  # Test to make sure proper messages are returned if no league/sport is
  # supplied
  expect_message(caniplot_league('testleague'), fixed = TRUE, regexp = 'Sorry, TESTLEAGUE is not a viable league to plot at this time. Please create an issue on GitHub with the league\'s playing surface specifications for the league to be added to the package')
  expect_message(caniplot_sport('testsport'), fixed = TRUE, regexp = 'Sorry, TESTSPORT is not a viable sport to plot at this time. Please create an issue on GitHub with the sport\'s playing surface specifications for the league to be added to the package')
})

test_that("caniplot_() functions return correct messages for single leagues/sports", {
  # Test to make sure proper messages are returned if no league/sport is
  # supplied
  expect_message(caniplot_league('nfl'), fixed = TRUE, regexp = 'A plot for NFL can be created via the geom_football() function')
  expect_message(caniplot_sport('baseball'), fixed = TRUE, regexp = 'A plot for BASEBALL can be created via the geom_baseball() function for the following league: MLB')
})
