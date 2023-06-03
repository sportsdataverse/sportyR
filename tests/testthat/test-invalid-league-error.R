# If an invalid league is supplied, an error should be returned
test_that("Error when league is not supplied", {
  # Baseball
  expect_error(geom_baseball("invalid_league"))

  # Basketball
  expect_error(geom_basketball("invalid_league"))

  # Curling
  expect_error(geom_curling("invalid_league"))

  # Football
  expect_error(geom_football("invalid_league"))

  # Hockey
  expect_error(geom_hockey("invalid_league"))

  # Lacrosse
  expect_error(geom_lacrosse("invalid_league"))

  # Soccer
  expect_error(geom_soccer("invalid_league"))

  # Tennis
  expect_error(geom_tennis("invalid_league"))

  # Volleyball
  expect_error(geom_volleyball("invalid_league"))
})
