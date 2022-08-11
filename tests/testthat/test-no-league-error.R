# If no league is supplied, an error should be returned
test_that("Error when league is not supplied", {
  # Baseball
  expect_error(geom_baseball())

  # Basketball
  expect_error(geom_basketball())

  # Football
  expect_error(geom_football())

  # Hockey
  expect_error(geom_hockey())

  # Soccer
  expect_error(geom_soccer())

  # Tennis
  expect_error(geom_tennis())
})
