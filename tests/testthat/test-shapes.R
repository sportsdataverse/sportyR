test_that("create_square() correctly generates a square", {
  # Unit square centered around (0, 0)
  unit_square = data.frame(
    x = c(-.5, .5, .5, -.5, -.5),
    y = c(-.5, -.5, .5, .5, -.5)
  )

  # Run the test
  expect_identical(create_square(1), unit_square)
})

test_that("create_circle() correctly generates a unit circle", {
  # Create a unit circle
  unit_circle = create_circle()

  # Check that all x and y values are between -1 and 1 (inclusive). NOTE: a
  # point that is radially at least .999 units in distance from the origin will
  # be considered at radius 1
  unit_circle['dist'] = distance_formula(unit_circle$x, unit_circle$y, 0, 0)
  unit_circle[unit_circle['dist'] >= .999, 'dist_okay'] = 1

  # If each point on the circle is an equal number of units away from the
  # origin, then the sum of dist_okay should be the same as the number of rows
  # in the data frame

  # Run the test
  expect_equal(nrow(unit_circle), sum(unit_circle$dist_okay))
})

test_that("create_rectangle() correctly generates a rectangle", {
  # Unit square centered around (0, 0)
  unit_square = data.frame(
    x = c(-.5, .5, .5, -.5, -.5),
    y = c(-.5, -.5, .5, .5, -.5)
  )

  # A unit square centered around (0, 0) should be a rectangle with:
  # xmin = -0.5,
  # xmax =  0.5,
  # ymin = -0.5,
  # ymax =  0.5

  # Run the test
  expect_identical(create_rectangle(-.5, .5, -.5, .5), unit_square)
})

test_that("create_diamond() correctly generates a diamond", {
  # Unit diamond centered at (0, 0)
  unit_diamond = data.frame(
    x = c(-.5, 0, .5, 0, -.5),
    y = c(0, -.5, 0, .5, 0)
  )

  # Run the test
  expect_identical(create_diamond(1, 1), unit_diamond)
})
