# Test the coordinate transformations
test_that("Shapes are generated correctly", {
  # Unit square centered around (0, 0)
  unit_square = data.frame(
    x = c(-.5, .5, .5, -.5, -.5),
    y = c(-.5, -.5, .5, .5, -.5)
  )

  expect_identical(create_square(1), unit_square)

  # Unit circle centered around (0, 0) should produce the same results each time
  # it's called
  expect_identical(create_circle(), create_circle())

  # Unit rectangle centered around (0, 0) should produce a unit square
  expect_identical(create_rectangle(-.5, .5, -.5, .5), unit_square)

  # Unit diamond centered at (0, 0)
  unit_diamond = data.frame(
    x = c(-.5, 0, .5, 0, -.5),
    y = c(0, -.5, 0, .5, 0)
  )

  expect_identical(create_diamond(1, 1), unit_diamond)
})
