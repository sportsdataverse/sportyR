# Test the coordinate transformations
test_that("Coordinate transformations should reflect, rotate, and translate the unit square", {
  # Create a unit square
  unit_square = create_square(1)

  # Test translation
  translated_result = create_square(1, c(.5, .5))

  expect_equal(translate(unit_square, translate_x = .5, translate_y = .5), translated_result)

  # Test rotation. To avoid issues with numbers close to 0, a square centered at
  # (1, 1) will be used
  rotation_square = translate(unit_square, translate_x = 1, translate_y = 1)

  rotated_result = data.frame(
    x = c(-.5, -.5, -1.5, -1.5, -.5),
    y = c(.5, 1.5, 1.5, .5, .5)
  )

  expect_equal(rotate_coords(rotation_square), rotated_result)

  # Test reflection
  reflected_square = translate(unit_square, translate_x = .5, translate_y = .5)
  reflected_result = data.frame(
    x = c(0, -1, -1, 0, 0),
    y = c(0, 0, 1, 1, 0)
  )

  expect_equal(reflect(reflected_square), reflected_result)
})
