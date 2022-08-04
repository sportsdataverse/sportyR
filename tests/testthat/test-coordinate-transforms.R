# Test the coordinate transformations
test_that(
  "Rotate a unit square about the origin in either direction", {
    # Create a unit square. To avoid issues with numbers close to 0, a square
    # centered at (1, 1) will be used
    unit_square <- create_square(1, center = c(1, 1))

    rotated_ccw_result <- data.frame(
      x = c(-.5, -.5, -1.5, -1.5, -.5),
      y = c(.5, 1.5, 1.5, .5, .5)
    )

    rotated_cw_result <- data.frame(
      x = c(.5, .5, 1.5, 1.5, .5),
      y = c(-.5, -1.5, -1.5, -.5, -.5)
    )

    empty_df <- data.frame()

    expect_equal(
      rotate_coords(unit_square, angle = 90),
      rotated_ccw_result
    )

    expect_equal(
      rotate_coords(unit_square, angle = -90),
      rotated_cw_result
    )

    expect_equal(rotate_coords(empty_df, angle = 90), empty_df)
  }
)

test_that(
  "Reflect a unit square centered at (1, 1) over the x and y axes", {
    # Create a unit square. To avoid issues with numbers close to 0, a square
    # centered at (1, 1) will be used
    unit_square <- create_square(1, center = c(0.5, 0.5))

    # Test reflection
    reflected_result <- data.frame(
      x = c(0, -1, -1, 0, 0),
      y = c(0, 0, 1, 1, 0)
    )

    # Reflect over y but not x
    expect_equal(
      reflect(unit_square),
      reflected_result
    )
  }
)
