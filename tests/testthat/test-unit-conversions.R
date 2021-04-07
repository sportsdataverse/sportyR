# Check that unit conversions function correctly
test_that("convert_units() works with all possible input unit arguments (i.e. 'in' or 'inches' on single measures", {
  # 1in is 2.54cm, and the possible names are "in", "inches", "cm", or
  # "centimeters"
  expect_equal(convert_units(2.54, 'cm', 'in'), 1)
  expect_equal(convert_units(2.54, 'cm', 'inches'), 1)
  expect_equal(convert_units(2.54, 'centimeters', 'in'), 1)
  expect_equal(convert_units(2.54, 'centimeters', 'inches'), 1)
})

test_that("convert_units() works with all possible input unit arguments (i.e. 'in' or 'inches' on data frames", {
  # Make a test data frame
  test_df = data.frame(
    x = rep(1, 3),
    y = rep(2, 3),
    z = c('a', 'b', 'c')
  )

  # Create the expected outcome data frame
  expected_df = data.frame(
    x = rep(2.54, 3),
    y = rep(5.08, 3),
    z = c('a', 'b', 'c')
  )

  # Run the tests
  expect_identical(convert_units(test_df, 'in', 'centimeters', c('x', 'y')), expected_df)
  expect_identical(convert_units(test_df, 'inches', 'cm', c('x', 'y')), expected_df)
  expect_identical(convert_units(test_df, 'inches', 'centimeters', c('x', 'y')), expected_df)
})

test_that("convert_units() skips non-numeric and non-integer columns successfully", {
  # Make a test data frame
  test_df = data.frame(
    x = rep(1, 3),
    y = rep(2, 3),
    z = c('a', 'b', 'c')
  )

  # Create the expected outcome data frame
  expected_df = data.frame(
    x = rep(2.54, 3),
    y = rep(5.08, 3),
    z = c('a', 'b', 'c')
  )

  # Run the test
  expect_identical(convert_units(test_df, 'in', 'centimeters', c('x', 'y', 'z')), expected_df)
})

test_that("convert_units() errors when input or output units are unavailable", {
  # If a made-up unit, like "foo" or "bar" are passed as either input units or
  # output units, the function should error
  expect_error(convert_units(2.54, 'foo', 'bar'))
  expect_error(convert_units(2.54, 'foo', 'in'))
  expect_error(convert_units(2.54, 'cm', 'bar'))
})

test_that("convert_units() errors when a data frame is supplied with no conversion_columns", {
  # Make a test data frame
  test_df = data.frame(
    x = 1:5,
    y = 6:10,
    z = c('a', 'b', 'c', 'd', 'e')
  )

  # Produce the error
  expect_error(convert_units(test_df, 'ft', 'm'))
})

test_that("convert_units() messages user when a column supplied in conversion_columns is not in the data frame", {
  # Make a test data frame
  test_df = data.frame(
    x = 1:5,
    y = 6:10,
    z = c('a', 'b', 'c', 'd', 'e')
  )

  # Produce the message
  expect_message(convert_units(test_df, 'ft', 'm', conversion_columns = c('x', 'y', 'a')))
})
