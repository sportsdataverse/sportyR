test_that("is_hex() returns FALSE for invalid strings", {
  # The following should return false from is_hex():
  # - Empty strings
  # - Strings that don't begin with a "#"
  # - Strings that don't use only characters a-f and/or 0-9
  # - Strings that are not either a "#" followed by 3, 6, or 8 characters
  expect_false(is_hex(""))
  expect_false(is_hex("13294b"))
  expect_false(is_hex("#13294"))
  expect_false(is_hex("#13294r"))
})
