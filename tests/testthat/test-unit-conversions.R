# Check that unit conversions function correctly
test_that("Unit Conversions", {
  # 1 inch should be 2.54cm, so
  expect_equal(m_to_ft(.0254), 1/12)
  expect_equal(in_to_m(1), .0254)

  # 1 yard is 3 feet, so 36" should be 1 yard
  expect_equal(inches_to_yd(36), 1)

  # 1 yard is 3 feet
  expect_equal(ft_to_yd(3), 1)
})
