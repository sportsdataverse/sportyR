test_that(
  "quadratic_formula() errors out when not given a squared term", {
    # This should error because of zero-division
    expect_error(quadratic_formula(0, 1, 2))
  }
)
