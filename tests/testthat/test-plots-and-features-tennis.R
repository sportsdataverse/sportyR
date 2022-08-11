test_that(
  "geom_tennis() returns a plot when called with a league", {
    # Create a tennis court plot
    itf_court <- geom_tennis("itf")

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(itf_court)[1], "gg")
    expect_equal(class(itf_court)[2], "ggplot")
  }
)

test_that(
  "geom_tennis() can successfully transform coordinates", {
    # Create a tennis court plot
    ita_court <- geom_tennis("ita", court_units = "m", rotation = 270)

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(ita_court)[1], "gg")
    expect_equal(class(ita_court)[2], "ggplot")
  }
)
