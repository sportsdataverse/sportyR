test_that(
  "geom_volleyball() returns a plot when called with a league", {
    # Create a volleyball court plot
    ncaa_court <- geom_volleyball("ncaa")

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(ncaa_court)[1], "gg")
    expect_equal(class(ncaa_court)[2], "ggplot")
  }
)

test_that(
  "geom_volleyball() can successfully transform coordinates", {
    # Create a volleyball court plot
    fivb_court <- geom_volleyball("fivb", court_units = "ft", rotation = 270)

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(fivb_court)[1], "gg")
    expect_equal(class(fivb_court)[2], "ggplot")
  }
)
