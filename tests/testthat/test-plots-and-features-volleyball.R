test_that(
  "geom_volleyball() returns a plot when called with a league", {
    # Create a volleyball court plot
    ncaa_court <- geom_volleyball("ncaa")

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(ncaa_court))
  }
)

test_that(
  "geom_volleyball() can successfully transform coordinates", {
    # Create a volleyball court plot
    fivb_court <- geom_volleyball("fivb", court_units = "ft", rotation = 270)

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(fivb_court))
  }
)
