test_that(
  "geom_football() returns a plot when called with a league", {
    # Create a football field plot
    nfl_field <- geom_football("nfl")

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(nfl_field)[1], "gg")
    expect_equal(class(nfl_field)[2], "ggplot")
  }
)

test_that(
  "geom_football() can successfully transform coordinates", {
    # Create a football field plot
    cfl_field <- geom_football("cfl", field_units = "m", rotation = 270)

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(cfl_field)[1], "gg")
    expect_equal(class(cfl_field)[2], "ggplot")
  }
)

test_that(
  "geom_football() can successfully plot a rectangular bench area", {
    # Create a football field plot
    ncaa_field <- geom_football("ncaa")

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(ncaa_field)[1], "gg")
    expect_equal(class(ncaa_field)[2], "ggplot")
  }
)
