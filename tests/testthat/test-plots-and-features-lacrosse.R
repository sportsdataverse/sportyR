test_that(
  "geom_lacrosse() returns a plot when called with a league", {
    # Create a lacrosse field plot
    nll_field <- geom_lacrosse("nll")

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(nll_field)[1], "gg")
    expect_equal(class(nll_field)[2], "ggplot")
  }
)

test_that(
  "geom_lacrosse() can successfully transform coordinates", {
    # Create a lacrosse field plot
    ncaaw_field <- geom_lacrosse("ncaaw", field_units = "ft", rotation = 270)

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(ncaaw_field)[1], "gg")
    expect_equal(class(ncaaw_field)[2], "ggplot")
  }
)

test_that(
  "geom_lacrosse() can successfully plot with all radii being 0", {
    # Create a lacrosse field plot
    suppressWarnings(
      pll_field <- geom_lacrosse(
        "pll",
        field_updates = list(
          faceoff_circle_radius = 0,
          noncenter_faceoff_spot_radius = 0,
          goal_crease_radius = 0,
          corner_radius = 0
        )
      )
    )

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(pll_field)[1], "gg")
    expect_equal(class(pll_field)[2], "ggplot")
  }
)

test_that(
  "geom_lacrosse() can successfully plot with a square center face-off mark", {
    # Create a lacrosse field plot
    suppressWarnings(
      ncaam_field <- geom_lacrosse(
        "ncaam",
        field_updates = list(
          faceoff_circle_radius = 0,
          noncenter_faceoff_spot_radius = 0,
          goal_crease_radius = 0,
          corner_radius = 0
        )
      )
    )

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(ncaam_field)[1], "gg")
    expect_equal(class(ncaam_field)[2], "ggplot")
  }
)
