skip_on_cran()

test_that(
  "geom_lacrosse() returns a plot when called with a league", {
    # Create a lacrosse field plot
    nll_field <- geom_lacrosse("nll")

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(nll_field))
  }
)

test_that(
  "geom_lacrosse() can successfully transform coordinates", {
    # Create a lacrosse field plot
    ncaaw_field <- geom_lacrosse("ncaaw", field_units = "ft", rotation = 270)

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(ncaaw_field))
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

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(pll_field))
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

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(ncaam_field))
  }
)

test_that(
  "rectangular lacrosse zones tile the field without overlapping", {
    # Zone features for a rectangular field with a 10 unit neutral zone
    args <- list(field_length = 100, field_width = 60, nzone_length = 10)
    dzone <- do.call(lacrosse_defensive_zone, args)
    nzone <- lacrosse_neutral_zone(nzone_length = 10, field_width = 60)
    ozone <- do.call(lacrosse_offensive_zone, args)

    # The zones should abut at the neutral zone's edges
    expect_equal(range(dzone$x), c(-50, -5))
    expect_equal(range(nzone$x), c(-5, 5))
    expect_equal(range(ozone$x), c(5, 50))
  }
)
