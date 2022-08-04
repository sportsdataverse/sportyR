test_that(
  "geom_hockey() returns a plot when called with a league", {
    # Create a hockey rink plot
    nhl_rink <- geom_hockey("nhl")

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(nhl_rink)[1], "gg")
    expect_equal(class(nhl_rink)[2], "ggplot")
  }
)

test_that(
  "geom_hockey() can successfully transform coordinates", {
    # Create a hockey rink plot
    iihf_rink <- geom_hockey("iihf", rink_units = "ft", rotation = 270)

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(iihf_rink)[1], "gg")
    expect_equal(class(iihf_rink)[2], "ggplot")
  }
)

test_that(
  "geom_hockey() can successfully plot with all radii being 0", {
    # Create a hockey rink plot
    suppressWarnings(
      phf_rink <- geom_hockey(
        "phf",
        rink_updates = list(
          faceoff_circle_radius = 0,
          noncenter_faceoff_spot_radius = 0,
          goal_crease_radius = 0,
          corner_radius = 0
        )
      )
    )

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(phf_rink)[1], "gg")
    expect_equal(class(phf_rink)[2], "ggplot")
  }
)
