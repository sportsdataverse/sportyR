test_that(
  "geom_hockey() returns a plot when called with a league", {
    # Create a hockey rink plot
    nhl_rink <- geom_hockey("nhl")

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(nhl_rink))
  }
)

test_that(
  "geom_hockey() can successfully transform coordinates", {
    # Create a hockey rink plot
    iihf_rink <- geom_hockey("iihf", rink_units = "ft", rotation = 270)

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(iihf_rink))
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

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(phf_rink))
  }
)
