test_that(
  "geom_soccer() returns a plot when called with a league", {
    # Create a soccer pitch plot
    epl_pitch <- geom_soccer("epl")

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(epl_pitch)[1], "gg")
    expect_equal(class(epl_pitch)[2], "ggplot")
  }
)

test_that(
  "geom_soccer() can successfully transform coordinates", {
    # Create a soccer pitch plot
    nwsl_pitch <- geom_soccer("nwsl", pitch_units = "in", rotation = 270)

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(nwsl_pitch)[1], "gg")
    expect_equal(class(nwsl_pitch)[2], "ggplot")
  }
)

test_that(
  "geom_soccer() can successfully plot with all radii being 0", {
    # Create a soccer pitch plot
    nwsl_pitch <- geom_soccer(
      "fifa",
      pitch_updates = list(
        penalty_circle_radius = 0
      )
    )

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(nwsl_pitch)[1], "gg")
    expect_equal(class(nwsl_pitch)[2], "ggplot")
  }
)

test_that(
  glue::glue(
    "soccer_corner_defensive_marks() returns an empty data frame if feature ",
    "is neither goal line or touchline"
  ), {
    # Instantiate the feature
    test_feature <- soccer_corner_defensive_marks(
      is_touchline = FALSE,
      is_goal_line = FALSE
    )

    expect_equal(nrow(test_feature), 0)
  }
)

test_that(
  "geom_soccer() can successfully plot with penalty box radius of 1", {
    # Create a soccer pitch plot
    suppressWarnings(
      nwsl_pitch <- geom_soccer(
        "mls",
        pitch_updates = list(
          penalty_circle_radius = 1
        )
      )
    )

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(nwsl_pitch)[1], "gg")
    expect_equal(class(nwsl_pitch)[2], "ggplot")
  }
)
