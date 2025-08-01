test_that(
  "geom_baseball() returns a plot when called with a league", {
    # Create a baseball field plot
    mlb_field <- geom_baseball("mlb", rotation = -90)

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(mlb_field))
  }
)

test_that(
  "geom_baseball() can successfully transform coordinates", {
    # Create a baseball field plot
    little_league_field <- geom_baseball("little league", field_units = "m")

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(little_league_field))
  }
)

test_that(
  "geom_baseball() can successfully plot with all radii being 0", {
    suppressWarnings(
      # Create a baseball field plot
      ncaa_field <- geom_baseball(
        "ncaa",
        field_updates = list(
          infield_arc_radius = 0,
          pitchers_mound_radius = 0,
          home_plate_circle_radius = 0,
          base_anchor_to_infield_grass_radius = 0
        )
      )
    )

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(ncaa_field))
  }
)
