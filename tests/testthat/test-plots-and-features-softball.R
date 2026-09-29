test_that(
  "geom_softball() returns a plot when called with a league", {
    # Create a softball field plot
    ncaa_field <- geom_softball("ncaa", rotation = -90)

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(ncaa_field))
  }
)

test_that(
  "geom_softball() can successfully transform coordinates", {
    # Create a softball field plot
    ncaa_field <- geom_softball("ncaa", field_units = "m")

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(ncaa_field))
  }
)

test_that(
  "geom_softball() can plot the infield display range", {
    # Create a softball field plot
    ncaa_field <- geom_softball("ncaa", display_range = "infield")

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(ncaa_field))
  }
)

test_that(
  "geom_softball() can successfully plot with all radii being 0", {
    suppressWarnings(
      # Create a softball field plot
      ncaa_field <- geom_softball(
        "ncaa",
        field_updates = list(
          infield_arc_radius = 0,
          home_plate_circle_radius = 0,
          pitchers_circle_radius = 0
        )
      )
    )

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(ncaa_field))
  }
)

test_that(
  "geom_softball() can plot a custom field", {
    suppressWarnings(
      # Create a softball field plot
      custom_field <- geom_softball(
        "custom",
        field_updates = list(
          field_units = "ft",
          left_field_distance = 200,
          right_field_distance = 200,
          center_field_distance = 225,
          baseline_distance = 60,
          pitchers_plate_front_to_home_plate = 40
        )
      )
    )

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(custom_field))
  }
)
