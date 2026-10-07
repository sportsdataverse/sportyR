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
    # Create the same softball field plot in feet and in meters
    ncaa_field_ft <- geom_softball("ncaa", display_range = "infield")
    ncaa_field_m <- geom_softball(
      "ncaa",
      display_range = "infield",
      field_units = "m"
    )

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(ncaa_field_m))

    # The plot limits, including the padding around the field, should be the
    # same as the plot in feet once converted to meters
    expect_equal(
      ncaa_field_m$coordinates$limits$x,
      ncaa_field_ft$coordinates$limits$x * 0.3048
    )
    expect_equal(
      ncaa_field_m$coordinates$limits$y,
      ncaa_field_ft$coordinates$limits$y * 0.3048
    )

    # Every feature should be in the same place as on the plot in feet once
    # converted to meters (e.g. the bases and pitcher's plate should not be
    # left at their distances in feet)
    expect_equal(length(ncaa_field_m$layers), length(ncaa_field_ft$layers))
    for (i in seq_along(ncaa_field_ft$layers)) {
      expect_equal(
        ncaa_field_m$layers[[i]]$data[, c("x", "y")],
        ncaa_field_ft$layers[[i]]$data[, c("x", "y")] * 0.3048,
        ignore_attr = TRUE
      )
    }
  }
)

test_that(
  "geom_softball() sets the correct full-field display range", {
    # Create a softball field plot
    ncaa_field <- geom_softball("ncaa")

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(ncaa_field))

    # The x limits should extend to the ends of the 190-foot foul lines, and the
    # y limits from 5 feet behind the 25-foot backstop to 5 feet beyond the
    # 220-foot center field fence
    expect_equal(
      ncaa_field$coordinates$limits$x,
      c(-190 * cos(pi / 4), 190 * cos(pi / 4))
    )
    expect_equal(ncaa_field$coordinates$limits$y, c(-30, 225))
  }
)

test_that(
  "geom_softball() can plot the infield display range", {
    # Create a softball field plot
    ncaa_field <- geom_softball("ncaa", display_range = "infield")

    # Check the class of the resulting plot. This should be a ggplot object
    expect_true(ggplot2::is_ggplot(ncaa_field))

    # The x limits should extend 5 feet beyond the 60-foot infield arc on both
    # sides, and the y limits from 5 feet behind the 13-foot home plate circle
    # to 5 feet beyond the infield arc (43 + 60 feet from home plate)
    expect_equal(ncaa_field$coordinates$limits$x, c(-65, 65))
    expect_equal(ncaa_field$coordinates$limits$y, c(-18, 108))
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
