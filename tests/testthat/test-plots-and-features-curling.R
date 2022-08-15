test_that(
  "geom_curling() returns a plot when called with a league", {
    # Create a curling sheet plot
    wcf_sheet <- geom_curling("wcf")

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(wcf_sheet)[1], "gg")
    expect_equal(class(wcf_sheet)[2], "ggplot")
  }
)

test_that(
  "geom_curling() can successfully transform coordinates", {
    # Create a curling sheet plot
    wcf_sheet <- geom_curling("wcf", sheet_units = "in", rotation = 270)

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(wcf_sheet)[1], "gg")
    expect_equal(class(wcf_sheet)[2], "ggplot")
  }
)

test_that(
  "geom_curling() can successfully plot with all radii being 0", {
    # Create a curling sheet plot
    wcf_sheet <- geom_curling(
      "wcf",
      sheet_updates = list(
        house_ring_radii = rep(0, 3),
        button_radius = 0
      )
    )

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(wcf_sheet)[1], "gg")
    expect_equal(class(wcf_sheet)[2], "ggplot")
  }
)
