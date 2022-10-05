test_that(
  "geom_basketball() returns a plot when called with a league", {
    # Create a basketball court plot
    nba_court <- geom_basketball("nba")
    ncaa_court <- geom_basketball(
      "ncaa",
      color_updates = list(
        court_apron = "#0088ceab"
      )
    )

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(nba_court)[1], "gg")
    expect_equal(class(nba_court)[2], "ggplot")
    expect_equal(class(ncaa_court)[1], "gg")
    expect_equal(class(ncaa_court)[2], "ggplot")
  }
)

test_that(
  "geom_basketball() can successfully transform coordinates", {
    # Create a basketball court plot
    fiba_court <- geom_basketball("fiba", court_units = "ft", rotation = 270)

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(fiba_court)[1], "gg")
    expect_equal(class(fiba_court)[2], "ggplot")
  }
)

test_that(
  "geom_basketball() can successfully plot with all radii being 0", {
    # Create a basketball court plot
    suppressWarnings(
      wnba_court <- geom_basketball(
        "wnba",
        court_updates = list(
          basket_center_to_three_point_arc = 0.1667,
          free_throw_circle_radius = 0,
          symmetric_inbounding_line = FALSE,
          basket_ring_inner_radius = 0
        )
      )
    )

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(wnba_court)[1], "gg")
    expect_equal(class(wnba_court)[2], "ggplot")
  }
)
