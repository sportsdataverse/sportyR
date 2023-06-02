test_that(
  glue::glue(
    "load_default_parameters() loads the correct variables into the global env"
  ),
  {
    load_default_parameters("ncaam")

    # The default league should be "ncaam"
    expect_equal(league, "ncaam")

    # The default display range should be "full"
    expect_equal(display_range, "full")

    # The default court, field, pitch, rink, and sheet updates lists should all
    # be empty lists
    expect_equal(court_updates, list())
    expect_equal(field_updates, list())
    expect_equal(pitch_updates, list())
    expect_equal(rink_updates, list())
    expect_equal(sheet_updates, list())

    # The default color_updates should also be an empty list
    expect_equal(color_updates, list())

    # The default court, field, pitch, rink, and sheet units should all be NULL
    expect_null(court_units)
    expect_null(field_units)
    expect_null(pitch_units)
    expect_null(rink_units)
    expect_null(sheet_units)

    # The default translations should be 0 units, and the default rotation
    # should be 0°
    expect_equal(x_trans, 0)
    expect_equal(y_trans, 0)
    expect_equal(rotation, 0)

    # The default plot limits should be NULL
    expect_null(xlims)
    expect_null(ylims)
  }

)
