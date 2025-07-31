test_that(
  "geom_badminton() returns a plot when called with a league", {
    # Create a badminton court plot
    bwf_court <- geom_badminton("bwf")

    # Check the class of the resulting plot. This should be "gg" and "ggplot"
    expect_equal(class(bwf_court)[1], "gg")
    expect_equal(class(bwf_court)[2], "ggplot")
  }
)
