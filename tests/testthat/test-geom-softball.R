test_that("geom_softball produces a valid ggplot object", {
  p <- geom_softball(league = "NCAA")
  
  expect_s3_class(p, "ggplot")
  expect_true("ggplot" %in% class(p))
})

test_that("geom_softball works across supported leagues", {
  expect_silent(geom_softball(league = "NCAA"))
  expect_silent(geom_softball(league = "WBSC"))
  expect_silent(geom_softball(league = "AUSL"))
})

test_that("geom_softball respects display ranges and rotations", {
  p_infield <- geom_softball(league = "NCAA", display_range = "infield")
  p_rotated <- geom_softball(league = "NCAA", rotation = 90)
  
  expect_s3_class(p_infield, "ggplot")
  expect_s3_class(p_rotated, "ggplot")
})

test_that("softball_features_set_colors returns a named list", {
  colors <- softball_features_set_colors()
  
  expect_type(colors, "list")
  expect_named(
    colors,
    c("plot_background", "outfield_grass", "infield_dirt", "infield_grass",
      "pitching_circle", "bases", "foul_lines", "fence")
  )
})

test_that("geom_softball errors cleanly on missing or invalid league", {
  expect_error(geom_softball())
  expect_error(geom_softball(league = "INVALID_LEAGUE"))
})