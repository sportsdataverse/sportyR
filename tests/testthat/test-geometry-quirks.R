skip_on_cran()

# Layers of a sportyR plot drawn with the given fill colour
layers_filled <- function(p, fill) {
  Filter(function(l) identical(l$aes_params$fill, fill), p$layers)
}

test_that("NLL and World Lacrosse neutral zones span the restraining lines (#60)", {
  for (league in c("nll", "world lacrosse")) {
    p <- geom_lacrosse(league, color_updates = list(neutral_zone = "#123456"))
    nz <- layers_filled(p, "#123456")
    expect_length(nz, 1)
    expect_equal(range(nz[[1]]$data$x), c(-42.5, 42.5))
  }
})

test_that("penalty box fills start at the inside edge of the boards (#61)", {
  p <- geom_lacrosse(
    "nll",
    color_updates = list(
      team_a_penalty_box = "#123456",
      team_b_penalty_box = "#654321"
    )
  )
  # penalty_box_separation / 2 + boards_thickness = 4 + 0.25
  expect_equal(max(layers_filled(p, "#123456")[[1]]$data$x), -4.25)
  expect_equal(min(layers_filled(p, "#654321")[[1]]$data$x), 4.25)
})
