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

test_that("goal-fan hash marks sit on the fan, perpendicular, at arc distance s (#62)", {
  params <- surface_dimensions[["lacrosse"]][["ncaaw"]]
  r <- params$goal_fan_radius + params$goal_circle_radius
  center_x <- (params$field_length / 2) - params$goal_line_center_to_end_line +
    params$goal_circle_radius

  p <- geom_lacrosse("ncaaw", color_updates = list(goal_fan_hash_mark = "#123456"))
  marks <- layers_filled(p, "#123456")

  # The right-hand, upper copy of every mark (x > 0, y >= 0)
  marks <- Filter(
    function(l) mean(l$data$x) > 0 && mean(l$data$y) >= -1e-9,
    marks
  )
  arc_distances <- vapply(marks, function(l) {
    v <- l$data[1:4, c("x", "y")]
    centre <- colMeans(v)
    radial <- centre - c(center_x, 0)

    # Centre on the fan arc
    expect_equal(sqrt(sum(radial^2)), r, tolerance = 1e-6)

    # Long axis along the radius (perpendicular to the arc)
    e1 <- unlist(v[2, ] - v[1, ])
    e2 <- unlist(v[3, ] - v[2, ])
    long <- if (sum(e1^2) > sum(e2^2)) e1 else e2
    cross <- long[1] * radial[2] - long[2] * radial[1]
    expect_equal(unname(cross), 0, tolerance = 1e-6)

    r * atan2(radial[2], -radial[1])
  }, numeric(1))

  expect_equal(
    sort(unname(arc_distances)),
    sort(params$goal_fan_hash_mark_separation_from_center),
    tolerance = 1e-6
  )
})

test_that("display_range = \"full\" contains every drawn feature (#63)", {
  leagues <- setdiff(names(surface_dimensions[["lacrosse"]]), "custom")
  for (league in leagues) {
    p <- geom_lacrosse(league)
    xs <- unlist(lapply(p$layers, function(l) l$data$x))
    ys <- unlist(lapply(p$layers, function(l) l$data$y))
    lims <- p$coordinates$limits
    expect_true(min(xs) >= lims$x[1] - 1e-9, label = league)
    expect_true(max(xs) <= lims$x[2] + 1e-9, label = league)
    expect_true(min(ys) >= lims$y[1] - 1e-9, label = league)
    expect_true(max(ys) <= lims$y[2] + 1e-9, label = league)
  }
})

test_that("a goal arc added without a thickness is not drawn as a solid block (#64)", {
  area <- function(d) {
    abs(sum(d$x * c(d$y[-1], d$y[1]) - c(d$x[-1], d$x[1]) * d$y)) / 2
  }
  p <- geom_lacrosse(
    "ncaam",
    field_updates = list(goal_arc_radius = 15, goal_arc_extension = 15),
    color_updates = list(goal_arc = "#123456")
  )
  arcs <- layers_filled(p, "#123456")
  expect_length(arcs, 2)
  for (arc in arcs) {
    expect_equal(area(arc$data), 0, tolerance = 1e-6)
  }
})

test_that("a user-supplied centre face-off marker colour is kept (#65)", {
  p <- geom_lacrosse(
    "ncaam",
    color_updates = list(center_face_off_marker = "#ff0000")
  )
  expect_length(layers_filled(p, "#ff0000"), 1)
  expect_length(layers_filled(p, "#ffcb05"), 0)

  # Without a user colour the contrasting default still applies
  p <- geom_lacrosse("ncaam")
  expect_length(layers_filled(p, "#ffcb05"), 1)
})

test_that("the NCAA football coaching line is six feet outside the sideline (#66)", {
  params <- surface_dimensions[["football"]][["ncaa"]]

  # Rule 1-2-2: measured from the inside edge of the sideline
  coaching_line <- params$boundary_line_thickness + params$restricted_area_width
  expect_equal(coaching_line, 2.0, tolerance = 1e-4)

  # Rule 1-2-3-a: the limit line (back of the coaching box) is 12 feet out
  expect_equal(coaching_line + params$coaching_box_width, 4.0, tolerance = 1e-4)

  # The drawn coaching box starts at the coaching line
  p <- geom_football("ncaa", color_updates = list(coaching_box = "#123456"))
  box <- layers_filled(p, "#123456")[[1]]$data
  expect_equal(
    min(box$y),
    (params$field_width / 2) + 2.0,
    tolerance = 1e-4
  )
})

# Default fills of the border-outline layers, located by tagging them with a
# unique colour and reading the same layers from the default plot
outline_fills <- function(league) {
  tagged <- geom_football(
    league,
    color_updates = list(
      field_border_outline = "#123456",
      red_zone_border_outline = "#654321"
    )
  )
  default <- geom_football(league)
  fills <- vapply(tagged$layers, function(l) {
    if (is.null(l$aes_params$fill)) NA_character_ else l$aes_params$fill
  }, character(1))
  idx <- which(fills %in% c("#123456", "#654321"))
  expect_true(length(idx) > 0)
  for (i in idx) {
    expect_equal(default$layers[[i]]$data, tagged$layers[[i]]$data)
  }
  vapply(default$layers[idx], function(l) l$aes_params$fill, character(1))
}

test_that("bordered football fields draw a white border outline by default (#67)", {
  expect_true(surface_dimensions[["football"]][["ncaa"]]$field_bordered)
  expect_true(all(outline_fills("ncaa") == "#ffffff"))
  expect_true(all(outline_fills("nfl") == "#ffffff"))
  expect_true(all(outline_fills("nfhs11") == "#ffffff"))
})

test_that("unbordered football fields keep the transparent outline (#67)", {
  expect_false(surface_dimensions[["football"]][["cfl"]]$field_bordered)
  expect_true(all(outline_fills("cfl") == "#ffffff00"))

  # The fills are unchanged
  colors <- football_features_set_colors()
  expect_equal(colors$field_border, "#196f0c")
  expect_equal(colors$red_zone_border, "#196f0c00")
})

test_that("a user border outline colour wins on any field (#67)", {
  for (league in c("cfl", "ncaa")) {
    p <- geom_football(league, color_updates = list(field_border_outline = "#ff0000"))
    expect_true(length(layers_filled(p, "#ff0000")) > 0, label = league)
  }
})
