skip_on_cran()

# Layers of a sportyR plot drawn with the given fill colour
layers_filled <- function(p, fill) {
  Filter(function(l) identical(l$aes_params$fill, fill), p$layers)
}

# Count the proper crossings (two edges that strictly cross) in a closed ring
count_crossings <- function(ring, tolerance = 1e-9) {
  n <- nrow(ring) - 1
  if (n < 3) {
    return(0)
  }
  from <- ring[-nrow(ring), , drop = FALSE]
  to <- ring[-1, , drop = FALSE]

  # Signed area of the triangle (a, b, c); its sign says which side of the edge
  # a -> b the point c is on
  side <- function(a, b, c) {
    (b[, 1] - a[, 1]) * (c[, 2] - a[, 2]) - (b[, 2] - a[, 2]) * (c[, 1] - a[, 1])
  }

  crossings <- 0
  for (i in seq_len(n - 2)) {
    # Skip the edge before and after edge i, which share an endpoint with it
    j <- (i + 2):n
    if (i == 1) {
      j <- j[j != n]
    }
    if (length(j) == 0) {
      next
    }
    ai <- from[rep(i, length(j)), , drop = FALSE]
    bi <- to[rep(i, length(j)), , drop = FALSE]
    crossed_i <- side(ai, bi, from[j, , drop = FALSE]) *
      side(ai, bi, to[j, , drop = FALSE]) < -tolerance^2
    crossed_j <- side(from[j, , drop = FALSE], to[j, , drop = FALSE], ai) *
      side(from[j, , drop = FALSE], to[j, , drop = FALSE], bi) < -tolerance^2
    crossings <- crossings + sum(crossed_i & crossed_j)
  }

  crossings
}

# Count the vertices of a closed ring where the path turns straight back along
# the edge it just travelled. These zero-width spikes are not proper crossings,
# so count_crossings() does not see them
count_reversals <- function(ring, tolerance = 1e-9) {
  if (any(abs(ring[1, ] - ring[nrow(ring), ]) > tolerance)) {
    ring <- rbind(ring, ring[1, ])
  }
  # Drop the repeated closing point so that every vertex has two neighbours
  vertices <- ring[-nrow(ring), , drop = FALSE]
  n <- nrow(vertices)
  previous <- vertices[c(n, seq_len(n - 1)), , drop = FALSE]
  following <- vertices[c(seq_len(n)[-1], 1), , drop = FALSE]
  incoming <- vertices - previous
  outgoing <- following - vertices

  # Parallel edges (zero cross product) that point in opposite directions
  cross_product <- incoming[, 1] * outgoing[, 2] - incoming[, 2] * outgoing[, 1]
  dot_product <- rowSums(incoming * outgoing)
  lengths <- sqrt(rowSums(incoming^2)) * sqrt(rowSums(outgoing^2))
  sum(abs(cross_product) <= tolerance * lengths & dot_product < 0)
}

# Count the rings that cross themselves, over all the layers of a plot
crossing_rings <- function(p) {
  unlist(lapply(p$layers, function(l) {
    vapply(split_rings(l$data), count_crossings, numeric(1))
  }))
}

# Layers that were drawn with no area and no visible outline
invisible_empty_layers <- function(p) {
  Filter(function(l) {
    colour <- l$aes_params$colour
    outline_visible <- !is.na(colour) && substr(colour, 8, 9) != "00"
    !outline_visible && !has_area(l$data)
  }, p$layers)
}

test_that("create_circle() puts the quarter points exactly on the axes (#79)", {
  half <- create_circle(r = 15, start = 0.5, end = 1.5, npoints = 101)
  expect_identical(half$x[1], 0)
  expect_identical(half$y[1], 15)
  expect_identical(half$x[101], 0)
  expect_identical(half$y[101], -15)
})

test_that("split_rings() and has_area() read back-to-back rings (#79)", {
  square <- create_square(2)
  two_squares <- rbind(square, create_square(2, center = c(5, 0)))
  expect_length(split_rings(two_squares), 2)
  expect_true(has_area(two_squares))

  # Opposite windings must not cancel
  expect_true(has_area(rbind(square, create_square(2, c(5, 0))[5:1, ])))

  # A line, a single point and a collapsed rectangle have no area
  expect_false(has_area(data.frame(x = c(0, 1, 0), y = c(0, 0, 0))))
  expect_false(has_area(data.frame(x = 0, y = 0)))

  # Repeats of one point are still a single ring, not an empty list
  expect_length(split_rings(data.frame(x = c(1, 1, 1), y = c(2, 2, 2))), 1)
  expect_false(has_area(data.frame(x = c(1, 1, 1), y = c(2, 2, 2))))
  expect_false(has_area(create_rectangle(-100, 100, 0, 0)))
})

test_that("the NHL trapezoid is two simple strips from the goal line (#79)", {
  df <- hockey_goaltenders_restricted_area(
    rink_length = 200,
    feature_thickness = 2 / 12,
    short_base_width = 22,
    long_base_width = 28,
    x_anchor = 89
  )
  rings <- split_rings(df)
  expect_length(rings, 2)
  expect_equal(vapply(rings, count_crossings, numeric(1)), c(0, 0))
  expect_equal(vapply(rings, count_reversals, numeric(1)), c(0, 0))
  expect_equal(vapply(rings, nrow, integer(1)), c(5L, 5L))

  # Each strip is its own polygon group, so the plot does not join the strips
  expect_equal(unique(df$group), 1:2)
  expect_equal(df$group, rep(1:2, each = 5))
})

test_that("the mirrored faceoff lines keep their thickness on the same side (#79)", {
  p <- geom_hockey("nhl", color_updates = list(faceoff_line = "#123456"))
  expect_true(all(crossing_rings(p) == 0))

  # Both sets of lines are drawn in all four quadrants. If the mirrored set
  # had its thickness on the wrong side its rings would differ in area
  # from the first set's (the upright would span [-2, -1.833] and cross)
  lines <- layers_filled(p, "#123456")
  expect_length(lines, 8)
  # Newer ggplot2 names its layers, so the names are dropped before comparing
  areas <- unname(vapply(lines, function(l) {
    sum(vapply(split_rings(l$data), function(ring) {
      n <- nrow(ring)
      abs(sum(ring[-n, 1] * ring[-1, 2] - ring[-1, 1] * ring[-n, 2])) / 2
    }, numeric(1)))
  }, numeric(1)))
  expect_equal(areas, rep(areas[1], 8))
  for (l in lines) {
    expect_equal(vapply(split_rings(l$data), nrow, integer(1)), c(7L, 7L))
  }
})

test_that("the NHL goal crease's inner arc meets the inner straight sides (#79)", {
  df <- hockey_goal_crease_outline(
    feature_radius = 6,
    feature_thickness = 2 / 12,
    crease_style = "nhl98",
    crease_length = 4.5,
    crease_width = 8,
    notch_dist_x = 4,
    notch_width = 0.5
  )
  expect_equal(sum(vapply(split_rings(df), count_crossings, numeric(1))), 0)
})

test_that("circle-based hockey features are simple closed rings (#79)", {
  circle <- hockey_center_faceoff_circle(
    feature_radius = 15,
    feature_thickness = 2 / 12
  )
  expect_equal(count_crossings(as.matrix(circle)), 0)
  expect_equal(count_reversals(as.matrix(circle)), 0)
  expect_equal(circle[nrow(circle), ], circle[1, ], ignore_attr = TRUE)

  spot_ring <- hockey_nodzone_faceoff_spot_ring(
    feature_radius = 1,
    feature_thickness = 2 / 12
  )
  rings <- split_rings(spot_ring)
  expect_length(rings, 2)
  expect_equal(vapply(rings, count_crossings, numeric(1)), c(0, 0))
})

test_that("lacrosse goal and bench outlines close on their first point (#79)", {
  frame <- lacrosse_goal_frame(4.75, 1 / 24, 3.82)
  net <- lacrosse_goal_net(4.75, 1 / 24, 3.82)
  bench <- lacrosse_player_bench_outline(0.25, 20, 5)
  bench_hockey <- hockey_player_bench_outline(0.25, 20, 5)

  for (df in list(frame, net, bench, bench_hockey)) {
    expect_equal(df[nrow(df), ], df[1, ], ignore_attr = TRUE)
    expect_length(split_rings(df), 1)
    expect_equal(count_crossings(as.matrix(df)), 0)
    expect_equal(count_reversals(as.matrix(df)), 0)
    expect_true(has_area(df))
  }
})

test_that("hockey features are simple rings in every league but the OHL (#79)", {
  # OHL: the nhl92 crease style overlaps the L-marks on its arc
  for (league in setdiff(names(surface_dimensions[["hockey"]]), c("ohl", "custom"))) {
    p <- geom_hockey(league)
    expect_true(all(crossing_rings(p) == 0), info = league)
  }
})

test_that("lacrosse features are simple rings where the goal mouth is a ring (#79)", {
  # NCAA and USA Lacrosse women's goal mouths overlap their own end caps
  for (league in c("nll", "world lacrosse", "pll", "usam")) {
    p <- geom_lacrosse(league)
    expect_true(all(crossing_rings(p) == 0), info = league)
  }
})

test_that("features with no area and no outline are not drawn (#79)", {
  for (league in c("nhl", "nll")) {
    p <- if (league == "nhl") geom_hockey(league) else geom_lacrosse(league)
    expect_length(invisible_empty_layers(p), 0)
  }
})

test_that("add_feature() keeps a no-area feature only when its outline is visible (#79)", {
  line <- create_rectangle(-1, 1, 0, 0)
  square <- create_square(2)
  n_layers <- function(feature_df, outline) {
    p <- add_feature(
      ggplot2::ggplot(),
      x_anchor = 0,
      y_anchor = 0,
      feature_df = feature_df,
      feature_color = "#123456",
      feature_outline_color = outline,
      reflect_x = FALSE,
      reflect_y = FALSE
    )
    length(p$layers)
  }

  # No area and no outline: skipped
  expect_equal(n_layers(line, "#ffffff00"), 0)
  # No area but a visible outline: drawn as a stroke, as before
  expect_equal(n_layers(line, "#ff0000"), 1)
  # Has area: drawn whatever the outline is
  expect_equal(n_layers(square, "#ffffff00"), 1)
})
