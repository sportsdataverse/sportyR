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
  # The fan's arc has radius goal_fan_radius about the center of the goal line
  # (#69)
  r <- params$goal_fan_radius
  center_x <- (params$field_length / 2) - params$goal_line_center_to_end_line

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

test_that("the women's 8-meter arc follows NCAA Rule 1, Sections 6-7 (#69)", {
  for (league in c("ncaaw", "usaw")) {
    params <- surface_dimensions[["lacrosse"]][[league]]
    r <- params$goal_fan_radius
    g <- params$goal_circle_radius
    goal_x <- (params$field_length / 2) - params$goal_line_center_to_end_line

    p <- geom_lacrosse(league, color_updates = list(goal_fan = "#123456"))
    fan <- Filter(
      function(l) mean(l$data$x) > 0,
      layers_filled(p, "#123456")
    )[[1]]$data
    x <- fan$x - goal_x
    y <- fan$y
    d <- sqrt(x^2 + y^2)

    # The arc is 10.6 m from the center of the goal line, never further
    expect_equal(max(d), r, tolerance = 1e-6, label = league)

    # The ends start on the goal circle where the goal line extended crosses it
    # (B) and run at 45 degrees to the goal line extended to the arc (C)
    u <- (-g + sqrt(g^2 + 2 * (r^2 - g^2))) / 2
    nearest <- function(px, py) min(sqrt((x - px)^2 + (y - py)^2))
    for (s in c(-1, 1)) {
      expect_lt(nearest(0, s * g), 1e-6)
      expect_lt(nearest(-u, s * (g + u)), 1e-6)
    }

    # Section 7: the two marks on the goal line extended are 30.5 cm long
    expect_equal(params$goal_mouth_hash_mark_length, 0.305)
  }

  # A field with no goal fan draws nothing for it (and "custom" now draws)
  expect_equal(nrow(lacrosse_goal_fan(goal_circle_radius = 3)), 0)
  expect_no_error(geom_lacrosse("custom"))
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

test_that("the PLL two-point arc follows the 2024 PLL Rulebook, II.G (#70)", {
  params <- surface_dimensions[["lacrosse"]][["pll"]]
  goal_x <- (params$field_length / 2) - params$goal_line_center_to_end_line

  p <- geom_lacrosse("pll", color_updates = list(goal_arc = "#123456"))
  arc <- Filter(
    function(l) mean(l$data$x) > 0,
    layers_filled(p, "#123456")
  )[[1]]$data
  x <- arc$x - goal_x
  y <- arc$y
  nearest <- function(px, py) min(sqrt((x - px)^2 + (y - py)^2))

  # Parallel lines 13 yd from the center of the goal line, from the end line...
  expect_equal(max(abs(y)), 13)
  for (s in c(-1, 1)) {
    expect_lt(nearest(params$goal_line_center_to_end_line, s * 13), 1e-9)
    # ...to where they are 15 yd from the center of the goal line
    expect_lt(nearest(-sqrt(15^2 - 13^2), s * 13), 1e-9)
  }

  # A uniform 15 yd arc between them: every point in front of where the lines
  # meet the arc lies on its outer (15 yd) or inner edge
  d <- sqrt(x^2 + y^2)
  t <- params$goal_arc_line_thickness
  front <- x < -sqrt(15^2 - 13^2) - 1e-9
  expect_true(all(abs(d[front] - 15) < 1e-9 | abs(d[front] - (15 - t)) < 1e-9))
  expect_equal(min(x), -15, tolerance = 1e-4)

  # Without the key the arc falls back to a semi-circle: every point in front
  # of the goal line is on its outer (15 yd) or inner (15 - t) edge
  semi <- lacrosse_goal_arc(
    goal_arc_extension = 15,
    goal_arc_radius = 15,
    line_thickness = t
  )
  semi_d <- sqrt(semi$x^2 + semi$y^2)
  semi_front <- semi$x < -1e-9
  expect_true(any(semi_front))
  expect_true(all(
    abs(semi_d[semi_front] - 15) < 1e-9 |
      abs(semi_d[semi_front] - (15 - t)) < 1e-9
  ))
  expect_equal(max(abs(semi$y)), 15)
})

test_that("the NFL field border follows the bench's slanted side (#71)", {
  q <- surface_dimensions[["football"]][["nfl"]]
  args <- list(
    field_length = q$field_length,
    field_width = q$field_width,
    endzone_length = q$endzone_length,
    boundary_line_thickness = q$boundary_line_thickness,
    restricted_area_length = q$team_bench_length_field_side,
    restricted_area_width = q$restricted_area_width,
    coaching_box_length = q$team_bench_length_field_side,
    coaching_box_width = q$coaching_box_width,
    team_bench_length_field_side = q$team_bench_length_field_side,
    team_bench_length_back_side = q$team_bench_length_back_side,
    team_bench_width = q$team_bench_width,
    team_bench_area_border_thickness = q$team_bench_area_border_thickness,
    field_border_thickness = q$field_border_thickness,
    surrounds_team_bench_area = TRUE,
    bench_shape = q$bench_shape
  )
  border <- do.call(
    football_field_border,
    c(
      args[names(args) != "field_border_thickness"],
      list(feature_thickness = q$field_border_thickness)
    )
  )
  outline <- do.call(
    football_field_border_outline,
    c(args, list(feature_thickness = q$minor_line_thickness))
  )

  # The border's outer corner behind the bench, and the next vertex down its
  # outer edge towards the field
  top <- which(abs(border$y - max(border$y)) < 1e-9)
  i <- top[which.max(border$x[top])]
  j <- c(i - 1, i + 1)[border$y[c(i - 1, i + 1)] < max(border$y) - 1e-9][1]
  edge <- (border$x[j] - border$x[i]) / (border$y[i] - border$y[j])

  # Parallel to the bench's slanted side
  bench_side <- (
    (q$team_bench_length_field_side - q$team_bench_length_back_side) / 2
  ) / q$team_bench_width
  expect_equal(edge, bench_side, tolerance = 1e-9)

  # The white outline starts from that same corner
  expect_true(any(
    abs(outline$x - border$x[i]) < 1e-9 & abs(outline$y - border$y[i]) < 1e-9
  ))

  # So does the apron's bump round the bench (19.14685 yd from center)
  apron <- football_field_apron(
    field_length = q$field_length,
    field_width = q$field_width,
    endzone_length = q$endzone_length,
    boundary_thickness = q$boundary_line_thickness,
    field_border_thickness = q$field_border_thickness,
    restricted_area_length = q$team_bench_length_field_side,
    restricted_area_width = q$restricted_area_width,
    coaching_box_length = q$team_bench_length_field_side,
    coaching_box_width = q$coaching_box_width,
    team_bench_length_field_side = q$team_bench_length_field_side,
    team_bench_length_back_side = q$team_bench_length_back_side,
    team_bench_width = q$team_bench_width,
    team_bench_area_border_thickness = q$team_bench_area_border_thickness,
    extra_apron_padding = q$extra_apron_padding,
    bench_shape = q$bench_shape
  )
  bump <- abs(apron$y - border$y[i]) < 1e-9
  expect_equal(max(apron$x[bump]), border$x[i], tolerance = 1e-9)
  expect_equal(border$x[i], 19.14685, tolerance = 1e-6)

  # Rectangular benches keep their corner at the bench's field-side end
  n <- surface_dimensions[["football"]][["ncaa"]]
  rect <- football_field_border(
    field_width = n$field_width,
    feature_thickness = n$field_border_thickness,
    boundary_line_thickness = n$boundary_line_thickness,
    restricted_area_width = n$restricted_area_width,
    coaching_box_width = n$coaching_box_width,
    team_bench_length_field_side = n$team_bench_length_field_side,
    team_bench_length_back_side = n$team_bench_length_back_side,
    team_bench_width = n$team_bench_width,
    team_bench_area_border_thickness = n$team_bench_area_border_thickness,
    surrounds_team_bench_area = TRUE,
    bench_shape = n$bench_shape
  )
  expect_equal(
    max(rect$x[abs(rect$y - max(rect$y)) < 1e-9]),
    (n$team_bench_length_field_side / 2) +
      n$team_bench_area_border_thickness +
      n$field_border_thickness
  )
})

test_that("each NLL / World Lacrosse penalty box is penalty_box_length inside (#72)", {
  for (league in c("nll", "world lacrosse")) {
    params <- surface_dimensions[["lacrosse"]][[league]]
    t <- params$boards_thickness
    outline <- lacrosse_penalty_box_outline(
      penalty_box_outline_thickness = t,
      penalty_box_length = params$penalty_box_length,
      penalty_box_separation = params$penalty_box_separation,
      penalty_box_depth = params$penalty_box_depth
    )

    # Walls along x: the divider ends at separation / 2 + t, the outer wall is
    # one wall thickness thick, and the box between them is 8 ft inside
    divider_inside <- (params$penalty_box_separation / 2) + t
    outer_wall_inside <- max(outline$x) - t
    expect_equal(outer_wall_inside - divider_inside, params$penalty_box_length)

    # 8 + 8 + 8 = 24 ft of interior (NLL Rule 8.1, reading (a))
    expect_equal(
      (2 * (outer_wall_inside - divider_inside)) + params$penalty_box_separation,
      24
    )

    # The fill (#61) sits exactly inside the walls
    p <- geom_lacrosse(league, color_updates = list(team_b_penalty_box = "#654321"))
    fill <- layers_filled(p, "#654321")[[1]]$data
    expect_equal(range(fill$x), c(divider_inside, outer_wall_inside))
  }
})
