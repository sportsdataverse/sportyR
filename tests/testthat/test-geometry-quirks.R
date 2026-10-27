skip_on_cran()

# Layers of a sportyR plot drawn with the given fill colour
layers_filled <- function(p, fill) {
  Filter(function(l) identical(l$aes_params$fill, fill), p$layers)
}

test_that("NLL and World Lacrosse neutral zones span the restraining lines", {
  for (league in c("nll", "world lacrosse")) {
    p <- geom_lacrosse(league, color_updates = list(neutral_zone = "#123456"))
    nz <- layers_filled(p, "#123456")
    expect_length(nz, 1)
    expect_equal(range(nz[[1]]$data$x), c(-42.5, 42.5))
  }
})

test_that("penalty box fills start at the inside edge of the boards", {
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

test_that("goal-fan hash marks sit on the fan, perpendicular, at arc distance s", {
  params <- surface_dimensions[["lacrosse"]][["ncaaw"]]
  # The fan's arc has radius goal_fan_radius about the center of the goal line
  # and the marks sit on the middle of its line
  r <- params$goal_fan_radius - (params$goal_fan_line_thickness / 2)
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

test_that("the women's 8-meter arc follows NCAA Rule 1, Sections 6-7", {
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

test_that("display_range = \"full\" contains every drawn feature", {
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

test_that("a goal arc added without a thickness is not drawn as a solid block", {
  p <- geom_lacrosse(
    "ncaam",
    field_updates = list(goal_arc_radius = 15, goal_arc_extension = 15),
    color_updates = list(goal_arc = "#123456")
  )
  # With no thickness the arc has no area, so add_feature() leaves it out
  expect_length(layers_filled(p, "#123456"), 0)
})

test_that("a user-supplied centre face-off marker colour is kept", {
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

test_that("the contrasting centre face-off marker matches on rendered colour", {
  # Each spelling of white renders the same as the default #ffffff marker
  for (white in c("white", "#FFFFFF", "#ffffffff")) {
    p <- geom_lacrosse("ncaam", color_updates = list(center_line = white))
    expect_length(layers_filled(p, "#ffcb05"), 1)
  }

  # A named colour vector compares on its value, not its name
  p <- geom_lacrosse("ncaam", color_updates = list(center_line = c(a = "white")))
  expect_length(layers_filled(p, "#ffcb05"), 1)

  # A centre line that renders differently keeps the default white marker
  p <- geom_lacrosse("ncaam", color_updates = list(center_line = "#123456"))
  expect_length(layers_filled(p, "#ffcb05"), 0)

  # The marker is still drawn white: recolouring it removes exactly one white
  # layer, since other white features share the fill
  p_red <- geom_lacrosse(
    "ncaam",
    color_updates = list(
      center_line = "#123456",
      center_face_off_marker = "#ff0000"
    )
  )
  expect_length(
    layers_filled(p, "#ffffff"),
    length(layers_filled(p_red, "#ffffff")) + 1
  )
})

test_that("the NCAA football coaching line is six feet outside the sideline", {
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
  # Both tagged layers must be drawn, so a missing outline cannot go unnoticed
  expect_true("#123456" %in% fills)
  expect_true("#654321" %in% fills)
  idx <- which(fills %in% c("#123456", "#654321"))
  for (i in idx) {
    expect_equal(default$layers[[i]]$data, tagged$layers[[i]]$data)
  }
  vapply(default$layers[idx], function(l) l$aes_params$fill, character(1))
}

test_that("bordered football fields draw a white border outline by default", {
  expect_true(surface_dimensions[["football"]][["ncaa"]]$field_bordered)
  expect_true(all(outline_fills("ncaa") == "#ffffff"))
  expect_true(all(outline_fills("nfl") == "#ffffff"))
  expect_true(all(outline_fills("nfhs11") == "#ffffff"))
})

test_that("unbordered football fields keep the transparent outline", {
  expect_false(surface_dimensions[["football"]][["cfl"]]$field_bordered)
  expect_true(all(outline_fills("cfl") == "#ffffff00"))

  # The fills are unchanged
  colors <- football_features_set_colors()
  expect_equal(colors$field_border, "#196f0c")
  expect_equal(colors$red_zone_border, "#196f0c00")
})

test_that("a user border outline colour wins on any field", {
  for (league in c("cfl", "ncaa")) {
    p <- geom_football(league, color_updates = list(field_border_outline = "#ff0000"))
    expect_true(length(layers_filled(p, "#ff0000")) > 0, label = league)
  }
})

test_that("the PLL two-point arc follows the 2024 PLL Rulebook, II.G", {
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

test_that("the NFL field border follows the bench's slanted side", {
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

test_that("each NLL / World Lacrosse penalty box is penalty_box_length inside", {
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

    # The penalty box fill sits exactly inside the walls
    p <- geom_lacrosse(league, color_updates = list(team_b_penalty_box = "#654321"))
    fill <- layers_filled(p, "#654321")[[1]]$data
    expect_equal(range(fill$x), c(divider_inside, outer_wall_inside))
  }
})

test_that("every geom_*() scales its anchors and limits with the units", {
  surfaces <- list(
    baseball = function(u, ...) geom_baseball("mlb", field_units = u, ...),
    basketball = function(u, ...) geom_basketball("nba", court_units = u, ...),
    curling = function(u, ...) geom_curling("wcf", sheet_units = u, ...),
    football = function(u, ...) geom_football("nfl", field_units = u, ...),
    hockey = function(u, ...) geom_hockey("nhl", rink_units = u, ...),
    lacrosse = function(u, ...) geom_lacrosse("nll", field_units = u, ...),
    soccer = function(u, ...) geom_soccer("fifa", pitch_units = u, ...),
    softball = function(u, ...) geom_softball("ncaa", field_units = u, ...),
    tennis = function(u, ...) geom_tennis("itf", court_units = u, ...),
    volleyball = function(u, ...) geom_volleyball("fivb", court_units = u, ...)
  )
  native_units <- c(
    baseball = "ft", basketball = "ft", curling = "ft", football = "yd",
    hockey = "ft", lacrosse = "ft", soccer = "m", softball = "ft",
    tennis = "ft", volleyball = "m"
  )
  # Every position column of every layer, including the yardage-number boxes
  # (x_min / x_max / y_min / y_max)
  coords <- function(p, axis) {
    columns <- paste0(axis, c("", "_min", "_max", "min", "max"))
    unlist(lapply(p$layers, function(l) {
      unlist(l$data[intersect(columns, names(l$data))], use.names = FALSE)
    }))
  }

  for (sport in names(surfaces)) {
    target <- if (native_units[[sport]] == "m") "ft" else "m"
    f <- convert_units(1, from_unit = native_units[[sport]], to_unit = target)

    native <- surfaces[[sport]](NULL)
    converted <- surfaces[[sport]](target)

    # Every feature, and where it sits, scales by the unit factor
    expect_equal(coords(converted, "x"), coords(native, "x") * f, label = sport)
    expect_equal(coords(converted, "y"), coords(native, "y") * f, label = sport)

    # So does the display range
    expect_equal(
      converted$coordinates$limits$x,
      native$coordinates$limits$x * f,
      label = sport
    )
    expect_equal(
      converted$coordinates$limits$y,
      native$coordinates$limits$y * f,
      label = sport
    )

    # Limits passed in are in the requested units, so they come back as given
    limited <- surfaces[[sport]](target, xlims = c(-10, 10), ylims = c(-5, 5))
    expect_equal(limited$coordinates$limits$x, c(-10, 10), label = sport)
    expect_equal(limited$coordinates$limits$y, c(-5, 5), label = sport)
  }

  # Translations stay in the requested units
  moved <- geom_lacrosse("nll", field_units = "m", x_trans = 3)
  still <- geom_lacrosse("nll", field_units = "m")
  expect_equal(moved$coordinates$limits$x, still$coordinates$limits$x + 3)
})

test_that("goal-fan hash marks bisect the fan's line", {
  params <- surface_dimensions[["lacrosse"]][["ncaaw"]]
  goal_x <- (params$field_length / 2) - params$goal_line_center_to_end_line

  p <- geom_lacrosse(
    "ncaaw",
    color_updates = list(goal_fan = "#123456", goal_fan_hash_mark = "#654321")
  )

  # The fan's line near its apex (right-hand goal) runs between these radii
  fan <- Filter(
    function(l) mean(l$data$x) > 0,
    layers_filled(p, "#123456")
  )[[1]]$data
  near_apex <- abs(fan$y) < 1
  d <- sqrt((fan$x[near_apex] - goal_x)^2 + fan$y[near_apex]^2)
  expect_equal(diff(range(d)), params$goal_fan_line_thickness, tolerance = 1e-9)
  line_middle <- mean(range(d))

  # The center hash mark's midpoint is on the middle of that line
  center_mark <- Filter(
    function(l) mean(l$data$x) > 0 && abs(mean(l$data$y[1:4])) < 1e-9,
    layers_filled(p, "#654321")
  )[[1]]$data
  expect_equal(
    goal_x - mean(center_mark$x[1:4]),
    line_middle,
    tolerance = 1e-9
  )
})

test_that("x_trans / y_trans move every football layer by exactly that much", {
  coords <- function(p, axis) {
    columns <- paste0(axis, c("", "_min", "_max", "min", "max"))
    unlist(lapply(p$layers, function(l) {
      unlist(l$data[intersect(columns, names(l$data))], use.names = FALSE)
    }))
  }

  base <- geom_football("nfl")
  moved <- geom_football("nfl", x_trans = 3, y_trans = -2)

  expect_equal(coords(moved, "x"), coords(base, "x") + 3)
  expect_equal(coords(moved, "y"), coords(base, "y") - 2)
})

test_that("drawing a custom surface in other units explains the missing units", {
  expect_error(
    geom_lacrosse("custom", field_units = "m"),
    "This field has no rule-book units"
  )
  expect_error(
    geom_hockey("custom", rink_units = "m"),
    "Set rink_units in the dimensions first"
  )

  # Once the units are set, the custom surface converts
  expect_no_error(
    geom_lacrosse(
      "custom",
      field_updates = list(field_units = "ft"),
      field_units = "m"
    )
  )
  expect_equal(unit_conversion_factor("ft", "m"), 0.3048)
  expect_equal(unit_conversion_factor("", NULL), 1)
})

test_that("geom_basketball(\"custom\") draws", {
  # ggplotGrob() opens a device; a null pdf keeps Rplots.pdf out of the tree
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Empty JSON arrays arrive as list(); %or% falls back to the default
  expect_equal(list() %or% 0, 0)
  expect_equal(numeric(0) %or% 1, 1)
  expect_equal(c(3, 4) %or% 0, c(3, 4))

  # The custom template's asin(0 / 0) on its zero-size court warns NaN
  p <- suppressWarnings(geom_basketball("custom"))
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplotGrob(p))

  # Every other sport's custom template builds too, except football (a
  # zero-length field, a separate problem)
  for (geom in list(
    geom_baseball, geom_curling, geom_hockey, geom_lacrosse, geom_soccer,
    geom_softball, geom_tennis, geom_volleyball
  )) {
    expect_no_error(ggplot2::ggplotGrob(suppressWarnings(geom("custom"))))
  }
})
