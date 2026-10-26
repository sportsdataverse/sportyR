skip_on_cran()

test_that("get_surface_dimensions() returns one league's parameters", {
  nll <- get_surface_dimensions("lacrosse", "NLL")
  expect_type(nll, "list")
  expect_equal(nll$field_length, 200)
  expect_identical(nll, surface_dimensions[["lacrosse"]][["nll"]])

  # Matched case-insensitively, as the geoms match league
  expect_identical(get_surface_dimensions("LACROSSE", "nll"), nll)
  expect_identical(
    get_surface_dimensions("lacrosse", "World Lacrosse"),
    surface_dimensions[["lacrosse"]][["world lacrosse"]]
  )
})

test_that("get_surface_dimensions() returns every league of a sport", {
  hockey <- get_surface_dimensions("Hockey")
  expect_identical(hockey, surface_dimensions[["hockey"]])
  expect_true("nhl" %in% names(hockey))
})

test_that("get_surface_dimensions() errors clearly on unknown input", {
  expect_error(get_surface_dimensions("quidditch"), "not a supported sport")
  expect_error(
    get_surface_dimensions("lacrosse", "XYZ"),
    "XYZ is not a supported lacrosse league"
  )
})
