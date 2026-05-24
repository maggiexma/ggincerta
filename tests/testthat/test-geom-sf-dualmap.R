test_that("geom_sf_dualmap has stable output", {
  p <- ggplot(nc) +
    geom_sf_dualmap(aes(fill = sd, colour = value))

  suppressWarnings(vdiffr::expect_doppelganger("dualmap", p))
})

test_that("geom_sf_dualmap adds default fill scale for ordinary fill mapping", {
  layers <- geom_sf_dualmap(
    aes(fill = sd, colour = value),
    data = nc
  )

  fill_scales <- Filter(
    function(x) inherits(x, "Scale") && identical(x$aesthetics, "fill"),
    layers
  )

  expect_length(fill_scales, 1)
  expect_s3_class(fill_scales[[1]], "ScaleContinuous")
})

test_that("geom_sf_dualmap does not add default fill scale for duo fill mapping", {
  layers <- geom_sf_dualmap(
    aes(fill = duo(value, sd), colour = value),
    data = nc
  )

  fill_scales <- Filter(
    function(x) inherits(x, "Scale") && identical(x$aesthetics, "fill"),
    layers
  )

  expect_length(fill_scales, 0)
})

test_that("parse_dualmap_mapping separates outer and inner mappings", {
  parsed <- parse_dualmap_mapping(
    aes(fill = sd, colour = value, angle = sd)
  )

  expect_null(parsed$outer$colour)
  expect_null(parsed$outer$angle)
  expect_null(parsed$outer$smile)
  expect_null(parsed$inner$fill)

  expect_false(is.null(parsed$outer$fill))
  expect_false(is.null(parsed$inner$colour))
  expect_false(is.null(parsed$inner$angle))
})
