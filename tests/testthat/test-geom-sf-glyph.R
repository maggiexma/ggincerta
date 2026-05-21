test_that("geom_sf_glyph draws regular glyphs with bivariate colour scale", {
  nc_flat <- sf::st_transform(nc, 3857)

  p <- ggplot(nc_flat) +
    geom_sf_glyph(
      aes(colour = duo(value, sd)),
      shape = "circle",
      size = 2
    )

  suppressWarnings(
    vdiffr::expect_doppelganger("glyph map regular", p)
  )
})

test_that("geom_sf_glyph draws drop glyphs with angle mapping", {
  nc_flat <- sf::st_transform(nc, 3857)

  p <- ggplot(nc_flat) +
    geom_sf_glyph(
      aes(colour = value, angle = sd),
      shape = "drop",
      size = 2
    )

  suppressWarnings(
    vdiffr::expect_doppelganger("glyph map drop", p)
  )
})

test_that("geom_sf_glyph draws chernoff glyphs correctly", {
  nc_flat <- sf::st_transform(nc, 3857)

  p <- ggplot(nc_flat) +
    geom_sf_glyph(
      aes(
        colour = value,
        smile = sd
      ),
      shape = "chernoff"
    )

  suppressWarnings(
    vdiffr::expect_doppelganger("glyph map chernoff", p)
  )
})

test_that("geom_sf_glyph warns when angle is used with non-drop glyphs", {
  expect_warning(
    geom_sf_glyph(
      aes(colour = value, angle = sd),
      shape = "circle"
    ),
    "angle.*only used when"
  )
})

test_that("geom_sf_glyph warns when smile is used with non-chernoff glyphs", {
  expect_warning(
    geom_sf_glyph(
      aes(colour = value, smile = sd),
      shape = "circle"
    ),
    "smile.*only used when"
  )
})
