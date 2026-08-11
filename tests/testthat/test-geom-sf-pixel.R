test_that("geom_sf_pixel draws correctly", {
  nc_flat <- sf::st_transform(nc, 3857)

  p <- ggplot(nc_flat) +
    geom_sf_pixel(
      mapping = aes(fill = duo_pixel(value, sd)),
      seed = 123, n = 20
    )

  vdiffr::expect_doppelganger("pixel map", p)
})

test_that("geom_sf_pixel builds without error", {
  nc_flat <- sf::st_transform(nc[1:3, ], 3857)

  p <- ggplot(nc_flat, aes(fill = duo_pixel(value, sd))) +
    geom_sf_pixel(n = 10, seed = 1)

  expect_no_error(ggplot_build(p))
})

test_that("geom_sf_pixel is reproducible with a fixed seed", {
  nc_flat <- sf::st_transform(nc[1:3, ], 3857)

  p1 <- ggplot(nc_flat, aes(fill = duo_pixel(value, sd))) +
    geom_sf_pixel(n = 10, seed = 123)

  p2 <- ggplot(nc_flat, aes(fill = duo_pixel(value, sd))) +
    geom_sf_pixel(n = 10, seed = 123)

  expect_equal(ggplot_build(p1)$data, ggplot_build(p2)$data)
})

test_that("geom_sf_pixel supports available pixel shapes", {
  nc_flat <- sf::st_transform(nc[1:3, ], 3857)

  for (shape in c("hex", "square", "rect")) {
    p <- ggplot(nc_flat, aes(fill = duo_pixel(value, sd))) +
      geom_sf_pixel(n = 8, pixel_shape = shape, seed = 1)

    expect_no_error(ggplot_build(p))
  }
})

test_that("geom_sf_pixel supports available distributions", {
  nc_flat <- sf::st_transform(nc[1:3, ], 3857)

  for (dist in c("uniform", "normal")) {
    p <- ggplot(nc_flat, aes(fill = duo_pixel(value, sd))) +
      geom_sf_pixel(n = 8, distribution = dist, seed = 1)

    expect_no_error(ggplot_build(p))
  }
})

test_that("geom_sf_pixel requires a valid CRS", {
  nc_flat <- sf::st_transform(nc[1:3, ], 3857)
  sf::st_crs(nc_flat) <- NA

  p <- ggplot(nc_flat, aes(fill = duo_pixel(value, sd))) +
    geom_sf_pixel(n = 8, seed = 1)

  expect_error(
    ggplot_build(p),
    "Input data must have a valid CRS."
  )
})

test_that("geom_sf_pixel validates pixel_shape and distribution", {
  expect_error(
    geom_sf_pixel(pixel_shape = "circle"),
    "`pixel_shape` must be one of",
    fixed = TRUE
  )

  nc_flat <- sf::st_transform(nc[1:3, ], 3857)

  p <- ggplot(nc_flat, aes(fill = duo_pixel(value, sd))) +
    geom_sf_pixel(distribution = "gamma")

  expect_error(
    ggplot_build(p),
    "`distribution` must be one of",
    fixed = TRUE
  )
})
