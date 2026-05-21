test_that("geom_sf_pixel draws correctly", {
  nc_flat <- sf::st_transform(nc, 3857)

  p <- ggplot(nc_flat) +
    geom_sf_pixel(
      mapping = aes(fill = duo_pixel(value, sd)),
      seed = 123
    )

  vdiffr::expect_doppelganger("pixel map", p)
})

test_that("geom_sf_pixel builds pixel and boundary layers", {
  nc_flat <- sf::st_transform(nc[1:3, ], 3857)

  p <- ggplot(nc_flat, aes(fill = duo_pixel(value, sd))) +
    geom_sf_pixel(n = 10, seed = 1)

  gb <- ggplot_build(p)

  expect_equal(length(gb$data), 2)
  expect_true(nrow(gb$data[[1]]) > nrow(nc_flat))
  expect_equal(nrow(gb$data[[2]]), nrow(nc_flat))
  expect_true("geometry" %in% names(gb$data[[1]]))
  expect_true("fill" %in% names(gb$data[[1]]))
  expect_true(any(!is.na(gb$data[[1]]$fill)))
})

test_that("geom_sf_pixel is reproducible with a fixed seed", {
  nc_flat <- sf::st_transform(nc[1:3, ], 3857)

  p1 <- ggplot(nc_flat, aes(fill = duo_pixel(value, sd))) +
    geom_sf_pixel(n = 10, seed = 123)

  p2 <- ggplot(nc_flat, aes(fill = duo_pixel(value, sd))) +
    geom_sf_pixel(n = 10, seed = 123)

  d1 <- ggplot_build(p1)$data[[1]]
  d2 <- ggplot_build(p2)$data[[1]]

  expect_equal(d1$fill, d2$fill)
})

test_that("geom_sf_pixel supports available pixel shapes", {
  nc_flat <- sf::st_transform(nc[1:3, ], 3857)

  for (shape in c("hex", "square", "rect")) {
    p <- ggplot(nc_flat, aes(fill = duo_pixel(value, sd))) +
      geom_sf_pixel(n = 8, pixel_shape = shape, seed = 1)

    pixel_data <- ggplot_build(p)$data[[1]]

    expect_true(nrow(pixel_data) > 0)
    expect_true("geometry" %in% names(pixel_data))
    expect_true(any(!is.na(pixel_data$fill)))
  }
})

test_that("geom_sf_pixel supports available distributions", {
  nc_flat <- sf::st_transform(nc[1:3, ], 3857)

  for (dist in c("uniform", "normal")) {
    p <- ggplot(nc_flat, aes(fill = duo_pixel(value, sd))) +
      geom_sf_pixel(n = 8, distribution = dist, seed = 1)

    pixel_data <- ggplot_build(p)$data[[1]]

    expect_true(nrow(pixel_data) > 0)
    expect_true("fill" %in% names(pixel_data))
    expect_true(any(!is.na(pixel_data$fill)))
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

test_that("geom_sf_pixel validates seed", {
  nc_flat <- sf::st_transform(nc[1:3, ], 3857)

  p <- ggplot(nc_flat, aes(fill = duo_pixel(value, sd))) +
    geom_sf_pixel(n = 8, seed = NA_real_)

  expect_error(
    ggplot_build(p),
    "`seed` must be a finite numeric scalar.",
    fixed = TRUE
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
