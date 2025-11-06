test_that('geom_sf_pixel draws correctly', {
  p <- ggplot(nc) +
    geom_sf_pixel(mapping = aes(v1 = value, v2 = sd), seed = 123, n = 10)

  vdiffr::expect_doppelganger("pixel map", p)
})

test_that("geom_sf_pixel supports both distributions", {
  res_u <- ggplot_build(ggplot(nc) + geom_sf_pixel(aes(v1 = value, v2 = sd), distribution = "uniform", n = 10))$data[[1]]

  res_n <- ggplot_build(ggplot(nc) + geom_sf_pixel(aes(v1 = value, v2 = sd), distribution = "normal", n = 5))$data[[1]]

  expect_true(all(c("geometry", "fill") %in% names(res_u)))
  expect_true(all(c("geometry", "fill") %in% names(res_n)))
  expect_true(inherits(res_u$geometry, "sfc_POLYGON"))
  expect_true(inherits(res_n$geometry, "sfc_POLYGON"))
})

test_that("geom_sf_pixel works with NAs", {
  nc$value[1:3] <- NA
  nc$sd[c(2,4)] <- NA

  p <- ggplot(nc) +
    geom_sf_pixel(
      mapping = aes(v1 = value, v2 = sd),
      distribution = "normal",
      seed = 123,
      n = 5
    )

  suppressWarnings(
    vdiffr::expect_doppelganger("pixel map with NAs", p)
  )
})

test_that("geom_sf_pixel prints message when adding another fill scale", {
  expect_message(ggplot(nc) +
                   geom_sf_pixel(aes(v1 = value, v2 = sd), n = 10) +
                   scale_fill_distiller(palette = "Blues"))
})
