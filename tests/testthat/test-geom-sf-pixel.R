test_that('geom_sf_pixel works', {
  set.seed(10086)

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>%
    dplyr::mutate(value = rnorm(dplyr::n()), sd = rnorm(dplyr::n()))

  names(nc)[ names(nc) == "NAME" ] <- "id"
  names(nc)[ names(nc) == "value" ] <- "estimate"
  names(nc)[ names(nc) == "sd" ] <- "error"

  p <- ggplot() +
    geom_sf_pixel(
      data = nc,
      mapping = aes(geometry = geometry),
      id_col = "id",
      pixelSize = 40,
      distribution = "uniform"
    ) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
    geom_sf(data = nc, fill = NA, color = 'black', size = 0.2)

  vdiffr::expect_doppelganger("pixel map", p)
})
