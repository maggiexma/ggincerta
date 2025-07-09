test_that('geom_sf_glyph works', {
  set.seed(10086)

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>%
    dplyr::mutate(value = rnorm(dplyr::n()), sd = rnorm(dplyr::n()))

  p <- ggplot(nc, aes(geometry = geometry, estimate = value, error = sd)) +
    geom_sf_glyph(size = 50, glyph = "icone") +
    coord_sf() + theme_void()
  vdiffr::expect_doppelganger("pixel map", p)
})
