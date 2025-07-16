test_that('geom_sf_glyph works', {
  data(nc)

  p <- ggplot(nc, aes(geometry = geometry, estimate = value, error = sd)) +
    geom_sf_glyph(size = 50, glyph = "icone") +
    coord_sf() + theme_void()
  vdiffr::expect_doppelganger("pixel map", p)
})
