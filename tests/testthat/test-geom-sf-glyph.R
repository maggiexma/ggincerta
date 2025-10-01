test_that('geom_sf_glyph works', {
  data(nc)

  p <- ggplot(nc) +
    geom_sf_glyph(aes(geometry = geometry, estimate = value, error = sd), size = 50, glyph = "icone") +
    scale_fill_viridis_c(name = "value", guide = guide_colorbar(order = 1)) +
    scale_glyph_continuous(name = "sd", order = 2) +
    theme(legend.position = "right", legend.box = "horizontal")

  vdiffr::expect_doppelganger("pixel map", p)
})
