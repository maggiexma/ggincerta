test_that('geom_sf_glyph works', {
  data(nc)

  p <- ggplot(nc) +
    geom_sf_glyph(aes(geometry = geometry, v1 = value, v2 = sd), size = 50, glyph = "icone") +
    theme(legend.position = "right", legend.box = "horizontal")

  ggplot(nc) +
    geom_sf_glyph(
      aes(geometry = geometry, v1 = value, v2 = sd),
      style = "semi"
    ) +
    theme(legend.position = "right", legend.box = "horizontal")

  vdiffr::expect_doppelganger("pixel map", p)
})
