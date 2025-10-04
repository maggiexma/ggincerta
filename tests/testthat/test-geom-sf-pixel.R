test_that('geom_sf_pixel works', {
  ggplot(nc) +
    geom_sf_pixel(mapping = aes(v1 = value, v2 = sd))


  p <- ggplot(nc) +
    geom_sf_pixel(
      mapping = aes(v1 = value, v2 = sd)
    )

  vdiffr::expect_doppelganger("pixel map", p)
})
