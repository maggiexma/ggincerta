test_that('test geom_sf_bivariate function with custom guide and scale', {
  data(nc)

  p <- ggplot(nc) +
    geom_sf_bivariate(aes(
      geometry = geometry,
      primary = value,
      secondary = sd
    ))

  vdiffr::expect_doppelganger("bivariate map", p)
})
