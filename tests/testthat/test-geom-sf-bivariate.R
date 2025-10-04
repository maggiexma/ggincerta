test_that('test geom_sf_bivariate function with custom guide and scale', {
  data(nc)

  p <- ggplot(nc) +
    geom_sf_bivariate(aes(v1 = value, v2 = sd))

  vdiffr::expect_doppelganger("bivariate map", p)
})
