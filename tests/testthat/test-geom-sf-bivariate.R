test_that('geom_sf_bivariate works with guide_bivariate', {
  data(nc)

  p <- ggplot(nc) +
    geom_sf_bivariate(aes(
      geometry = geometry,
      estimate = value,
      error = sd
    )) +
    scale_fill_bivariate(
      colors = c("gold", "red4"),
      difC = c(4, 4),
      blend = "additive",
      flip = "none"
    )


  vdiffr::expect_doppelganger("bivariate map", p)
})
