test_that('geom_sf_bivariate works with guide_bivariate', {
  data(nc)

  p <- ggplot(nc) +
    geom_sf_bivariate(aes(geometry = geometry, estimate = value, error = sd)) +
    scale_fill_bivariate(
      data = nc,
      estimate = "value",
      error = "sd",
      colrange = list(colour = c("gold", "red4"), difC = c(4, 4))
    ) +
    theme(
      legend.position = "right",
      legend.box.just = "left",
      plot.margin = margin(10, 30, 10, 10)
    )

  vdiffr::expect_doppelganger("bivariate map", p)
})
