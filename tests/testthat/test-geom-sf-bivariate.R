test_that('geom_sf_bivariate works', {
  set.seed(10086)

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>%
    dplyr::mutate(value = rnorm(dplyr::n()), sd = rnorm(dplyr::n()))

  p <- ggplot(nc) +
    geom_sf_bivariate(aes(estimate = value, error = sd)) +
    scale_fill_bivariate(
      colrange = list(colour = c("gold", "red4"), difC = c(4, 4)),
      subtractive = FALSE,
      flip_vertical = FALSE,
      flip_horizontal = FALSE
    )
  vdiffr::expect_doppelganger("bivariate map", p)
})
