test_that('geom_point_bivariate works', {
  data(nc_centroids)

  p <- ggplot(nc_centroids, aes(x = x, y = y)) +
    geom_point_bivariate(
      mapping = aes(
        estimate = estimate,
        error = error,
        fill = after_stat(fill)
      ),
      size = 4,
      shape = 21,
      terciles = TRUE,
      flipAxis = FALSE
    ) +
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
    ) +
    coord_equal()
  vdiffr::expect_doppelganger("bivariate point map", p)
})
