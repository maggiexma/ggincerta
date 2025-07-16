test_that('geom_point_exceed works', {
  data(nc_centroids)

  p <- ggplot(nc_centroids) +
    geom_point_exceed(
      aes(
        x = x,
        y = y,
        estimate = estimate,
        error = error
      ),
      threshold = 0.5,
      palette = "YlOrRd",
      direction = 1,
      size = 4,
      shape = 21
    ) +
    coord_equal() +
    theme_minimal()
  vdiffr::expect_doppelganger("exceed point map", p)

})
