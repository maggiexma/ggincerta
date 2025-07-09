test_that('geom_point_bivariate works', {
  nc_centroids <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>%
    st_centroid() %>%
    mutate(
      x = st_coordinates(.)[, 1],
      y = st_coordinates(.)[, 2],
      estimate = rnorm(n()),
      error = runif(n(), 0, 1)
    )

  ggplot(nc_centroids, aes(x = x, y = y)) +
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
    scale_fill_bivariate(colrange = list(colour = c("gold", "red4"), difC = c(4, 4))) +
    coord_equal() +
    theme_minimal()
})
