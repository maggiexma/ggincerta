test_that('geom_point_exceed works', {
  nc_centroids <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>%
    st_centroid() %>%
    mutate(
      x = st_coordinates(.)[, 1],
      y = st_coordinates(.)[, 2],
      estimate = rnorm(n()),
      error = runif(n(), 0, 1)
    )

  ggplot(nc_centroids, aes(
    x = x,
    y = y,
    estimate = estimate,
    error = error
  )) +
    geom_point_exceed(
      mapping   = aes(fill = after_stat(pr_exc)),
      threshold = 0.5,
      palette = "YlOrRd",
      size = 4,
      shape = 21
    ) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1) +
    coord_equal() +
    theme_minimal()

  ggplot(nc_centroids) +
    geom_point_exceed(
      aes(
        x        = x,
        y        = y,
        estimate = estimate,
        error    = error
      ),
      threshold = 0.5,
      palette   = "YlOrRd",
      direction = 1,
      size      = 4,
      shape     = 21
    ) +
    coord_equal() +
    theme_minimal()

})
