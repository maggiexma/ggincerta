test_that('geom_sf_exceed works', {
  data(nc)

  nc$sd <- abs(rnorm(nrow(nc), mean = 1, sd = 0.3))

  p <- ggplot(nc) +
    geom_sf_exceed(
      mapping = aes(
        geometry = geometry,
        estimate = value,
        error = sd,
        fill = after_stat(pr_exc)
      ),
      threshold = 0.5,
      dist_fun = stats::pnorm
    ) +
    coord_sf()
  vdiffr::expect_doppelganger("exceed map", p)
})
