test_that('geom_sf_exceed works', {
  data(nc)

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
    )

  vdiffr::expect_doppelganger("exceed map", p)
})
