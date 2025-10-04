test_that('geom_sf_exceed works', {
  data(nc)

  p <- ggplot(nc) +
    geom_sf_exceed(
      mapping = aes(
        v1 = value,
        v2 = sd
      ),
      threshold = 0.5,
      dist_fun = stats::pnorm
    )

  vdiffr::expect_doppelganger("exceed map", p)
})
