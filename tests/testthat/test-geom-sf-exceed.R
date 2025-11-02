test_that('geom_sf_exceed draws correctly', {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo_exceed(estimate = value, error = sd)))

  vdiffr::expect_doppelganger("exceed map", p)
})

test_that("ScaleExceed$transform computes exceedance probs (normal)", {
  sc <- scale_fill_exceed(threshold = 1.64)
  x <- list(list(v1 = 0, v2 = 1), list(v1 = 0.5, v2 = 2))
  p <- sc$transform(x)

  expect_type(p, "double")
  expect_length(p, 2)
  expect_equal(
    p,
    stats::pnorm(1.64, mean = c(0, 0.5), sd = c(1, 2), lower.tail = FALSE),
    tolerance = 1e-12
  )
})

test_that("geom_sf_exceed works with labs and theme", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo_exceed(estimate = value, error = sd))) +
    labs(title = "exceedance probability map on nc") +
    theme(legend.position = "left")

  gb <- ggplot_build(p)
  expect_equal(gb$plot$labels$title, "exceedance probability map on nc")
  vdiffr::expect_doppelganger("exceedance probability map with left guide", p)
})
