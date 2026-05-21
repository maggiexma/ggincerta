test_that("vsup scale draws correctly", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    scale_fill_vsup()

  vdiffr::expect_doppelganger("vsup map", p)
})

test_that("vsup scale respects custom breaks, limits, and transform", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    scale_fill_vsup(
      breaks = list(
        c(0, 1, 2, 3, 4, 5, 6, 7, 8),
        c(1, 2, 4, 8, 16)
      ),
      limits = list(c(0, 8), c(1, 16)),
      transform = list("identity", "log2"),
      na.value = "grey80"
    )

  gb <- ggplot_build(p)
  mapped <- gb$data[[1]]$fill

  expect_equal(length(mapped), nrow(nc))
  expect_true(any(mapped == "grey80" | is.na(mapped)))
  expect_true(any(!is.na(mapped) & mapped != "grey80"))

  guide_info <- gb$plot$scales$get_scales("fill")$get_guide_info()

  expect_equal(
    guide_info$value_breaks,
    c(0, 1, 2, 3, 4, 5, 6, 7, 8)
  )

  expect_equal(
    guide_info$uncertainty_breaks,
    c(1, 2, 4, 8, 16)
  )
})

test_that("vsup scale applies transformed limits when mapping", {
  df <- data.frame(
    value = c(1, 2, 3),
    sd = c(1, 4, 32),
    x = c(1, 2, 3),
    y = c(1, 1, 1)
  )

  p <- ggplot(df, aes(x, y)) +
    geom_point(aes(fill = duo(value, sd)), shape = 21, size = 4) +
    scale_fill_vsup(
      breaks = list(
        c(0, 1, 2, 3),
        c(1, 4, 16)
      ),
      limits = list(c(0, 3), c(1, 16)),
      transform = list("identity", "log2"),
      layers = 2,
      branch = 3,
      na.value = "grey80"
    )

  mapped <- ggplot_build(p)$data[[1]]$fill

  expect_equal(mapped[3], "grey80")
  expect_false(mapped[1] == "grey80")
  expect_false(mapped[2] == "grey80")
})

test_that("vsup scale handles missing bivariate values", {
  df <- data.frame(
    value = c(1, 2, NA, 4),
    sd = c(1, NA, 4, 8),
    x = 1:4,
    y = 1
  )

  p <- ggplot(df, aes(x, y)) +
    geom_point(aes(fill = duo(value, sd)), shape = 21, size = 4) +
    scale_fill_vsup(
      breaks = list(
        c(0, 1, 2, 3, 4),
        c(1, 2, 4, 8)
      ),
      limits = list(c(0, 4), c(1, 8)),
      transform = list("identity", "identity"),
      layers = 3,
      branch = 2,
      na.value = "grey80"
    )

  mapped <- ggplot_build(p)$data[[1]]$fill

  expect_equal(mapped[2], "grey80")
  expect_equal(mapped[3], "grey80")
  expect_false(mapped[1] == "grey80")
  expect_false(mapped[4] == "grey80")
})

