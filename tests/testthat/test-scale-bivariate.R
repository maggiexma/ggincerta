test_that("bivariate scale works automatically with geom_sf", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd)))

  p1 <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    scale_fill_bivariate()

  vdiffr::expect_doppelganger("bivariate map", p)
  vdiffr::expect_doppelganger("bivariate map", p1)
})

test_that("bivariate scale works with labs and theme", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    scale_fill_bivariate(n_breaks = 4) +
    labs(title = "bivariate map on nc") +
    theme(legend.position = "left")

  gb <- ggplot_build(p)
  expect_equal(gb$plot$labels$title, "bivariate map on nc")
  vdiffr::expect_doppelganger("bivariate map with left guide", p)
})

test_that("bivariate scale works with NAs", {
  nc$value[1:3] <- NA
  nc$sd[c(2,4)] <- NA

  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd)))
  vdiffr::expect_doppelganger("bivariate map with NAs", p)
})

test_that("bivariate scale correctly maps bin combinations to colors.", {
  p <- ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd)))

  gb <- ggplot_build(p)
  fills_mapped <- gb$data[[1]]$fill

  qx <- quantile(nc$value, seq(0, 1, length.out = 4), na.rm = TRUE)
  qy <- quantile(nc$sd, seq(0, 1, length.out = 4), na.rm = TRUE)
  bin1 <- cut(nc$value, breaks = qx, include.lowest = TRUE, labels = FALSE)
  bin2 <- cut(nc$sd, breaks = qy, include.lowest = TRUE, labels = FALSE)
  combo <- (bin2 - 1L) * 3 + bin1
  pal <- bivar_palette(colors = c("gold", "red4"), n_breaks = c(3,3))
  fills <- pal[combo]
  expect_equal(fills_mapped, fills)
})

