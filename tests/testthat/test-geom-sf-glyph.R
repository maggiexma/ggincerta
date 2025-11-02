test_that('geom_sf_glyph draws correctly', {
  p <- ggplot(nc) +
    geom_sf_glyph(aes(v1 =value, v2 = sd))

  vdiffr::expect_doppelganger("glyph map", p)
})

test_that("geom_sf_glyph supports both icone and semi styles", {
  p1 <- ggplot(nc) +
    geom_sf_glyph(aes(v1 = value, v2 = sd), style = "icone", size = 50)
  p2 <- ggplot(nc) +
    geom_sf_glyph(aes(v1 = value, v2 = sd), style = "semi", size = 50)

  vdiffr::expect_doppelganger("icone glyph map", p1)
  vdiffr::expect_doppelganger("semi glyph map", p2)
})

test_that("geom_sf_glyph automatically adds two scales", {
  p <- ggplot(nc) +
    geom_sf_glyph(aes(v1 = value, v2 = sd))
  scale_names <- vapply(p$scales$scales, function(s) s$aesthetics[1], character(1))

  expect_true("fill" %in% scale_names)
  expect_true("glyph" %in% scale_names)
})

test_that("geom_sf_glyph prints message when adding another fill scale", {
  expect_message(
    ggplot(nc) +
      geom_sf_glyph(aes(v1 = value, v2 = sd)) +
      scale_fill_distiller(palette = "Blues", guide = guide_colorbar(order = 1))
  )
})

test_that("geom_sf_glyph works with labs and theme", {
  p <- ggplot(nc) +
    geom_sf_glyph(aes(v1 = value, v2 = sd)) +
    labs(title = "glyph map on nc") +
    theme(legend.position = "bottom", legend.box = "horizontal")

  gb <- ggplot_build(p)
  expect_equal(gb$plot$labels$title, "glyph map on nc")
  vdiffr::expect_doppelganger("pixel map with bottom horizontal guides", p)
})
