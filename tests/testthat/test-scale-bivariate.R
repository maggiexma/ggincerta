test_that("scale-bivariate", {
  ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd)))

  ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    scale_fill_bivariate(n_breaks = 3, blend = 'subtractive',
                         name1 = 'var1', name2 = 'var2',
                         flip = 'both', guide_size = 2)

  # possibly fixed
  # TODO: add explicit for palette
  ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd))) +
    geom_sf_text(aes(label = round(value, 1)))

  vdiffr::expect_doppelganger("bivariate map", p)

  ggplot(anscombe, aes(x1, x2)) +
    geom_point(aes(color = duo(y1, y2)), size = 5)

  ggplot(economics) +
    geom_line(aes(date, pce, color = duo(psavert, pop)),
              linewidth = 3) +
    scale_color_bivariate(n_breaks = 3,
                          name1 = "Blabla",
                          name2 = "hello") +
    labs(x = "XXXX")


  set.seed(1)
  expand.grid(row = 1:5, col = 1:3) |>
    transform(v = rnorm(15), u = rnorm(15)) |>
    ggplot(aes(row, col, fill = duo(v, u))) +
    geom_tile(color = "black")

})
