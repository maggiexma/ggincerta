test_that("scale-bivariate", {
  ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd)))


  ggplot(anscombe, aes(x1, x2)) +
    geom_point(aes(color = duo(y1, y2)), size = 10)

  ggplot(economics) +
    geom_line(aes(date, pce, color = duo(psavert, pop)),
              linewidth = 3) +
    scale_color_bivariate(n_breaks = 3)


  set.seed(1)
  expand.grid(row = 1:5, col = 1:3) |>
    transform(v = rnorm(15), u = rnorm(15)) |>
    ggplot(aes(row, col, fill = duo(v, u))) +
    geom_tile(color = "black")


})
