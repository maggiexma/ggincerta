test_that("scale-bivariate", {
  ggplot(nc) +
    geom_sf(aes(fill = duo(value, sd)))


  ggplot(anscombe, aes(x1, x2)) +
    geom_point(aes(color = duo(y1, y2)), size = 10)


})
