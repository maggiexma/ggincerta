test_that("geom-point-bivariate works", {

  ggplot(anscombe, aes(x1, x2)) +
    geom_point_bivariate(aes(color = vc(y1, y2)), size = 10)


  get_guide_data(last_plot(), "colour")


  get_guide_data
  layer_data(last_plot())

})
