test_that("geom-point-bivariate works", {

  ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point_bivariate(aes(color = vc(Petal.Length, Petal.Width)))

})
