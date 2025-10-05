test_that("geom-point-bivariate works", {

  ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point_bivariate(aes(v1 = Petal.Length, v2 = Petal.Width),
                         shape = "circle filled")

})
