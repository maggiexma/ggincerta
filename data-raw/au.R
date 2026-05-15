library(sf)

sa3 <- sf::st_read("/Users/maxueqi/Vizu/AU shape/sa3.shp")
sa3_3857 <- sf::st_transform(sa3, 3857) |>
  dplyr::mutate(
    value = rnorm(dplyr::n()),
    sd = runif(dplyr::n(), 0.5, 4),

    value_log10 = 10^rnorm(dplyr::n()),
    sd_log2 = 2^runif(dplyr::n(), 0.5, 4)
  )
