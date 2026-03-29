library(sf)
library(dplyr)

set.seed(10086)

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) |>
  dplyr::mutate(
    value = rnorm(dplyr::n()),
    sd = runif(dplyr::n(), 0.5, 4),

    value_log10 = 10^runif(dplyr::n(), -1, 2),
    sd_log2 = 2^runif(dplyr::n(), -2, 3)
  )

usethis::use_data(nc, internal = FALSE, overwrite = TRUE)

