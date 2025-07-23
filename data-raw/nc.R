library(sf)
library(dplyr)

set.seed(10086)

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>%
  mutate(
    value = rnorm(n()),
    sd = rnorm(n())
  )

usethis::use_data(nc, internal = FALSE, overwrite = TRUE)

