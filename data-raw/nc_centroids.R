library(sf)
library(dplyr)

set.seed(10086)

nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>%
  mutate(
    value = rnorm(n()),
    sd = rnorm(n())
  )

nc_centroids <- st_centroid(nc)

usethis::use_data(nc_centroids, internal = FALSE, overwrite = TRUE)
