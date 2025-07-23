library(sf)
library(dplyr)

set.seed(10086)

tmp_zip <- file.path(tempdir(), "SA3_2021_SHP.zip")
download.file(
  url = "https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files/SA3_2021_AUST_SHP_GDA2020.zip",
  destfile = tmp_zip,
  mode = "wb"
)

unzip(tmp_zip, exdir = tmpdir <- tempdir())

shp <- list.files(tmpdir, pattern = "\\.shp$", full.names = TRUE)[1]

sa3_sf <- st_read(shp, quiet = TRUE)

sa3 <- sa3_sf %>%
  select(SA3_CODE21, SA3_NAME21, geometry) %>%
  mutate(
    value = rnorm(n()),
    sd = abs(rnorm(n(), mean = 0.3, sd = 0.1))
  )

usethis::use_data(sa3, internal = FALSE, overwrite = TRUE)
