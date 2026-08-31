library(sf)
library(dplyr)
library(spdep)
library(MASS)

load("data/nc.rda")

nc <- nc |>
  dplyr::select(-value, -sd)

coordinates <- st_coordinates(st_centroid(st_geometry(nc)))

x <- scale(coordinates[, 1])[, 1]
y <- scale(coordinates[, 2])[, 1]

x_01 <- (x - min(x)) / (max(x) - min(x))

signal_trend <- 1.2 * x - 0.8 * y
signal_trend <- scale(signal_trend)[, 1]

signal_hotspot_x <- 0.5
signal_hotspot_y <- 0.2

signal_distance_sq <- (x - signal_hotspot_x)^2 + (y - signal_hotspot_y)^2

signal_hotspot <- exp(-signal_distance_sq / 0.5)

signal_hotspot <- scale(signal_hotspot)[, 1]

sd_constant <- rep(1, nrow(nc))

sd_gradient <- 0.6 + 0.8 * x_01

uncertainty_hotspot_x <- signal_hotspot_x
uncertainty_hotspot_y <- signal_hotspot_y

uncertainty_distance_sq <- (x - uncertainty_hotspot_x)^2 + (y - uncertainty_hotspot_y)^2

uncertainty_shape <- exp(-uncertainty_distance_sq / 0.9)

uncertainty_01 <- (uncertainty_shape - min(uncertainty_shape)) / (max(uncertainty_shape) - min(uncertainty_shape))

sd_hotspot <- 0.6 + 0.8 * uncertainty_01

set.seed(2026)

trend_constant <- nc |>
  mutate(
    signal_pattern = "Linear",
    uncertainty_pattern = "Constant",
    signal = signal_trend,
    sd = sd_constant,
    noise = rnorm(n(), mean = 0, sd = sd),
    value_sim = signal + noise
  )

trend_gradient <- nc |>
  mutate(
    signal_pattern = "Linear",
    uncertainty_pattern = "Gradient",
    signal = signal_trend,
    sd = sd_gradient,
    noise = rnorm(n(), mean = 0, sd = sd),
    value_sim = signal + noise
  )

trend_hotspot <- nc |>
  mutate(
    signal_pattern = "Linear",
    uncertainty_pattern = "Hotspot",
    signal = signal_trend,
    sd = sd_hotspot,
    noise = rnorm(n(), mean = 0, sd = sd),
    value_sim = signal + noise
  )

hotspot_constant <- nc |>
  mutate(
    signal_pattern = "Hotspot",
    uncertainty_pattern = "Constant",
    signal = signal_hotspot,
    sd = sd_constant,
    noise = rnorm(n(), mean = 0, sd = sd),
    value_sim = signal + noise
  )

hotspot_gradient <- nc |>
  mutate(
    signal_pattern = "Hotspot",
    uncertainty_pattern = "Gradient",
    signal = signal_hotspot,
    sd = sd_gradient,
    noise = rnorm(n(), mean = 0, sd = sd),
    value_sim = signal + noise
  )

hotspot_hotspot <- nc |>
  mutate(
    signal_pattern = "Hotspot",
    uncertainty_pattern = "Hotspot",
    signal = signal_hotspot,
    sd = sd_hotspot,
    noise = rnorm(n(), mean = 0, sd = sd),
    value_sim = signal + noise
  )

nc_sim <- bind_rows(
  hotspot_constant,
  hotspot_gradient,
  hotspot_hotspot,
  trend_constant,
  trend_gradient,
  trend_hotspot
)

nc_sim$signal_pattern <- factor(
  nc_sim$signal_pattern,
  levels = c("Hotspot", "Linear"),
  labels = c("Hotspot", "Linear")
)

nc_sim$uncertainty_pattern <- factor(
  nc_sim$uncertainty_pattern,
  levels = c("Constant", "Gradient", "Hotspot"),
  labels = c(
    "Constant SD",
    "Gradient SD",
    "Hotspot SD"
  )
)
# value_mean <- mean(nc_sim$value_sim)
# value_sd <- sd(nc_sim$value_sim)
#
# nc_sim <- nc_sim |>
#   mutate(
#     value_sim = (value_sim - value_mean) / value_sd,
#     sd = sd / value_sd
#   )

usethis::use_data(nc_sim, overwrite = TRUE)
