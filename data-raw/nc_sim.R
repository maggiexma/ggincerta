library(sf)
library(dplyr)
library(spdep)
library(MASS)

load("data/nc.rda")

# simulate_car_value <- function(sf_data, rho, seed) {
#   set.seed(seed)
#
#   nb <- poly2nb(sf_data)
#   W <- nb2mat(nb, style = "B", zero.policy = TRUE)
#
#   D <- diag(rowSums(W))
#   Q <- D - rho * W + diag(1e-6, nrow(W))
#
#   car_value <- MASS::mvrnorm(n = 1,
#                              mu = rep(0, nrow(sf_data)),
#                              Sigma = solve(Q))
#
#   drop(scale(car_value))
# }
#
# simulate_pattern_strength <- function(sf_data,
#                                       noise_medium,
#                                       noise_strong,
#                                       hotspot_bandwidth,
#                                       hotspot_power,
#                                       hotspot_candidate_n,
#                                       rho,
#                                       seed) {
#   n <- nrow(sf_data)
#
#   sf_projected <- st_transform(sf_data, 5070)
#
#   centroids <- st_centroid(st_geometry(sf_projected))
#   coordinates <- st_coordinates(centroids)
#
#   x <- scale(coordinates[, 1])
#   y <- scale(coordinates[, 2])
#
#   trend_signal <- drop(scale(1.2 * x - 0.8 * y))
#
#   map_centre <- st_centroid(st_union(st_geometry(sf_projected)))
#
#   distance_to_centre <- st_distance(centroids, map_centre) |>
#     units::drop_units()
#
#   candidate_ids <- order(distance_to_centre)
#
#   candidate_ids <- candidate_ids[seq_len(min(hotspot_candidate_n, n))]
#
#   set.seed(seed)
#   hotspot_id <- sample(candidate_ids, size = 1)
#
#   hotspot_distance <- st_distance(centroids, centroids[hotspot_id]) |>
#     units::drop_units()
#
#   hotspot_distance <- hotspot_distance / max(hotspot_distance)
#
#   hotspot_raw <- exp(-(hotspot_distance^2) / (2 * hotspot_bandwidth^2))
#
#   hotspot_signal <- drop(scale(hotspot_raw^hotspot_power))
#
#   car_signal <- simulate_car_value(sf_data = sf_data,
#                                    rho = rho,
#                                    seed = seed + 1)
#
#   set.seed(seed + 100)
#   trend_noise_medium <- rnorm(n)
#
#   set.seed(seed + 101)
#   trend_noise_strong <- rnorm(n)
#
#   set.seed(seed + 200)
#   hotspot_noise_medium <- rnorm(n)
#
#   set.seed(seed + 201)
#   hotspot_noise_strong <- rnorm(n)
#
#   set.seed(seed + 300)
#   car_noise_medium <- rnorm(n)
#
#   set.seed(seed + 301)
#   car_noise_strong <- rnorm(n)
#
#   trend_medium <- trend_signal + noise_medium * trend_noise_medium
#
#   trend_strong <- trend_signal + noise_strong * trend_noise_strong
#
#   hotspot_medium <- hotspot_signal + noise_medium * hotspot_noise_medium
#
#   hotspot_strong <- hotspot_signal + noise_strong * hotspot_noise_strong
#
#   car_medium <- car_signal + noise_medium * car_noise_medium
#
#   car_strong <- car_signal + noise_strong * car_noise_strong
#
#   simulated_data <- bind_rows(
#     sf_data |>
#       mutate(
#         pattern = "Linear trend",
#         strength = "medium",
#         noise_level = noise_medium,
#         value_sim = trend_medium
#       ),
#
#     sf_data |>
#       mutate(
#         pattern = "Linear trend",
#         strength = "strong",
#         noise_level = noise_strong,
#         value_sim = trend_strong
#       ),
#
#     sf_data |>
#       mutate(
#         pattern = "Hotspot",
#         strength = "medium",
#         noise_level = noise_medium,
#         value_sim = hotspot_medium
#       ),
#
#     sf_data |>
#       mutate(
#         pattern = "Hotspot",
#         strength = "strong",
#         noise_level = noise_strong,
#         value_sim = hotspot_strong
#       ),
#
#     sf_data |>
#       mutate(
#         pattern = "CAR smooth field",
#         strength = "medium",
#         noise_level = noise_medium,
#         value_sim = car_medium
#       ),
#
#     sf_data |>
#       mutate(
#         pattern = "CAR smooth field",
#         strength = "strong",
#         noise_level = noise_strong,
#         value_sim = car_strong
#       )
#   )
#
#   simulated_data |>
#     mutate(
#       pattern = factor(
#         pattern,
#         levels = c("Linear trend", "Hotspot", "CAR smooth field")
#       ),
#       strength = factor(strength, levels = c("medium", "strong"))
#     )
# }
#
# nc_sim <- simulate_pattern_strength(
#   sf_data = nc,
#   noise_medium = 1,
#   noise_strong = 0.25,
#   hotspot_bandwidth = 0.12,
#   hotspot_power = 0.3,
#   hotspot_candidate_n = 20,
#   rho = 0.9,
#   seed = 1
# )
#
# usethis::use_data(nc_sim, overwrite = TRUE)

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

nc_sim1 <- bind_rows(
  hotspot_constant,
  hotspot_gradient,
  hotspot_hotspot,
  trend_constant,
  trend_gradient,
  trend_hotspot
)

nc_sim1$signal_pattern <- factor(
  nc_sim1$signal_pattern,
  levels = c("Hotspot", "Linear"),
  labels = c("Hotspot", "Linear")
)

nc_sim1$uncertainty_pattern <- factor(
  nc_sim1$uncertainty_pattern,
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

usethis::use_data(nc_sim1, overwrite = TRUE)
