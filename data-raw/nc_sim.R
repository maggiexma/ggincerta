library(sf)
library(dplyr)
library(spdep)
library(MASS)

load("data/nc.rda")

simulate_car_value <- function(sf_data, rho, seed) {
  set.seed(seed)

  nb <- poly2nb(sf_data)
  W <- nb2mat(nb, style = "B", zero.policy = TRUE)

  D <- diag(rowSums(W))
  Q <- D - rho * W + diag(1e-6, nrow(W))

  car_value <- MASS::mvrnorm(n = 1,
                             mu = rep(0, nrow(sf_data)),
                             Sigma = solve(Q))

  drop(scale(car_value))
}

simulate_pattern_strength <- function(sf_data,
                                      noise_medium,
                                      noise_strong,
                                      hotspot_bandwidth,
                                      hotspot_power,
                                      hotspot_candidate_n,
                                      rho,
                                      seed) {
  n <- nrow(sf_data)

  sf_projected <- st_transform(sf_data, 5070)

  centroids <- st_centroid(st_geometry(sf_projected))
  coordinates <- st_coordinates(centroids)

  x <- scale(coordinates[, 1])
  y <- scale(coordinates[, 2])

  trend_signal <- drop(scale(1.2 * x - 0.8 * y))

  map_centre <- st_centroid(st_union(st_geometry(sf_projected)))

  distance_to_centre <- st_distance(centroids, map_centre) |>
    units::drop_units()

  candidate_ids <- order(distance_to_centre)

  candidate_ids <- candidate_ids[seq_len(min(hotspot_candidate_n, n))]

  set.seed(seed)
  hotspot_id <- sample(candidate_ids, size = 1)

  hotspot_distance <- st_distance(centroids, centroids[hotspot_id]) |>
    units::drop_units()

  hotspot_distance <- hotspot_distance /
    max(hotspot_distance)

  hotspot_raw <- exp(-(hotspot_distance^2) /
                       (2 * hotspot_bandwidth^2))

  hotspot_signal <- drop(scale(hotspot_raw^hotspot_power))

  car_signal <- simulate_car_value(sf_data = sf_data,
                                   rho = rho,
                                   seed = seed + 1)

  set.seed(seed + 100)
  trend_noise_medium <- rnorm(n)

  set.seed(seed + 101)
  trend_noise_strong <- rnorm(n)

  set.seed(seed + 200)
  hotspot_noise_medium <- rnorm(n)

  set.seed(seed + 201)
  hotspot_noise_strong <- rnorm(n)

  set.seed(seed + 300)
  car_noise_medium <- rnorm(n)

  set.seed(seed + 301)
  car_noise_strong <- rnorm(n)

  trend_medium <-
    trend_signal +
    noise_medium * trend_noise_medium

  trend_strong <-
    trend_signal +
    noise_strong * trend_noise_strong

  hotspot_medium <-
    hotspot_signal +
    noise_medium * hotspot_noise_medium

  hotspot_strong <-
    hotspot_signal +
    noise_strong * hotspot_noise_strong

  car_medium <-
    car_signal +
    noise_medium * car_noise_medium

  car_strong <-
    car_signal +
    noise_strong * car_noise_strong

  simulated_data <- bind_rows(
    sf_data |>
      mutate(
        pattern = "Linear trend",
        strength = "medium",
        noise_level = noise_medium,
        value_sim = trend_medium
      ),

    sf_data |>
      mutate(
        pattern = "Linear trend",
        strength = "strong",
        noise_level = noise_strong,
        value_sim = trend_strong
      ),

    sf_data |>
      mutate(
        pattern = "Hotspot",
        strength = "medium",
        noise_level = noise_medium,
        value_sim = hotspot_medium
      ),

    sf_data |>
      mutate(
        pattern = "Hotspot",
        strength = "strong",
        noise_level = noise_strong,
        value_sim = hotspot_strong
      ),

    sf_data |>
      mutate(
        pattern = "CAR smooth field",
        strength = "medium",
        noise_level = noise_medium,
        value_sim = car_medium
      ),

    sf_data |>
      mutate(
        pattern = "CAR smooth field",
        strength = "strong",
        noise_level = noise_strong,
        value_sim = car_strong
      )
  )

  simulated_data |>
    mutate(
      pattern = factor(
        pattern,
        levels = c("Linear trend", "Hotspot", "CAR smooth field")
      ),
      strength = factor(strength, levels = c("medium", "strong"))
    )
}

nc_sim <- simulate_pattern_strength(
  sf_data = nc,
  noise_medium = 1,
  noise_strong = 0.25,
  hotspot_bandwidth = 0.12,
  hotspot_power = 0.3,
  hotspot_candidate_n = 20,
  rho = 0.9,
  seed = 1
)

usethis::use_data(nc_sim, overwrite = TRUE)
