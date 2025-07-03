library(ggplot2)
library(sf)

nc_centroids <- st_centroid(nc)

nc_centroids <- cbind(
  nc_centroids,
  st_coordinates(nc_centroids) %>%
    as.data.frame() %>%
    setNames(c("x", "y"))
)

set.seed(2025)
nc_centroids <- nc_centroids %>%
  mutate(
    estimate = rnorm(n()),
    error    = runif(n(), 0, 1)
  )

StatPointBivariate <- ggproto(
  "StatPointBivariate", Stat,
  required_aes = c("estimate", "error", "x", "y"),

  compute_panel = function(data, scales, terciles, flipAxis) {
    x <- data$estimate
    y <- data$error

    if (terciles) {
      x_breaks <- unique(quantile(x, probs = c(0,1/3,2/3,1), na.rm = TRUE))
      y_breaks <- unique(quantile(y, probs = c(0,1/3,2/3,1), na.rm = TRUE))
    } else {
      x_breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 4)
      y_breaks <- seq(min(y, na.rm = TRUE), max(y, na.rm = TRUE), length.out = 4)
    }

    if (!flipAxis) {
      est_bin <- cut(x, breaks = x_breaks, include.lowest = TRUE, labels = FALSE)
      err_bin <- cut(y, breaks = y_breaks, include.lowest = TRUE, labels = FALSE)
    } else {
      est_bin <- cut(y, breaks = y_breaks, include.lowest = TRUE, labels = FALSE)
      err_bin <- cut(x, breaks = x_breaks, include.lowest = TRUE, labels = FALSE)
    }

    n_err <- length(unique(err_bin))
    combo <- (est_bin - 1) * n_err + err_bin

    data$fill <- factor(combo)

    data
  }
)

stat_point_bivariate <- function(mapping = NULL,
                                 data = NULL,
                                 geom = "point",
                                 position = "identity",
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE,
                                 terciles = TRUE,
                                 flipAxis = FALSE,
                                 ...) {
  layer(
    stat = StatPointBivariate,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      terciles = terciles,
      flipAxis = flipAxis,
      ...
    )
  )
}

ggplot(nc_centroids,
       aes(x = x, y = y)) +
  stat_point_bivariate(
    mapping = aes(
      estimate = estimate,
      error = error,
      fill = after_stat(fill)
    ),
    size = 4,
    shape = 21,
    terciles = TRUE,
    flipAxis = FALSE
  ) +
  scale_fill_bivariate(
    colrange = list(colour = c("gold","red4"), difC = c(4,4))
  ) +
  coord_equal() +
  theme_minimal()
