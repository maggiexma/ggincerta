library(tidyverse)

set.seed(10086)

# Load example spatial data
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# Add simulated estimate and standard error
nc <- nc %>%
  mutate(
    value = rnorm(nrow(.)),
    sd    = rnorm(nrow(.))
  )

ggplot(nc, aes(fill = value)) +
  geom_sf(colour = "white", size = 0.1) +
  scale_fill_distiller(palette = 'Blues')

StatBivariate <- ggproto('StatBivariate', Stat,
                         required_aes = c('estimate', 'error'),
                         compute_panel = function(data, scales, terciles, bound, flipAxis, coord) {
                           data <- StatSf$compute_panel(data, scales, coord)
                           x <- data$estimate
                           y <- data$error
                           if (terciles) {
                             x_breaks <- unique(quantile(x, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE))
                             y_breaks <- unique(quantile(y, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE))
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
                           data$fill <- as.factor(combo)
                           
                           data
                         })

stat_sf_bivariate <- function(mapping = NULL, data = NULL, geom = 'sf', 
                              position = 'identity', na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE, 
                              terciles = TRUE, flipAxis = FALSE, ...) {
  c(
    layer_sf(
      stat = StatBivariate,
      geom = geom,
      data = data,
      mapping = mapping,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        terciles = terciles,
        flipAxis = flipAxis,
        coord = coord_sf(),
        ...
      )
    ),
    coord_sf()
  )
}
