library(tidyverse)

set.seed(10086)

# Load example spatial data
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# Add simulated estimate and standard error
nc <- mutate(nc, value = rnorm(n()), sd = rnorm(n()))

# Define a custom stat with reasonable defaults. These are just examples. Choose your own defaults.
stat_sf_bivariate <- function(mapping = aes(geometry), data = NULL, geom = "sf",
                              position = "identity", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE, 
                              palette = RColorBrewer::brewer.pal(9, "Reds"),
                              terciles = TRUE, flipAxis = FALSE, ...) {
  
  # Note: Might need to revisit the difference between `layer()` and `layer_sf()`
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
        palette = palette,
        terciles = terciles,
        flipAxis = flipAxis,
        coord = coord_sf(),
        ...
      )
    ),
    # Automatically adds coord_sf() to avoid requiring the user to specify it
    coord_sf()
  )
}

StatBivariate <- ggproto(
  "StatBivariate", Stat,
  required_aes = c("estimate", "se", "geometry"),
  
  compute_panel = function(data, scales, palette, terciles, flipAxis, coord) {
    browser()
    # Key fix: Call the parent compute_panel method to ensure proper format for geom_sf
    data <- StatSf$compute_panel(data, scales, coord)
    
    x <- data$estimate
    y <- data$se
    
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
    
    data$fill <- palette[combo]
    
    data
  }
)

# Plot using the custom stat
p <- ggplot() +
  stat_sf_bivariate(
    data = nc,
    mapping = aes(
      estimate = value,
      se = sd,
      geometry = geometry
    )
  )

print(p)
