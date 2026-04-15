StatSfCentre <- ggproto(
  "StatSfCentre",
  StatSf,

  required_aes = "geometry",

  make_polygon = function(self,
                          x,
                          y,
                          radius,
                          centre_shape,
                          n_circle = 40,
                          angle,
                          size) {
    radius <- radius * size

    if (centre_shape == "circle") {
      theta <- seq(0, 2 * pi, length.out = n_circle + 1)
      pts <- cbind(x + radius * cos(theta + angle), y + radius * sin(theta + angle))
    } else if (centre_shape == "square") {
      theta <- seq(0, 2 * pi, length.out = 5)[-5] + pi / 4 + angle
      pts <- cbind(x + radius * cos(theta), y + radius * sin(theta))
      pts <- rbind(pts, pts[1, , drop = FALSE])
    } else if (centre_shape == "triangle") {
      theta <- seq(0, 2 * pi, length.out = 4)[-4] + pi / 2 + angle
      pts <- cbind(x + radius * cos(theta), y + radius * sin(theta))
      pts <- rbind(pts, pts[1, , drop = FALSE])
    } else if (centre_shape == "hex") {
      theta <- seq(0, 2 * pi, length.out = 7)[-7] + angle
      pts <- cbind(x + radius * cos(theta), y + radius * sin(theta))
      pts <- rbind(pts, pts[1, , drop = FALSE])
    } else {
      stop("centre_shape must be one of: circle, square, triangle, hex")
    }

    sf::st_polygon(list(pts))
  },

  compute_panel = function(self,
                           data,
                           scales,
                           coord,
                           centre_shape,
                           angle,
                           size,
                           point_fun) {
    data <- StatSf$compute_panel(data, scales, coord)
    sf_obj <- sf::st_as_sf(data)
    crs <- sf::st_crs(sf_obj)

    if (is.na(crs)) {
      warning("Input sf object has no CRS; radius is in raw coordinate units.")
    }

    centres <- point_fun(sf_obj)
    centre_xy <- sf::st_coordinates(centres)

    bbox <- sf::st_bbox(sf_obj)
    radius <- min(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin) / 50

    polys <- lapply(seq_len(nrow(centre_xy)), function(i) {
      self$make_polygon(
        x = centre_xy[i, "X"],
        y = centre_xy[i, "Y"],
        radius = radius,
        centre_shape = centre_shape,
        angle = angle,
        size = size
      )
    })

    sf::st_geometry(sf_obj) <- sf::st_sfc(polys, crs = crs)
    sf_obj
  }
)

geom_sf_centre <- function(mapping = NULL,
                           data = NULL,
                           position = "identity",
                           ...,
                           colour = NA,
                           centre_shape = "circle",
                           angle = 0,
                           size = 1,
                           point_fun = sf::st_centroid,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  list(
    layer_sf(
      stat = StatSfCentre,
      geom = "sf",
      data = data,
      mapping = mapping,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        centre_shape = centre_shape,
        angle = angle,
        size = size,
        colour = colour,
        point_fun = point_fun,
        na.rm = na.rm,
        ...
      )
    ),
    coord_sf()
  )
}
