StatSfPin <- ggproto(
  "StatSfPin",
  StatSf,

  required_aes = "geometry",
  optional_aes = "angle",
  default_aes = aes(),

  make_polygon = function(self,
                          x,
                          y,
                          radius,
                          shape,
                          n_circle = 40,
                          angle = 0,
                          size = 1) {
    radius <- radius * size

    if (shape == "circle") {
      theta <- seq(0, 2 * pi, length.out = n_circle + 1)
      pts <- cbind(x + radius * cos(theta), y + radius * sin(theta))

    } else if (shape == "square") {
      theta <- seq(0, 2 * pi, length.out = 5)[-5] + pi / 4 + angle
      pts <- cbind(x + radius * cos(theta), y + radius * sin(theta))
      pts <- rbind(pts, pts[1, , drop = FALSE])

    } else if (shape == "triangle") {
      theta <- seq(0, 2 * pi, length.out = 4)[-4] + pi / 2 + angle
      pts <- cbind(x + radius * cos(theta), y + radius * sin(theta))
      pts <- rbind(pts, pts[1, , drop = FALSE])

    } else if (shape == "hex") {
      theta <- seq(0, 2 * pi, length.out = 7)[-7] + pi / 6 + angle
      pts <- cbind(x + radius * cos(theta), y + radius * sin(theta))
      pts <- rbind(pts, pts[1, , drop = FALSE])

    } else if (shape == "drop") {
      x1 <- seq(-3, 3, 0.05)
      y1 <- sqrt(pmax(0, 9 - x1^2))
      cir <- cbind(x1, y1)

      tri <- cbind(c(-3, 0, 3), c(0, -5, 0))
      pts0 <- rbind(cir, tri)
      pts0 <- pts0 / max(abs(pts0))

      R <- matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), nrow = 2)
      pts <- pts0 %*% R
      pts <- cbind(x + radius * pts[, 1], y + radius * pts[, 2])
      pts <- rbind(pts, pts[1, , drop = FALSE])

    } else {
      cli::cli_abort(
        "{.arg shape} must be one of {.val circle}, {.val square}, {.val triangle}, {.val hex}, or {.val drop}."
      )
    }

    sf::st_polygon(list(pts))
  },

  angle_to_theta = function(self, vals, max_angle = NULL) {
    vals <- as.numeric(vals)

    if (!length(vals) || !any(is.finite(vals))) {
      return(rep(0, length(vals)))
    }

    maxv <- if (is.null(max_angle)) {
      max(abs(vals[is.finite(vals)]))
    } else {
      max_angle
    }

    if (!is.finite(maxv) || maxv == 0) {
      return(rep(0, length(vals)))
    }

    theta <- (vals / maxv) * pi
    theta[!is.finite(theta)] <- 0
    theta
  },

  compute_panel = function(self,
                           data,
                           scales,
                           coord,
                           shape,
                           max_angle = NULL,
                           size,
                           point_fun,
                           na.rm = FALSE) {
    data <- StatSf$compute_panel(data, scales, coord)

    sf_obj <- sf::st_as_sf(data)
    crs <- sf::st_crs(sf_obj)

    centres <- point_fun(sf_obj)
    centre_xy <- sf::st_coordinates(centres)

    bbox <- sf::st_bbox(sf_obj)
    radius <- min(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin) / 50

    theta <- if ("angle" %in% names(data)) {
      self$angle_to_theta(vals = data$angle, max_angle = max_angle)
    } else {
      rep(0, nrow(data))
    }

    polys <- lapply(seq_len(nrow(data)), function(i) {
      self$make_polygon(
        x = centre_xy[i, "X"],
        y = centre_xy[i, "Y"],
        radius = radius,
        shape = shape,
        angle = theta[i],
        size = size
      )
    })

    sf::st_geometry(sf_obj) <- sf::st_sfc(polys, crs = crs)
    sf_obj
  }
)

GeomSfPin <- ggproto(
  "GeomSfPin",
  GeomSf,

  default_aes = aes(
    colour = "black",
    angle = NA,
    linewidth = 0.2,
    linetype = 1,
    alpha = NA
  ),

  draw_panel = function(self,
                        data,
                        panel_params,
                        coord,
                        legend = NULL,
                        lineend = "butt",
                        linejoin = "round",
                        linemitre = 10,
                        na.rm = FALSE,
                        border_colour = NA) {
    data$fill <- data$colour

    if (!is.na(border_colour)) {
      data$colour <- border_colour
    }

    GeomSf$draw_panel(
      data = data,
      panel_params = panel_params,
      coord = coord,
      legend = legend,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      na.rm = na.rm
    )
  }
)

geom_sf_pin <- function(mapping = NULL,
                        data = NULL,
                        position = "identity",
                        ...,
                        shape = "circle",
                        max_angle = NULL,
                        size = 1,
                        point_fun = sf::st_point_on_surface,
                        border_colour = NA,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  list(
    layer_sf(
      stat = StatSfPin,
      geom = GeomSfPin,
      data = data,
      mapping = mapping,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        shape = shape,
        max_angle = max_angle,
        size = size,
        point_fun = point_fun,
        border_colour = border_colour,
        na.rm = na.rm,
        ...
      )
    ),
    coord_sf()
  )
}
